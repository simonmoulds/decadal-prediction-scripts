#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(sf)
library(rnrfa)
library(lubridate)
library(RcppRoll)
library(yaml)

options(dplyr.summarise.inform = FALSE)

if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  config = read_yaml(args[1])
  aggr_period = args[2]
  output_root = args[3]
  args = commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}
source(file.path(cwd, "utils.R")) # TODO eventually put utils in package
config = parse_config(config)

study_period = 1960:2005
extended_study_period = 1960:2015
climate_vars = c("nao", "ea", "amv", "european_precip", "uk_precip", "uk_temp")

observed_discharge_data =
  open_dataset(file.path(output_root, "nrfa-discharge-summaries")) %>%
  collect()
station_ids = observed_discharge_data$ID %>% unique
n_stations = length(station_ids)

## Parse aggregation period specification
period = config$aggregation_period[[aggr_period]]
lead_tm = period$lead_time
start = min(lead_tm)
end = max(lead_tm)
study_period = period$study_period
save_obs = period$observed
save_fcst = period$hindcast
label = period$name
error_label = period$error_name

## output_dir = file.path(output_root, "hindcast-analysis", label, "input")
output_dir = file.path(output_root, "analysis", label, "input")

## Load observed climate data
obs = get_obs(output_root, extended_study_period, start = start, end = end)

## Load ensemble data
ensemble_fcst = read_parquet(
  file.path(output_root, "analysis", label, "matched_ensemble.parquet")
) %>%
##   file.path(output_root, config$output_data$hindcast, label, "matched_ensemble.parquet")
## ) %>%
  mutate(lag = init_year - init_year_matched + 1)

ensemble_fcst_error = read_parquet(
  file.path(output_root, "analysis", error_label, "matched_ensemble_error.parquet")
) %>%
##   file.path(output_root, config$output_data$hindcast, error_label, "matched_ensemble_error.parquet")
## ) %>%
  mutate(across(contains("init_year"), as.integer)) %>%
  arrange(source_id, member, init_year, init_year_matched)

## Clean output subdirectory
## aggr_output_dir = file.path(hindcast_output_dir, label, "input")
## ## aggr_output_dir = file.path(output_root, "hindcast-analysis", label, "input")
## if (dir.exists(aggr_output_dir))
##   unlink(aggr_output_dir, recursive = TRUE)
## dir.create(aggr_output_dir, recursive = TRUE)

## Loop through catchments to create input datasets for statistical modelling
cat(sprintf("Creating catchment dataset for accumulation period %s\n", label))
pb = txtProgressBar(min=0, max=n_stations, initial=0)
for (i in 1:n_stations) {

  ## Select discharge data for current station
  stn_id = station_ids[i]
  dis_djfm =
    observed_discharge_data %>%
    filter(ID %in% stn_id) %>%
    filter(clim_season %in% "DJFM") %>%
    arrange(season_year)

  ## Compute summary statistics for DJFM
  dis_djfm_aggregated = rolling_fun(
    dis_djfm$season_year,
    dis_djfm,
    cols = c(
      "missing_pct",
      "Q_max", "Q_mean", "Q_05", "Q_50", "Q_90", "Q_95",
      "P_sum", "POT_1", "POT_2", "POT_3", "POT_4"
    ),
    funs = list(
      missing_pct = mean,
      Q_max = mean, Q_mean = mean, Q_05 = mean,
      Q_50 = mean, Q_90 = mean, Q_95 = mean,
      P_sum = mean, POT_1 = sum, POT_2 = sum,
      POT_3 = sum, POT_4 = sum
    ),
    start = start, end = end
  )

  ## Join with complete timeseries and update missing_pct
  complete_annual_ts = tibble(init_year = study_period)
  dis_djfm_aggregated =
    complete_annual_ts %>%
    left_join(dis_djfm_aggregated, by=c("init_year")) %>%
    mutate(missing_pct = ifelse(is.na(missing_pct), 100, missing_pct))

  ## Save observed data (observed discharge + observed climate indices)
  ## if (save_obs) {
  ## Compute anomalies and standardize
  standardize = function(x) return((x - mean(x)) / sd(x))
  obs_aggregated =
    obs %>%
    filter(init_year %in% study_period) %>%
    mutate(across(all_of(climate_vars), standardize))
  ## Join with observed discharge data
  dis_djfm_obs =
    dis_djfm_aggregated %>%
    left_join(obs_aggregated, by="init_year")
  ## ## Write dataset to file
  ## output_dir = file.path(output_root, "observed-analysis", label, "input")
  ## if (!dir.exists(output_dir)) {
  ##   dir.create(output_dir, recursive = TRUE)
  ## }
  ## dis_djfm_obs <-
  dis_djfm_obs %>%
    rename(year = init_year) %>%
    mutate(lead_time = min(lead_tm), period = label, subset = "observed", ID = stn_id) %>%
    arrange(year) %>%
    group_by(subset, ID) %>%
    write_dataset(output_dir, format = "parquet")
    ## mutate(period = label, ID = stn_id, subset = "observed")
    ## mutate(period = label, ID = stn_id) %>%
    ## group_by(ID) %>%
    ## write_dataset(output_dir, format = "parquet")
  ## }

  ## Save hindcast data (observed discharge + hindcast climate indices)
  ## if (save_fcst) {
  ## Make ensemble forecasts
  for (k in 1:length(config$subset)) {
    subset = config$subset[[k]]
    ## TODO ensure `value` in ensemble_fcst is anomaly
    ensemble_fcst_subset = create_ensemble_forecast(
      ensemble_fcst_error,
      ensemble_fcst,
      vars = climate_vars,
      model_select = subset$models,
      project_select = subset$projects,
      full = subset$full,
      best_n = subset$best_n,
      worst_n = subset$worst_n,
      n_select = 20
    )
    fcst =
      dis_djfm_aggregated %>%
      left_join(ensemble_fcst_subset, by = "init_year")
      ## ensemble_fcst_subset %>%
      ## right_join(dis_djfm_aggregated, by = "init_year")
    fcst %>%
      rename(year = init_year) %>%
      mutate(lead_time = min(lead_tm), period = label, subset = subset$name, ID = stn_id) %>%
      arrange(year) %>%
      group_by(subset, ID) %>%
      write_dataset(output_dir, format = "parquet")
  }
  ## }
  ## Update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)
## }

## meta = data.frame(
##   id = ukbn2_stations,
##   gdf_start_date = as.Date(start_date, format="%Y-%m-%d"),
##   gdf_end_date = as.Date(end_date, format="%Y-%m-%d")
## ) %>% as_tibble()

## ## Write metadata to file
## write_parquet(
##   meta,
##   file.path(output_dir, "ukbn2_catchments_metadata.parquet")
## )

## ## Specify the CAMELS GB data directory
## datadir = "/home/simon/Data/8344e4f3-d2ea-44f5-8afa-86d2987543a9/data"

## boundaries = st_read(file.path(datadir, "CAMELS_GB_catchment_boundaries.shp"))

## ## Read some metadata which we can use to select gauging stations
## hydrometry_attr = read_csv(
##   file.path(
##     datadir,
##     "CAMELS_GB_hydrometry_attributes.csv"
##   ),
##   show_col_types=FALSE
## )
## human_infl_attr = read_csv(
##   file.path(
##     datadir,
##     "CAMELS_GB_humaninfluence_attributes.csv"
##   ),
##   show_col_types=FALSE
## )
## camels_metadata = left_join(
##   hydrometry_attr,
##   human_infl_attr,
##   by="gauge_id"
## )

## camels_metadata_subset =
##   camels_metadata %>%
##   ## mutate(
##   ##   flow_period_duration = time_length(
##   ##     flow_period_end - flow_period_start,
##   ##     "years"
##   ##   )
##   ## ) #%>%
##   filter(flow_perc_complete > 95 & flow_period_start <= as.Date("1970-10-01"))

## gauge_selection = camels_metadata_subset$gauge_id

## rolling_fun = function(x, yrs, start=2, end=9, stat=mean) {
##   ## Compute rolling n-year means.
##   ##
##   ## Args:
##   ##   x     : numeric. Time series data.
##   ##   start : integer. Index of start point (where the current index = 1)
##   ##   end   : integer. Index of end point.
##   ##
##   ## Return:
##   ##   numeric of length(x)
##   xstat = rep(NA, length(x))
##   period_start = rep(NA, length(x))
##   for (i in 1:(length(x)-end)) {
##     start_index = i + start - 1
##     end_index = i + end - 1
##     xstat[i] = stat(x[start_index:end_index], na.rm=TRUE)
##     period_start[i] = yrs[start_index]
##   }
##   df = data.frame(
##     period_start=period_start,
##     forecast_year=yrs,
##     xstat=xstat
##   ) %>% as_tibble()
##   df
## }

## for (i in 1:length(gauge_selection)) {
##   id = gauge_selection[i]
##   fn = list.files(
##     file.path(datadir, 'timeseries'),
##     paste0('CAMELS_GB_hydromet_timeseries_', id, "_[0-9]{8}-[0-9]{8}.csv"),
##     full.names=TRUE
##   )
##   if (length(fn) != 1) {
##     stop()
##   }

##   ts = read_csv(fn[1], show_col_types=FALSE)

##   ## separate date into year/month/day so that
##   ## we can group on month later on
##   ts =
##     ts %>%
##     mutate(date = as.Date(date)) %>%
##     mutate(
##       year = format(date, "%Y") %>% as.numeric,
##       month = format(date, "%m") %>% as.numeric,
##       day = format(date, "%d") %>% as.numeric
##     )

##   ## here we first group by month and compute monthly
##   ## statistics, then further aggregate by season.
##   season_ts =
##     ts %>%
##     ## Group by month and compute summary statistics
##     group_by(year, month) %>%
##     summarise(
##       Q_max = max(discharge_vol, na.rm=TRUE),
##       P_max = max(precipitation, na.rm=TRUE),
##       P_mean = mean(precipitation, na.rm=TRUE),
##       T_mean = mean(temperature, na.rm=TRUE),
##       PET_mean = mean(pet, na.rm=TRUE),
##       count=n(),
##       availability=sum(!is.na(discharge_vol))
##     ) %>%
##     ## Summarise statistics by season
##     mutate(season_year = ifelse(month %in% c(1,2,3), year-1, year)) %>%
##     mutate(
##       season = case_when(
##         month %in% c(12, 1, 2, 3) ~ "DJFM",
##         month %in% c(4, 5) ~ "AM",
##         month %in% c(6, 7, 8, 9) ~ "JJAS",
##         month %in% c(10, 11) ~ "ON"
##       )
##     ) %>%
##     group_by(season_year, season) %>%
##     summarise(
##       Q_max = max(Q_max),
##       P_max = max(P_max),
##       P_mean = mean(P_mean),
##       T_mean = mean(T_mean),
##       PET_mean = mean(PET_mean),
##       availability=sum(availability) / sum(count)
##     ) %>%
##     ## Combine statistic with season (e.g. Q_max_DJFM)
##     gather(key=key, value=value, -season_year, -season) %>%
##     unite(key, key, season) %>%
##     spread(key=key, value=value)
##   season_ts = season_ts %>% left_join(obs_nao)

##   out_fn = file.path("data", paste0("camels_gb_seasonal_summary_", id, ".csv"))
##   season_ts %>% write_csv(out_fn)

##   ## ## Mean annual maximum discharge during 8-year period
##   ## discharge_vol_max =
##   ##   rolling_fun(
##   ##     month_ts$discharge_vol_max,
##   ##     month_ts$season_year,
##   ##     stat=mean
##   ##   ) %>%
##   ##   rename(Q_max = xstat)

##   ## ## Mean DJFM precipitation during 8-year period
##   ## prec =
##   ##   rolling_fun(
##   ##     month_ts$precipitation_max,
##   ##     month_ts$season_year,
##   ##     stat=mean
##   ##   ) %>%
##   ##   rename(P_mean = xstat)

##   ## ## Mean DJFM temperature during 8-year period
##   ## temp =
##   ##   rolling_fun(
##   ##     month_ts$temperature_mean,
##   ##     month_ts$season_year,
##   ##     stat=mean
##   ##   ) %>%
##   ##   rename(T_mean = xstat)

##   ## pet =
##   ##   rolling_fun(
##   ##     month_ts$pet_mean,
##   ##     month_ts$season_year,
##   ##     stat=mean
##   ##   ) %>%
##   ##   rename(PET_mean = xstat)

##   ## df =
##   ##   discharge_vol_max %>%
##   ##   left_join(prec) %>%
##   ##   left_join(temp) %>%
##   ##   left_join(pet) %>%
##   ##   filter(forecast_year %in% 1970:2005)

##   ## df = month_ts %>% mutate(forecast_year=season_year)
## }