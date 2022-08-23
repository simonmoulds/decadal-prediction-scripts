## Author : Simon Moulds
## Date   : Nov-Dec 2021

library(tidyverse)
library(zoo)
library(lubridate)

check_file_exists = function(fn) {
  if (file.exists(fn)) {
    return(fn)
  } else {
    stop(sprintf("File %s does not exist", fn))
  }
}

parse_config_observed = function(config) {
  items = config$observed
  giss = items$giss %>% check_file_exists()
  gpcc = items$gpcc %>% check_file_exists()
  hadcrut4 = items$hadcrut4 %>% check_file_exists()
  hadslp2r = items$hadslp2r %>% check_file_exists()
  ncdc = items$ncdc %>% check_file_exists()
  list(giss = giss, gpcc = gpcc, hadcrut4 = hadcrut4, hadslp2r = hadslp2r, ncdc = ncdc)
}

parse_config_ensemble = function(config) {
  ## TODO
  return(config$ensemble)
}

parse_config_aux = function(config) {
  ## TODO
  return(config$aux)
}

parse_config_output = function(config) {
  ## TODO
  return(config$output)
}

parse_config_subset = function(config) {
  if (!"subset" %in% names(config))
    return(NULL)

  nms = sapply(config$subset, FUN=function(x) x$name)
  config_subset =
    vector(mode = "list", length = length(nms)) %>%
    setNames(nms)

  for (i in 1:length(nms)) {
    item = config$subset[[i]]
    nm = item$name
    if (is.null(item$best_n)) {
      n = NA
      best_n = FALSE
    } else {
      n = as.integer(item$best_n)
      best_n = TRUE
    }
    if (is.null(item$worst_n)) {
      n = NA
      worst_n = FALSE
    } else {
      n = as.integer(item$worst_n)
      worst_n = TRUE
    }
    if (best_n & worst_n) {
      stop("Only one of `best_n` or `worst_n` may be supplied")
    }
    full = FALSE
    if (!any(best_n, worst_n)) {
      full = TRUE
    }
    projects = ifelse(is.null(item$projects), NA, item$projects)
    models = ifelse(is.null(item$models), NA, item$models)
    config_subset[[nm]] =
      list(name = nm,
           full = full,
           best_n = best_n,
           worst_n = worst_n,
           n = n,
           projects = projects,
           models = models)
  }
  config_subset
}

parse_config_aggregation_period = function(config) {
  nms = sapply(config$aggregation_period, FUN=function(x) x$name)
  config_aggregation_period =
    vector(mode = "list", length = length(nms)) %>%
    setNames(nms)

  for (i in 1:length(nms)) {
    nm = nms[i]
    item = config$aggregation_period[[i]]
    error_name = ifelse(is.null(item$error_name), nm, item$error_name)
    lead_time = eval(str2lang(as.character(item$lead_time)))
    study_period = eval(str2lang(as.character(item$study_period)))
    if (is.null(lead_time)) stop("`lead_time` cannot be missing")
    if (is.null(study_period)) stop("`study_period` cannot be missing")
    observed = ifelse(is.null(item$observed), FALSE, as.logical(item$observed))
    hindcast = ifelse(is.null(item$hindcast), FALSE, as.logical(item$hindcast))
    lag = ifelse(is.null(item$lag), FALSE, as.logical(item$lag))
    n_lag = ifelse(is.null(item$n_lag), 4, as.integer(item$n_lag))
    constant_lead_time = ifelse(is.null(item$constant_lead_time), TRUE, as.logical(item$constant_lead_time))
    constant_fcst_period = ifelse(is.null(item$constant_fcst_period), FALSE, as.logical(item$constant_fcst_period))
    constant_lead_time = lag & constant_lead_time
    constant_fcst_period = lag & constant_fcst_period & !constant_lead_time
    config_aggregation_period[[nm]] =
      list(name = nm,
           error_name = error_name,
           lead_time = lead_time,
           study_period = study_period,
           observed = observed,
           hindcast = hindcast,
           lag = lag,
           n_lag = n_lag,
           constant_fcst_period = constant_fcst_period,
           constant_lead_time = constant_lead_time)
  }
  config_aggregation_period
}

parse_config_modelling = function(config) {
  datasets = sapply(config$modelling, FUN=function(x) x$name)
  keys = c(
    "name", "input_dataset", "predictand",
    "model_family", "aggregation_periods",
    "study_period", "formulas",
    "sigma_formulas", "subsets"
  )
  config_modelling =
    vector(mode = "list", length = length(datasets)) %>%
    setNames(datasets)

  for (i in 1:length(datasets)) {
    dataset = datasets[[i]]
    opt_list =
      vector(mode = "list", length=length(keys)) %>%
      setNames(keys)

    for (j in 1:length(keys)) {
      key = keys[[j]]
      opt_list[[key]] = config$modelling[[i]][[key]]
    }
    opt_list$study_period = eval(str2lang(opt_list$study_period))
    opt_list$formulas = lapply(opt_list$formulas, FUN=function(x) as.formula(x))
    model_nms = names(opt_list$formulas)
    if (is.null(opt_list$sigma_formulas)) {
      sigma_formulas = list()
      for (j in 1:length(model_nms)) {
        nm = model_nms[j]
        sigma_formulas[[nm]] = as.formula("~1")
      }
    }
    opt_list$sigma_formulas = sigma_formulas
    if (!isTRUE(all.equal(model_nms, names(opt_list$sigma_formulas)))) {
      print(model_nms)
      print(names(opt_list$sigma_formulas))
      stop("model names not the same")
    }
    config_modelling[[dataset]] = opt_list
  }
  return(config_modelling)
}

parse_config = function(config) {
  observed_section = parse_config_observed(config)
  ensemble_section = parse_config_ensemble(config)
  aux_data_section = parse_config_aux(config)
  output_section = parse_config_output(config)
  subset_section = parse_config_subset(config)
  aggregation_period_section = parse_config_aggregation_period(config)
  modelling_section = parse_config_modelling(config)
  ## TODO checks (e.g. aggregation periods used in modelling section defined)
  list(observed_data = observed_section,
       ensemble_data = ensemble_section,
       aux_data = aux_data_section,
       output_data = output_section,
       subset = subset_section,
       aggregation_period = aggregation_period_section,
       modelling = modelling_section)
}

parse_config_io = function(config) {
  observed_section = parse_config_observed(config)
  ensemble_section = parse_config_ensemble(config)
  aux_data_section = parse_config_aux(config)
  output_section = parse_config_output(config)
  ## subset_section = parse_config_subset(config)
  aggregation_period_section = parse_config_aggregation_period(config)
  ## modelling_section = parse_config_modelling(config)
  ## TODO checks (e.g. aggregation periods used in modelling section defined)
  list(observed_data = observed_section,
       ensemble_data = ensemble_section,
       aux_data = aux_data_section,
       output_data = output_section,
       ## subset = subset_section,
       aggregation_period = aggregation_period_section)
       ## modelling = modelling_section)
}

get_obs = function(filename, study_period, start = 2, end = 9) {
  ## Read raw observed data
  obs_raw = read_parquet(filename) #file.path(dir, "obs.parquet"))
  ## Pivot from long to wide
  obs_raw = obs_raw %>% pivot_wider(names_from=variable, values_from=value)
  ## Assign a reference year to DJFM and select this season
  obs = obs_raw %>%
    mutate(season_year = ifelse(month %in% c(1,2,3), year-1, year)) %>%
    filter(month %in% c(12, 1, 2, 3)) %>%
    group_by(season_year) %>%
    filter(n() == 4) # Only complete DJFM seasons
  vars = c("nao", "ea", "amv", "european_precip", "uk_precip", "uk_temp")

  ## Compute average seasonal values
  obs = obs %>% summarize(across(all_of(vars), mean))

  ## Calculate decadal means [function defined in `utils.R`]
  ## N.B. in this data frame season year is the year in which
  ## the start of the season falls [i.e. for DJFM it is the
  ## year of December]
  obs = rolling_fun(
    yrs = obs$season_year,
    data = obs,
    cols = vars,
    funs = mean,
    start = start, end = end
  )
  ## The result of the above function is that the value for each
  ## initialization year is the mean of the observed values for
  ## 2-9 years ahead. For example, the value assigned to the 1960
  ## initialization year is the average value for years 1961 to 1968.
  ## This essentially makes the observations comparable with the
  ## forecasts.

  ## Filter study period
  obs = obs %>% filter(init_year %in% study_period)
  compute_anomaly = function(x) x - mean(x, na.rm = TRUE)
  obs =
    obs %>%
    mutate(across(all_of(vars), compute_anomaly)) %>%
    ungroup()
  ## ## Convert to long format
  ## obs = obs %>% gather(variable, obs, -init_year)
  obs
}

get_hindcast_data = function(dataset, study_period, lead_times) {

  ensemble_fcst_raw =
    open_dataset(dataset) %>%
    mutate(lead_time = season_year - init_year) %>%
    filter(lead_time %in% lead_times) %>%
    collect()

  ## Pivot from long to wide format
  ensemble_fcst_raw = ensemble_fcst_raw %>%
    pivot_wider(names_from=variable, values_from=value)

  ## Unit conversion
  ensemble_fcst_raw =
    ensemble_fcst_raw %>%
    mutate(nao = nao / 100) %>%
    mutate(ea = ea / 100) %>%
    mutate(european_precip = european_precip * 60 * 60 * 24) %>%
    mutate(uk_precip = uk_precip * 60 * 60 * 24)

  ## ## Correct initialisation years for GFDL data, which appear to be incorrect
  ## gfdl_index = ensemble_fcst_raw$source_id %in% "GFDL-CM2p1"
  ## ensemble_fcst_raw$init_year[gfdl_index] = ensemble_fcst_raw$init_year[gfdl_index] - 1

  ensemble_fcst_raw_complete =
    ensemble_fcst_raw %>%
    filter(init_year %in% study_period)

  ## ## OLD:
  ## ## It's helpful to ensure that the dataset contains all
  ## ## unique combinations of experiment and initialisation
  ## ## years [e.g. CanCM4/r10i1p1/1960] so that we can
  ## ## investigate missingness. Here we do this by treating
  ## ## each model in turn and using expand_grid(...) to get
  ## ## the unique combinations. We merge this with the model
  ## ## dataframe such that any combination with missing data
  ## ## is assigned a value of NA, rather than  being absent
  ## ## from the dataset.
  ## ## all_models = unique(ensemble_fcst_raw[["source_id"]])
  ## all_years = ensemble_fcst_raw[["init_year"]] %>% unique %>% sort
  ## all_years = seq(min(all_years), max(all_years))
  ## complete_data = list()
  ## for (i in 1:length(models)) {
  ##   model = models[i]
  ##   model_ensemble_fcst = ensemble_fcst_raw %>% filter(source_id %in% model)
  ##   keys = expand_grid(
  ##     project = unique(model_ensemble_fcst[["project"]]),
  ##     source_id = model,
  ##     mip = unique(model_ensemble_fcst[["mip"]]),
  ##     member = unique(model_ensemble_fcst[["member"]]),
  ##     init_year = all_years
  ##   ) %>%
  ##     arrange(source_id, member, init_year)
  ##   model_ensemble_fcst =
  ##     model_ensemble_fcst %>%
  ##     right_join(keys)
  ##   complete_data[[i]] = model_ensemble_fcst
  ## }

  ## ## Put data together again
  ## ensemble_fcst_raw_complete = do.call(rbind, complete_data)

  ## ## Select data for study period
  ## ensemble_fcst_raw_complete =
  ##   ensemble_fcst_raw_complete %>%
  ##   filter(init_year %in% study_period)
  ## ## Investigate missingness
  ## tmp = ensemble_fcst_raw_complete
  ## missing_ensemble_members =
  ##   tmp %>%
  ##   group_by(project, source_id, init_year) %>%
  ##   summarize(ensemble_size=sum(is.na(nao))) %>%
  ##   mutate(ensemble_size=ifelse(ensemble_size == 0, NA, ensemble_size)) %>%
  ##   arrange(desc(project), desc(source_id)) %>%
  ##   filter(!is.na(ensemble_size))
  ## missing_ensemble_members

  ## ## Ensemble size considering all members:
  ## total_ensemble_size =
  ##   tmp %>%
  ##   filter(!is.na(nao)) %>%
  ##   group_by(init_year) %>%
  ##   summarize(ensemble_size=n()) %>%
  ##   filter(init_year %in% study_period)
  ## total_ensemble_size

  ## p = ggplot(data=total_ensemble_size, aes(y=ensemble_size, x=init_year)) +
  ##   geom_point() +
  ##   xlab("Year") +
  ##   ylab("Ensemble size")
  ## print(p)

  ## ggsave(
  ##   file.path(plot_outdir, "ensemble_size.png"),
  ##   p,
  ##   width = 5, height = 5, units = "in"
  ## )
  ensemble_fcst_raw_complete
}

download_nrfa_data = function(stn_id, metadata) {
  ## TODO tidy up this function, giving user more control over which variables are derived
  meta = metadata %>% filter(id %in% stn_id)
  ## Gauged daily flow [m3 s-1]
  gdf = get_ts(stn_id, "gdf") %>% as_tibble(rownames="time")
  ## Catchent daily rainfall [mm]
  cdr = try(get_ts(stn_id, "cdr") %>% as_tibble(rownames="time"))
  if (inherits(cdr, "try-error"))
    cdr <- gdf %>% rename(cdr = gdf) %>% mutate(cdr = NA)

  ## Create complete time series, in case the
  ## raw time series has missing values.
  start_date = gdf$time[1]
  end_date = gdf$time[nrow(gdf)]
  complete_ts = seq.POSIXt(
    as.POSIXct(start_date, tz="GMT", format="%Y-%m-%d"),
    as.POSIXct(end_date, tz="GMT", format="%Y-%m-%d"),
    by="1 day"
  ) %>% as.Date() %>% as.character()
  gdf =
    tibble(time = complete_ts) %>%
    left_join(gdf, by="time")
  availability = sum(!is.na(gdf$gdf)) / nrow(gdf) * 100
  df =
    gdf %>%
    left_join(cdr, by="time") %>%
    mutate(ID=stn_id, .after=time) %>%
    mutate(time = as.Date(time))
  ## TODO Filter out years with fewer than 330 days of records
  df =
    df %>%
    mutate(year = format(time, "%Y") %>% as.integer) %>%
    mutate(month = format(time, "%m") %>% as.integer)
  ## Neri et al [https://doi.org/10.1002/joc.5915]:
  ## "To avoid double counting the same event, we only consider
  ## one event in a window of +/- 5 days + logarithm of the
  ## drainage area"
  catchment_area = meta[["catchment_area"]]
  window_size = ((5 + log(catchment_area * 0.386102)) * 2) %>% round()
  ## Solari et al [https://doi.org/10.1002/2016WR019426]:
  ## "As the moving window travels through the series, each
  ## time that the data maximum in the window is located at
  ## its center, the maximum is regarded as a peak"
  df =
    df %>% # Neri et al
    mutate(pot = roll_max(gdf, n=window_size, align="center", fill=NA)) %>%
    mutate(is_peak = Vectorize(isTRUE)(pot == gdf)) %>%
    mutate(peak = ifelse(is_peak, pot, NA))
  ## Unsure whether we need to make this unique or not?
  peaks = df$pot[df$is_peak] ##%>% unique()
  n_years = length(df$year %>% unique())
  peaks_sorted = sort(peaks, decreasing = TRUE)
  threshold_1 = peaks_sorted[n_years]
  threshold_2 = peaks_sorted[n_years * 2]
  threshold_3 = peaks_sorted[n_years * 3]
  threshold_4 = peaks_sorted[n_years * 4]

  df =
    df %>%
    mutate(pot_1 = ifelse(Vectorize(isTRUE)(peak >= threshold_1), 1, 0)) %>%
    mutate(pot_2 = ifelse(Vectorize(isTRUE)(peak >= threshold_2), 1, 0)) %>%
    mutate(pot_3 = ifelse(Vectorize(isTRUE)(peak >= threshold_3), 1, 0)) %>%
    mutate(pot_4 = ifelse(Vectorize(isTRUE)(peak >= threshold_4), 1, 0))

  ## Add climate season label to rows
  df =
    df %>%
    mutate(
      clim_season = case_when(
        month %in% c(12, 1, 2, 3) ~ "DJFM",
        month %in% c(4, 5) ~ "AM",
        month %in% c(6, 7, 8, 9) ~ "JJAS",
        month %in% c(10, 11) ~ "ON"
      )
    ) %>%
    mutate(season_year = ifelse(month %in% c(1, 2, 3), year - 1, year))

  ## Summarize to get flood counts
  df =
    df %>%
    group_by(ID, clim_season, season_year) %>%
    summarize(
      missing_pct = (sum(is.na(gdf)) / n()) * 100,
      Q_max = max(gdf, na.rm = TRUE),
      Q_mean = mean(gdf, na.rm = TRUE),
      Q_05 = quantile(gdf, probs = 0.05, na.rm = TRUE, names = FALSE),
      Q_50 = quantile(gdf, probs = 0.50, na.rm = TRUE, names = FALSE),
      Q_90 = quantile(gdf, probs = 0.90, na.rm = TRUE, names = FALSE),
      Q_95 = quantile(gdf, probs = 0.95, na.rm = TRUE, names = FALSE),
      P_sum = sum(cdr, na.rm = TRUE),
      POT_1 = sum(pot_1, na.rm = TRUE),
      POT_2 = sum(pot_2, na.rm = TRUE),
      POT_3 = sum(pot_3, na.rm = TRUE),
      POT_4 = sum(pot_4, na.rm = TRUE),
    ) %>%
    ungroup() %>%
    mutate(across(Q_max:POT_4, ~ifelse(is.finite(.), ., NA)))
}
## Set some default formatting options for ggplot
plot_format_objects = list(
  scale_x_continuous(breaks=seq(1960, 2010, 10), limits=c(1960, 2005)),
  labs(x="Start of 8-year period", y="NAO anomaly (hPa)")
)

rolling_fun = function(yrs, data, cols, funs, start=2, end=9) {
  ## Compute rolling n-year means.
  ##
  ## Args:
  ##   yrs   : integer. Year index
  ##   data  : data.frame.
  ##   cols  : character.
  ##   funs  : function or list of functions.
  ##   start : integer. Index of start point (where index
  ##           of the point for which the rolling mean is
  ##           being computed is 1).
  ##   end   : integer. Index of end point.
  ##
  ## Return:
  ##   Data frame
  if (is.list(funs) & (!isTRUE(all(cols == names(funs)))))
    stop()

  ## Preallocate output
  out = lapply(cols, FUN=function(x) rep(NA, length(yrs)))
  names(out) = cols
  out = c(list(init_year = yrs), out)

  ## Also collect the start year of the time window
  period_start = rep(NA, length(yrs))
  for (i in 1:(length(yrs)-end+1)) {
  ## for (i in 1:(length(yrs)-end)) {
    start_index = i + start - 1
    end_index = i + end - 1
    for (j in 1:length(cols)) {
      nm = cols[j]
      x = data[[nm]]
      if (is.function(funs)) {
        fun = funs
      } else {
        fun = funs[[nm]]
      }
      ## fun = ifelse(is.function(funs), funs, funs[[nm]])
      out[[nm]][i] = fun(x[start_index:end_index], na.rm=TRUE)
    }
    ## period_start[i] = yrs[start_index]
  }
  out = out %>% as.data.frame() %>% as_tibble()
  out
}

corr_cross_validate = function(fcst, obs, leave_out_add=0) {
  ## Compute cross validated anomaly correlation.
  ##
  ## Args:
  ##   fcst : numeric. Forecast data
  ##   obs  : numeric. Observed data
  ##   n    : integer. Number of additional
  ##          points to leave out
  ##
  ## Return:
  ##   Numeric
  ntimes = length(obs)
  index = seq(1, ntimes) # for selection
  corr = rep(0., ntimes)
  for (i in 1:ntimes) {
    if (leave_out_add > 0) {
      leave_out_start = max(c(1, i - leave_out_add))
      leave_out_end = min(c(ntimes, i + leave_out_add))
      leave_out_index = seq(leave_out_start, leave_out_end)
    } else {
      leave_out_index = c()
    }
    keep_index = !(index %in% leave_out_index)
    corr_this = cor.test(
      fcst[keep_index],
      obs[keep_index],
      method="pearson", alternative="greater"
    )
    corr[i] = unname(corr_this$estimate)
  }
  corr
}

calculate_error = function(fcst, ensemble_fcst, n_years, n_forecast, match_var) {
  ## Match ensemble members.
  ##
  ## Select n members by comparing with variance-adjusted
  ## ensemble mean data.
  ##
  ## Args:
  ##   fcst : data.frame. Ensemble mean forecast data.
  ##   ensemble_fcst : data.frame. Ensemble fcst data.
  ##   n_years       : integer. Number of years in the study period.
  ##   n_forecast    : integer. Number of forecasts to use to create
  ##                   lagged forecast.
  ##   match_var     : character. Variable to use in matching
  ##                   algorithm.
  ##   n_select      : integer. Number of members to select.
  ##   best          : bool. Whether to select the `n_select` best
  ##                   performing or the `n_select` worst performing.
  ##
  ## Return:
  ##   Data frame.

  ## Select only the forecast data for the variable against
  ## which we will perform the matching
  match_fcst =
    fcst %>%
    filter(variable %in% match_var) %>%
    rename(init_year_lag = init_year)

  ## When the ensemble data is lagged we need to calculate
  ## the absolute error between the variance-adjusted ensemble
  ## mean for time point i and the individual ensemble members
  ## which contribute to the lagged value, i.e. time point i,
  ## i-1, ..., i-n, where n is the total lag (3 in our case).
  ## Here we create a data frame which we can join with
  ## `ensemble_fcst` to account for this.
  init_year_lag =
    data.frame(init_year = study_period) %>%
    slice(
      rep(1:n_forecast, n_years - n_forecast + 1)
      + (rep(1:(n_years - n_forecast + 1), each = n_forecast) - 1)
    ) %>%
    as_tibble()
  ## init_year_lag

  ## Now we create a temporary grouping variable to compute the
  ## year to which the lagged values contribute (e.g. the lagged
  ## value for year 1963 is the mean of values for years 1960,
  ## 1961, 1962, 1963). As we lag backwards this is simply the latest
  ## year in each group. We call this year `init_year_lag`
  init_year_lag =
    init_year_lag %>%
    mutate(tmp_group = rep(1:(n() / n_forecast), each = n_forecast)) %>%
    group_by(tmp_group) %>%
    mutate(init_year_lag = max(init_year)) %>%
    ungroup() %>%
    dplyr::select(-tmp_group)

  ## ## Have a look at a few examples:
  ## init_year_lag %>% filter(init_year_lag %in% 1965)
  ## init_year_lag %>% filter(init_year_lag %in% 1983)
  ## init_year_lag %>% filter(init_year_lag %in% 2005)

  ## Join the data frame with ensemble_fcst, which approximately
  ## quadruples its size (values are duplicated when they are
  ## included in a different lag year (i.e. `init_year_lag`))
  ensemble_fcst_lag =
    ensemble_fcst %>%
    left_join(init_year_lag, by = "init_year") %>%
    arrange(source_id, member, init_year_lag, init_year)

  ## Join the datasets
  ensemble_fcst_lag =
    ensemble_fcst_lag %>%
    left_join(match_fcst, by = c("variable", "init_year_lag"))

  ## Calculate mean absolute error for the matching variable
  ensemble_fcst_lag =
    ensemble_fcst_lag %>%
    filter(variable %in% match_var) %>%
    mutate(error = abs(std - ens_mean_lag_std))

  ## OLD:

  ## ## For each lagged year, select the n best performing members:
  ## nao_matched_ensemble_fcst =
  ##   ensemble_fcst_lag %>%
  ##   group_by(source_id, member, init_year_lag) %>%
  ##   mutate(min_error_year = init_year[which.min(error)]) %>%
  ##   filter(init_year %in% min_error_year) %>%
  ##   ## summarise(error = min(error, na.rm=TRUE)) #%>%
  ##   ungroup() %>%
  ##   group_by(init_year_lag) ## %>%
  ## ## slice_min(error, n=n_select)

  ## if (best) {
  ##   nao_matched_ensemble_fcst =
  ##     nao_matched_ensemble_fcst %>%
  ##     slice_min(error, n=n_select)
  ## } else {
  ##   nao_matched_ensemble_fcst =
  ##     nao_matched_ensemble_fcst %>%
  ##     slice_max(error, n=n_select)
  ## }

  ## ## Tidy up
  ## nao_matched_ensemble_fcst =
  ##   nao_matched_ensemble_fcst %>%
  ##   dplyr::select(project, source_id, mip, member, init_year, init_year_lag)

  ## ## Join with original ensemble data
  ## nao_matched_ensemble_fcst =
  ##   nao_matched_ensemble_fcst %>%
  ##   left_join(ensemble_fcst) %>%
  ##   rename(init_year_matched = init_year) %>%
  ##   rename(init_year = init_year_lag)

  ## ## OLD 2:

  ## ## For each lagged year, select the n best performing members:
  ## nao_matched_ensemble_fcst =
  ##   ensemble_fcst_lag %>%
  ##   group_by(source_id, member, init_year_lag) %>%
  ##   mutate(min_error_year = init_year[which.min(error)]) %>%
  ##   filter(init_year %in% min_error_year) %>%
  ##   ## summarise(error = min(error, na.rm=TRUE)) #%>%
  ##   ungroup() ## %>%
  ##   ## group_by(init_year_lag) ## %>%
  ## ## slice_min(error, n=n_select)

  ## nao_matched_ensemble_fcst =
  ##   nao_matched_ensemble_fcst %>%
  ##   dplyr::select(project, source_id, mip, member, init_year, init_year_lag, error)

  ## ## Join with original ensemble data
  ## nao_matched_ensemble_fcst =
  ##   nao_matched_ensemble_fcst %>%
  ##   ## left_join(ensemble_fcst) %>% # join on init_year
  ##   rename(init_year_matched = init_year) %>%
  ##   rename(init_year = init_year_lag)

  ## ## Return matched forecast
  ## nao_matched_ensemble_fcst

  ## NEW:

  ## For each lagged year, select the n best performing members:
  nao_matched_ensemble_fcst =
    ensemble_fcst_lag %>%
    ## group_by(source_id, member, init_year_lag) %>%
    ## mutate(min_error_year = init_year[which.min(error)]) %>%
    ## filter(init_year %in% min_error_year) %>%
    ## ## summarise(error = min(error, na.rm=TRUE)) #%>%
    ungroup() ## %>%
    ## group_by(init_year_lag) ## %>%
  ## slice_min(error, n=n_select)

  nao_matched_ensemble_fcst =
    nao_matched_ensemble_fcst %>%
    dplyr::select(project, source_id, mip, member, init_year, init_year_lag, error)

  ## Join with original ensemble data
  nao_matched_ensemble_fcst =
    nao_matched_ensemble_fcst %>%
    ## left_join(ensemble_fcst) %>% # join on init_year
    rename(init_year_matched = init_year) %>%
    rename(init_year = init_year_lag)

  ## Return matched forecast
  nao_matched_ensemble_fcst
}

## create_annual_ensemble_forecast = function(ensemble_fcst_error,
##                                            ensemble_fcst_raw,
##                                            vars = climate_vars,
##                                            model_select = NA,
##                                            project_select = NA,
##                                            full = TRUE,
##                                            best_n = FALSE,
##                                            worst_n = FALSE,
##                                            n_select = 20,
##                                            lead_times = 2,
##                                            lag = TRUE) {

##   stop("Not yet implemented")
##   if (length(lead_times) == 1) {
##     if (lag) {
##       lead_times = seq(lead_times, lead_times + 3)
##       ensemble_fcst_raw = ensemble_fcst_raw %>% filter(lead_time %in% lead_times)
##       ensemble_fcst_raw = ensemble_fcst_raw %>% group_by(season_year)
##     }
##   }
## }

## create_multiyear_ensemble_forecast = function() {}
create_ensemble_forecast = function(ensemble_fcst_error,
                                    ensemble_fcst,
                                    vars = climate_vars,
                                    model_select = NA,
                                    project_select = NA,
                                    full = TRUE,
                                    best_n = FALSE,
                                    worst_n = FALSE,
                                    n_select = 20) {
                                    ## lead_times = c(2:9),
                                    ## lag = TRUE,
                                    ## n_lag = 4) {

  ## Filter by models
  models = ensemble_fcst$source_id %>% unique() %>% toupper()
  if (!is.na(model_select)) {
    model_select = toupper(model_select)
    if (!all(model_select %in% models)) {
      stop(paste0("Invalid model specification. Valid models are:\n", paste0(models, collapse = ", ")))
    }
    ensemble_fcst =
      ensemble_fcst %>%
      filter(toupper(source_id) %in% model_select)
  }

  ## Filter by projects (i.e. CMIP5/6)
  projects = ensemble_fcst$project %>% unique() %>% toupper()
  if (!is.na(project_select)) {
    project_select = toupper(project_select)
    if (!all(project_select %in% projects)) {
      stop(paste0("Invalid project specification. Valid projects are:\n", paste0(projects, collapse = ", ")))
    }
    ensemble_fcst =
      ensemble_fcst %>%
      filter(toupper(project) %in% project_select)
  }

  # Only allow models without NA values [notable at present is CESM1-1-CAM5]
  nao_matched_ensemble_fcst_subset =
    ensemble_fcst_error %>%
    filter(!any_na)

  ## Select n best (worst) performing members
  if (best_n | worst_n) {
    ## `init_year_matched` is the actual initialization year of the member
    ## `init_year` is the initialization year of the current forecast
    slice_fun = slice_min
    if (worst_n) {
      slice_fun = slice_max
    }
    nao_matched_ensemble_fcst_subset =
      nao_matched_ensemble_fcst_subset %>%
      group_by(init_year) %>%
      slice_fun(error, n = n_select)
  }
  nao_matched_ensemble_fcst_subset =
    nao_matched_ensemble_fcst_subset %>%
    dplyr::select(-any_na, -error)

  ## Join with forecast data
  ensemble_fcst = ensemble_fcst %>% dplyr::select(-std) %>% pivot_wider(names_from = variable, values_from = value)
  ensemble_fcst =
    nao_matched_ensemble_fcst_subset %>%
    left_join(ensemble_fcst, by = c("project", "mip", "source_id", "member", "init_year_matched", "init_year"))

  ## Compute ensemble mean for each year
  ensemble_group_vars = c("init_year")
  ensemble_fcst =
    ensemble_fcst %>%
    group_by_at(ensemble_group_vars) %>%
    summarize(across(all_of(vars), mean, na.rm = TRUE))
  ensemble_fcst
}

## create_ensemble_forecast_old = function(ensemble_fcst_error,
##                                     ensemble_fcst_raw,
##                                     vars = climate_vars,
##                                     model_select = NA,
##                                     project_select = NA,
##                                     full = TRUE,
##                                     best_n = FALSE,
##                                     worst_n = FALSE,
##                                     n_select = 20,
##                                     lead_times = c(2:9),
##                                     lag = TRUE,
##                                     n_lag = 4) {

##   ## ## Filter by lead time
##   ## ensemble_fcst_raw =
##   ##   ensemble_fcst_raw %>%
##   ##   filter(lead_time %in% lead_times)
##   ## anomaly_group_vars = c("source_id", "member", "lead_time")
##   ## anomalyfun = function(x) x - mean(x, na.rm = TRUE)
##   ## ensemble_fcst_raw =
##   ##   ensemble_fcst_raw %>%
##   ##   group_by_at(anomaly_group_vars) %>%
##   ##   mutate(across(all_of(vars), anomalyfun))

##   ## Filter by models
##   models = ensemble_fcst_raw$source_id %>% unique() %>% toupper()
##   if (!is.na(model_select)) {
##     model_select = toupper(model_select)
##     if (!all(model_select %in% models)) {
##       stop(paste0("Invalid model specification. Valid models are:\n", paste0(models, collapse = ", ")))
##     }
##     ensemble_fcst_raw = ensemble_fcst_raw %>% filter(toupper(source_id) %in% model_select)
##   }

##   ## Filter by projects (i.e. CMIP5/6)
##   projects = ensemble_fcst_raw$project %>% unique() %>% toupper()
##   if (!is.na(project_select)) {
##     project_select = toupper(project_select)
##     if (!all(project_select %in% projects)) {
##       stop(paste0("Invalid project specification. Valid projects are:\n", paste0(projects, collapse = ", ")))
##     }
##     ensemble_fcst_raw = ensemble_fcst_raw %>% filter(toupper(project) %in% project_select)
##   }

##   # Only allow models without NA values [notable at present is CESM1-1-CAM5]
##   nao_matched_ensemble_fcst_subset =
##     ensemble_fcst_error %>%
##     filter(!any_na)

##   ## Select n best (worst) performing members
##   if (best_n | worst_n) {
##     ## `init_year_matched` is the actual initialization year of the member
##     ## `init_year` is the initialization year of the current forecast
##     slice_fun = slice_min
##     if (worst_n) {
##       slice_fun = slice_max
##     }
##     nao_matched_ensemble_fcst_subset =
##       nao_matched_ensemble_fcst_subset %>%
##       group_by(init_year) %>%
##       slice_fun(error, n = n_select)
##   }
##   nao_matched_ensemble_fcst_subset =
##     nao_matched_ensemble_fcst_subset %>%
##     dplyr::select(-any_na, -error)

##   stop()

##   ## Aggregate over lead times (multi-year forecast only)
##   if (length(lead_times) > 1) {
##     group_vars = c(
##       "project", "mip", #"experiment",
##       "source_id", "member", "init_year"
##     )
##     ## Average over multi-year period
##     ensemble_fcst_raw =
##       ensemble_fcst_raw %>%
##       filter(lead_time %in% lead_times) %>%
##       group_by_at(group_vars) %>%
##       summarize(across(all_of(vars), mean))
##     ## Compute anomaly
##     anomaly_group_vars = c("source_id", "member")
##   } else {
##     lead_times = seq(lead_tm, lead_tm + n_lag - 1)
##     ensemble_fcst_raw =
##       ensemble_fcst_raw %>%
##       filter(lead_time %in% lead_times)
##     anomaly_group_vars = c("source_id", "member", "lead_time")
##   }
##   anomalyfun = function(x) x - mean(x, na.rm = TRUE)
##   ensemble_fcst_raw =
##     ensemble_fcst_raw %>%
##     group_by_at(anomaly_group_vars) %>%
##     mutate(across(all_of(vars), anomalyfun))

##   stop()

##   ## Rename init_year prior to merging with subset
##   ensemble_fcst_raw =
##     ensemble_fcst_raw %>%
##     rename(init_year_matched = init_year)

##   ## Join with NAO-matched members
##   if (lag) {
##     ensemble_fcst_raw =
##       nao_matched_ensemble_fcst_subset %>%
##       ## filter(lag %in% 1) %>%
##       left_join(
##         ensemble_fcst_raw,
##         by=c("project", "mip", "source_id", "member", "init_year_matched")
##       )
##   } else {
##     ensemble_fcst_raw =
##       nao_matched_ensemble_fcst_subset %>%
##       filter(lag %in% 1) %>%
##       left_join(
##         ensemble_fcst_raw,
##         by=c("project", "mip", "source_id", "member", "init_year_matched")
##       )
##   }

##   ## ## Recalculate seasonal_averageson_year if output is not aggregated
##   ## if (!aggregate) {
##   ##   ensemble_fcst_raw =
##   ##     ensemble_fcst_raw %>%
##   ##     mutate(season_year = init_year + lead_time)
##   ## }

##   ensemble_group_vars = c("init_year")

##   ## Compute ensemble mean for each year
##   ensemble_fcst_raw =
##     ensemble_fcst_raw %>%
##     group_by_at(ensemble_group_vars) %>%
##     summarize(across(all_of(vars), mean, na.rm = TRUE))

##   ensemble_fcst_raw
## }

compute_quantiles = function(newdata,
                             data,
                             ...,
                             quantiles = c(0.5, 0.25, 0.75, 0.05, 0.95),
                             model_family) {
  ## Create centiles for a list of models
  ##
  ## Args:
  ##   data:         data.frame. Data to use for model prediction
  ##   ...:          model objects
  ##   quantiles:    numeric. quantiles to compute.
  ##   model_family: character. Currently only GA and PO are supported.
  ##
  ## Returns:
  ##   Tibble.

  ## Extract model names from dots
  dots = match.call(expand.dots=FALSE)$...
  model_nms = names(dots)
  models = list(...)
  names(models) = model_nms
  id_cols = c("ID", "clim_season", "year", "lead_time")
  quantile_names = paste0("Q", formatC(quantiles * 100, width=2, flag=0))
  computed_quantiles = list()
  for (i in 1:length(model_nms)) {
    nm = model_nms[i]
    model = models[[nm]]
    tbl =
      rep(NA, length(quantile_names) + 1) %>%
      setNames(c("model", quantile_names)) %>%
      as.list() %>% as_tibble()
    tbl[["model"]] = nm
    if (!is.null(model)) {
      mu = predict(
        models[[nm]],
        newdata = newdata,
        what = "mu",
        type = "response",
        data = data
      )
      if (model_family %in% c("GA")) { # TODO add more cases
        sigma = predict(
          models[[nm]],
          newdata = newdata,
          what = "sigma",
          type = "response",
          data = data
        )
      }
      for (j in 1:length(quantile_names)) {
        q = quantiles[j]
        qnm = quantile_names[j]
        if (model_family == "GA") {
          tbl[[qnm]] = qGA(q, mu, sigma)
        } else if (model_family == "PO") {
          tbl[[qnm]] = qPO(q, mu)
        }
      }
    }
    tbl = cbind(
      tbl,
      newdata %>% dplyr::select(any_of(id_cols))
    )
    computed_quantiles[[i]] = tbl
  }
  computed_quantiles =
    do.call("rbind", computed_quantiles) %>%
    as_tibble()
  computed_quantiles
}

fit_models = function(formulas,
                      sigma_formulas,
                      model_family,
                      data,
                      ...) {

  model_names = names(formulas)
  if (!isTRUE(all.equal(model_names, names(sigma_formulas)))) {
    stop()
  }
  fitted_models = list()
  for (i in 1:length(model_names)) {
    model_nm = model_names[i]
    model = try(
      gamlss(
        formula = formulas[[i]],
        sigma.formula = sigma_formulas[[i]],
        family = model_family,
        trace = FALSE,
        data = data,
        ...
      )
    )
    if (inherits(model, "try-error")) model = NULL
    fitted_models[[model_nm]] = model
  }
  fitted_models
}

get_aic = function(model_list) {
  aic = sapply(
    models,
    FUN=function(x) ifelse(is.null(x), NA, AIC(x))
  ) %>% as_tibble_row()
  aic
}

mean_square_error_skill_score = function(obs, exp) {
  ## mse = mean((exp - obs) ^ 2)
  ## mse_ref = mean((mean(obs) - obs) ^ 2)
  ## msss = 1 - (mse / mse_ref)
  ## correlation
  r = cor(obs, exp, method = "pearson")
  ## potential skill [= coefficient of determination]
  ps = r ^ 2
  ## slope reliability
  srel = (r - (sd(exp) / sd(obs))) ^ 2
  ## standardized mean error
  sme = ((mean(exp) - mean(obs)) / sd(obs)) ^ 2
  msss = ps - srel - sme
  list(msss = msss, ps = ps, srel = srel, sme = sme)
}

## plot_discrete_cbar = function(breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
##                               palette = "Greys", # RColorBrewer palette to use
##                               colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette), # Alternatively, manually set colors
##                               direction = 1, # Flip colors? Can be 1 or -1
##                               spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
##                               border_color = NA, # NA = no border color
##                               legend_title = NULL,
##                               legend_direction = "horizontal", # Can be "horizontal" or "vertical"
##                               font_size = 5,
##                               expand_size = 1, # Controls spacing around legend plot
##                               spacing_scaling = 1, # Multiplicative factor for label and legend title spacing
##                               width = 0.1, # Thickness of color bar
##                               triangle_size = 0.1 # Relative width of +-Inf triangles
##                               ) {

##   ## Inspired by this issue discussion:
##   ## https://github.com/tidyverse/ggplot2/issues/2673
##   ## Code more or less copied from this SO answer:
##   ## https://stackoverflow.com/a/50540633

##   require(ggplot2)
##   if (!(spacing %in% c("natural", "constant"))) {
##     stop("spacing must be either 'natural' or 'constant'")
##   }

##   if (!(direction %in% c(1, -1))) {
##     stop("direction must be either 1 or -1")
##   }

##   if (!(legend_direction %in% c("horizontal", "vertical"))) {
##     stop("legend_direction must be either 'horizontal' or 'vertical'")
##   }

##   breaks = as.numeric(breaks)
##   new_breaks = sort(unique(breaks))
##   if (any(new_breaks != breaks)) {
##     warning("Wrong order or duplicated breaks")
##   }
##   breaks = new_breaks

##   if (class(colors) == "function") {
##     colors = colors(length(breaks) - 1)
##   }

##   if (length(colors) != length(breaks) - 1) {
##     stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
##   }

##   if (!missing(colors)) {
##     warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")
##   }

##   if (direction == -1) {
##     colors = rev(colors)
##   }

##   inf_breaks = which(is.infinite(breaks))
##   if (length(inf_breaks) != 0) {
##     breaks = breaks[-inf_breaks]
##   }
##   plotcolors = colors
##   n_breaks = length(breaks)
##   labels = breaks

##   if (spacing == "constant") {
##     breaks = 1:n_breaks
##   }

##   r_breaks = range(breaks)
##   cbar_df = data.frame(
##     stringsAsFactors = FALSE,
##     y = breaks,
##     yend = c(breaks[-1], NA),
##     color = as.character(1:n_breaks)
##   )[-n_breaks,]
##   xmin = 1 - width/2
##   xmax = 1 + width/2

##   cbar_plot = ggplot(
##     cbar_df,
##     aes(
##       xmin=xmin, xmax = xmax, ymin = y, ymax = yend,
##       fill = factor(color, levels = 1:length(colors))
##     )
##   ) +
##     geom_rect(show.legend = FALSE, color=border_color)

##   if (any(inf_breaks == 1)) { # Add < arrow for -Inf
##     firstv = breaks[1]
##     polystart = data.frame(
##       x = c(xmin, xmax, 1),
##       y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
##     )
##     plotcolors = plotcolors[-1]
##     cbar_plot = cbar_plot +
##       geom_polygon(
##         data=polystart, aes(x=x, y=y),
##         show.legend = FALSE,
##         inherit.aes = FALSE,
##         fill = colors[1],
##         color=border_color
##       )
##   }

##   if (any(inf_breaks > 1)) { # Add > arrow for +Inf
##     lastv = breaks[n_breaks]
##     polyend = data.frame(
##       x = c(xmin, xmax, 1),
##       y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
##     )
##     plotcolors = plotcolors[-length(plotcolors)]
##     cbar_plot = cbar_plot +
##       geom_polygon(
##         data=polyend, aes(x=x, y=y),
##         show.legend = FALSE,
##         inherit.aes = FALSE,
##         fill = colors[length(colors)],
##         color=border_color
##       )
##   }

##   if (legend_direction == "horizontal") { #horizontal legend
##     mul = 1
##     x = xmin
##     xend = xmax
##     cbar_plot = cbar_plot + coord_flip()
##     angle = 0
##     legend_position = xmax + 0.1 * spacing_scaling
##   } else { # vertical legend
##     mul = -1
##     x = xmax
##     xend = xmin
##     angle = -90
##     legend_position = xmax + 0.2 * spacing_scaling
##   }

##   cbar_plot = cbar_plot +
##     ## Modify this section to control tick sizes
##     geom_segment(
##       data=data.frame(y = breaks, yend = breaks),
##       aes(y=y, yend=yend),
##       x = x - 0 * mul * spacing_scaling, xend = xend,
##       ## x = x - 0.05 * mul * spacing_scaling, xend = xend,
##       inherit.aes = FALSE
##     ) +
##     annotate(
##       geom = 'text', x = x - 0.1 * mul * spacing_scaling, y = breaks,
##       label = labels,
##       size = font_size
##     ) +
##     scale_x_continuous(expand = c(expand_size,expand_size)) +
##     scale_fill_manual(values=plotcolors) +
##     theme_void()

##   if (!is.null(legend_title)) { # Add legend title
##     cbar_plot = cbar_plot +
##       annotate(geom = 'text', x = legend_position, y = mean(r_breaks),
##                label = legend_title,
##                angle = angle,
##                size = font_size)
##   }
##   cbar_plot
## }

## myplotfun1 = function(ensemble_fcst, fcst, varname, ylab) {
##   ## Plot individual ensemble members with ensemble mean
##   ##
##   ## Args:
##   ##   ensemble_fcst : data.frame. Ensemble forecast data.
##   ##   fcst          : data.frame. Ensemble mean forecast data.
##   ##   varname       : character. Variable to plot.
##   ##   ylab          : character or expression. Y-axis label.
##   ##
##   ## Return:
##   ##   Plot object
##   plotdata1 =
##     ensemble_fcst %>%
##     unite(model_member, source_id, member, remove=FALSE)

##   plotdata2 = fcst %>%
##     filter(variable %in% varname) %>%
##     dplyr::select(init_year, ens_mean, obs) %>%
##     gather(key, value, -init_year)

##   plotdata2$key = factor(
##     plotdata2$key,
##     levels=c("ens_mean", "obs"),
##     labels=c("Ensemble mean", "Observed")
##   )

##   p = ggplot(
##     data=plotdata1,
##     aes_string(
##       y=varname,
##       x="init_year",
##       group="model_member"
##     )
##   ) +
##     geom_line(color="lightgrey") +
##     geom_hline(yintercept=0, size=0.25) +
##     geom_line(
##       data=plotdata2,
##       aes(y=value, x=init_year, color=key),
##       inherit.aes = FALSE
##     ) +
##     xlab("Initialisation year") +
##     ylab(ylab) +
##     theme(
##       legend.title = element_blank(),
##       legend.position = "bottom",
##       legend.direction = "vertical"
##     )
##   p
## }


## myplotfun2 = function(fcst, varname, ylab) {
##   ## Plot variance-adjusted ensemble mean data
##   ##
##   ## Args:
##   ##   fcst    : data.frame. Ensemble mean forecast data.
##   ##   varname : character. Variable to plot.
##   ##   ylab    : character or expression. Y-axis label.
##   ## Return:
##   ##   Plot object
##   levels = c("obs", "ens_mean_var_adj", "ens_mean_lag_var_adj")
##   labels=c("Observations", "Variance-adjusted", "Variance-adjusted and lagged")
##   fcst = fcst %>% mutate(period_start = init_year + 1)
##   plotdata =
##     fcst %>%
##     filter(variable %in% varname) %>%
##     pivot_longer(
##       c(-period_start, -init_year, -variable),
##       names_to="statistic",
##       values_to=varname
##     ) %>%
##     filter(statistic %in% levels)

##   plotdata$statistic = factor(
##     plotdata$statistic,
##     levels=levels,
##     labels=labels
##   )

##   p = ggplot(data=plotdata) +
##     geom_line(
##       aes_string(
##         y=varname,
##         x="period_start",
##         group="statistic",
##         colour="statistic"
##       )
##     ) +
##     xlab("Initialisation year") +
##     ylab(ylab) +
##     theme(
##       legend.title = element_blank(),
##       legend.position = "bottom",
##       legend.direction = "vertical"
##     )
##   p
## }


## myplotfun3 = function(ensemble_fcst, fcst, varname, ylab) {
##   ## Plot variance-adjusted ensemble mean data
##   ##
##   ## Args:
##   ##   fcst    : data.frame. Ensemble mean forecast data.
##   ##   varname : character. Variable to plot.
##   ##   ylab    : character or expression. Y-axis label.
##   ## Return:
##   ##   Plot object
##   ## ensemble_fcst = nao_matched_ensemble_fcst
##   ## fcst = nao_matched_fcst

##   ## Ensemble plotdata
##   plotdata1 =
##     ensemble_fcst %>%
##     filter(variable %in% varname) %>%
##     unite(model_member, source_id, member, remove=FALSE)

##   ## levels = c("obs", "ens_mean", "ens_mean_var_adj", "ens_mean_lag_var_adj")
##   ## labels=c("Observations", "Raw ensemble mean", "Variance-adjusted", "Variance-adjusted and lagged")
##   levels = c("obs", "full_ens_mean", "ens_mean")
##   labels=c("Observations", "Ensemble mean", "NAO-matched ensemble mean")
##   fcst = fcst %>% mutate(period_start = init_year + 1)
##   plotdata2 =
##     fcst %>%
##     filter(variable %in% varname) %>%
##     pivot_longer(
##       c(-period_start, -init_year, -variable),
##       names_to="statistic",
##       values_to=varname
##     ) %>%
##     filter(statistic %in% levels)

##   plotdata2$statistic = factor(
##     plotdata2$statistic,
##     levels=levels,
##     labels=labels
##   )

##   p = ggplot(
##     data=plotdata1,
##     aes_string(
##       y="value",
##       x="init_year",
##       group="model_member"
##     )
##   ) +
##     geom_point(color="lightgrey") +
##     geom_hline(yintercept=0, size=0.25) +
##     geom_line(
##       data=plotdata2,
##       aes_string(
##         y=varname,
##         x="init_year",
##         group="statistic",
##         color="statistic"
##       ),
##       inherit.aes = FALSE
##     ) +
##     xlab("Initialisation year") +
##     ylab(ylab) +
##     theme(
##       legend.title=element_blank(),
##       legend.position="bottom",
##       legend.direction="vertical"
##     )
##   p
## }

## myplotfun4 = function(x, model_display_names) {

##   ## We construct the legend using the model, so here we convert
##   ## it to a factor and change the labels to those we want to
##   ## display (defined in preamble)
##   x = x %>% mutate(model = as.factor(model))
##   x$model = do.call("recode_factor", c(list(x$model), model_display_names))
##   obs =
##     x %>%
##     dplyr::select(year, obs) %>%
##     ## dplyr::select(clim_season, year, obs) %>%
##     distinct(year, .keep_all = TRUE) %>%
##     mutate(type = "Observed")

##   p = ggplot() +
##     theme_bw() +
##     geom_ribbon(
##       aes(ymin=Q25, ymax=Q75, x=year, fill=model),
##       alpha=0.5, data=x
##     ) +
##     geom_line(
##       aes(y=Q50, x=year, colour=model), data=x
##     ) +
##     ylab(expression(Streamflow~(m^{3}~s^{-1}))) +
##     xlab("") +
##     scale_x_continuous(breaks = pretty_breaks()) +
##     ## N.B. use alpha to create another legend
##     ## https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
##     geom_point(
##       aes(y=obs, x=year, alpha="Observed"),
##       color = "black",
##       data=obs
##     ) +
##     scale_alpha_manual(name=NULL, values=1, breaks="Observed") +
##     ggtitle(sprintf("ID = %d", stn_id)) +
##     theme(legend.position = "bottom",
##           legend.direction = "vertical",
##           legend.title = element_blank())

##   p = p + guides(alpha = guide_legend(order = 1),
##                  fill = guide_legend(order = 2),
##                  color = guide_legend(order = 2))
##   p
## }

## myplotfun5 = function(x) {
##   rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")
##   p =
##     ggplot() +
##     geom_sf(data = europe_boundary, color=NA, fill="lightgrey") +
##     geom_sf(data = uk_boundary) +
##     geom_sf(data = skill, aes(fill = skill), shape=21) +
##     facet_wrap(. ~ period, ncol = 3, labeller = label_parsed) +
##     ## geom_sf(data = catchment_boundaries) +
##     coord_sf(xlim=c(-8, 2), ylim=c(50, 60), default_crs = st_crs(4326)) +
##     ## theme(legend.position = "bottom") +
##     guides(fill = guide_colorbar(
##              ## barwidth = 15,
##              title="MSSS",
##              title.position="top",
##              ## legend.position = "bottom"
##              legend.position = "right"
##            )) +
##     scale_fill_gradientn(
##       colours = c(rdbu_pal[2], rdbu_pal[5], rdbu_pal[9]),
##       values = scales::rescale(c(-0.2, 0, 0.8))
##     )
##   p
## }

## myplotfun6 = function(x) {
##   p =
##     ggplot() +
##     geom_sf(data = europe_boundary, color=NA, fill="lightgrey") +
##     geom_sf(data = uk_boundary) +
##     geom_sf(data = skill, aes(fill = difference), shape=21) +
##     facet_wrap(. ~ period, ncol = 3, labeller = label_parsed) +
##     ## geom_sf(data = catchment_boundaries) +
##     coord_sf(xlim=c(-8, 2), ylim=c(50, 60), default_crs = st_crs(4326)) +
##     ## theme(legend.position = "bottom") +
##     guides(fill = guide_colorbar(
##              ## barwidth = 15,
##              title="Diff",
##              title.position="top",
##              legend.position = "right"
##            )) +
##     scale_fill_gradientn(
##       colours = c(rdbu_pal[2], rdbu_pal[5], rdbu_pal[9]),
##       values = scales::rescale(c(-0.4, 0, 0.2))
##     )
##   p
## }

## ## csv_to_sqlite <- function(csv_file, sqlite_file, table_name,
## ##                           delim = ",",
## ##                           pre_process_size = 1000, chunk_size = 50000,
## ##                           show_progress_bar = TRUE, ...) {
## ##   ## Save a delimited text table into a single table sqlite database
## ##   ##
## ##   ## The table can be a comma separated (csv) or a tab separated (tsv) or any
## ##   ## other delimited text file. The file is read in chunks. Each chunk is copied
## ##   ## in the same sqlite table database before the next chunk is loaded into
## ##   ## memory. See the INBO tutorial \href{https://github.com/inbo/tutorials/blob/master/source/data-handling/large-files-R.Rmd}{Handling large files in R}
## ##   ## to learn more about.
## ##   ##
## ##   ## @section Remark:
## ##   ## The \code{callback} argument in the \code{read_delim_chunked} function call
## ##   ## refers to the custom written callback function `append_to_sqlite` applied
## ##   ## to each chunk.
## ##   ##
## ##   ## @param csv_file Name of the text file to convert.
## ##   ## @param sqlite_file Name of the newly created sqlite file.
## ##   ## @param table_name Name of the table to store the data table in the sqlite
## ##   ##   database.
## ##   ## @param delim Text file delimiter (default ",").
## ##   ## @param pre_process_size Number of lines to check the data types of the
## ##   ##   individual columns (default 1000).
## ##   ## @param chunk_size Number of lines to read for each chunk (default 50000).
## ##   ## @param show_progress_bar Show progress bar (default TRUE).
## ##   ## @param ... Further arguments to be passed to \code{read_delim}.
## ##   ##
## ##   ## @return a SQLite database
## ##   ## @family Data_handling_utilities
## ##   con <- dbConnect(SQLite(), dbname = sqlite_file)

## ##   # read a first chunk of data to extract the colnames and types
## ##   # to figure out the date and the datetime columns
## ##   df <- read_delim(csv_file, delim = delim, n_max = pre_process_size, ...)
## ##   date_cols <- df %>%
## ##       select_if(is.Date) %>%
## ##       colnames()
## ##   datetime_cols <- df %>%
## ##       select_if(is.POSIXt) %>%
## ##       colnames()

## ##   # write the first batch of lines to SQLITE table, converting dates to string
## ##   # representation
## ##   df <- df %>%
## ##     mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
## ##     mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
## ##   dbWriteTable(con, table_name, df, overwrite = TRUE)

## ##   # readr chunk functionality
## ##   read_delim_chunked(
## ##     csv_file,
## ##     callback = append_to_sqlite(con = con, table_name = table_name,
## ##                                 date_cols = date_cols,
## ##                                 datetime_cols = datetime_cols),
## ##     delim = delim,
## ##     skip = pre_process_size + 1,
## ##     chunk_size = chunk_size,
## ##     progress = show_progress_bar,
## ##     col_names = names(attr(df, "spec")$cols),
## ##     ...)
## ##   dbDisconnect(con)
## ## }

## ## append_to_sqlite <- function(con, table_name,
## ##                              date_cols, datetime_cols) {
## ##   ## Callback function that appends new sections to the SQLite table.
## ##   ##
## ##   ## Copied from inborutils::append_to_sqlite
## ##   ##
## ##   ## Args:
## ##   ## con A valid connection to SQLite database.
## ##   ## table_name Name of the table to store the data table in the sqlite
## ##   ##   database.
## ##   ## date_cols Name of columns containing Date objects
## ##   ## datetime_cols Name of columns containint POSIXt objects.
## ##   ##
## ##   function(x, pos) {
## ##     ## Args:
## ##     ## x Data.frame we are reading from.
## ##     x <- as.data.frame(x)
## ##     x <- x %>%
## ##       mutate_at(.vars = date_cols, .funs = as.character.Date) %>%
## ##       mutate_at(.vars = datetime_cols, .funs = as.character.POSIXt)
## ##     # append data frame to table
## ##     dbWriteTable(con, table_name, x, append = TRUE)
## ##   }
## ## }

## ## get_short_name = function(variable) {
## ##   if (variable == 'european_precip') short_name = 'pr'
## ##   if (variable == 'uk_precip') short_name = 'pr'
## ##   if (variable == 'amv') short_name = 'tas'
## ##   if (variable == 'nao') short_name = 'psl'
## ##   short_name
## ## }

## ## esmvaltool_file_pattern = function(project,
## ##                                    source,
## ##                                    mip,
## ##                                    init_year,
## ##                                    member,
## ##                                    variable,
## ##                                    provenance_file = FALSE) {
## ##   ## Construct a regular expression.
## ##   ##
## ##   ## This function constructs a regular expression
## ##   ## to match an ESMValTool output file.

## ##   short_name = get_short_name(variable)
## ##   if (provenance_file) {
## ##     suffix = "_provenance.xml"
## ##   } else {
## ##     suffix = ".nc"
## ##   }
## ##   if (toupper(project) == "CMIP6") {
## ##     experiment = paste0("dcppA-hindcast", "_", "s", init_year, "-", member)
## ##     grid_time = "[a-z]+_[0-9]{4}-[0-9]{4}"
## ##   } else {
## ##     experiment = paste0("decadal", init_year, "_", member)
## ##     grid_time = "[0-9]{4}-[0-9]{4}"
## ##   }
## ##   ptn = paste0(
## ##     toupper(project), "_", source, "_", mip, "_", experiment,
## ##     "_", short_name, "_", grid_time, suffix
## ##   )
## ##   ptn
## ## }

## ## retrieve_model_output_files = function(project,
## ##                                        source,
## ##                                        mip,
## ##                                        init_year,
## ##                                        member,
## ##                                        variable) {
## ##   ## Get ESMValTool input files.
## ##   ##
## ##   ## This function retrieves the input files which
## ##   ## were used to produce a certain ESMValTool output.
## ##   ## It does this by parsing the XML provenance file
## ##   ## which is created alongside each ESMValTool output netCDF.

## ##   short_name = get_short_name(variable)
## ##   if (toupper(project) == "CMIP6") {
## ##     recipe_root = esmvaltool_cmip6_root
## ##   } else {
## ##     recipe_root = esmvaltool_cmip5_root
## ##   }
## ##   ptn = esmvaltool_file_pattern(
## ##     project, source, mip, init_year,
## ##     member, variable, provenance_file=TRUE
## ##   )
## ##   prov_file = list.files(
## ##     file.path(recipe_root, "preproc", variable, short_name),
## ##     ptn,
## ##     full.names = TRUE
## ##   )
## ##   ## Parse each XML provenance file to extract
## ##   ## the paths of input files.
## ##   fs = c()
## ##   if (length(prov_file) > 0) {
## ##     for (i in 1:length(prov_file)) {
## ##       prov = read_xml(prov_file)
## ##       fn = xml_attr(
## ##         xml_find_all(prov, "//prov:usedEntity"), "ref"
## ##       ) %>%
## ##         gsub("file:", "", .)
## ##       fs = c(fs, fn)
## ##     }
## ##     fs = sort(fs, decreasing = FALSE)
## ##   }
## ##   fs
## ## }

## ## retrieve_esmvaltool_output_files = function(project,
## ##                                             source,
## ##                                             mip,
## ##                                             init_year,
## ##                                             member,
## ##                                             variable) {
## ##   ## Get ESMValTool output files.
## ##   ##
## ##   ## This function retrieves ESMValTool output files. It is
## ##   ## simpler than `retrieve_model_output_files()` because
## ##   ## it is only concerned with the ESMValTool output, rather
## ##   ## than the raw input data used to create them.

## ##   short_name = get_short_name(variable)
## ##   if (toupper(project) == "CMIP6") {
## ##     recipe_root = esmvaltool_cmip6_root
## ##   } else {
## ##     recipe_root = esmvaltool_cmip5_root
## ##   }
## ##   ptn = esmvaltool_file_pattern(project, source, mip, init_year, member, variable)
## ##   fs = list.files(
## ##     file.path(recipe_root, "work", variable, variable),
## ##     ptn,
## ##     full.names=TRUE
## ##   )
## ##   fs
## ## }

## ## interpret_netcdf_time = function(ds) {
## ##   ## Interpret netCDF time.
## ##   tunits = ncatt_get(ds, "time", "units")
## ##   time = ncvar_get(ds, "time")
## ##   tustr = strsplit(tunits$value, " ")
## ##   tdstr = strsplit(unlist(tustr)[3], "-")
## ##   tmonth = as.integer(unlist(tdstr)[2])
## ##   tday = as.integer(substring(unlist(tdstr)[3],1,2))
## ##   tyear = as.integer(unlist(tdstr)[1])
## ##   time_val = as.Date(chron(time,origin=c(tmonth, tday, tyear)))
## ##   time_val
## ## }

## ## extract_netcdf_data = function(fpath, short_name) {
## ##   ## Extract data from netCDF.
## ##   ##
## ##   ## This function converts monthly gridded values
## ##   ## to a RasterBrick object.
## ##   ds = nc_open(fpath)
## ##   varsize = ds$var[[short_name]]$varsize
## ##   ndims = ds$var[[short_name]]$ndims
## ##   ## Time dimension:
## ##   ts = try(nc.get.time.series(ds), silent=TRUE)
## ##   if (inherits(ts, "try-error") | is.na(ts)) {
## ##     ts = interpret_netcdf_time(ds)
## ##   }
## ##   ts = ts %>% as.POSIXct() %>% as.Date()
## ##   n_time = length(ts)
## ##   lat = ncvar_get(ds, "lat")
## ##   nlat = length(lat)
## ##   lat_sn = lat[1] < lat[2]
## ##   lon = ncvar_get(ds, "lon")
## ##   nlon = length(lon)
## ##   ## Exact boundaries:
## ##   ymn = min(lat) - (diff(lat)[1] / 2)
## ##   ymx = max(lat) + (diff(lat)[nlat-1] / 2)
## ##   xmn = min(lon) - (diff(lon)[1] / 2)
## ##   xmx = max(lon) + (diff(lon)[nlon-1] / 2)
## ##   shift_lon = lon[1] >= 0.
## ##   ## Data
## ##   maps = list()
## ##   for (i in 1:n_time) {
## ##     ## Data
## ##     start = rep(1, ndims)
## ##     start[ndims] = i
## ##     count = varsize
## ##     count[ndims] = 1
## ##     arr = ncvar_get(ds, short_name, start=start, count=count)
## ##     arr = t(arr)
## ##     ## Transpose to n * m, where n is number of rows and m is number of columns
## ##     if (lat_sn) {
## ##       arr = arr[rev(seq_len(nrow(arr))),]
## ##     }
## ##     ## Account for 0-360 longitude values
## ##     if (shift_lon) {
## ##       xmni = xmn - 180
## ##       xmxi = xmx - 180
## ##       arr = arr[,c(seq(nlon / 2 + 1, nlon), seq(1, nlon / 2))]
## ##     } else {
## ##       xmni = xmn
## ##       xmxi = xmx
## ##     }
## ##     ## x = raster(tas_i, xmn=-180, xmx=180, ymn=-90, ymx=90)
## ##     x = raster(
## ##       arr, xmn=xmni, xmx=xmxi, ymn=ymn, ymx=ymx,
## ##       crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
## ##     )
## ##     maps[[i]] = x
## ##   }
## ##   nc_close(ds)
## ##   out = list(times = ts, maps = brick(maps))
## ## }

## ## season_data = read.table(header=TRUE, text="
## ## month clim_season
## ## 1 djfm
## ## 2 djfm
## ## 3 djfm
## ## 4 am
## ## 5 am
## ## 6 jjas
## ## 7 jjas
## ## 8 jjas
## ## 9 jjas
## ## 10 on
## ## 11 on
## ## 12 djfm
## ## ")

## ## extract_forecast_data = function(project,
## ##                                  source,
## ##                                  mip,
## ##                                  init_year,
## ##                                  member,
## ##                                  variable) {
## ##   ## Extract forecast data for catchments.
## ##   ##
## ##   ## This function extracts decadal forecast data for
## ##   ## a given climate variable for each UKBN catchment
## ##   ## (defined in global variable `ukbn2_catchments`).
## ##   ## It uses exact_extract(x, y, 'mean', weights='area')
## ##   short_name = get_short_name(variable)
## ##   fs = retrieve_model_output_files(
## ##     project, source, mip, init_year, member, variable
## ##   )
## ##   ## Return NULL if no files are returned
## ##   if (length(fs) == 0) {
## ##     return(NULL)
## ##   }
## ##   ## Loop through each file and extract data
## ##   catchment_data_list = list()
## ##   for (i in 1:length(fs)) {
## ##     f = fs[i]
## ##     data = extract_netcdf_data(f, short_name)
## ##     maps = data[["maps"]]
## ##     ts = data[["times"]]
## ##     ## Extract data from maps using exact_extract
## ##     d = exact_extract(
## ##       maps,
## ##       ukbn2_catchments,
## ##       'mean', weights='area'
## ##     )
## ##     names(d) = ts
## ##     d$ID = ukbn2_catchments$ID
## ##     d =
## ##       d %>%
## ##       pivot_longer(
## ##         !ID,
## ##         names_to = "time",
## ##         values_to = short_name
## ##       ) %>%
## ##       mutate(time = as.Date(time))
## ##     catchment_data_list[[i]] = d
## ##   }
## ##   ## Join data frames
## ##   catchment_data = do.call("rbind", catchment_data_list)
## ##   ## Assign seasons to data
## ##   catchment_data =
## ##     catchment_data %>%
## ##     arrange(ID, time) %>%
## ##     mutate(
## ##       year = as.integer(format(time, "%Y")),
## ##       month = as.integer(format(time, "%m"))
## ##     ) %>%
## ##     dplyr::select(-time) %>%
## ##     left_join(season_data) %>%
## ##     mutate(season_year = ifelse(month == 12 & clim_season == "djfm", year + 1, year))
## ##   catchment_data
## ## }


## ## extract_index_data = function(project,
## ##                               source,
## ##                               mip,
## ##                               init_year,
## ##                               member,
## ##                               variable) {
## ##   fs = retrieve_esmvaltool_output_files(
## ##     project, source, mip, init_year, member, variable
## ##   )
## ##   ## In case no files are returned
## ##   if (length(fs) == 0) {
## ##     return(NULL)
## ##   }
## ##   ds = nc_open(fs)
## ##   nao = ncvar_get(ds, variable)
## ##   ## NB season year is taken as the year in which
## ##   ## the last month of the season falls [this is
## ##   ## different to how we define season_year in
## ##   ## nao-matching.R and subsequently]
## ##   season_year = ncvar_get(ds, "season_year")
## ##   print(season_year)
## ##   clim_season = ncvar_get(ds, "clim_season")
## ##   ## TODO better to do this in function
## ##   ts = try(nc.get.time.series(ds), silent=TRUE)
## ##   if (inherits(ts, "try-error") | is.na(ts)) {
## ##     ts = interpret_netcdf_time(ds)
## ##   }
## ##   ts = ts %>% as.POSIXct() %>% as.Date()
## ##   n_time = length(ts)
## ##   nc_close(ds)
## ##   nao_df = data.frame(
## ##     ts,
## ##     season_year,
## ##     tolower(clim_season),
## ##     nao
## ##   )
## ##   names(nao_df) = c("time", "season_year", "clim_season", variable)
## ##   nao_df =
## ##     nao_df %>%
## ##     mutate(
## ##       year = as.integer(format(time, "%Y")),
## ##       month = as.integer(format(time, "%m"))
## ##     ) %>%
## ##     dplyr::select(-time) %>%
## ##     left_join(season_data) %>%
## ##     mutate(season_year = ifelse(month == 12 & clim_season == "djfm", year + 1, year))
## ##   nao_df
## ## }
