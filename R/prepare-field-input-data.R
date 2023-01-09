#!/usr/bin/env Rscript

library(tidyverse)
library(magrittr)
library(zoo)
library(yaml)
library(arrow)

options(bitmapType = "cairo")
options(dplyr.summarise.inform = FALSE)

if (exists("snakemake")) {
  config <- snakemake@config
  obspath <- snakemake@input[["obs"]]
  fcstpath <- snakemake@input[["fcst"]]
  grid_coord <- snakemake@wildcards[["grid"]]
  aggregation_period <- snakemake@wildcards[["aggr"]]
  outputroot <- snakemake@params[["outputdir"]]
  snakemake@source("utils.R")
} else {
  ## TESTING
  config <- read_yaml("config/config_1.yml")
  obspath <- "results/intermediate/observed-field.parquet"
  fcstpath <- "results/intermediate/ensemble-forecast-field"
  grid_coord <- "s50e115"
  aggregation_period <- "yr2to5"
  outputroot <- "results"
  cwd = "workflow/decadal-prediction-scripts/R"
  source(file.path(cwd, "utils.R"))
}

config[["aggregation_period"]] = parse_config_aggregation_period(config)

## Parse aggregation period specification
period <- config$aggregation_period[[aggregation_period]]
lead_tm <- period$lead_time
start <- min(lead_tm)
end <- max(lead_tm)
study_period <- period$study_period

## Make output directory
outputdir = file.path(outputroot, aggregation_period)
dir.create(outputdir, recursive = TRUE, showWarnings = FALSE)

## TODO put these in config somehow
vars <- c("precip_field", "temp_field")
antecedent_vars <- c("precip_field")
months <- c(12, 1, 2, 3)
antecedent_months <- c(9, 10, 11)

## The antecedent year may need to be offset in some cases,
## e.g. for MAM the antecedent period could be DJF, which
## starts in the previous year
if (antecedent_months[1] >= months[1]) {
  year_offset <- 1
} else {
  year_offset <- 0
}

## ################################### ##
## Load observed data
## ################################### ##

dataset <- read_parquet(obspath)
subdataset <-
  dataset %>%
  filter(coord %in% grid_coord) %>%
  collect()

obs_field <- get_obs_new(
  subdataset,
  study_period,
  start = start, end = end,
  vars = vars,
  months = months
)
obs_field_antecedent <- get_obs_new(
  subdataset,
  study_period,
  start = start,
  end = end,
  vars = antecedent_vars,
  months = antecedent_months
)
obs_field_antecedent <-
  obs_field_antecedent %>%
  mutate(variable = paste0(variable, "_antecedent")) %>%
  mutate(init_year = init_year + year_offset)

## Join together and save output as dataset
obs_field <-
  rbind(obs_field, obs_field_antecedent) %>%
  arrange(init_year, variable)
obs_field <-
  obs_field %>%
  mutate(coord = grid_coord, .before = init_year) %>%
  arrange(init_year, variable) %>%
  group_by(coord) %>%
  write_dataset(
    file.path(outputdir, "observed-field"),
    format = "parquet"
  )

## ################################### ##
## Load modelled data
## ################################### ##

lead_times <- lead_tm
dataset <- open_dataset(
  file.path(fcstpath, grid_coord),
  partitioning = c("source_id", "member", "variable")
) %>% collect()

ensemble_fcst <- get_hindcast_data_new(
  dataset, study_period, lead_times,
  vars = vars, months = months
)

ensemble_fcst_antecedent <- get_hindcast_data_new(
  dataset, study_period, lead_times,
  vars = antecedent_vars, months = antecedent_months
)
ensemble_fcst_antecedent <-
  ensemble_fcst_antecedent %>%
  mutate(variable = paste0(variable, "_antecedent")) %>%
  mutate(init_year = init_year + year_offset)

## Join together and save output
ensemble_fcst <- rbind(ensemble_fcst, ensemble_fcst_antecedent)
ensemble_fcst <-
  ensemble_fcst %>%
  mutate(coord = grid_coord) %>%
  group_by(coord) %>%
  write_dataset(
    file.path(outputdir, "ensemble-forecast-field"),
    format = "parquet",
    hive_style = FALSE
  )
