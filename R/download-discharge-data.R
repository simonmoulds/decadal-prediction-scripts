#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(rnrfa)
library(RcppRoll)
library(yaml)

options(dplyr.summarise.inform = FALSE)

## Extract configuration info
if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  ukbn2_stations <- args[1]
  outputdir <- args[2]
  args = commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}
source(file.path(cwd, "utils.R"))

metadata = catalogue()
names(metadata) = names(metadata) %>% gsub("-", "_", .)
metadata =
  metadata %>%
  mutate(
    gdf_start_date = as.Date(gdf_start_date),
    gdf_end_date = as.Date(gdf_end_date)
  ) %>%
  mutate(
    gdf_record_length = time_length(
      gdf_end_date - gdf_start_date, "years"
    )
  ) %>%
  filter(
    gdf_record_length >= 40 & gdf_start_date <= as.Date("1975-01-01")
  )

## First get the ukbn2 stations with the most reliable data
## ukbn_root = file.path(config$aux_data$ukbn)
## ukbn2_stations <- read_csv(
##   file.path(output_root, "UKBN_Station_List_vUKBN2.0_1.csv"),
##   show_col_types = FALSE
## )
ukbn2_stations <- read_csv(ukbn2_stations, show_col_types = FALSE)
##   file.path("data", "UKBN_Station_List_vUKBN2.0_1.csv"),
##   show_col_types = FALSE
## )
## Allow benchmark scores of 1 (caution) and 2 (suitable)
ukbn2_stations <- ukbn2_stations[ukbn2_stations$High_Score >= 1, "Station"]
ukbn2_stations <- unlist(ukbn2_stations) %>% unname()

## Now filter UKBN2 stations
metadata = metadata %>% filter(id %in% ukbn2_stations)

metadata$id <- as.numeric(metadata$id)
station_ids = metadata$id

## stations <- stations[all_stations$id %in% c(ukbn2_stations),]
n_stations = length(metadata$id)
pb = txtProgressBar(min=0, max=n_stations, initial=0)
for (i in 1:n_stations) {
  stn_id = station_ids[i]
  df = download_nrfa_data(stn_id, metadata)
  ## Save data
  df %>%
    group_by(ID) %>%
    write_dataset(file.path(outputdir), format = "parquet")
    ## write_dataset(file.path(output_root, "nrfa-discharge-summaries"), format = "parquet")
    ## write_dataset(file.path(output_root, "nrfa-discharge-summaries"), format = "parquet")
  ## Update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

