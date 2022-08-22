#!/usr/bin/env Rscript

library(tidyverse)
library(scales)
library(sf)
library(gamlss)
library(arrow)
library(yaml)
library(optparse)

options(dplyr.summarise.inform = FALSE)

if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  ## if (length(args) == 0) {
  ##   stop("Configuration file not supplied.", call. = FALSE)
  ## }
  config = read_yaml(args[1])
  experiment = args[2]
  output_root = args[3]
  args = commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}
source(file.path(cwd, "utils.R"))
config = parse_config(config)

## if (is.null(opt$options$experiment)) {
##   print_help(opt_parser)
##   stop("At least one experiment must be specified")
## } else {
##   experiments = opt$options$experiment %>% str_split(",") %>% `[[`(1) %>% str_trim()
## }

## if (!all(experiments %in% names(config$modelling))) {
##   print_help(opt_parser)
##   stop("Specified experiments do not exist in config file")
## }

experiment_conf = config$modelling[[experiment]]
for (j in 1:length(experiment_conf$aggregation_periods)) {
  label = experiment_conf$aggregation_periods[j]
  input_dir = file.path(output_root, "analysis", label, "input")
  output_dir = file.path(output_root, "analysis", experiment, label)

  ## Load input dataset
  ds =
    open_dataset(input_dir) %>%
    collect() %>%
    arrange(ID, year, period) %>%
    dplyr::select(-lead_time)

  ## Identify subsets
  ## has_subset = "subset" %in% names(ds)
  ## if (has_subset) {
  subsets = experiment_conf$subsets
  ds_subsets = ds$subset %>% unique() %>% sort()
  if (!all(subsets %in% ds_subsets)) {
    stop("Dataset does not contain all specified subsets")
  }
  ## } else {
  ##   ds$subset = "full"
  ##   subsets = ds$subset %>% unique() %>% sort()
  ## }

  ## Create output directories
  subdirs = c("prediction", "skill") #, "catchment_plots")
  for (m in 1:length(subdirs)) {
    sd_output_dir = file.path(output_dir, subdirs[m])
    unlink(sd_output_dir, recursive = TRUE)
    dir.create(sd_output_dir, recursive = TRUE)
  }
  ## for (m in 1:length(subsets)) {
  ##   plot_output_dir = file.path(output_dir, "catchment_plots", subsets[m])
  ##   unlink(plot_output_dir, recursive = TRUE)
  ##   dir.create(plot_output_dir, recursive = TRUE)
  ## }
  ## Set predictand
  ds[["Q"]] = ds[[experiment_conf$predictand]]
  station_ids = ds$ID %>% unique() %>% sort()
  ## Loop through catchments
  print(sprintf("Fitting models with %s input data", label))
  pb = txtProgressBar(min=0, max=length(station_ids), initial=0, title=pb_title)
  for (k in 1:length(station_ids)) {
    stn_id = station_ids[k]
    catchment_data =
      ds %>%
      filter(ID %in% stn_id & year %in% experiment_conf$study_period)
    ## Handle missing data in training data
    exclude = catchment_data$missing_pct > 30 | !complete.cases(catchment_data)
    if (experiment_conf$model_family == "GA") {
      ## Cannot fit a Gamma distribution if the response variable contains zeroes
      exclude = exclude | (catchment_data[[experiment_conf$predictand]] == 0.)
    }
    if (sum(exclude) > (length(exclude) * 0.33)) {
      next
    }
    catchment_data = catchment_data[!exclude,]

    x = catchment_data
    catchment_data_list = list()
    for (m in 1:length(subsets)) {
      subset = subsets[m]
      catchment_data_list[[m]] = x %>% filter(subset %in% subsets[m])
    }

    for (m in 1:length(catchment_data_list)) {
      xx = catchment_data_list[[m]]
      subset = subsets[m]
      n_fold = nrow(xx)
      catchment_prediction_list = list()

      ## Fit models on all data to extract AIC
      models = fit_models(
        experiment_conf$formulas,
        experiment_conf$sigma_formulas,
        experiment_conf$model_family,
        xx
      )
      aic = get_aic(models)

      for (p in 1:n_fold) {
        ## Implement a leave-one-out cross-validation approach
        idx = seq_len(n_fold)
        test_idx = p
        train_idx = idx[!idx %in% test_idx]
        train_data = xx[train_idx,]
        test_data = xx[test_idx,]

        ## N.B. because we use a function to fit models the data.frame
        ## used in the original fit is not stored in the model call
        ## correctly. This means that when we call `predict.gamlss`
        ## we have to supply the data.frame used for the original
        ## fit (i.e. train_data)
        models = fit_models(
          experiment_conf$formulas,
          experiment_conf$sigma_formulas,
          experiment_conf$model_family,
          train_data
        )
        prediction = do.call(
          "compute_quantiles",
          c(list(newdata = test_data, data = train_data), models, list(model_family=experiment_conf$model_family))
        )
        ## Create output data frame
        obs = test_data[[experiment_conf$predictand]]
        prediction =
          prediction %>%
          mutate(
            predictand = experiment_conf$predictand,
            obs = obs,
            exp = Q50
          )
        pred_idx = length(catchment_prediction_list) + 1
        catchment_prediction_list[[pred_idx]] = prediction
      }
      ## Only proceed if we have some predictions
      if (length(catchment_prediction_list) == 0) {
        next
      }
      catchment_prediction = do.call("rbind", catchment_prediction_list)

      ## Make complete time series
      complete_ts = expand_grid(
        ID = stn_id,
        ## clim_season = "DJFM",
        year = experiment_conf$study_period,
        model = unique(catchment_prediction$model),
        predictand = experiment_conf$predictand
      )

      catchment_prediction =
        complete_ts %>%
        left_join(
          catchment_prediction,
          ## by=c("ID", "clim_season", "year", "model", "predictand")
          by=c("ID", "year", "model", "predictand")
        ) %>%
        mutate(period = label, subset = subsets[m], .after = predictand) %>%
        ## mutate(lead_time = lead_tm, .before = model) %>%
        filter(year %in% experiment_conf$study_period) %>%
        arrange(year, model)

      ## Write output
      catchment_prediction %>%
        group_by(ID, predictand, subset) %>%
        write_dataset(file.path(output_dir, "prediction"), format = "parquet")

      model_nms = distinct(catchment_prediction, model)$model
      ## Evaluate model skill
      skill_scores_list = list()
      for (p in 1:length(model_nms)) {
        model_nm = model_nms[p]
        pred =
          catchment_prediction %>%
          filter(model %in% model_nm) %>%
          na.omit()

        skill =
          mean_square_error_skill_score(pred$obs, pred$exp) %>%
          as_tibble() %>%
          mutate(
            ID = stn_id,
            model = model_nm,
            ## lead_time = lead_tm,
            predictand = experiment_conf$predictand,
            period = label,
            subset = subsets[m],
            .before = "msss"
          )

        idx = length(skill_scores_list) + 1
        skill_scores_list[[idx]] = skill
      }
      skill_scores = do.call("rbind", skill_scores_list)
      aic = aic %>% pivot_longer(everything(), names_to = "model", values_to = "aic")
      skill_scores = left_join(skill_scores, aic, by = "model")

      skill_scores %>%
        group_by(ID, predictand, subset) %>%
        write_dataset(file.path(output_dir, "skill"), format = "parquet")

      ## ## Make plot, adding MSSS to legend
      ## display_names =
      ##   skill_scores %>%
      ##   mutate(
      ##     display_name = paste0(
      ##       gsub("_", " + ", model),
      ##       " (MSSS = ",
      ##       formatC(msss, digits=3), ")"
      ##     )
      ##   ) %>% dplyr::select(model, display_name)
      ## display_names = setNames(as.list(display_names$display_name), display_names$model)
      ## p = myplotfun4(catchment_prediction, display_names)
      ## ggsave(
      ##   file.path(output_dir, "catchment_plots", subset, sprintf("model_output_%d_%s.png", stn_id, label)),
      ##   device="png",
      ##   height=6, width=6, units=c("in")
      ## )
    }
    setTxtProgressBar(pb, k)
  }
  close(pb)
}
