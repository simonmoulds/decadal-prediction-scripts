#!/usr/bin/env Rscript

## Author : Simon Moulds
## Date   : Jan 2022

library(tidyverse)
library(ggrepel)
library(RColorBrewer)
library(cowplot)
library(patchwork)
library(scales)
library(arrow)
library(sf)
## library(gamlss)
## library(rnrfa)
library(yaml)

options(dplyr.summarise.inform = FALSE)

## Extract configuration info
if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  config = read_yaml(args[1])
  output_dir = args[2]
  args = commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}
source(file.path(cwd, "utils.R"))
source(file.path(cwd, "plotting.R"))

config = parse_config(config)
output_root <- "data" # FIXME

## ####################################################### ##
## ####################################################### ##
##
## Preamble
##
## ####################################################### ##
## ####################################################### ##

load_skill_scores <- function(config, experiment) {
  ## skill_scores_list = list()
  ## for (i in 1:length(datasets)) {
  ## dataset = datasets[[i]]
  ## dataset_dir = config$modelling[[dataset]]$input_dataset
  aggregation_periods = config$modelling[[experiment]]$aggregation_periods
  period_skill_scores_list = list()
  for (i in 1:length(aggregation_periods)) {
    period_skill_scores_list[[i]] = open_dataset(
      file.path(output_root, 'analysis', experiment, aggregation_periods[i], "skill")
    ) %>% collect()
  }
  skill_scores = do.call("rbind", period_skill_scores_list) %>% as_tibble()
  ## }
  ## skill_scores = do.call("rbind", skill_scores_list) %>% as_tibble()
  skill_scores
}

obs_model_levels <- c("STATIONARY", "TIME", "NAO", "NAO_P", "P", "P_T", "NAO_P_T")
obs_model_labels <- c("STATIONARY", "TIME", "NAO", "NAOP", "P", "PT", "NAOPT")
obs_skill_scores <- load_skill_scores(config, "observed") %>%
  filter(model %in% obs_model_levels) %>%
  mutate(model = factor(model, levels = obs_model_levels, labels = obs_model_labels))

model_levels <- c("NAO", "NAO_P", "P", "P_T", "NAO_P_T")
model_labels <- c("NAO", "NAOP", "P", "PT", "NAOPT")
skill_scores <-
  load_skill_scores(config, "hindcast") %>%
  filter(model %in% model_levels) %>%
  mutate(model = factor(model, levels = model_levels, labels = model_labels))
station_ids <- skill_scores$ID %>% unique()

## Overall statistic
## % stations with +ve MSSS
stat <-
  skill_scores %>%
  filter(period %in% "yr2to9_lag" & model %in% c("P", "PT")) %>%
  group_by(ID, subset) %>%
  filter(aic == min(aic)) %>%
  ungroup() %>%
  group_by(subset) %>%
  summarize(pct_positive = sum(msss > 0) / n() * 100)

## stat <-
##   skill_scores %>%
##   filter(period %in% "yr2to9_lag" & model %in% c("P", "PT", "NAOPT")) %>%
##   group_by(ID, subset) %>%
##   filter(aic == min(aic)) %>%
##   ungroup() %>%
##   group_by(subset) %>%
##   summarize(pct_positive = sum(msss > 0) / n() * 100, n_positive = sum(msss > 0))

## For spatial plots:
uk_boundary =
  st_read("../data-raw/CNTR_RG_01M_2020_4326.shp") %>%
  filter(CNTR_NAME %in% "United Kingdom") %>%
  st_transform(crs = 27700)

europe_boundary =
  st_read("../data-raw/CNTR_RG_01M_2020_4326.shp") %>%
  filter(!CNTR_NAME %in% "United Kingdom") %>%
  st_transform(crs = 27700)

gauge_stns =
  catalogue() %>%
  rename(ID = id, area = "catchment-area") %>%
  filter(ID %in% station_ids) %>%
  dplyr::select(ID, name, area, latitude, longitude) %>%
  st_as_sf(coords=c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(27700)

## catchment_boundaries =
##   st_read("data/ukbn2_catchments.gpkg") %>%
##   st_transform("EPSG:4326") %>%
##   as_Spatial()

## Text size for plot labels
axis_title_size_large = 9
axis_title_size = 8
axis_title_size_small = 7
axis_label_size_large = 7
axis_label_size = 6
axis_label_size_small = 5
legend_label_size = 6
legend_title_size = 8
tag_label_size = 8
strip_label_size = 8

myfun = function(x) {
  ## Select the best model based on AIC value
  x_best =
    x %>%
    group_by(ID, subset, period) %>%
    filter(aic == min(aic))
  skill =
    gauge_stns %>%
    left_join(x_best) %>%
    mutate(skill = msss)
  skill
}

## ####################################################### ##
## ####################################################### ##
##
## Figure 1
##
## ####################################################### ##
## ####################################################### ##

skill_scores_subset =
  skill_scores %>%
  filter(model %in% c("P", "PT", "NAOPT")) %>%
  filter(subset %in% "full", period %in% "yr2to9_lag") %>%
  mutate(period = factor(period, levels = "yr2to9_lag", labels = "Year 2-9"))
skill_scores_median <-
  skill_scores_subset %>%
  group_by(model) %>%
  summarize(md = median(msss), n = n()) %>%
  mutate(md_label = sprintf(md, fmt = "%#.3f"))

skill1 = myfun(skill_scores_subset)
skill2 = myfun(skill_scores_subset %>% filter(!model %in% "NAOPT"))

## Number of stations with positive skill across models
skill_scores_subset %>% group_by(ID) %>% filter(msss == max(msss)) %>% ungroup() %>% summarize(n = sum(msss > 0))
## Number of stations with positive skill considering only model NAOPT
skill_scores_subset %>% filter(model %in% "NAOPT") %>% summarize(n = sum(msss > 0))
## Number of stations with positive skill not including NAOPT
skill_scores_subset %>% filter(model %in% c("P", "PT")) %>% group_by(ID) %>% filter(msss == max(msss)) %>% ungroup() %>% summarize(n = sum(msss > 0))
## Median skill of all models
median(skill1$msss)
## Median skill of models P and PT
median(skill2$msss)
## Number of stations where NAOPT/PT/P is the best model
table(skill1$model)[["NAOPT"]]
table(skill1$model)[["PT"]]
table(skill1$model)[["P"]]

p1 = myplotfun1(na.omit(skill1))
p2 = myplotfun1(na.omit(skill2))
p3 = myplotfun2(skill_scores_subset)

## TEST
n_naopt <- table(skill1$model)[["NAOPT"]]
n_pt <- table(skill1$model)[["PT"]]
n_p <- table(skill1$model)[["P"]]
labs <- c(paste0("italic(n)==", n_p), paste0("italic(n)==", n_pt), paste0("italic(n)==", n_naopt))
d <- data.frame(x = c(0, 0, 0), y = c(59, 58.5, 58), lab = labs, model = c("P", "PT", "NAOPT"))
p1 <- p1 +
  geom_point(data = d, aes(x, y, shape = model), size = 1, lwd = 0.1, show.legend = FALSE) +
  geom_text(data = d, aes(x, y, label = lab), parse = TRUE, hjust = 0, nudge_x = 0.3, size = 2) +
  scale_shape_manual(values = c(21, 24, 22)) +
  theme(axis.title = element_blank())

## n_naopt <- table(skill2$model)[["NAOPT"]]
n_pt <- table(skill2$model)[["PT"]]
n_p <- table(skill2$model)[["P"]]
labs <- c(paste0("italic(n)==", n_p), paste0("italic(n)==", n_pt)) #, paste0("italic(n)==", n_naopt))
d <- data.frame(x = c(0, 0), y = c(59, 58.5), lab = labs, model = c("P", "PT"))
p2 <- p2 +
  geom_point(data = d, aes(x, y, shape = model), size = 1, lwd = 0.1, show.legend = FALSE) +
  geom_text(data = d, aes(x, y, label = lab), parse = TRUE, hjust = 0, nudge_x = 0.3, size = 2) +
  scale_shape_manual(values = c(21, 24)) + #, 22)) +
  theme(axis.title = element_blank())

p3 <- p3 + coord_fixed(ratio = 3)
rdbu_pal = brewer.pal(9, "RdBu")
p1 <- p1 +
  scale_fill_stepsn(
    colours = rev(rdbu_pal)[3:9],
    breaks = seq(-0.4, 0.8, 0.2),
    limits = c(-0.3, 0.9)
  )
p2 <- p2 +
  scale_fill_stepsn(
    colours = rev(rdbu_pal)[3:9],
    breaks = seq(-0.4, 0.8, 0.2),
    limits = c(-0.3, 0.9)
  )
p2 <- p2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
p2 <- p2 + guides(shape = "none")

## p = p1 + p2 + p3 + plot_layout(widths = c(2, 2, 2), ncol = 3, nrow = 1)
## p = p +
##   plot_layout(guides = "collect") &
##   theme(legend.position = "bottom",
##         legend.margin = margin(0, 0, 0, 0, unit = "cm"),
##         legend.box = "vertical",
##         legend.justification = "left",
##         legend.box.just = "left",
##         legend.box.margin = margin(-1, 0, 0, 0, unit = "cm"))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.145, 0.97),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.05, 0.97),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.185, 0.91),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/fig1.png", plot = p, width = 6, height = 4.25, units = "in")

obs_skill_scores_subset <-
  obs_skill_scores %>%
  filter(model %in% c("P", "PT", "NAOPT")) %>%
  filter(period %in% "yr2to9") %>%
  mutate(period = factor(period, levels = "yr2to9", labels = "Year 2-9")) %>%
  filter(!ID %in% 25003)
obs_skill_scores_median <-
  obs_skill_scores_subset %>%
  group_by(model) %>%
  summarize(md = median(msss), n = n()) %>%
  mutate(md_label = sprintf(md, fmt = "%#.3f"))

## Perfection prediction case
skill1 = myfun(obs_skill_scores_subset)
skill2 = myfun(obs_skill_scores_subset %>% filter(!model %in% "NAOPT"))

## stat1 <- obs_skill_scores_subset %>% group_by(ID) %>% filter(msss == max(msss)) %>% ungroup() %>% summarize(n = sum(msss > 0))
## stat2 <- obs_skill_scores_subset %>% filter(model %in% "NAOPT") %>% summarize(n = sum(msss > 0))
## stat3 <- obs_skill_scores_subset %>% filter(model %in% c("P", "PT")) %>% group_by(ID) %>% filter(msss == max(msss)) %>% ungroup() %>% summarize(n = sum(msss > 0))

## Median skill of all models
median(skill1$msss)
## Median skill of models P and PT
median(skill2$msss)
## Number of stations where NAOPT/PT/P is the best model
table(skill1$model)[["NAOPT"]]
table(skill1$model)[["PT"]]
table(skill1$model)[["P"]]

p4 = myplotfun1(na.omit(skill1))
p5 = myplotfun1(na.omit(skill2))
p6 = myplotfun2(obs_skill_scores_subset)

## TEST
n_naopt <- table(skill1$model)[["NAOPT"]]
n_pt <- table(skill1$model)[["PT"]]
n_p <- table(skill1$model)[["P"]]
labs <- c(paste0("italic(n)==", n_p), paste0("italic(n)==", n_pt), paste0("italic(n)==", n_naopt))
d <- data.frame(x = c(0, 0, 0), y = c(59, 58.5, 58), lab = labs, model = c("P", "PT", "NAOPT"))
p4 <- p4 +
  geom_point(data = d, aes(x, y, shape = model), size = 1, lwd = 0.1, show.legend = FALSE) +
  geom_text(data = d, aes(x, y, label = lab), parse = TRUE, hjust = 0, nudge_x = 0.3, size = 2) +
  scale_shape_manual(values = c(21, 24, 22)) +
  theme(axis.title = element_blank())

## n_naopt <- table(skill2$model)[["NAOPT"]]
n_pt <- table(skill2$model)[["PT"]]
n_p <- table(skill2$model)[["P"]]
labs <- c(paste0("italic(n)==", n_p), paste0("italic(n)==", n_pt)) #, paste0("italic(n)==", n_naopt))
d <- data.frame(x = c(0, 0), y = c(59, 58.5), lab = labs, model = c("P", "PT"))
p5 <- p5 +
  geom_point(data = d, aes(x, y, shape = model), size = 1, lwd = 0.1, show.legend = FALSE) +
  geom_text(data = d, aes(x, y, label = lab), parse = TRUE, hjust = 0, nudge_x = 0.3, size = 2) +
  scale_shape_manual(values = c(21, 24)) + #, 22)) +
  theme(axis.title = element_blank())

p6 <- p6 + coord_fixed(ratio = 3)
rdbu_pal = brewer.pal(9, "RdBu")
p4 <- p4 +
  scale_fill_stepsn(
    colours = rev(rdbu_pal)[3:9],
    breaks = seq(-0.4, 0.8, 0.2),
    limits = c(-0.3, 0.9)
  )
p5 <- p5 +
  scale_fill_stepsn(
    colours = rev(rdbu_pal)[3:9],
    breaks = seq(-0.4, 0.8, 0.2),
    limits = c(-0.3, 0.9)
  )
p5 <- p5 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
p5 <- p5 + guides(shape = "none")

p = p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(widths = c(2, 2, 2), ncol = 3, nrow = 2)
p = p +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = "left",
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0, unit = "cm"))

p$patches$plots[[1]] =
  p$patches$plots[[1]] +
  labs(tag = "a") +
  theme(plot.tag.position = c(0.145, 1.02),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p$patches$plots[[2]] =
  p$patches$plots[[2]] +
  labs(tag = "b") +
  theme(plot.tag.position = c(0.05, 1.02),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p$patches$plots[[3]] =
  p$patches$plots[[3]] +
  labs(tag = "c") +
  theme(plot.tag.position = c(0.185, 0.98),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p$patches$plots[[4]] =
  p$patches$plots[[4]] +
  labs(tag = "d") +
  theme(plot.tag.position = c(0.145, 1.02),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p$patches$plots[[5]] =
  p$patches$plots[[5]] +
  labs(tag = "e") +
  theme(plot.tag.position = c(0.05, 1.02),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p =
  p +
  labs(tag = "f") +
  theme(plot.tag.position = c(0.185, 0.965),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## Include both modelled and observed results
ggsave(file.path(output_dir, "fig1.png"), plot = p, width = 6, height = 7.25, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Figure 2
##
## ####################################################### ##
## ####################################################### ##

## ## Best model across best_n and full
## best_skill <-
##   skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT") & subset %in% c("best_n")) %>% #, "full")) %>%
##   group_by(ID) %>%
##   filter(msss == max(msss))

skill_scores_subset =
  skill_scores %>%
  ## filter(model %in% c("NAO")) %>%
  filter(model %in% c("P", "PT", "NAOPT")) %>%
  ## filter(model %in% c("P", "PT")) %>%
  filter(subset %in% c("best_n", "full")) %>%
  filter(period %in% c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag")) %>%
  mutate(period = factor(period, levels = c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag"), labels = c("Year 2-9", "Year 2-5", "Year 6-9"))) %>%
  dplyr::select(-ps, -srel, -sme, -aic) %>%
  pivot_wider(names_from = subset, values_from = msss) %>%
  mutate(msss_diff = best_n - full)

p4 <- myplotfun444(skill_scores_subset %>% filter(model %in% c("P", "PT") & period %in% "Year 2-9"))

skill_scores_subset <-
  skill_scores_subset %>% ungroup() %>%
  filter(model %in% c("P", "PT")) %>%
  left_join(
    skill_scores %>% filter(subset %in% "best_n") %>% dplyr::select(model, msss, ID),
    by = c("model", "ID")
  )

skill <-
  skill_scores_subset %>%
  group_by(ID, period) %>%
  filter(msss == max(msss)) %>%
  ## filter(msss_diff == max(msss_diff)) %>%
  mutate(increase = msss_diff > 0) %>%
  mutate(msss_diff = abs(msss_diff))
skill <- gauge_stns %>% left_join(skill) %>% na.omit()

p2 = myplotfun3(skill %>% filter(period %in% "Year 2-9"))
p2 = p2 +
  guides(
    shape = guide_legend(order = 1),
    ## size = guide_legend(order = 2, override.aes = list(shape = 21, fill = "transparent")),
    size = guide_legend(order = 2, nrow = 3, byrow = FALSE, override.aes = list(shape = c(rep(21, 3), rep(24, 3))), direction = "vertical"),
    ## fill = guide_legend(order = 3, override.aes = list(shape = 21, fill = c("#F8766D", "#00BFC4")))
    fill = guide_legend(order = 3, override.aes = list(shape = 21, fill = c("#FC8D62", "#66C2A5")))
  )
p2 = p2 +
  theme(
    ## legend.position = "bottom",
    ## legend.box = "vertical",
    ## legend.justification = "left",
    ## legend.box.just = "left",
    legend.margin = margin(0, 0, 0, 0, unit = "cm")
  )

## a/c are total skill of NAO-matched forecasts
myfun = function(x) {
  x_best =
    x %>%
    group_by(ID, subset, period) %>%
    filter(aic == min(aic))
  skill =
    gauge_stns %>%
    left_join(x_best) %>%
    mutate(skill = msss)
  skill
}

skill_scores_subset =
  skill_scores %>%
  filter(model %in% c("P", "PT")) %>%
  mutate(model = factor(model, levels = c("P", "PT"), labels = c("P", "PT"))) %>%
  ## filter(model %in% c("P", "PT", "NAOPT")) %>%
  ## mutate(model = factor(model, levels = c("P", "PT", "NAOPT"), labels = c("P", "PT", "NAOPT"))) %>%
  filter(subset %in% c("best_n", "full"), period %in% "yr2to9_lag") %>%
  mutate(subset = factor(subset, levels = c("full", "best_n"), labels = c("Full ensemble", "NAO-matched ensemble"))) %>%
  ## filter(subset %in% "best_n", period %in% "yr2to9_lag") %>%
  mutate(period = factor(period, levels = "yr2to9_lag", labels = "Year 2-9"))

## skill_scores_subset_full =
##   skill_scores %>%
##   filter(model %in% "NAOPT") %>%
##   filter(subset %in% "full", period %in% "yr2to9_lag") %>%
##   mutate(period = factor(period, levels = "yr2to9_lag", labels = "Year 2-9")) %>%
##   mutate(model = ifelse(model == "NAOPT", "NAOPT_full", "NAOPT"))
p3 <- myplotfun22(skill_scores_subset %>% filter(period %in% "Year 2-9")) # & !model %in% "NAOPT"))

## Signif [one-sided Wilcoxon signed rank test]
x <- skill_scores_subset %>% filter(model %in% "P" & subset %in% "Full ensemble") %>% `[[`("msss")
y <- skill_scores_subset %>% filter(model %in% "P" & subset %in% "NAO-matched ensemble") %>% `[[`("msss")
pval1 = wilcox.test(y, x, paired = TRUE, alternative = "greater")$p.value

x <- skill_scores_subset %>% filter(model %in% "PT" & subset %in% "Full ensemble") %>% `[[`("msss")
y <- skill_scores_subset %>% filter(model %in% "PT" & subset %in% "NAO-matched ensemble") %>% `[[`("msss")
pval2 = wilcox.test(y, x, paired = TRUE, alternative = "greater")$p.value

## x <- skill_scores_subset %>% filter(model %in% "NAOPT" & subset %in% "Full ensemble") %>% `[[`("msss")
## y <- skill_scores_subset %>% filter(model %in% "NAOPT" & subset %in% "NAO-matched ensemble") %>% `[[`("msss")
## pval3 = wilcox.test(x, y, alternative = "two.sided")$p.value

p3 <- p3 +
  geom_signif(
    ## y_position = 0.92, xmin = c(1, 3, 5), xmax = c(2, 4, 6),
    y_position = 0.92, xmin = c(1, 3), xmax = c(2, 4),
    annotation = c(format_p(pval1), format_p(pval2)), #, format_p(pval3)),
    ## annotation = c(format_p(pval1), format_p(pval2), format_p(pval3)),
    step_increase = 0.06,
    tip_length = 0.02,
    size = 0.25,
    textsize = 2) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = legend_label_size),
        legend.direction = "vertical",
        legend.margin = margin(0, 0, 0, 0, unit = "cm"),
        legend.box = "vertical",
        ## legend.justification = "left",
        ## legend.box.just = "left",
        legend.box.margin = margin(-0.5, 0, 0, 0, unit = "cm"))

skill = myfun(skill_scores_subset %>% filter(!model %in% "NAOPT")) %>% na.omit()
p1 = myplotfun5(skill)
rdbu_pal = brewer.pal(9, "RdBu")
p1 <- p1 +
  scale_fill_stepsn(
    colours = rev(rdbu_pal)[3:9],
    breaks = seq(-0.4, 0.8, 0.2),
    limits = c(-0.3, 0.9)
  ) +
  theme(legend.position = "right")

p <- p1 + p2 + p3 + p4 + plot_layout(nrow = 2, ncol = 2)
p <- p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))

p$patches$plots[[1]] =
  p$patches$plots[[1]] +
  labs(tag = "a") +
  theme(plot.tag.position = c(0.145, 0.94),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p$patches$plots[[2]] =
  p$patches$plots[[2]] +
  labs(tag = "b") +
  theme(plot.tag.position = c(0.14, 0.94),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p$patches$plots[[3]] =
  p$patches$plots[[3]] +
  labs(tag = "c") +
  theme(plot.tag.position = c(0.145, 1.045),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p =
  p +
  labs(tag = "d") +
  theme(plot.tag.position = c(0.14, 1.005),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

ggsave(file.path(output_dir, "fig2.png"), plot = p, width = 6, height = 6, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure 2 (alternative)
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT")) %>%
##   filter(subset %in% c("best_n", "full")) %>%
##   filter(period %in% c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag")) %>%
##   mutate(period = factor(period, levels = c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag"), labels = c("Year 2-9", "Year 2-5", "Year 6-9"))) %>%
##   dplyr::select(-ps, -srel, -sme, -aic) %>%
##   pivot_wider(names_from = subset, values_from = msss) %>%
##   mutate(msss_diff = best_n - full)

## skill <-
##   skill_scores_subset %>%
##   group_by(ID, period) %>%
##   filter(msss_diff == max(msss_diff)) %>%
##   mutate(increase = msss_diff > 0) %>%
##   mutate(msss_diff = abs(msss_diff))

## skill <- gauge_stns %>% left_join(skill) %>% na.omit()
## p2 = myplotfun3(skill %>% filter(period %in% "Year 2-9"))
## p2 = p2 +
##   guides(
##     shape = guide_legend(
##       title = "Model", title.position = "top", order = 1
##     ),
##     size = guide_legend(
##       title = "Difference", title.position = "top",
##       order = 2, override.aes = list(shape = 21, fill = "transparent")
##     ),
##     fill = guide_legend(
##       title = "Direction", title.position = "top",
##       order = 3, override.aes = list(shape = 21, fill = c("#F8766D", "#00BFC4"))
##     )
##   )

## p2 = p2 +
##   theme(
##     strip.background = element_blank(),
##     legend.position = "bottom",
##     legend.box = "horizontal",
##     ## legend.box = "vertical",
##     legend.justification = "left",
##     legend.box.just = "left",
##     legend.title = element_text(size = legend_title_size),
##     legend.text = element_text(size = legend_label_size),
##     strip.text = element_blank(),
##     panel.grid.major = element_line(size = 0.25),
##     axis.text = element_text(size = axis_label_size_small)
##   )

## p4 <- myplotfun444(skill_scores_subset %>% filter(period %in% "Year 2-9"))
## p4 <- p4 + coord_fixed(ratio = 1.5)

## p <- p2 + p4 + plot_layout(widths = c(2, 2), ncol = 2, nrow = 1)
## p <- p +
##   plot_layout(guides = "collect") &
##   theme(legend.position = "bottom",
##         legend.box = "horizontal",
##         ## legend.box = "vertical",
##         legend.justification = "left",
##         legend.box.just = "left")
##         ## legend.margin = margin(0, 0, 0, 0, unit = "cm"))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.1, 0.99),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.13, 0.825),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/fig2_alternative.png", plot = p, width = 6, height = 6, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Figure 3
##
## ####################################################### ##
## ####################################################### ##

skill_scores_subset =
  skill_scores %>%
  filter(subset %in% c("best_n", "full")) %>%
  ## filter(subset %in% c("worst_n", "full")) %>%
  filter(period %in% c("yr2to9_lag")) %>%
  dplyr::select(-ps, -srel, -sme, -aic) %>%
  pivot_wider(names_from = subset, values_from = msss) %>%
  mutate(msss_diff = best_n - full)
  ## mutate(msss_diff = worst_n - full)

skill =
  skill_scores_subset %>%
  filter(model %in% c("P", "PT")) %>%
  group_by(ID) %>%
  filter(msss_diff == max(msss_diff)) %>%
  arrange(desc(msss_diff))

ids_best = head(skill$ID, n=5)

## dataset_dir = config$modelling[["hindcast"]]$input_dataset
predictions = open_dataset(
  file.path(output_root, "analysis", "hindcast", "yr2to9_lag", "prediction")
) %>% collect() %>%
  filter(model %in% "P_T" & subset %in% c("full", "best_n")) %>%
  mutate(subset = ifelse(subset == "best_n", "NAO-matched ensemble", "Full ensemble"))

## Compute ACC
acc <- predictions %>%
  group_by(model, period, ID, predictand, subset) %>%
  summarize(
    acc = cor.test(obs, Q50, method = "pearson")$estimate,
    acc_p = cor.test(obs, Q50, method = "pearson")$p.value
  )
library(viridis)
p1 = predictions %>% filter(ID %in% ids_best[1]) %>% myplotfun6()
p2 = predictions %>% filter(ID %in% ids_best[2]) %>% myplotfun6()
p3 = predictions %>% filter(ID %in% ids_best[3]) %>% myplotfun6()
p4 = predictions %>% filter(ID %in% ids_best[4]) %>% myplotfun6()
p5 = predictions %>% filter(ID %in% ids_best[5]) %>% myplotfun6()

format_p_value = function(p_value) {
  if (p_value < 0.01) {
    return("(P < 0.01)")
  } else {
    return(paste0("(P = ", sprintf(p_value, fmt = "%#.2f"), ")"))
  }
}
make_annotation = function(acc, id) {
  acc_full <- acc %>% filter(ID %in% id & subset %in% "Full ensemble")
  acc_matched <- acc %>% filter(ID %in% id & subset %in% "NAO-matched ensemble")
  annotation = paste0(
    paste0(
      "ACC (Full) = ", sprintf(acc_full$acc, fmt = "%#.2f"), " ", format_p_value(acc_full$acc_p), "\n"
    ),
    paste0(
      "ACC (NAO-matched) = ", sprintf(acc_matched$acc, fmt = "%#.2f"), " ", format_p_value(acc_matched$acc_p), "\n"
    )
    ## "RPC = ", sprintf(rpc, fmt = "%#.1f")
  )
  annotation
}

get_y_range <- function(p) {
  yrange <- layer_scales(p)$y$range$range
  return(yrange)
}
get_y_position <- function(p, rel_pos) {
  yrange <- get_y_range(p)
  return(yrange[1] + diff(yrange) * rel_pos)
}

annotation_size = 4
annotation_rel_pos = 1
yrange <- get_y_range(p1)
yrange[2] <- yrange[2] * 1.025
p1 <- p1 +
  ylim(yrange) +
  annotate(
    geom = "text",
    x = 1960,
    y = yrange[1] + diff(yrange) * annotation_rel_pos,
    label = make_annotation(acc, ids_best[1]),
    hjust=0,
    vjust=1,
    size = annotation_size / ggplot2::.pt
  )

yrange <- get_y_range(p2)
yrange[2] <- yrange[2] * 1.025
p2 <- p2 +
  annotate(
    geom = "text",
    x = 1960,
    y = yrange[1] + diff(yrange) * annotation_rel_pos,
    label = make_annotation(acc, ids_best[2]),
    hjust=0,
    vjust=1,
    size = annotation_size / ggplot2::.pt
  )

yrange <- get_y_range(p3)
yrange[2] <- yrange[2] * 1.025
p3 <- p3 +
  annotate(
    geom = "text",
    x = 1960,
    y = yrange[1] + diff(yrange) * annotation_rel_pos,
    label = make_annotation(acc, ids_best[3]),
    hjust=0,
    vjust=1,
    size = annotation_size / ggplot2::.pt
  )

yrange <- get_y_range(p4)
yrange[2] <- yrange[2] * 1.025
p4 <- p4 +
  annotate(
    geom = "text",
    x = 1960,
    y = yrange[1] + diff(yrange) * annotation_rel_pos,
    label = make_annotation(acc, ids_best[4]),
    hjust=0,
    vjust=1,
    size = annotation_size / ggplot2::.pt
  )

yrange <- get_y_range(p5)
yrange[2] <- yrange[2] * 1.025
p5 <- p5 +
  annotate(
    geom = "text",
    x = 1960,
    y = yrange[1] + diff(yrange) * annotation_rel_pos,
    label = make_annotation(acc, ids_best[5]),
    hjust=0,
    vjust=1,
    size = annotation_size / ggplot2::.pt
  )

gauge_stns_subset = gauge_stns %>% filter(ID %in% ids_best)
p6 = myplotfun777(gauge_stns_subset)
p6 =
  p6 +
  ggrepel::geom_label_repel(
             data=gauge_stns_subset,
             aes(label=ID, geometry=geometry),
             stat="sf_coordinates",
             min.segment.length = 0,
             size = 1.75)

p1 = p1 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                ## legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p2 = p2 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                ## legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p3 = p3 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                ## legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p4 = p4 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                ## legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p5 = p5 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                ## legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p6 = p6 + theme(panel.grid.major = element_line(size = 0.25),
                axis.title = element_blank(),
                axis.text = element_text(size = axis_label_size_small))

p = p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3, nrow = 2, widths = c(2, 2, 2)) & theme(legend.position = "bottom")
p = p + plot_layout(guides = "collect")

ggsave(file.path(output_dir, "fig3.png"), plot = p, width = 5, height = 5, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Figure 4
##
## ####################################################### ##
## ####################################################### ##

## skill_scores =
##   load_skill_scores(config, c("hindcast", "hindcast2", "hindcast3")) %>%
##   filter(model %in% c("NAO", "NAO_P", "NAO_P_T", "P", "PT"))
skill_scores_subset <-
  skill_scores %>%
  filter(subset %in% c("best_n", "full")) %>%
  ## filter(subset %in% c("worst_n", "full")) %>%
  filter(period %in% c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag")) %>%
  dplyr::select(-ps, -srel, -sme, -aic) %>%
  pivot_wider(names_from = subset, values_from = msss) %>%
  mutate(msss_diff = best_n - full)
  ## mutate(msss_diff = worst_n - full)

skill_scores_subset <-
  skill_scores_subset %>%
  filter(period %in% "yr2to9_lag")

## obs_skill_scores_subset <-
##   obs_skill_scores %>%
##   filter(period %in% "yr2to9" & model %in% c("NAO", "STATIONARY", "TIME")) %>%
##   group_by(ID) %>% filter(aic == min(aic)) %>%
##   filter(model %in% "NAO") %>%
##   dplyr::select(ID, msss) %>% rename(obs_msss = msss)

## x = skill_scores_subset %>% filter(ID %in% obs_skill_scores_subset$ID) %>% left_join(obs_skill_scores_subset) %>% filter(model %in% c("P", "PT"))

obs_skill_scores_subset <-
  obs_skill_scores %>% filter(period %in% "yr2to9" & model %in% "NAO") %>%
  dplyr::select(ID, msss) %>% rename(obs_msss = msss)

x = skill_scores_subset %>% left_join(obs_skill_scores_subset) %>% filter(model %in% c("P", "PT"))
## x$model = factor(x$model, levels = c("P", "P_T"), labels = c("P", "PT"))

cbbPalette <- RColorBrewer::brewer.pal(3, "Set2")
p = myplotfun9(x)
p =
  p +
  geom_vline(xintercept = 0, size = 0.25) +
  geom_hline(yintercept = 0, size = 0.25) +
  scale_color_manual(name = "Model", values = cbbPalette) +
  ## scale_color_discrete(
  ##   name = "Model"##,
  ##   ## labels = c("P", "PT")
  ## ) +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.y = element_text(size = axis_label_size),
    axis.text.x = element_text(size = axis_label_size),
    axis.title.x = element_text(size = axis_title_size),
    legend.title = element_text(size = legend_title_size),
    ## legend.title = element_text(size = legend_title_size),
    legend.text = element_text(size = legend_label_size))

ggsave(file.path(output_dir, "fig4.png"), plot = p, width = 5, height = 4, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Figure S1
##
## ####################################################### ##
## ####################################################### ##

d <- tibble(Q = c(3.5, 5, 8, 10.5),
            Lead = 1,
            Period = 8,
            Init = c(1979, 1980, 1981, 1982),
            Start = c(1980, 1981, 1982, 1983),
            End = c(1988, 1989, 1990, 1991))

cbbPalette <- RColorBrewer::brewer.pal(3, "Set2")
library(ggnewscale)
p1 <- ggplot(d) +
  geom_segment(
    data = d,
    aes(x = Start, y = Q, xend = End, yend = Q, colour = "Forecast period"),
    size = 2,
    alpha = .5
  ) +
  geom_segment(
    data = d,
    aes(x = Init, y = Q, xend = Start, yend = Q, colour = "Lead time"),
    size = 2,
    alpha = .5
  ) +
  scale_color_discrete(
    name = "",
    limits = c("Lead time", "Forecast period"),
    guide = guide_legend(ncol = 1),
    type = cbbPalette
  ) +
  new_scale_color() +
  geom_point(
    data = d %>%
      gather(-Q, -Lead, -Period, key = key, value = value) %>%
      mutate(
        key = factor(
          key,
          levels = c("Init", "Start", "End"),
          labels = c("Initialization", "Period start", "Period end")
        )
      ),
    aes(x = value, y = Q, colour= key),
    size = 3.5
  ) +
  scale_color_discrete(
    name = "",
    guide = guide_legend(ncol = 1),
    type = cbbPalette
  ) +
  scale_y_continuous(
    name = "X", #expression(bar(Y)),
    limits = c(0, 17)
  ) +
  scale_x_continuous(
    name="",
    breaks=seq(1978, 1991, by = 2),
    limits=c(1978.5, 1991.5),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_line(colour = "lightgrey", size = 0.25),
    panel.grid.minor.x = element_line(colour = "lightgrey", size = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    ## axis.text.y = element_text(size = axis_label_size_large),
    axis.title.y = element_text(size = axis_title_size),
    axis.title.x = element_blank(), #element_text(size = axis_title_size_large),
    ## axis.text.x = element_text(size = axis_label_size_large),
    axis.text.y = element_blank(),
    ## axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(0.5, 0, 0.5, 0, unit = "cm")
  )

## ggsave("fig/decadal_forecast_illustration.png", width = 6, height = 4, units = "in")

g <- ggplot_build(p)
## blue <- "#619CFF"
## green <- "#00BA38"
## turquoise <- "#00BFC4"
blue <- "#8DA0CB"
green <- "#FC8D62"
turquoise <- "#FC8D62"

## Discharge

d <-
  open_dataset(file.path(output_root, "nrfa-discharge-summaries")) %>%
  collect() %>%
  filter(ID %in% 21017 & clim_season %in% "DJFM" & season_year %in% 1978:1991) %>%
  mutate(Q_95_multiyear = rollapply(Q_95, 8, mean, align = "left", fill = NA)) %>%
  mutate(Start = season_year, End = Start + 8) %>%
  dplyr::select(clim_season, season_year, Q_95, Q_95_multiyear, Start, End) %>%
  mutate(group = ifelse(season_year %in% 1983:1991, "a", "b")) %>%
  mutate(Q_95_multiyear = ifelse(season_year %in% 1983, Q_95_multiyear, NA))

p2 <- ggplot(d) +
  geom_segment(
    data = d,
    aes(x = Start, y = Q_95_multiyear, xend = End, yend = Q_95_multiyear, colour = "Aggregation period"),
    size = 2,
    alpha = .5
  ) +
  scale_color_manual(
    name = "",
    values = turquoise,
    limits = c("Aggregation period"),
    guide = "none"
  ) +
  new_scale_color() +
  geom_point(
    data = d %>%
      dplyr::select(-group) %>%
      na.omit() %>%
      gather(-(clim_season:Q_95_multiyear), key = key, value = value) %>%
      mutate(
        key = factor(
          key,
          levels = c("Start", "End"),
          labels = c("Period start", "Period end")
        )
      ),
    aes(x = value, y = Q_95_multiyear, colour= key),
    size = 3.5
  ) +
  scale_color_manual(
    name = "",
    values = c(green, blue),
    guide = "none"
  ) +
  new_scale_color() +
  geom_point(
    data = d,
    aes(x = season_year, y = Q_95, colour = group)
  ) +
  scale_color_manual(
    name = "",
    values = c(turquoise, "darkgrey"),
    guide = "none"
  ) +
  scale_x_continuous(
    name="",
    breaks=seq(1978, 1991, by = 2),
    limits=c(1978.5, 1991.5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(name = "Q") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_line(colour = "lightgrey", size = 0.25),
    panel.grid.minor.x = element_line(colour = "lightgrey", size = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    ## axis.text.y = element_text(size = axis_label_size_large),
    axis.title.y = element_text(size = axis_title_size),
    axis.title.x = element_blank(), #element_text(size = axis_title_size_large),
    ## axis.text.x = element_text(size = axis_label_size_large),
    axis.text.y = element_blank(),
    ## axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(0.5, 0, 0.5, 0, unit = "cm")
    ## legend.position = "none"
  )

p <-
  p2 + p1 + #p1 + p2 +
  plot_layout(nrow = 2, ncol = 1, guides = "collect") &
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size))
p$patches$plots[[1]] =
  p$patches$plots[[1]] +
  labs(tag = "a") +
  theme(plot.tag.position = c(0.04, 1.01),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p = p +
  labs(tag = "b") +
  theme(plot.tag.position = c(0.04, 1.01),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

ggsave(file.path(output_dir, "figS1.png"), plot = p, width = 6, height = 6, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S1
## ##
## ## ####################################################### ##
## ## ####################################################### ##

load_fcst <- function(aggregation_period) {
  fcst = read_parquet(
    file.path(output_root, "analysis", aggregation_period, "ensemble_mean_fcst.parquet")
  )
  fcst <-
    fcst %>%
    group_by(variable) %>%
    arrange(init_year) %>%
    mutate(
      ens_q95_lag = zoo::rollmean(ens_q95, 4, na.pad = TRUE, align = "right"),
      ens_q05_lag = zoo::rollmean(ens_q05, 4, na.pad = TRUE, align = "right")
    )

  ## Add full ensemble mean
  full_fcst <-
    fcst %>%
    dplyr::select(init_year, variable, starts_with("obs"), starts_with("ens")) %>%
    rename(full_ens_mean = ens_mean)
  full_fcst
}

compute_acc <- function(fcst, var_name, obs_name, model_name) {
  obs = fcst %>% filter(variable %in% var_name) %>% `[[`(obs_name)
  mod = fcst %>% filter(variable %in% var_name) %>% `[[`(model_name)
  na_ix = is.na(obs) | is.na(mod)
  obs = obs[!na_ix]
  mod = mod[!na_ix]
  acc = cor.test(obs, mod, method = "pearson")
  acc
}

compute_predictable_sd <- function(fcst, var_name, model_name) {
  mod = fcst %>% filter(variable %in% var_name) %>% `[[`(model_name)
  pred_sd = sd(mod, na.rm = T)
  pred_sd
}

compute_total_sd <- function(ensemble_fcst, var_name) {
  sd =
    ensemble_fcst %>%
    group_by(project, mip, source_id, member) %>%
    summarize(across(all_of(var_name), list(sd = ~sd(.x, na.rm = T)), .names = "sd"))
  tot_sd = mean(sd$sd, na.rm = T)
  tot_sd
}

compute_rpc <- function(acc, pred_sd, tot_sd) {
  rpc = acc / (pred_sd / tot_sd)
  rpc
}

format_p_value <- function(p_value) {
  if (p_value < 0.01) {
    return("(P < 0.01)")
  } else {
    return(paste0("(P = ", sprintf(p_value, fmt = "%#.2f"), ")"))
  }
}

make_annotation <- function(acc, rpc) {
  annotation = paste0(
    "ACC = ", sprintf(acc$estimate, fmt = "%#.2f"), " ", format_p_value(acc$p.value), ", ",
    "RPC = ", sprintf(rpc, fmt = "%#.1f")
  )
  annotation
}

## full_fcst <- load_fcst("yr2to9_lag") %>% mutate(period = "Year 2-9")
## ensemble_fcst <- read_parquet(
##   file.path(output_root, "hindcast-analysis", "yr2to9_lag", "ensemble_fcst.parquet")
## )

## p1 <- myplotfun1010(full_fcst, "nao")

## acc <- compute_acc(full_fcst, "nao", "obs", "ens_mean_lag")
## pred_sd <- compute_predictable_sd(full_fcst, "nao", "ens_mean_lag")
## tot_sd <- compute_total_sd(ensemble_fcst, "nao")
## rpc <- compute_rpc(acc$estimate, pred_sd, tot_sd)

## p1 <-
##   p1 +
##   scale_y_continuous(
##     name="NAO anomaly (hPa)",
##     breaks=seq(-7.5, 7.5, by=2.5),
##     limits=c(-7.5, 7.5)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 7.5,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -7.5,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )


## p2 = myplotfun1010(full_fcst, "uk_temp")

## acc = compute_acc(full_fcst, "uk_temp", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "uk_temp", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "uk_temp")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p2 =
##   p2 +
##   scale_y_continuous(
##     name=expression(atop("Northern European", "temperature anomaly (K)")),
##     breaks=seq(-1.25, 1.25, by=0.25),
##     limits=c(-1.25, 1.25)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 1.25,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -1.2,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## p3 = myplotfun1010(full_fcst, "european_precip")

## acc = compute_acc(full_fcst, "european_precip", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "european_precip", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "european_precip")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p3 =
##   p3 +
##   scale_y_continuous(
##     name=expression(atop("Northern European", paste("precipitation anomaly " (mm~day^{-1})))),
##     breaks=c(-0.5, -0.25, 0.0, 0.25, 0.5),
##     limits=c(-0.5, 0.5)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 0.5,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -0.5,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## full_fcst = load_fcst("yr2to5_lag") %>% mutate(period = "Year 2-5")
## ensemble_fcst = read_parquet(
##   file.path(output_root, "hindcast-analysis", "yr2to5_lag", "ensemble_fcst.parquet")
## )

## p4 = myplotfun1010(full_fcst, "nao")

## acc = compute_acc(full_fcst, "nao", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "nao", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "nao")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p4 =
##   p4 +
##   scale_y_continuous(
##     name="NAO anomaly (hPa)",
##     breaks=seq(-7.5, 7.5, by=2.5),
##     limits=c(-7.5, 7.5)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 7.5,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -7.5,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## p5 = myplotfun1010(full_fcst, "uk_temp")

## acc = compute_acc(full_fcst, "uk_temp", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "uk_temp", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "uk_temp")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p5 =
##   p5 +
##   scale_y_continuous(
##     name=expression(atop("Northern European", "temperature anomaly (K)")),
##     breaks=seq(-1.25, 1.25, by=0.25),
##     limits=c(-1.25, 1.25)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 1.25,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -1,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## p6 = myplotfun1010(full_fcst, "european_precip")

## acc = compute_acc(full_fcst, "european_precip", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "european_precip", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "european_precip")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p6 =
##   p6 +
##   scale_y_continuous(
##     name=expression(atop("Northern European", paste("precipitation anomaly " (mm~day^{-1})))),
##     breaks=c(-0.5, -0.25, 0.0, 0.25, 0.5),
##     limits=c(-0.5, 0.5)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 0.5,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -0.5,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## full_fcst = load_fcst("yr6to9_lag") %>% mutate(period = "Year 6-9")
## ensemble_fcst = read_parquet(
##   file.path(output_root, "hindcast-analysis", "yr6to9_lag", "ensemble_fcst.parquet")
## )

## p7 = myplotfun1010(full_fcst, "nao")

## acc = compute_acc(full_fcst, "nao", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "nao", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "nao")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p7 =
##   p7 +
##   scale_y_continuous(
##     name="NAO anomaly (hPa)",
##     breaks=seq(-7.5, 7.5, by=2.5),
##     limits=c(-7.5, 7.5)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 7.5,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -7.5,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## p8 = myplotfun1010(full_fcst, "uk_temp")

## acc = compute_acc(full_fcst, "uk_temp", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "uk_temp", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "uk_temp")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p8 =
##   p8 +
##   scale_y_continuous(
##     name=expression(atop("Northern European", "temperature anomaly (K)")),
##     breaks=seq(-1.25, 1.25, by=0.25),
##     limits=c(-1.25, 1.25)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 1.25,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -1,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## p9 = myplotfun1010(full_fcst, "european_precip")

## acc = compute_acc(full_fcst, "european_precip", "obs", "ens_mean_lag")
## pred_sd = compute_predictable_sd(full_fcst, "european_precip", "ens_mean_lag")
## tot_sd = compute_total_sd(ensemble_fcst, "european_precip")
## rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

## p9 =
##   p9 +
##   scale_y_continuous(
##     name=expression(atop("Northern European", paste("precipitation anomaly " (mm~day^{-1})))),
##     breaks=c(-0.5, -0.25, 0.0, 0.25, 0.5),
##     limits=c(-0.5, 0.5)
##   ) +
##   annotate( #
##     geom = "text",
##     x = 1960, y = 0.5,
##     label = make_annotation(acc, rpc),
##     hjust=0,
##     vjust=1,
##     size = axis_label_size / ggplot2::.pt
##   ) ## +
##   ## annotate(
##   ##   geom = "text",
##   ##   x = 1960, y = -0.5,
##   ##   label = "Raw lagged ensemble",
##   ##   hjust=0,
##   ##   vjust=0,
##   ##   size = axis_label_size / ggplot2::.pt
##   ## )

## p1 = p1 + theme(axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 strip.background = element_blank(),
##                 axis.text.y = element_text(size = axis_label_size_small),
##                 axis.title.y = element_text(size = axis_title_size_small))
## p2 = p2 + theme(axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 strip.background = element_blank(),
##                 strip.text = element_blank(),
##                 axis.text.y = element_text(size = axis_label_size_small),
##                 axis.title.y = element_text(size = axis_title_size_small))
## p3 = p3 + theme(axis.text.x = element_text(size = axis_label_size_small),
##                 axis.title.x = element_blank(),
##                 strip.background = element_blank(),
##                 strip.text = element_blank(),
##                 axis.text.y = element_text(size = axis_label_size_small),
##                 axis.title.y = element_text(size = axis_title_size_small))
## p4 = p4 + theme(axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 strip.background = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.title.y = element_blank())
## p5 = p5 + theme(axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 strip.background = element_blank(),
##                 strip.text = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.title.y = element_blank())
## p6 = p6 + theme(axis.text.x = element_text(size = axis_label_size_small),
##                 axis.title.x = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 strip.background = element_blank(),
##                 strip.text = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.title.y = element_blank())
## p7 = p7 + theme(axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 strip.background = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.title.y = element_blank())
## p8 = p8 + theme(axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 strip.background = element_blank(),
##                 strip.text = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.title.y = element_blank())
## p9 = p9 + theme(axis.text.x = element_text(size = axis_label_size_small),
##                 axis.title.x = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 strip.background = element_blank(),
##                 strip.text = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.title.y = element_blank())

## p = p1 + p4 + p7 + p2 + p5 + p8 + p3 + p6 + p9 + plot_layout(nrow = 3, ncol = 3) & theme(legend.position = "bottom")
## p = p + plot_layout(guides = "collect")

## ggsave("fig/figS1.png", plot = p, width = 6, height = 6, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Figure S2
##
## ####################################################### ##
## ####################################################### ##

skill_scores_subset =
  skill_scores %>%
  filter(
    subset %in% "full",
    period %in% "yr2to9_lag",
    model %in% c("P", "PT", "NAOPT")
  )

skill =
  skill_scores_subset %>%
  group_by(ID, subset, period) %>%
  filter(aic == min(aic)) %>%
  arrange(desc(msss))
ids_best = head(skill$ID, n=5)
ids_worst = tail(skill$ID, n=5)

dataset_dir = config$modelling[["hindcast"]]$input_dataset
predictions = open_dataset(
  file.path(output_root, "analysis", "hindcast", "yr2to9_lag", "prediction")
) %>%
  collect() %>%
  filter(model %in% c("P", "P_T", "NAO_P_T"))
  ## filter(model %in% model_levels)
predictions$model = factor(predictions$model, levels = model_levels, labels = model_labels)

p1 = predictions %>% filter(ID %in% ids_best[1] & subset %in% "full") %>% myplotfun11()
p2 = predictions %>% filter(ID %in% ids_best[2] & subset %in% "full") %>% myplotfun11()
p3 = predictions %>% filter(ID %in% ids_best[3] & subset %in% "full") %>% myplotfun11()
p4 = predictions %>% filter(ID %in% ids_best[4] & subset %in% "full") %>% myplotfun11()
p5 = predictions %>% filter(ID %in% ids_best[5] & subset %in% "full") %>% myplotfun11()
p6 = predictions %>% filter(ID %in% ids_worst[1] & subset %in% "full") %>% myplotfun11()
p7 = predictions %>% filter(ID %in% ids_worst[2] & subset %in% "full") %>% myplotfun11()
p8 = predictions %>% filter(ID %in% ids_worst[3] & subset %in% "full") %>% myplotfun11()
p9 = predictions %>% filter(ID %in% ids_worst[4] & subset %in% "full") %>% myplotfun11()
p10 = predictions %>% filter(ID %in% ids_worst[5] & subset %in% "full") %>% myplotfun11()

p1 = p1 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p2 = p2 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p3 = p3 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p4 = p4 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p5 = p5 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p6 = p6 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p7 = p7 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p8 = p8 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p9 = p9 + theme(panel.grid = element_blank(),
                strip.text = element_text(size = strip_label_size),
                legend.title = element_text(size = legend_title_size),
                legend.text = element_text(size = legend_label_size),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p10 = p10 + theme(panel.grid = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_text(size = axis_label_size_small),
                  axis.text.x = element_text(size = axis_label_size_small))

p = p1 + p2 + p3 + p4 + p6 + p7 + p8 + p9 +
  plot_layout(ncol = 4, nrow = 2) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "left"
  )
p = p + plot_layout(guides = "collect")
ggsave(file.path(output_dir, "figS2.png"), plot = p, width = 6, height = 5, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Figure S3
##
## ####################################################### ##
## ####################################################### ##

full_fcst = load_fcst("yr2to9_lag")

obs = read_parquet(
  file.path(output_root, "analysis", "yr2to9_lag", "obs_study_period.parquet")
)

ensemble_fcst = read_parquet(
  file.path(output_root, "analysis", "yr2to9_lag", "ensemble_fcst.parquet")
)

nao_matched_ensemble_fcst = read_parquet(
  file.path(output_root, "analysis", "yr2to9_lag", "matched_ensemble.parquet")
)

## Select n best performing members
nao_matched_ensemble_fcst_best =
  nao_matched_ensemble_fcst %>%
  group_by(source_id, member, init_year) %>%
  ## mutate(any_na = any(is.na(value))) %>%
  ## filter(!any_na) %>%
  group_by(init_year, variable) %>%
  slice_min(error, n = 20)

nao_matched_fcst =
  nao_matched_ensemble_fcst_best %>%
  group_by(init_year, variable) %>%
  summarize(ens_mean = mean(value, na.rm=TRUE)) %>%
  ungroup()

## Join with observed data
nao_matched_fcst =
  nao_matched_fcst %>%
  left_join(obs, by = c("init_year", "variable"))

## Smooth
nao_matched_fcst =
  nao_matched_fcst %>%
  group_by(variable) %>%
  mutate(
    ens_mean_lag = rollmean(
      ens_mean,
      4,
      na.pad=TRUE,
      align="right"
    )
  )

## Adjust variance to match that of observed
nao_matched_fcst =
  nao_matched_fcst %>%
  group_by(variable) %>%
  mutate(ens_mean_var_adj = ens_mean * sd(obs) / sd(ens_mean, na.rm=T)) %>%
  mutate(ens_mean_lag_std = ens_mean_lag / sd(ens_mean_lag, na.rm=T)) %>%
  mutate(ens_mean_lag_var_adj = ens_mean_lag_std * sd(obs))

## Plot 1 [analog of Fig 2a from Smith et al. 2020]
acc = compute_acc(full_fcst, "nao", "obs", "full_ens_mean")
pred_sd = compute_predictable_sd(full_fcst, "nao", "full_ens_mean")
tot_sd = compute_total_sd(ensemble_fcst, "nao")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)


plotdata =
  full_fcst %>%
  filter(variable %in% "nao") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "full_ens_mean", "ens_q95", "ens_q05")) %>%
  mutate(statistic = factor(
           statistic,
           levels = c("obs", "full_ens_mean", "ens_q95", "ens_q05"),
           labels = c("Observed", "Modelled", "ens_q95", "ens_q05")))

p1 = ggplot() +
  geom_ribbon(
    data = plotdata %>% filter(statistic %in% c("ens_q95", "ens_q05")) %>% pivot_wider(names_from = statistic, values_from = value),
    aes(x = init_year, ymin = ens_q05, ymax = ens_q95), fill = "red", alpha=0.15
  ) +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name="NAO anomaly (hPa)",
    breaks=seq(-7.5, 7.5, by=2.5),
    limits=c(-7.5, 7.5)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p1 =
  p1 +
  annotate(
    geom = "text",
    x = 1960, y = 7.5,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -7.5,
    label = "Raw ensemble",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

## ggsave("fig/figS5a.png", width = 4, height = 4, units = "in")

## Plot 2 [analog of Fig 2b from Smith et al. 2020]
acc = compute_acc(full_fcst, "nao", "obs", "ens_mean_lag_var_adj")
pred_sd = compute_predictable_sd(full_fcst, "nao", "ens_mean_lag")
tot_sd = compute_total_sd(ensemble_fcst, "nao")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

plotdata =
  full_fcst %>%
  filter(variable %in% "nao") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "ens_mean_lag_var_adj", "ens_mean_var_adj")) %>%
  mutate(statistic = factor(
           statistic,
           levels = c("obs", "ens_mean_lag_var_adj", "ens_mean_var_adj") ,
           labels = c("Observed", "Modelled", "ens_mean_var_adj"))) %>%
  mutate(value = ifelse(init_year < 1964, NA, value))

p2 = ggplot() +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("ens_mean_var_adj")),
    aes(x = init_year, y = value), color = "#F8766D", size = 0.25
  ) +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name="NAO anomaly (hPa)",
    breaks=seq(-7.5, 7.5, by=2.5),
    limits=c(-7.5, 7.5)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p2 =
  p2 +
  annotate(
    geom = "text",
    x = 1960, y = 7.5,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -7.5,
    label = "Variance-adjusted and lagged",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

## Plot 3 [analog of Fig 2c from Smith et al. 2020]
acc = compute_acc(full_fcst, "amv", "obs", "ens_mean_lag")
pred_sd = compute_predictable_sd(full_fcst, "amv", "ens_mean_lag")
tot_sd = compute_total_sd(ensemble_fcst, "amv")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

plotdata =
  full_fcst %>%
  filter(variable %in% "amv") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "ens_mean_lag", "ens_q95_lag", "ens_q05_lag")) %>%
  mutate(statistic = factor(
           statistic,
           levels = c("obs", "ens_mean_lag", "ens_q95_lag", "ens_q05_lag"),
           labels = c("Observed", "Modelled", "ens_q95", "ens_q05"))) %>%
  mutate(value = ifelse(init_year < 1964, NA, value))

p3 = ggplot() +
  geom_ribbon(
    data = plotdata %>% filter(statistic %in% c("ens_q95", "ens_q05")) %>% pivot_wider(names_from = statistic, values_from = value),
    aes(x = init_year, ymin = ens_q05, ymax = ens_q95), fill = "red", alpha=0.15
  ) +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name="AMV anomaly (K)",
    breaks=c(-0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3),
    limits=c(-0.3, 0.3)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p3 =
  p3 +
  annotate(
    geom = "text",
    x = 1960, y = 0.3,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -0.3,
    label = "Raw lagged ensemble",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

## Plot 4 [analog of Fig 2d from Smith et al. 2020]
acc = compute_acc(nao_matched_fcst, "amv", "obs", "ens_mean")
pred_sd = compute_predictable_sd(nao_matched_fcst, "amv", "ens_mean")
tot_sd = compute_total_sd(ensemble_fcst, "amv")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

plotdata =
  nao_matched_fcst %>%
  filter(variable %in% "amv") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "ens_mean_var_adj")) %>%
  mutate(statistic = factor(statistic, levels = c("obs", "ens_mean_var_adj"), labels = c("Observed", "Modelled"))) %>%
  mutate(value = ifelse(init_year < 1964, NA, value))

p4 = ggplot() +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name="AMV anomaly (K)",
    breaks=c(-0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3),
    limits=c(-0.3, 0.3)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p4 =
  p4 +
  annotate(
    geom = "text",
    x = 1960, y = 0.3,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -0.3,
    label = "NAO-matched",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

## Plot 5 [analog of Fig 2e from Smith et al. 2020]
acc = compute_acc(full_fcst, "european_precip", "obs", "ens_mean_lag")
pred_sd = compute_predictable_sd(full_fcst, "european_precip", "ens_mean_lag")
tot_sd = compute_total_sd(ensemble_fcst, "european_precip")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

plotdata =
  full_fcst %>%
  filter(variable %in% "european_precip") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "ens_mean_lag", "ens_q95_lag", "ens_q05_lag")) %>%
  mutate(statistic = factor(
           statistic,
           levels = c("obs", "ens_mean_lag", "ens_q95_lag", "ens_q05_lag"),
           labels = c("Observed", "Modelled", "ens_q95", "ens_q05"))) %>%
  mutate(value = ifelse(init_year < 1964, NA, value))

p5 = ggplot() +
  geom_ribbon(
    data = plotdata %>% filter(statistic %in% c("ens_q95", "ens_q05")) %>% pivot_wider(names_from = statistic, values_from = value),
    aes(x = init_year, ymin = ens_q05, ymax = ens_q95), fill = "red", alpha=0.15
  ) +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name=expression(atop("Northern European", paste("precipitation anomaly " (mm~day^{-1})))),
    breaks=c(-0.5, -0.25, 0.0, 0.25, 0.5),
    limits=c(-0.5, 0.5)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p5 =
  p5 +
  annotate(
    geom = "text",
    x = 1960, y = 0.5,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -0.5,
    label = "Raw lagged ensemble",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

## Plot 6 [analog of Fig 2f from Smith et al. 2020]
acc = compute_acc(nao_matched_fcst, "european_precip", "obs", "ens_mean")
pred_sd = compute_predictable_sd(nao_matched_fcst, "european_precip", "ens_mean")
tot_sd = compute_total_sd(ensemble_fcst, "european_precip")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

plotdata =
  nao_matched_fcst %>%
  filter(variable %in% "european_precip") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "ens_mean_var_adj")) %>%
  mutate(statistic = factor(statistic, levels = c("obs", "ens_mean_var_adj"), labels = c("Observed", "Modelled"))) %>%
  mutate(value = ifelse(init_year < 1964, NA, value))

p6 = ggplot() +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name=expression(atop("Northern European", paste("precipitation anomaly " (mm~day^{-1})))),
    breaks=c(-0.5, -0.25, 0.0, 0.25, 0.5),
    limits=c(-0.5, 0.5)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p6 =
  p6 +
  annotate(
    geom = "text",
    x = 1960, y = 0.5,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -0.5,
    label = "NAO-matched",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

## Plot 7
acc = compute_acc(full_fcst, "uk_temp", "obs", "ens_mean_lag")
pred_sd = compute_predictable_sd(full_fcst, "uk_temp", "ens_mean_lag")
tot_sd = compute_total_sd(ensemble_fcst, "uk_temp")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

plotdata =
  full_fcst %>%
  filter(variable %in% "uk_temp") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "ens_mean_lag", "ens_q95_lag", "ens_q05_lag")) %>%
  mutate(statistic = factor(
           statistic,
           levels = c("obs", "ens_mean_lag", "ens_q95_lag", "ens_q05_lag"),
           labels = c("Observed", "Modelled", "ens_q95", "ens_q05"))) %>%
  mutate(value = ifelse(init_year < 1964, NA, value))

p7 = ggplot() +
  geom_ribbon(
    data = plotdata %>% filter(statistic %in% c("ens_q95", "ens_q05")) %>% pivot_wider(names_from = statistic, values_from = value),
    aes(x = init_year, ymin = ens_q05, ymax = ens_q95), fill = "red", alpha=0.15
  ) +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name=expression(atop("Northern European", "temperature anomaly (K)")),
    breaks=c(-1.2, -0.8, -0.4, 0, 0.4, 0.8, 1.2),
    limits=c(-1.2, 1.2)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p7 =
  p7 +
  annotate(
    geom = "text",
    x = 1960, y = 1.2,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -1.2,
    label = "Raw lagged ensemble",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

## Plot 8
acc = compute_acc(nao_matched_fcst, "uk_temp", "obs", "ens_mean")
pred_sd = compute_predictable_sd(nao_matched_fcst, "uk_temp", "ens_mean")
tot_sd = compute_total_sd(ensemble_fcst, "uk_temp")
rpc = compute_rpc(acc$estimate, pred_sd, tot_sd)

plotdata =
  nao_matched_fcst %>%
  filter(variable %in% "uk_temp") %>%
  pivot_longer(c(-init_year, -variable), names_to = "statistic", values_to = "value") %>%
  filter(statistic %in% c("obs", "ens_mean_var_adj")) %>%
  mutate(statistic = factor(statistic, levels = c("obs", "ens_mean_var_adj"), labels = c("Observed", "Modelled"))) %>%
  mutate(value = ifelse(init_year < 1964, NA, value))

p8 = ggplot() +
  geom_line(
    data = plotdata %>% filter(statistic %in% c("Observed", "Modelled")),
    aes(x = init_year, y = value, color = statistic)
  ) +
  geom_hline(yintercept=0, size=0.25) +
  scale_y_continuous(
    name=expression(atop("Northern European", "temperature anomaly (K)")),
    breaks=c(-1.2, -0.8, -0.4, 0, 0.4, 0.8, 1.2),
    limits=c(-1.2, 1.2)
  ) +
  scale_x_continuous(
    name = "",
    breaks = seq(1960, 2000, 10),
    limits = c(1960, 2005)
  ) +
  scale_color_discrete(
    name = "",
    labels = c("Observed", "Modelled"),
    type = cbbPalette[2:1]
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = strip_label_size),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = legend_label_size),
        axis.title.y = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_label_size_small),
        axis.text.x = element_text(size = axis_label_size_small))

p8 =
  p8 +
  annotate(
    geom = "text",
    x = 1960, y = 1.2,
    label = make_annotation(acc, rpc),
    hjust=0,
    vjust=1,
    size = axis_label_size / ggplot2::.pt
  ) +
  annotate(
    geom = "text",
    x = 1960, y = -1.2,
    label = "NAO-matched",
    hjust=0,
    vjust=0,
    size = axis_label_size / ggplot2::.pt
  )

p1 = p1 + theme(axis.title.y = element_text(size = axis_title_size_small))
p3 = p3 + theme(axis.title.y = element_text(size = axis_title_size_small))
p5 = p5 + theme(axis.title.y = element_text(size = axis_title_size_small))
p7 = p7 + theme(axis.title.y = element_text(size = axis_title_size_small))

p2 = p2 + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
p4 = p4 + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
p6 = p6 + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
p8 = p8 + theme(axis.text.y = element_blank(), axis.title.y = element_blank())

p = p1 + p2 + p3 + p4 + p7 + p8 + p5 + p6 + plot_layout(ncol = 2, nrow = 4) & theme(legend.position = "bottom")
p = p + plot_layout(guides = "collect")

p$patches$plots[[1]] =
  p$patches$plots[[1]] +
  labs(tag = "a") +
  theme(plot.tag.position = c(0.215, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p$patches$plots[[2]] =
  p$patches$plots[[2]] +
  labs(tag = "b") +
  theme(plot.tag.position = c(0.03, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p$patches$plots[[3]] =
  p$patches$plots[[3]] +
  labs(tag = "c") +
  theme(plot.tag.position = c(0.215, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p$patches$plots[[4]] =
  p$patches$plots[[4]] +
  labs(tag = "d") +
  theme(plot.tag.position = c(0.03, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p$patches$plots[[5]] =
  p$patches$plots[[5]] +
  labs(tag = "e") +
  theme(plot.tag.position = c(0.215, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p$patches$plots[[6]] =
  p$patches$plots[[6]] +
  labs(tag = "f") +
  theme(plot.tag.position = c(0.03, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p$patches$plots[[7]] =
  p$patches$plots[[7]] +
  labs(tag = "g") +
  theme(plot.tag.position = c(0.215, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p =
  p +
  labs(tag = "h") +
  theme(plot.tag.position = c(0.03, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

ggsave(file.path(output_dir, "figS3.png"), p, width = 6, height = 7, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Figure S4
##
## ####################################################### ##
## ####################################################### ##

dataset_dir = config$modelling[["hindcast"]]$input_dataset
predictions = open_dataset(
  file.path(output_root, "analysis", "hindcast", "yr2to9_lag", "prediction")
) %>%
  collect() %>%
  filter(model %in% "P_T" & subset %in% c("full", "best_n")) %>%
  mutate(subset = ifelse(subset == "best_n", "NAO-matched ensemble", "Full ensemble"))

## Compute anomalies
predictions0 <-
  predictions %>%
  group_by(period, ID, predictand, subset) %>%
  summarize(
    ens_mean = mean(Q95, na.rm = TRUE),
    obs_mean = mean(obs, na.rm = TRUE),
    ens_sd = sd(Q95, na.rm = TRUE),
    obs_sd = sd(obs, na.rm = TRUE)
  )

predictions1 <-
  predictions %>%
  filter(year %in% 1985:1989) %>%
  group_by(period, ID, predictand, subset) %>%
  summarize(
    ens_subset_mean = mean(Q95, na.rm = TRUE),
    obs_subset_mean = mean(obs, na.rm = TRUE),
    ens_subset_sd = sd(Q95, na.rm = TRUE),
    obs_subset_sd = sd(obs, na.rm = TRUE)
  )

predictions0 <- predictions0 %>% left_join(predictions1)

predictions0 <-
  predictions0 %>%
  mutate(
    obs_anom = (obs_subset_mean - obs_mean) / obs_sd,
    ens_anom = (ens_subset_mean - ens_mean) / ens_sd
  )

dat1 <- predictions0 %>% filter(subset %in% "Full ensemble") %>% mutate(anom = obs_anom)
dat1 <- gauge_stns %>% left_join(dat1, by = "ID")
p1 <- myplotfun888(dat1)

dat2 <- predictions0 %>% filter(subset %in% "Full ensemble") %>% mutate(anom = ens_anom)
dat2 <- gauge_stns %>% left_join(dat2, by = "ID")
p2 <- myplotfun888(dat2)

dat3 <- predictions0 %>% filter(subset %in% "NAO-matched ensemble") %>% mutate(anom = ens_anom)
dat3 <- gauge_stns %>% left_join(dat3, by = "ID")
p3 <- myplotfun888(dat3)

p1 = p1 + theme(legend.text = element_text(size = legend_label_size),
                axis.title.y = element_text(size = axis_title_size),
                axis.text.y = element_text(size = axis_label_size_small),
                axis.text.x = element_text(size = axis_label_size_small))
p2 = p2 + theme(axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x = element_text(size = axis_label_size_small))
p3 = p3 + theme(axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x = element_text(size = axis_label_size_small))

p <- p1 + p2 + p3 + plot_layout(ncol = 3, nrow = 1) & theme(legend.position = "bottom")
p = p + plot_layout(guides = "collect")

p$patches$plots[[1]] =
  p$patches$plots[[1]] +
  labs(tag = "a. Observed") +
  theme(plot.tag.position = c(0.15, 1.03),
        plot.tag = element_text(hjust = 0, size = tag_label_size, face="bold"))
p$patches$plots[[2]] =
  p$patches$plots[[2]] +
  labs(tag = "b. Full ensemble") +
  theme(plot.tag.position = c(0.025, 1.03),
        plot.tag = element_text(hjust = 0, size = tag_label_size, face="bold"))
p =
  p +
  labs(tag = "c. NAO-matched ensemble") +
  theme(plot.tag.position = c(0.025, 1.03),
        plot.tag = element_text(#vjust = -0.7,
                                hjust = 0, size = tag_label_size, face="bold"))

ggsave(file.path(output_dir, "figS4.png"), plot = p, width = 5, height = 4.5, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S3
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## obs_skill_scores_subset <-
##   obs_skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT")) %>%
##   filter(period %in% "yr2to9") %>%
##   mutate(period = factor(period, levels = "yr2to9", labels = "Year 2-9"))

## ## Perfection prediction case
## skill1 = myfun(obs_skill_scores_subset)
## skill2 = myfun(obs_skill_scores_subset %>% filter(!model %in% "NAOPT"))

## p1 = myplotfun1(na.omit(skill1))
## p2 = myplotfun1(na.omit(skill2))
## p3 <- myplotfun2(obs_skill_scores_subset)

## p3 <- p3 + coord_fixed(ratio = 3)
## rdbu_pal = brewer.pal(9, "RdBu")
## p1 <- p1 +
##   scale_fill_stepsn(
##     colours = rdbu_pal[3:9],
##     breaks = seq(-0.4, 0.8, 0.2),
##     limits = c(-0.3, 0.9)
##   )
## p2 <- p2 +
##   scale_fill_stepsn(
##     colours = rdbu_pal[3:9],
##     breaks = seq(-0.4, 0.8, 0.2),
##     limits = c(-0.3, 0.9)
##   )
## p2 <- p2 + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
## p2 <- p2 + guides(shape = "none")
## p = p1 + p2 + p3 + plot_layout(widths = c(2, 2, 2), ncol = 3, nrow = 1)
## p = p +
##   plot_layout(guides = "collect") &
##   theme(legend.position = "bottom",
##         legend.margin = margin(0, 0, 0, 0, unit = "cm"),
##         legend.box = "vertical",
##         legend.justification = "left",
##         legend.box.just = "left",
##         legend.box.margin = margin(-1, 0, 0, 0, unit = "cm"))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.145, 0.96),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.05, 0.96),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.185, 0.83),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/figS2.png", plot = p, width = 6, height = 4.75, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S3
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## Figure S2: As Fig 1, but year 2-5, 6-9 [Supp Mat]

## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT")) %>%
##   filter(subset %in% "full", period %in% "yr2to5_lag") %>%
##   mutate(period = factor(period, levels = "yr2to5_lag", labels = "Year 2-5"))
## skill1 = myfun(skill_scores_subset)
## skill2 = myfun(skill_scores_subset %>% filter(!model %in% "NAOPT"))

## p4 = myplotfun1(na.omit(skill1))
## p5 = myplotfun1(na.omit(skill2))
## p6 = myplotfun2(skill_scores_subset)
## p6 <- p6 + coord_fixed(ratio = 3)
## rdbu_pal = brewer.pal(9, "RdBu")
## p4 <- p4 +
##   scale_fill_stepsn(
##     colours = rdbu_pal[3:9],
##     breaks = seq(-0.4, 0.8, 0.2),
##     limits = c(-0.3, 0.9)
##   )
## p5 <- p5 +
##   scale_fill_stepsn(
##     colours = rdbu_pal[3:9],
##     breaks = seq(-0.4, 0.8, 0.2),
##     limits = c(-0.3, 0.9)
##   )
## p5 <- p5 + guides(shape = "none")

## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT")) %>%
##   filter(subset %in% "full", period %in% "yr6to9_lag") %>%
##   mutate(period = factor(period, levels = "yr6to9_lag", labels = "Year 6-9"))
## skill1 = myfun(skill_scores_subset)
## skill2 = myfun(skill_scores_subset %>% filter(!model %in% "NAOPT"))

## p7 = myplotfun1(na.omit(skill1))
## p8 = myplotfun1(na.omit(skill2))
## p9 = myplotfun2(skill_scores_subset)
## p9 <- p9 + coord_fixed(ratio = 3)
## rdbu_pal = brewer.pal(9, "RdBu")
## p7 <- p7 +
##   scale_fill_stepsn(
##     colours = rdbu_pal[3:9],
##     breaks = seq(-0.4, 0.8, 0.2),
##     limits = c(-0.3, 0.9)
##   )
## p8 <- p8 +
##   scale_fill_stepsn(
##     colours = rdbu_pal[3:9],
##     breaks = seq(-0.4, 0.8, 0.2),
##     limits = c(-0.3, 0.9)
##   )
## p8 <- p8 + guides(shape = "none")

## ## Join the plots together
## p = p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(widths = c(2, 2, 2), ncol = 3, nrow = 2)
## p = p +
##   plot_layout(guides = "collect") &
##   theme(legend.position = "bottom",
##         legend.box = "vertical",
##         legend.justification = "left",
##         legend.box.just = "left",
##         legend.margin = margin(0, 0, 0, 0, unit = "cm"))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.145, 0.99),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.145, 0.99),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[3]] =
##   p$patches$plots[[3]] +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.185, 0.85),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[4]] =
##   p$patches$plots[[4]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.145, 0.995),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[5]] =
##   p$patches$plots[[5]] +
##   labs(tag = "e") +
##   theme(plot.tag.position = c(0.145, 0.995),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "f") +
##   theme(plot.tag.position = c(0.185, 0.865),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/figS3.png", plot = p, width = 6, height = 8, units = "in")


## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S6
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## MSSS comparison between full and NAO-matched

## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT")) %>%
##   filter(subset %in% c("best_n", "full")) %>%
##   mutate(subset = ifelse(subset == "best_n", "NAO-matched ensemble", "Full ensemble")) %>%
##   filter(period %in% c("yr2", "yr2to5_lag", "yr2to9_lag", "yr6to9_lag")) %>%
##   mutate(period = factor(period, levels = c("yr2", "yr2to5_lag", "yr2to9_lag", "yr6to9_lag"), labels = c("Year 2", "Year 2-5", "Year 2-9", "Year 6-9")))

## p1 = myplotfun12(skill_scores_subset %>% filter(period %in% "Year 2-9"))
## p2 = myplotfun12(skill_scores_subset %>% filter(period %in% "Year 2-5"))
## p3 = myplotfun12(skill_scores_subset %>% filter(period %in% "Year 6-9"))

## p3 = p3 + theme(legend.title = element_blank(), legend.position = "bottom")

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size))
## p3 = p3 + theme(panel.grid = element_blank(),
##                 strip.text = element_text(size = strip_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size))

## p1 = p1 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))
## p2 = p2 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))
## p3 = p3 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))

## p =
##   p1 + p2 + p3 +
##   plot_layout(nrow = 1, ncol = 3, guides = "collect") &
##   theme(legend.title = element_blank(),
##         legend.position = "bottom",
##         legend.text = element_text(size = legend_label_size))
## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.20, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.05, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p = p +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.05, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/figS6.png", plot = p, width = 6, height = 4, units = "in")

## ####################################################### ##
## ####################################################### ##
##
## Significance testing
##
## ####################################################### ##
## ####################################################### ##

## ## Is skill significantly different across time periods?
## x = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## ## p = t.test(x, y, alternative = "two.sided", var.equal = FALSE)
## p = wilcox.test(x, y, alternative = "two.sided")
## signif = p$p.value <= 0.05

## x = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## ## p = t.test(x, y, alternative = "two.sided", var.equal = FALSE)
## p = wilcox.test(x, y, alternative = "two.sided")
## signif = p$p.value <= 0.05

## x = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## ## p = t.test(x, y, alternative = "two.sided", var.equal = FALSE)
## p = wilcox.test(x, y, alternative = "two.sided")
## signif = p$p.value <= 0.05

## Does the inclusion of NAO significantly increase model skill?

## ## Year 2-9
## x = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "full" & model %in% "NAOPT") %>% `[[`("msss")
## y1 = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## y2 = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "full" & model %in% "PT") %>% `[[`("msss")
## ## p = t.test(x, y, alternative = "two.sided", var.equal = FALSE)
## p = wilcox.test(x, y1, alternative = "greater")
## signif = p$p.value <= 0.05
## p = wilcox.test(x, y2, alternative = "greater")
## signif = p$p.value <= 0.05

## ## Year 2-5
## x = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "full" & model %in% "NAOPT") %>% `[[`("msss")
## y1 = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## y2 = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "full" & model %in% "PT") %>% `[[`("msss")
## ## p = t.test(x, y, alternative = "two.sided", var.equal = FALSE)
## p = wilcox.test(x, y1, alternative = "greater")
## signif = p$p.value <= 0.05
## p = wilcox.test(x, y2, alternative = "greater")
## signif = p$p.value <= 0.05

## ## Year 6-9
## x = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "full" & model %in% "NAOPT") %>% `[[`("msss")
## y1 = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## y2 = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "full" & model %in% "PT") %>% `[[`("msss")
## ## p = t.test(x, y, alternative = "two.sided", var.equal = FALSE)
## p = wilcox.test(x, y1, alternative = "greater")
## signif = p$p.value <= 0.05
## p = wilcox.test(x, y2, alternative = "greater")
## signif = p$p.value <= 0.05

## ## Year 2-9
## x = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "best_n" & model %in% "P") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## p = wilcox.test(x, y, paired = TRUE, alternative = "greater")
## signif = p$p.value <= 0.05 # False

## x = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "best_n" & model %in% "PT") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr2to9_lag" & subset %in% "full" & model %in% "PT") %>% `[[`("msss")
## p = wilcox.test(x, y, alternative = "greater")
## signif = p$p.value <= 0.05 # True

## x = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "best_n" & model %in% "P") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## p = wilcox.test(x, y, alternative = "greater")
## signif = p$p.value <= 0.05

## x = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "best_n" & model %in% "PT") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr2to5_lag" & subset %in% "full" & model %in% "PT") %>% `[[`("msss")
## p = wilcox.test(x, y, alternative = "greater")
## signif = p$p.value <= 0.05

## x = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "best_n" & model %in% "P") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "full" & model %in% "P") %>% `[[`("msss")
## p = wilcox.test(x, y, alternative = "greater")
## signif = p$p.value <= 0.05

## x = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "best_n" & model %in% "PT") %>% `[[`("msss")
## y = skill_scores %>% filter(period %in% "yr6to9_lag" & subset %in% "full" & model %in% "PT") %>% `[[`("msss")
## p = wilcox.test(x, y, alternative = "greater")
## signif = p$p.value <= 0.05





## NOT USED:
##
##
## panel_height = unit(1, "npc") - sum(ggplotGrob(p5)[["heights"]][-3]) - unit(1, "line")
## p3 =
##   p3 +
## ## p9 =
## ##   p9 +
##   guides(
##     shape = guide_legend(
##       title = "Model",
##       order = 1
##     ),
##     fill = guide_colorbar(
##       title="MSSS",
##       title.position="top",
##       frame.colour = "black",
##       ticks.colour = "black",
##       frame.linewidth = 0.25,
##       ticks.linewidth = 0.25,
##       legend.position = "right",
##       barwidth = 0.75,
##       barheight = panel_height * 0.75 * 0.5,
##       order = 2))

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 ## axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 plot.margin = margin(t = 0, b = 2.5, unit = "pt"))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 ## axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 plot.margin = margin(t = 0, b = 2.5, unit = "pt"))
## p3 = p3 + theme(#axis.text.x = element_text(size = axis_label_size),
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_blank(),
##                 axis.ticks.x = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 legend.title = element_text(size = legend_title_size),
##                 legend.text = element_text(size = legend_label_size),
##                 plot.margin = margin(t = 0, b = 2.5, unit = "pt"))
## p7 = p7 + theme(legend.position = "none",
##                 strip.text = element_blank(),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 plot.margin = margin(t = 0, b = 2.5, unit = "pt"))
## p8 = p8 + theme(legend.position = "none",
##                 strip.text = element_blank(),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 plot.margin = margin(t = 0, b = 2.5, unit = "pt"))
## p9 = p9 + theme(legend.position = "none",
##                 axis.text.x = element_text(size = axis_label_size),
##                 strip.text = element_blank(),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 legend.title = element_text(size = legend_title_size),
##                 legend.text = element_text(size = legend_label_size),
##                 plot.margin = margin(t = 0, b = 2.5, unit = "pt"))
## p4 = p4 + theme(panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size_small),
##                 plot.margin = margin(t = 2.5, b = 0, unit = "pt"))
## p5 = p5 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small),
##                 plot.margin = margin(t = 2.5, b = 0, unit = "pt"))
## p6 = p6 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small),
##                 plot.margin = margin(t = 2.5, b = 0, unit = "pt"))

## p = p1 + p2 + p3 + p7 + p8 + p9 + p4 + p5 + p6 + plot_layout(ncol = 3, nrow = 3)
## p = p + plot_layout(heights = c(2, 2, 1), widths = c(2, 2, 2))
## ## p = p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))
## p = p + theme(plot.margin = margin(r = 18.5, unit = "pt"))
## p = p + plot_layout(guides = "collect")

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.24, 0.93),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.06, 0.93),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[3]] =
##   p$patches$plots[[3]] +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.04, 0.93),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[4]] =
##   p$patches$plots[[4]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.24, 0.97),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[5]] =
##   p$patches$plots[[5]] +
##   labs(tag = "e") +
##   theme(plot.tag.position = c(0.06, 0.97),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[6]] =
##   p$patches$plots[[6]] +
##   labs(tag = "f") +
##   theme(plot.tag.position = c(0.04, 0.97),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[7]] =
##   p$patches$plots[[7]] +
##   labs(tag = "g") +
##   theme(plot.tag.position = c(0.24, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[8]] =
##   p$patches$plots[[8]] +
##   labs(tag = "h") +
##   theme(plot.tag.position = c(0.06, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "i") +
##   theme(plot.tag.position = c(0.04, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/fig1.png", plot = p, width = 6, height = 7.5, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S1
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## skill_scores =
## ##   load_skill_scores(config, c("hindcast", "hindcast2", "hindcast3")) %>%
## ##   filter(model %in% c("NAO", "NAO_P", "NAO_P_T", "P", "P_T"))
## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT")) %>%
##   filter(subset %in% "full", period %in% "yr2to9_lag") %>%
##   mutate(period = factor(period, levels = "yr2to9_lag", labels = "Year 2-9"))
## skill = myfun(skill_scores_subset)
## p1 = myplotfun1(na.omit(skill))
## p4 = myplotfun2(skill_scores_subset)

## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT")) %>%
##   filter(subset %in% "full", period %in% "yr2to5_lag") %>%
##   mutate(period = factor(period, levels = "yr2to5_lag", labels = "Year 2-5"))
## skill = myfun(skill_scores_subset)
## p2 = myplotfun1(na.omit(skill))
## p5 = myplotfun2(skill_scores_subset)

## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT")) %>%
##   filter(subset %in% "full", period %in% "yr6to9_lag") %>%
##   mutate(period = factor(period, levels = "yr6to9_lag", labels = "Year 6-9"))
## skill = myfun(skill_scores_subset)
## p3 = myplotfun1(na.omit(skill))
## p6 = myplotfun2(skill_scores_subset)

## panel_height = unit(1, "npc") - sum(ggplotGrob(p5)[["heights"]][-3]) - unit(1, "line")
## p3 =
##   p3 +
##   guides(
##     shape = guide_legend(
##       title = "Model",
##       order = 1
##     ),
##     fill = guide_colorbar(
##       title="MSSS",
##       title.position="top",
##       frame.colour = "black",
##       ticks.colour = "black",
##       frame.linewidth = 0.25,
##       ticks.linewidth = 0.25,
##       legend.position = "right",
##       barwidth = 0.75,
##       barheight = panel_height * 0.75 * 0.5,
##       order = 2))

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_text(size = axis_label_size))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank())
## p3 = p3 + theme(axis.text.x = element_text(size = axis_label_size),
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 legend.title = element_text(size = legend_title_size),
##                 legend.text = element_text(size = legend_label_size))
## p4 = p4 + theme(panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size_small))
## p5 = p5 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small))
## p6 = p6 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small))

## p = p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3, nrow = 2)
## p = p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))
## ## p = p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))

## p = p + theme(plot.margin = margin(r = 18.5, unit = "pt"))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.24, 0.885),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.06, 0.885),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[3]] =
##   p$patches$plots[[3]] +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.04, 0.885),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[4]] =
##   p$patches$plots[[4]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.24, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[5]] =
##   p$patches$plots[[5]] +
##   labs(tag = "e") +
##   theme(plot.tag.position = c(0.06, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "f") +
##   theme(plot.tag.position = c(0.04, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/figS2.png", plot = p, width = 6, height = 6, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S2
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## skill_scores =
## ##   load_skill_scores(config, c("hindcast", "hindcast2", "hindcast3")) %>%
## ##   filter(model %in% c("NAO", "NAO_P", "NAO_P_T", "P", "P_T"))

## skill_scores_subset =
##   skill_scores %>%
##   ## filter(model %in% c("P", "PT")) %>%
##   filter(subset %in% "full", period %in% "yr2_lag_constant_fcst_period") %>%
##   mutate(period = factor(period, levels = "yr2_lag_constant_fcst_period", labels = "Year 2"))
## skill = myfun(skill_scores_subset)
## p1 = myplotfun1(na.omit(skill))
## p5 = myplotfun2(skill_scores_subset)

## skill_scores_subset =
##   skill_scores %>%
##   filter(subset %in% "full", period %in% "yr3_lag_constant_fcst_period") %>%
##   mutate(period = factor(period, levels = "yr3_lag_constant_fcst_period", labels = "Year 3"))
## skill = myfun(skill_scores_subset)
## p2 = myplotfun1(na.omit(skill))
## p6 = myplotfun2(skill_scores_subset)

## skill_scores_subset =
##   skill_scores %>%
##   filter(subset %in% "full", period %in% "yr4_lag_constant_fcst_period") %>%
##   mutate(period = factor(period, levels = "yr4_lag_constant_fcst_period", labels = "Year 4"))
## skill = myfun(skill_scores_subset)
## p3 = myplotfun1(na.omit(skill))
## p7 = myplotfun2(skill_scores_subset)

## skill_scores_subset =
##   skill_scores %>%
##   filter(subset %in% "full", period %in% "yr5_lag_constant_fcst_period") %>%
##   mutate(period = factor(period, levels = "yr5_lag_constant_fcst_period", labels = "Year 5"))
## skill = myfun(skill_scores_subset)
## p4 = myplotfun1(na.omit(skill))
## p8 = myplotfun2(skill_scores_subset)
## panel_height = unit(1, "npc") - sum(ggplotGrob(p5)[["heights"]][-3]) - unit(1, "line")

## rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")
## p1 =
##   p1 +
##   scale_fill_stepsn(
##     colours = rdbu_pal,
##     breaks = seq(-0.2, 0.2, 0.05),
##     values = scales::rescale(c(-0.2, 0, 0.2)),
##     limits = c(-0.099, 0.199)
##   )
## p2 =
##   p2 +
##   scale_fill_stepsn(
##     colours = rdbu_pal,
##     breaks = seq(-0.2, 0.2, 0.05),
##     values = scales::rescale(c(-0.2, 0, 0.2)),
##     limits = c(-0.099, 0.199)
##     ## limits = c(-0.1, 0.2)
##   )
## p3 =
##   p3 +
##   scale_fill_stepsn(
##     colours = rdbu_pal,
##     breaks = seq(-0.2, 0.2, 0.05),
##     values = scales::rescale(c(-0.2, 0, 0.2)),
##     limits = c(-0.099, 0.199)
##     ## limits = c(-0.1, 0.2)
##   )
## p4 =
##   p4 +
##   scale_fill_stepsn(
##     colours = rdbu_pal,
##     breaks = seq(-0.2, 0.2, 0.05),
##     values = scales::rescale(c(-0.2, 0, 0.2)),
##     limits = c(-0.099, 0.199)
##     ## limits = c(-0.1, 0.2)
##   ) +
##   guides(fill = guide_colorbar(title="MSSS",
##                                title.position="top",
##                                frame.colour = "black",
##                                ticks.colour = "black",
##                                frame.linewidth = 0.25,
##                                ticks.linewidth = 0.25,
##                                legend.position = "right",
##                                barwidth = 0.75,
##                                barheight = panel_height * 0.75 * 0.5))
## p5 = p5 + ylim(-0.2, 0.2)
## p6 = p6 + ylim(-0.2, 0.2)
## p7 = p7 + ylim(-0.2, 0.2)
## p8 = p8 + ylim(-0.2, 0.2)

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_text(size = axis_label_size))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank())
## p3 = p3 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank())
## p4 = p4 + theme(axis.text.x = element_text(size = axis_label_size),
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 legend.title = element_text(size = legend_title_size),
##                 legend.text = element_text(size = legend_label_size))
## p5 = p5 + theme(panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size_small - 1))
## p6 = p6 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small - 1))
## p7 = p7 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small - 1))
## p8 = p8 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small - 1))

## p = p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + plot_layout(ncol = 4, nrow = 2)
## p = p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))
## ## p = p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))

## p = p + theme(plot.margin = margin(r = 18.5, unit = "pt"))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.31, 0.87),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.06, 0.87),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[3]] =
##   p$patches$plots[[3]] +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.06, 0.87),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[4]] =
##   p$patches$plots[[4]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.04, 0.87),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[5]] =
##   p$patches$plots[[5]] +
##   labs(tag = "e") +
##   theme(plot.tag.position = c(0.31, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[6]] =
##   p$patches$plots[[6]] +
##   labs(tag = "f") +
##   theme(plot.tag.position = c(0.075, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[7]] =
##   p$patches$plots[[7]] +
##   labs(tag = "g") +
##   theme(plot.tag.position = c(0.075, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "h") +
##   theme(plot.tag.position = c(0.04, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/figS3.png", plot = p, width = 6, height = 5, units = "in")

## skill_scores =
##   load_skill_scores(config, c("hindcast")) %>%
##   filter(model %in% c("P", "P_T", "UKP", "UKP_T"))

## ## TODO this should be best model, not NAO_P_T
## skill_scores_subset =
##   skill_scores %>%
##   filter(
##     subset %in% "full",
##     period %in% c("yr2", "yr2to5_lag", "yr2to9_lag", "yr6to9_lag")
##   ) %>%
##   mutate(lag = grepl("UK", model)) %>%
##   mutate(model = gsub("UK", "", model)) %>%
##   ## separate(period, c("period", "lag"), sep="_") %>%
##   mutate(lag = ifelse(!lag, "Not lagged", "Lagged")) %>%
##   mutate(period = factor(period, levels = c("yr2", "yr2to5_lag", "yr2to9_lag", "yr6to9_lag"), labels = c("Year 2", "Year 2-5", "Year 2-9", "Year 6-9")))

## p1 = myplotfun8(skill_scores_subset %>% filter(period %in% "Year 2-9"))
## p2 = myplotfun8(skill_scores_subset %>% filter(period %in% "Year 2-5"))
## p3 = myplotfun8(skill_scores_subset %>% filter(period %in% "Year 6-9"))

## p3 = p3 + theme(legend.title = element_blank(), legend.position = "bottom")

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size))
## p3 = p3 + theme(panel.grid = element_blank(),
##                 strip.text = element_text(size = strip_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size))

## p1 = p1 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))
## p2 = p2 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))
## p3 = p3 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))

## p =
##   p1 + p2 + p3 +
##   plot_layout(nrow = 1, ncol = 3, guides = "collect") &
##   theme(legend.title = element_blank(),
##         legend.position = "bottom",
##         legend.text = element_text(size = legend_label_size))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.20, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.05, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p = p +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.05, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/fig2.png", plot = p, width = 6, height = 4, units = "in")

## ## skill = myfun(skill_scores_subset %>% filter(model %in% "P" & period %in% "Year 2-5"))
## skill = myfun(skill_scores_subset %>% filter(period %in% "Year 2-5"))
## skill = na.omit(skill)
## p2 = myplotfun3(skill)
## p2 = p2 + guides(
##             shape = guide_legend(order = 1),
##             size = guide_legend(order = 2, override.aes = list(shape = 21, fill = "transparent")),
##             fill = guide_legend(order = 3, override.aes = list(shape = 21, fill = c("#F8766D", "#00BFC4"))))

## ## skill = myfun(skill_scores_subset %>% filter(model %in% "P" & period %in% "Year 6-9"))
## skill = myfun(skill_scores_subset %>% filter(period %in% "Year 6-9"))
## skill = na.omit(skill)
## p3 = myplotfun3(skill)
## p3 = p3 + guides(
##             shape = guide_legend(order = 1),
##             size = guide_legend(order = 2, override.aes = list(shape = 21, fill = "transparent")),
##             fill = guide_legend(order = 3, override.aes = list(shape = 21, fill = c("#F8766D", "#00BFC4"))))

## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT")) %>%
##   filter(subset %in% c("best_n", "full")) %>%
##   filter(period %in% c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag")) %>%
##   mutate(period = factor(period, levels = c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag"), labels = c("Year 2-9", "Year 2-5", "Year 6-9"))) %>%
##   dplyr::select(-ps, -srel, -sme, -aic) %>%
##   pivot_wider(names_from = subset, values_from = msss) %>%
##   mutate(msss_diff = best_n - full) ## %>%
##   ## mutate(subset = ifelse(subset == "best_n", "NAO-matched ensemble", "Full ensemble"))

## p4 = myplotfun4(skill_scores_subset %>% filter(period %in% "Year 2-9"))
## p5 = myplotfun4(skill_scores_subset %>% filter(period %in% "Year 2-5"))
## p6 = myplotfun4(skill_scores_subset %>% filter(period %in% "Year 6-9"))

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_text(size = axis_label_size))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank())
## p3 = p3 + theme(axis.text.x = element_text(size = axis_label_size),
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 legend.title = element_text(size = legend_title_size),
##                 legend.text = element_text(size = legend_label_size))
## p4 = p4 + theme(panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size_small))
## p5 = p5 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small))
## p6 = p6 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small))

## p3 = p3 + theme(legend.margin = margin(-0.25, 0, 0, 0, unit = "cm"))
## p = p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3, nrow = 2) #, guides = 'keep')
## p = p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.24, 0.925),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.06, 0.925),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[3]] =
##   p$patches$plots[[3]] +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.04, 0.925),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[4]] =
##   p$patches$plots[[4]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.24, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[5]] =
##   p$patches$plots[[5]] +
##   labs(tag = "e") +
##   theme(plot.tag.position = c(0.06, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "f") +
##   theme(plot.tag.position = c(0.04, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/fig2.png", plot = p, width = 6, height = 5, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S3
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## skill_scores_subset =
##   skill_scores %>%
##   filter(subset %in% c("worst_n", "full")) %>%
##   filter(period %in% c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag")) %>%
##   mutate(period = factor(period, levels = c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag"), labels = c("Year 2-9", "Year 2-5", "Year 6-9"))) %>%
##   dplyr::select(-ps, -srel, -sme, -aic) %>%
##   pivot_wider(names_from = subset, values_from = msss) %>%
##   mutate(msss_diff = worst_n - full) %>%
##   mutate(increase = msss_diff > 0) %>%
##   mutate(msss_diff = abs(msss_diff))

## skill = myfun(skill_scores_subset %>% filter(model %in% "P" & period %in% "Year 2-9"))
## skill = na.omit(skill)
## p1 = myplotfun3(skill)

## skill = myfun(skill_scores_subset %>% filter(model %in% "P" & period %in% "Year 2-5"))
## skill = na.omit(skill)
## p2 = myplotfun3(skill)

## skill = myfun(skill_scores_subset %>% filter(model %in% "P" & period %in% "Year 6-9"))
## skill = na.omit(skill)
## p3 = myplotfun3(skill)

## skill_scores_subset =
##   skill_scores %>%
##   filter(subset %in% c("worst_n", "full")) %>%
##   filter(period %in% c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag")) %>%
##   mutate(period = factor(period, levels = c("yr2to9_lag", "yr2to5_lag", "yr6to9_lag"), labels = c("Year 2-9", "Year 2-5", "Year 6-9"))) %>%
##   dplyr::select(-ps, -srel, -sme, -aic) %>%
##   pivot_wider(names_from = subset, values_from = msss) %>%
##   mutate(msss_diff = worst_n - full) ## %>%
##   ## mutate(subset = ifelse(subset == "worst_n", "NAO-matched ensemble", "Full ensemble"))

## p4 = myplotfun4(skill_scores_subset %>% filter(period %in% "Year 2-9"))
## p5 = myplotfun4(skill_scores_subset %>% filter(period %in% "Year 2-5"))
## p6 = myplotfun4(skill_scores_subset %>% filter(period %in% "Year 6-9"))

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_text(size = axis_label_size))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.x = element_text(size = axis_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank())
## p3 = p3 + theme(axis.text.x = element_text(size = axis_label_size),
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid.major = element_line(size = 0.25),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 legend.title = element_text(size = legend_title_size),
##                 legend.text = element_text(size = legend_label_size))
## p4 = p4 + theme(panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size_small))
## p5 = p5 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small))
## p6 = p6 + theme(panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size_small))

## p3 = p3 + theme(legend.margin = margin(-0.75, 0, 0, 0, unit = "cm"))
## p = p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3, nrow = 2) #, guides = 'keep')
## p = p + plot_layout(heights = c(2, 1), widths = c(2, 2, 2))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.24, 0.885),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.06, 0.885),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[3]] =
##   p$patches$plots[[3]] +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.04, 0.885),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p$patches$plots[[4]] =
##   p$patches$plots[[4]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.24, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[5]] =
##   p$patches$plots[[5]] +
##   labs(tag = "e") +
##   theme(plot.tag.position = c(0.06, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p =
##   p +
##   labs(tag = "f") +
##   theme(plot.tag.position = c(0.04, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/figS3.png", plot = p, width = 6, height = 6, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure S4
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## Importance of ensemble size

## ## skill_scores =
## ##   load_skill_scores(config, c("hindcast", "hindcast2", "hindcast3")) %>%
## ##   filter(model %in% c("NAO", "NAO_P", "NAO_P_T", "P", "P_T"))

## ## TODO this should be best model, not NAO_P_T
## skill_scores_subset =
##   skill_scores %>%
##   filter(model %in% c("P", "PT", "NAOPT")) %>%
##   group_by(ID, subset, period) %>%
##   filter(aic == min(aic)) %>%
##   filter(
##     subset %in% c("cmip5", "cmip6", "full"),
##     period %in% c("yr2", "yr2to5", "yr2to5_lag", "yr2to9", "yr2to9_lag", "yr6to9", "yr6to9_lag")
##   ) %>%
##   separate(period, c("period", "lag"), sep="_") %>%
##   mutate(lag = ifelse(is.na(lag), "Not lagged", "Lagged")) %>%
##   mutate(period = factor(
##            period,
##            levels = c("yr2", "yr2to5", "yr2to9", "yr6to9"),
##            labels = c("Year 2", "Year 2-5", "Year 2-9", "Year 6-9")
##          ))

## p1 = myplotfun7(skill_scores_subset %>% filter(period %in% "Year 2-9"))
## p2 = myplotfun7(skill_scores_subset %>% filter(period %in% "Year 2-5"))
## p3 = myplotfun7(skill_scores_subset %>% filter(period %in% "Year 6-9"))

## p3 = p3 + theme(legend.title = element_blank(), legend.position = "bottom")

## p1 = p1 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid = element_blank(),
##                 axis.title.y = element_text(size = axis_title_size),
##                 axis.text.y = element_text(size = axis_label_size),
##                 axis.text.x = element_text(size = axis_label_size))
## p2 = p2 + theme(legend.position = "none",
##                 strip.text = element_text(size = strip_label_size),
##                 panel.grid = element_blank(),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size))
## p3 = p3 + theme(panel.grid = element_blank(),
##                 strip.text = element_text(size = strip_label_size),
##                 axis.text.y = element_blank(),
##                 axis.ticks.y = element_blank(),
##                 axis.title.y = element_blank(),
##                 axis.text.x = element_text(size = axis_label_size))

## p1 = p1 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))
## p2 = p2 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))
## p3 = p3 + theme(plot.margin = margin(c(20.5, 5.5, 20.5, 5.5), unit="pt"))

## p =
##   p1 + p2 + p3 +
##   plot_layout(nrow = 1, ncol = 3, guides = "collect") &
##   theme(legend.title = element_blank(),
##         legend.position = "bottom",
##         legend.text = element_text(size = legend_label_size))

## p$patches$plots[[1]] =
##   p$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.20, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p$patches$plots[[2]] =
##   p$patches$plots[[2]] +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.05, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## p = p +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.05, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## ggsave("fig/figS4.png", plot = p, width = 6, height = 4, units = "in")
