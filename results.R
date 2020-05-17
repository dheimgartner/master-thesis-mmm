# Script description ----
# Organizes the data from the master.list and builds final deliverables. Corrseponds
# to the "results" section in the thesis. All iterations are collected in respective
# data frames. We build data frames for parameter values of the ROI-curves, the
# data points of the ROI-curves, number of no convergence in the roi_fit
# function, and MAPE measures.
#
# Requires: master.list

# -----------------------------------------------

# Libraries ----
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(Metrics)
library(data.table)
library(ggpubr)

select <- dplyr::select
map <- purrr::map
rename <- dplyr::rename

# -----------------------------------------------

# Reorganize ----
# Params ----
# media.1 --
# Reduce list
params.media.1 <- 
  map(.x = master.list, .f = ~.x[["params.media.1"]]) %>% 
  reduce(rbind) %>%
  drop_na()

# Extract true parameters
true.params.1 <-
  params.media.1 %>% 
  filter(model == "media.1.true.contribution") %>%
  dplyr::select(-all.params) %>%
  .[1:3, ]

# Drop true parameters
params.media.1 <-
  params.media.1 %>%
  filter(model != "media.1.true.contribution")

# Add column media, modeling and decomposition
params.media.1$media <- "media.1"

params.media.1$modeling <- 
  params.media.1$model %>% 
  str_remove(pattern = "media.1.") %>%
  str_remove(pattern = ".wfd|.shap|.ale")

params.media.1$decomposition <- 
  params.media.1$model %>% str_remove(params.media.1$media) %>% 
  str_remove(params.media.1$modeling) %>%
  str_remove("..")

params.media.1 <- params.media.1 %>% select(-model)

# media.2 --
# Same steps as above
params.media.2 <-
  map(.x = master.list, .f = ~.x[["params.media.2"]]) %>%
  reduce(rbind) %>%
  drop_na()

true.params.2 <-
  params.media.2 %>% 
  filter(model == "media.2.true.contribution") %>%
  dplyr::select(-all.params) %>%
  .[1:3, ]

params.media.2 <-
  params.media.2 %>%
  filter(model != "media.2.true.contribution")

params.media.2$media <- "media.2"

params.media.2$modeling <- 
  params.media.2$model %>% 
  str_remove(pattern = "media.2.") %>%
  str_remove(pattern = ".wfd|.shap|.ale")

params.media.2$decomposition <- 
  params.media.2$model %>% str_remove(params.media.2$media) %>% 
  str_remove(params.media.2$modeling) %>%
  str_remove("..")

params.media.2 <- params.media.2 %>% select(-model)

# Ultimate params df: combine first and second part --
params <- params.media.1 %>% bind_rows(params.media.2)

# Add values for respective hlines (could be done by join aswell I guess...)
params <-
  params %>%
  mutate(true.param = map2(.x = name, .y = media,
                           .f = ~ifelse(.x == "Asym" & .y == "media.1",
                                        true.params.1[true.params.1$name =="Asym", "value"],
                                        ifelse(.x == "Asym" & .y == "media.2",
                                               true.params.2[true.params.2$name =="Asym", "value"],
                                               ifelse(.x == "xmid" & .y == "media.1",
                                                      true.params.1[true.params.1$name =="xmid", "value"],
                                                      ifelse(.x == "xmid" & .y == "media.2",
                                                             true.params.2[true.params.2$name =="xmid", "value"],
                                                             ifelse(.x == "scal" & .y == "media.1",
                                                                    true.params.1[true.params.1$name =="scal", "value"],
                                                                    ifelse(.x == "scal" & .y == "media.2",
                                                                           true.params.2[true.params.2$name =="scal", "value"],
                                                                           0))))))) %>% unlist()) %>%
  mutate(media = ifelse(media == "media.1", "media 1", "media 2"))

# -----------------------------------------------

# Curves ----
# Add id (number of iteration) to curves
m <- 1:n
# media.1 --
# Reduce list
curves.media.1 <- 
  map2(.x = master.list, .y = m, 
       .f = ~.x[["curves.media.1"]] %>%
         mutate(id = .y)) %>% 
  reduce(rbind) %>%
  drop_na()

# Extract true curves
true.curves.1 <-
  curves.media.1 %>% 
  filter(name == "media.1.true.contribution") %>%
  distinct() %>%
  rename(true.curve = value) %>%
  select(-name)

# Drop true curves
curves.media.1 <-
  curves.media.1 %>%
  filter(name != "media.1.true.contribution")

# Add column modeling and decomposition
curves.media.1$modeling <- 
  curves.media.1$name %>% 
  str_remove(pattern = "media.1.") %>%
  str_remove(pattern = ".wfd|.shap|.ale")

curves.media.1$decomposition <- 
  curves.media.1$name %>% str_remove(curves.media.1$media) %>% 
  str_remove(curves.media.1$modeling) %>%
  str_remove("..")

curves.media.1 <- curves.media.1 %>% select(-name)

# Add mean curves
curves.media.1 <-
  curves.media.1 %>%
  mutate(m.spend.rounded = round(media.spend)) %>% 
  group_by(m.spend.rounded, modeling, decomposition) %>% 
  mutate(m.curve = mean(value)) %>%
  ungroup()

# curves.media.1 <- data.table(curves.media.1)
# curves.media.1 <- 
#   curves.media.1[, m.curve := mean(value), 
#                  by = list(media.spend, modeling, decomposition)] %>%
#   as_tibble()

# Add CI curves
curves.media.1 <-
  curves.media.1 %>%
  group_by(m.spend.rounded, modeling, decomposition) %>%
  mutate(ci.curve.upper = quantile(value, probs = 0.9, na.rm = T),
         ci.curve.lower = quantile(value, probs = 0.1, na.rm = T)) %>% 
  ungroup() %>% 
  select(-m.spend.rounded)

# Add values for respective true curves
curves.media.1 <-
  curves.media.1 %>%
  left_join(true.curves.1, by = c("media", "media.spend", "id"))

# media.2 --
curves.media.2 <- 
  map2(.x = master.list, .y = m,
       .f = ~.x[["curves.media.2"]] %>% 
         mutate(id = .y)) %>% 
  reduce(rbind) %>%
  drop_na()

true.curves.2 <-
  curves.media.2 %>% 
  filter(name == "media.2.true.contribution") %>% 
  distinct() %>%
  rename(true.curve = value) %>%
  select(-name)

curves.media.2 <-
  curves.media.2 %>%
  filter(name != "media.2.true.contribution")

curves.media.2$modeling <- 
  curves.media.2$name %>% 
  str_remove(pattern = "media.2.") %>%
  str_remove(pattern = ".wfd|.shap|.ale")

curves.media.2$decomposition <- 
  curves.media.2$name %>% str_remove(curves.media.2$media) %>% 
  str_remove(curves.media.2$modeling) %>%
  str_remove("..")

curves.media.2 <- curves.media.2 %>% select(-name)

# Add mean curves
curves.media.2 <-
  curves.media.2 %>%
  mutate(m.spend.rounded = round(media.spend)) %>% 
  group_by(m.spend.rounded, modeling, decomposition) %>% 
  mutate(m.curve = mean(value)) %>%
  ungroup()

# Add CI curves
curves.media.2 <-
  curves.media.2 %>%
  group_by(m.spend.rounded, modeling, decomposition) %>%
  mutate(ci.curve.upper = quantile(value, probs = 0.9, na.rm = T),
         ci.curve.lower = quantile(value, probs = 0.1, na.rm = T)) %>% 
  ungroup() %>% 
  select(-m.spend.rounded)

# Add values for respective true curves
curves.media.2 <-
  curves.media.2 %>%
  left_join(true.curves.2, by = c("media", "media.spend", "id"))

# -----------------------------------------------

# Not converged ----
# Count number of not converged
not.conv.1 <- 
  map(.x = master.list, .f = ~.x[["curves.media.1"]] %>% 
        pivot_wider(names_from = name, values_from = value) %>%
        select(-c(media, media.spend)) %>% 
        dplyr::summarise_all(~mean(., na.rm = T)) %>%
        mutate_all(.funs = ~ifelse(is.na(.), 1, 0))) %>% 
  reduce(rbind) %>%
  gather() %>%
  group_by(key) %>%
  dplyr::summarise(not.conv = sum(value))

not.conv.2 <- 
  map(.x = master.list, .f = ~.x[["curves.media.2"]] %>% 
        pivot_wider(names_from = name, values_from = value) %>%
        select(-c(media, media.spend)) %>% 
        dplyr::summarise_all(~mean(., na.rm = T)) %>%
        mutate_all(.funs = ~ifelse(is.na(.), 1, 0))) %>% 
  reduce(rbind) %>%
  gather() %>%
  group_by(key) %>%
  dplyr::summarise(not.conv = sum(value))

not.conv <- rbind(not.conv.1, not.conv.2) %>% filter(!key %in% c("media.1.true.contribution", "media.2.true.contribution"))
not.conv <- not.conv %>% mutate(media = ifelse(str_detect(key, pattern = "media.1"), "media 1", "media 2"))

not.conv$modeling <- 
  not.conv$key %>% 
  str_remove(pattern = "media.1.|media.2.") %>%
  str_remove(pattern = ".wfd|.shap|.ale")

not.conv$decomposition <- 
  not.conv$key %>% 
  str_remove(pattern = "media.1.|media.2.") %>% 
  str_remove(not.conv$modeling) %>%
  str_remove(".")

not.conv <- not.conv %>% select(-key)
not.conv <- not.conv %>% mutate(not.conv = not.conv / n)

# -----------------------------------------------

# Error measures ----
# mape.name.med.mod.dec --
mape.name.med.mod.dec <-
params %>%
  group_by(name, media, modeling, decomposition) %>%
  dplyr::summarise(mape = Metrics::mape(true.param, value)) %>%
  group_by(name, media) %>%
  group_split()

mape.name.med.mod.dec <-
  map(.x = mape.name.med.mod.dec, .f = ~arrange(.x, mape))

# # mape.name.mod.dec --
# # Careful with across media types comparison (concave vs. s-shaped response curves)
# mape.name.mod.dec <-
#   params %>%
#   group_by(name, modeling, decomposition) %>%
#   dplyr::summarise(mape = Metrics::mape(value, true.param)) %>%
#   group_by(name) %>%
#   group_split()
# 
# mape.name.mod.dec <-
#   map(.x = mape.name.mod.dec, .f = ~arrange(.x, mape))
# 
# # mape.name.med.mod --
# mape.name.med.mod <-
#   params %>%
#   group_by(name, media, modeling) %>%
#   dplyr::summarise(mape = Metrics::mape(value, true.param)) %>%
#   group_by(name, media) %>%
#   group_split()
# 
# mape.name.med.mod<-
#   map(.x = mape.name.med.mod, .f = ~arrange(.x, mape))
# 
# # mape.name.med.dec --
# mape.name.med.dec <-
#   params %>%
#   group_by(name, media, decomposition) %>%
#   dplyr::summarise(mape = Metrics::mape(value, true.param)) %>%
#   group_by(name, media) %>%
#   group_split()
# 
# mape.name.med.dec<-
#   map(.x = mape.name.med.dec, .f = ~arrange(.x, mape))


# Here, it would be nice to have a 3x2 matrix of with each col depicting the ranking
# order of the models (for the rows independently) with a nice color scheme...

# -----------------------------------------------

# Plots ----
# Here, we leverage all the above constructed data frames and try to summarize the key
# findings in graphs.

# Boxplots ----
# Remove outliers (otherwise boxplots are only horizontal lines...)
outliers_to_na <- function(x, k, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- k * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

params.outliers <-
  params %>%
  group_by(media, name, modeling, decomposition) %>%
  mutate(value = outliers_to_na(value, k = 2)) %>%
  ungroup() %>%
  drop_na()

plot_boxplot <-
ggplot(filter(params.outliers,
              !(modeling == "ols_multi" & decomposition == "wfd"),
              !(modeling == "svr_multi" & decomposition == "wfd"),
              !(modeling == "gam_2")),
       aes(x = modeling, y = value, group = interaction(modeling, decomposition, media), fill = decomposition)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = true.param), col = "red", linetype = "dotted") +
  facet_grid(rows = ggplot2::vars(media, name), cols = ggplot2::vars(modeling), scales = "free") +
  scale_fill_viridis_d() +
  labs(x = "modeling approach", y = "parameter values") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::scientific)

plot_boxplot_media_1 <-
  ggplot(filter(params, media == "media 1",
                !(modeling == "ols_multi" & decomposition == "wfd"),
                !(modeling == "svr_multi" & decomposition == "wfd"),
                !(modeling == "gam_2")), 
         aes(x = modeling, y = value, group = interaction(modeling, decomposition), fill = decomposition)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = true.param), col = "red", linetype = "dotted") +
  facet_wrap(facets = vars(modeling, name), ncol = 6, scales = "free") +
  scale_fill_viridis_d() +
  labs(x = "modeling approach", y = "parameter values", title = "Parameter fit media 1") +
  theme_bw()

plot_boxplot_media_2 <-
  ggplot(filter(params, media == "media 2",
                !(modeling == "ols_multi" & decomposition == "wfd"),
                !(modeling == "svr_multi" & decomposition == "wfd"),
                !(modeling == "gam_2")), 
         aes(x = modeling, y = value, group = interaction(modeling, decomposition), fill = decomposition)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = true.param), col = "red", linetype = "dotted") +
  facet_wrap(facets = vars(modeling, name), ncol = 6, scales = "free") +
  scale_fill_viridis_d() +
  labs(x = "modeling approach", y = "parameter values", title = "Parameter fit media 2") +
  theme_bw()

plot1 <-
  ggplot(filter(params.outliers,
                media == "media 1",
                !(modeling == "ols_multi" & decomposition == "wfd"),
                !(modeling == "svr_multi" & decomposition == "wfd"),
                !(modeling == "gam_2")),
         aes(x = modeling, y = value, group = interaction(modeling, decomposition, media), fill = decomposition)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = true.param), col = "red", linetype = "dotted") +
  facet_grid(rows = ggplot2::vars(media, name), cols = ggplot2::vars(modeling), scales = "free") +
  scale_fill_viridis_d() +
  labs(x = "modeling approach", y = "parameter values", title = "Media 1") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::scientific)

plot2 <-
  ggplot(filter(params.outliers,
                media == "media 2",
                !(modeling == "ols_multi" & decomposition == "wfd"),
                !(modeling == "svr_multi" & decomposition == "wfd"),
                !(modeling == "gam_2")),
         aes(x = modeling, y = value, group = interaction(modeling, decomposition, media), fill = decomposition)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = true.param), col = "red", linetype = "dotted") +
  facet_grid(rows = ggplot2::vars(media, name), cols = ggplot2::vars(modeling), scales = "free") +
  scale_fill_viridis_d() +
  labs(x = "modeling approach", y = "parameter values", title = "Media 2") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::scientific)

plot_boxplot_both <-
  ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = T, legend = "bottom")

# -----------------------------------------------

# Curve plot ----
# An alternative to facet_grid or facet_wrap can be found here:
# https://github.com/compstat-lmu/iml_methods_limitations/blob/master/02-5-ale-other.Rmd
plot_curves_media_1 <-
  ggplot(curves.media.1 %>% filter(!(modeling == "ols_multi" & decomposition == "wfd"),
                                   !(modeling == "svr_multi" & decomposition == "wfd"),
                                   !(modeling == "gam_2")), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  facet_wrap(facets = vars(modeling, decomposition), ncol = 5, scales = "free") +
  labs(x = "media spend", y = "return on investment", title = "ROI-curves media 1") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

plot_curves_media_2 <-
  ggplot(curves.media.2 %>% filter(!(modeling == "ols_multi" & decomposition == "wfd"),
                                   !(modeling == "svr_multi" & decomposition == "wfd"),
                                   !(modeling == "gam_2")), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  facet_wrap(facets = vars(modeling, decomposition), ncol = 5, scales = "free") +
  labs(x = "media spend", y = "return on investment", title = "ROI-curves media 2") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# -----------------------------------------------

# Mape plot ----
# The mape.name.med.mod.dec data frame created above, had a group split which we don"t want
# here for the plot. Hence we build a new data frame
mape.name.med.mod.dec <-
  params %>%
  group_by(name, media, modeling, decomposition) %>%
  dplyr::summarise(mape = Metrics::mape(true.param, value)) %>%
  ungroup()
mape.name.med.mod.dec


# Potentially use geom_errorbar()...
plot1 <-
  ggplot(mape.name.med.mod.dec %>% filter(!(modeling == "ols_multi" & decomposition == "wfd"),
                                          !(modeling == "svr_multi" & decomposition == "wfd"),
                                          !(modeling == "gam_2")), 
         aes(x = name, y = mape, group = interaction(media, modeling, decomposition))) +
  geom_line(aes(col = modeling)) +
  geom_point(aes(shape = decomposition)) +
  labs(x = "parameter", title = "Values mape all") +
  theme_bw() +
  facet_wrap(facets = vars(media), ncol = 2, scales = "free") +
  scale_colour_hue(l = 55)

plot2 <-
  ggplot(mape.name.med.mod.dec %>% filter(mape < 1,
                                          !(modeling == "ols_multi" & decomposition == "wfd"),
                                          !(modeling == "svr_multi" & decomposition == "wfd"),
                                          !(modeling == "gam_2")), 
         aes(x = name, y = mape, group = interaction(media, modeling, decomposition))) +
  geom_line(aes(col = modeling)) +
  geom_point(aes(shape = decomposition)) +
  labs(x = "parameter", title = "Values mape < 1") +
  theme_bw() +
  facet_wrap(facets = vars(media), ncol = 2, scales = "free") +
  scale_colour_hue(l = 55)

plot_mape <-
  ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

# -----------------------------------------------

# Not.conv plot ----
plot_not_conv <- 
  ggplot(not.conv, aes(x = modeling, y = not.conv, group = interaction(media, modeling, decomposition), col = decomposition)) +
  geom_point() +
  facet_grid(cols = ggplot2::vars(media)) +
  scale_colour_viridis_d() +
  labs(x = "modeling approach", y = "failure rate") +
  theme_bw()

# -----------------------------------------------

# Testing ----
test <-
  ggplot(curves.media.1, aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  facet_grid(rows = ggplot2::vars(modeling), cols = ggplot2::vars(decomposition), scales = "free") +
  labs(x = "media spend", y = "return on investment", title = "ROI-curves media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

test <-
  ggplot(filter(params, media == "media 1",
                !(modeling == "ols_multi" & decomposition == "wfd"),
                !(modeling == "svr_multi" & decomposition == "wfd"),
                !(modeling == "gam_2")), 
         aes(x = modeling, y = value, group = interaction(modeling, decomposition), fill = decomposition)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = true.param), col = "red", linetype = "dotted") +
  facet_wrap(facets = vars(modeling, name), ncol = 6, scales = "free") +
  scale_fill_viridis_d() +
  labs(x = "modeling approach", y = "parameter values", title = "Parameter fit media 1") +
  theme_bw()
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


