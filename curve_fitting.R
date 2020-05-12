# Script description ---- 
# Follows chronologically after modeling_and_decomposition and in particular needs the 
# output data frame as an input. The output data frame contains the scatter in the media 
# spend - contribution plane. In this script, we fit a curve with desirable properties to 
# the scatter s.t. we get one ROI-curve for each model, decomposition tuple. The script
# yields data frames containing the fitted curves for each media channel and data frames
# containing the respective parameters of these curves. The fit_roi function allows
# a logistic or hill function to be fitted to the data.
#
# Requires: Only the output data frame containing the contributions is required.

# -----------------------------------------------

# Libraries ----
library(drc)

# Source functions
source(paste0(getwd(),"/own_functions.R"))
select <- dplyr::select

# -----------------------------------------------

# Fitting ----
# The parameters fit and int are defined in the master script.
# Here, we leverage the fit_roi function from the function script. First, define the domain,
# over which the ROI-curve should be defined (fitted). The parameter int governs the granularity
# of the domain.

# Define fit_roi parameters
domain.1 <- seq(0, max(output$media.1.spend), length.out = int)
domain.2 <- seq(0, max(output$media.2.spend), length.out = int)
col_y <- names(output)[!(names(output) %in% c("media.1.spend", "media.2.spend", "market.rate",
                                              "roas.media.1", "roas.media.2"))]

col_first <- str_subset(col_y, pattern = "1")
col_second <- setdiff(col_y, col_first)

# Apply the fit_roi function for the specified columns (distinguishing media.1 or media.2)
fit.media.1 <- map(col_first, ~fit_roi(data = output, col_x = "media.1.spend", col_y = .x,
                                       domain = domain.1, fit = fit, noise_ratio = 4))

fit.media.2 <- map(col_second, ~fit_roi(data = output, col_x = "media.2.spend", col_y = .x,
                                        domain = domain.2, fit = fit, noise_ratio = 4))

# -----------------------------------------------

# Curves ----
# Reorganize for both media channels
curves.media.1 <- map(fit.media.1, ~.x[["curve"]]) %>% reduce(cbind)
colnames(curves.media.1) <- col_first
curves.media.1 <- as.data.frame(curves.media.1)
curves.media.1$media <- rep("media.1", nrow(curves.media.1))
curves.media.1$media.spend <- domain.1

curves.media.2 <- map(fit.media.2, ~.x[["curve"]]) %>% reduce(cbind)
colnames(curves.media.2) <- col_second
curves.media.2 <- as.data.frame(curves.media.2)
curves.media.2$media <- rep("media.2", nrow(curves.media.2))
curves.media.2$media.spend <- domain.2

# Bring into long format
curves.media.1.long <-
  curves.media.1 %>% pivot_longer(cols = names(curves.media.1[, !(names(curves.media.1) %in% c("media.spend", "media"))]))

curves.media.2.long <-
  curves.media.2 %>% pivot_longer(cols = names(curves.media.2[, !(names(curves.media.2) %in% c("media.spend", "media"))]))

# -----------------------------------------------

# Parameters ----
# Extract parameter values (also returned by the fit_roi function)
params.media.1 <- map(fit.media.1, ~list(.x[["params"]])) %>% reduce(cbind)
colnames(params.media.1) <- col_first
params.media.1 <- as.data.frame(params.media.1)

params.media.2 <- map(fit.media.2, ~list(.x[["params"]])) %>% reduce(cbind)
colnames(params.media.2) <- col_second
params.media.2 <- as.data.frame(params.media.2)

# Reorganize for both media channels. Additionally, contingent on the choice of the fit
# parameter the parameter values have different names and interpretations.
if (fit == "logis") {
  params.media.1 <-
    params.media.1 %>%
    pivot_longer(cols = names(.)) %>%
    mutate(Asym = map(.x = value, 
                      .f = ~ifelse(length(.x) == 1, NA, .x["Asym", "Estimate"])) %>% unlist(),
           xmid = map(.x = value, 
                      .f = ~ifelse(length(.x) == 1, NA, .x["xmid", "Estimate"])) %>% unlist(),
           scal = map(.x = value,
                      .f = ~ifelse(length(.x) == 1, NA, .x["scal", "Estimate"])) %>% unlist()) %>%
    rename(model = name, all.params = value)
  
  params.media.2 <-
    params.media.2 %>%
    pivot_longer(cols = names(.)) %>%
    mutate(Asym = map(.x = value,
                      .f = ~ifelse(length(.x) == 1, NA, .x["Asym", "Estimate"])) %>% unlist(),
           xmid = map(.x = value, 
                      .f = ~ifelse(length(.x) == 1, NA, .x["xmid", "Estimate"])) %>% unlist(),
           scal = map(.x = value,
                      .f = ~ifelse(length(.x) == 1, NA, .x["scal", "Estimate"])) %>% unlist()) %>%
    rename(model = name, all.params = value)
  
  # Bring into long format
  params.media.1.long <-
    params.media.1 %>% pivot_longer(cols = c(Asym, xmid, scal))
  
  params.media.2.long <-
    params.media.2 %>% pivot_longer(cols = c(Asym, xmid, scal))
} else {
  params.media.1 <-
    params.media.1 %>%
    pivot_longer(cols = names(.)) %>%
    mutate(b = map(.x = value, 
                   .f = ~ifelse(length(.x) == 1, NA, .x[1, "Estimate"])) %>% unlist(),
           c = map(.x = value, 
                   .f = ~ifelse(length(.x) == 1, NA, .x[2, "Estimate"])) %>% unlist(),
           d = map(.x = value,
                   .f = ~ifelse(length(.x) == 1, NA, .x[3, "Estimate"])) %>% unlist(),
           e = map(.x = value,
                   .f = ~ifelse(length(.x) == 1, NA, .x[4, "Estimate"])) %>% unlist()) %>%
    rename(model = name, all.params = value)
  
  params.media.2 <-
    params.media.2 %>%
    pivot_longer(cols = names(.)) %>%
    mutate(b = map(.x = value, 
                   .f = ~ifelse(length(.x) == 1, NA, .x[1, "Estimate"])) %>% unlist(),
           c = map(.x = value, 
                   .f = ~ifelse(length(.x) == 1, NA, .x[2, "Estimate"])) %>% unlist(),
           d = map(.x = value,
                   .f = ~ifelse(length(.x) == 1, NA, .x[3, "Estimate"])) %>% unlist(),
           e = map(.x = value,
                   .f = ~ifelse(length(.x) == 1, NA, .x[4, "Estimate"])) %>% unlist()) %>%
    rename(model = name, all.params = value)
  
  # Bring into long format
  params.media.1.long <-
    params.media.1 %>% pivot_longer(cols = c(b, c, d, e))
  
  params.media.2.long <-
    params.media.2 %>% pivot_longer(cols = c(b, c, d, e))
}
