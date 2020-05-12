# Script description ----
# Models the simulated sales process with different modeling approaches. These are:
# ols_poly, ols_multi, svr_poly, svr_multy and gam. Further the models are decomposed 
# according to different approaches. These are: wfd, shap and ale. The script also
# implements an R2 computation for our cross-validation. This measure is contained in the
# validation data frame. The computed contributions are appended in the output data frame.
# We further fit the models (or a subset of models) from part one to the structural change 
# dataset in order to get a further dimension when evaluating the tvem model.
#
# Requires: observed.data, observed.data.2 and output

# -----------------------------------------------

# Libraries -----
library(tidyverse)
library(data.table)
library(relaimpo)
library(stringr)
library(parallel)
library(iml)
library(shapper)
library(DALEX)
library(minpack.lm)
library(gam)
library(matrixStats)
library(tvReg)

# Source functions
source(paste0(getwd(),"/own_functions.R"))
source(paste0(getwd(),"/mmm_functions.R"))

select <- dplyr::select
map <- purrr::map
rename <- dplyr::rename

# -----------------------------------------------

# Part I ----
# Modeling ----
# ols_poly --
formula.poly <- 
  revenue ~ media.1.spend + media.2.spend + market.rate +
  I(media.1.spend^2) + I(media.2.spend^2) +
  I(media.1.spend^3) + I(media.2.spend^3) +
  I(media.1.spend*market.rate) + I(media.2.spend*market.rate) + I(media.1.spend*media.2.spend)

ols_poly <-
  lm(formula = formula.poly, data = observed.data)

# ols_multi --
# Add constant of 1 to all data points (log of 0 is -Inf, log of 1 is 0)
data.const <- observed.data + 1

ols_multi <- 
  lm(log(revenue) ~ log(media.1.spend) + log(media.2.spend) + log(market.rate),
     data = data.const)

# svr_poly --
# The shapley_value_reg function is explained in the functions script. It allows to
# comput shapey value regression for log-lin, log-log, and regular regression specifications.
# The function is implemented following the description outlined in Mishra (2016).
svr_poly <-
  shapley_value_reg(data = observed.data, predictors = c("media.1.spend", "media.2.spend", "market.rate"),
                    target = "revenue", always_in = NULL, log_dependent = F, log_predictors = F, reg = T,
                    reg_formula = formula.poly)
# svr_multi --
svr_multi <- 
  shapley_value_reg(data = observed.data, predictors = c("media.1.spend", "media.2.spend", "market.rate"),
                    target = "revenue", always_in = NULL, log_dependent = T, log_predictors = T)

# gam --
gam <-
  gam(formula = revenue ~ s(media.1.spend) + s(media.2.spend) + s(market.rate) +
        media.1.spend:market.rate + media.2.spend:market.rate + media.1.spend:media.2.spend,
      data = observed.data)

# -----------------------------------------------

# Part II ----
# Add index for pred_tvem function (s.t. correct coefficients can be matched)
observed.data$index <- 157:nrow(observed.data.2)

# Modeling ----
# tvem --
tvem <- 
  tvLM(formula = formula.poly, data = observed.data.2)

# We also fit the models from part one to observed.data.2 in order to get a further dimension
# when evaluating the tvem model. Alternatively, we might choose a subset of models because
# of computational time...
gam_2 <-
  gam(formula = revenue ~ s(media.1.spend) + s(media.2.spend) + s(market.rate) +
        media.1.spend:market.rate + media.2.spend:market.rate + media.1.spend:media.2.spend,
      data = observed.data.2)



# Add models from part one here and adjust subsequent vectors where needed! Adjustment is
# only needed in this script. Subsequent scripts handle it automatically.




# -----------------------------------------------

# Both parts ----
# Prediction functions ----
# Prediciton functions should take a model object (or a list or whatever) and a data frame
# newdata and return a vector of predicted values.

# Predict function for ols_multi model (fitted as log-log)
pred_ols_multi <- function(model, newdata) {
  newdata <- newdata + 1
  pred <- exp(predict(model, newdata))
  return(pred)
}

# Prediction function for svr_poly model
pred_svr_poly <- function(model, newdata) {
  coefs <- model$coefficients
  form <- model$formula[-2]
  data <- model.frame(form, newdata)
  pred <- coefs[1] + as.numeric(coefs[-1] %*% t(data))
  return(pred)
}

# Predict function for svr_multi model
pred_svr_multi <- function(model, newdata) {
  coefs <- model$coefficients
  data <- (newdata + 1) %>% log()
  data <- data[, names(coefs)[-1]]
  pred <- exp(coefs[1] + as.numeric(coefs[-1] %*% t(data)))
  return(pred)
}

# Predict function for tvem model
pred_tvem <- function(model, newdata) {
  # make sure newdata is df
  newdata <- as.data.frame(newdata)
  # extract index (for coefs subsetting)
  index <- as.numeric(newdata[ ,"index"])
  # transform data according to formula
  formula <- 
    ~ media.1.spend + media.2.spend + market.rate +
    I(media.1.spend^2) + I(media.2.spend^2) +
    I(media.1.spend^3) + I(media.2.spend^3) +
    I(media.1.spend*market.rate) + I(media.2.spend*market.rate) + I(media.1.spend*media.2.spend)
  
  newdata <- model.frame(formula, newdata) %>% as.matrix()
  newdata <- cbind(Intercept = 1, newdata)
  # extract coefs of relevance
  coefs <- as.data.frame(model$coefficients)[index, ]
  # predict
  pred <- as.data.frame(coefs * newdata) %>% rowSums()
  return(pred)
}

# -----------------------------------------------

# Model fit ----
if (F) {
# check the model fit
  model.fit <- tibble(x = 1:nrow(observed.data),
                      y = observed.data$revenue,
                      ols_poly = predict(ols_poly, observed.data),
                      ols_multi = pred_ols_multi(ols_multi, observed.data),
                      svr_poly = pred_svr_poly(svr_poly, observed.data),
                      svr_multi = pred_svr_multi(svr_multi, observed.data),
                      gam = predict(gam, observed.data),
                      tvem = pred_tvem(tvem, observed.data),
                      gam_2 = predict(gam_2, observed.data))
  model.fit.long <- model.fit %>% pivot_longer(cols = c(y, ols_poly, ols_multi, svr_poly, svr_multi, gam, tvem, gam_2))
  ggplot(model.fit.long, aes(x = x, y = value, col = name)) + geom_line()
}

# -----------------------------------------------

# Decomposition ----
# Each decomposition approach returns a data frame for each modeling approach containing
# the contributions for each mediatype. The main functions are explained in the functions
# script.

# Prepare for map functions
models <- list(ols_poly, ols_multi, svr_poly, svr_multi, gam, gam_2)
pfuns <- list(predict, pred_ols_multi, pred_svr_poly, pred_svr_multi, predict, predict)

# Define predictors, media variables and target
predictors <- c("media.1.spend", "media.2.spend", "market.rate")
mediatypes <- c("media.1.spend", "media.2.spend")
target <- "revenue"

# WFD --
wfd <- map2(.x = models, .y = pfuns, 
            .f = ~wfd_decomposition(model = .x, mediatypes = mediatypes, 
                                    data = observed.data[, c(target, predictors)], 
                                    target = target, predictors = predictors, pfun = .y))

wfd_tvem <- wfd_decomposition(tvem, mediatypes, observed.data, target, predictors, pred_tvem, tvem = T)

# SHAP --
shap <- map2(.x = models, .y = pfuns, 
             .f = ~shap_faster(model = .x, mediatypes = mediatypes, 
                               data = observed.data[, c(target, predictors)],
                               target = target, predictors = predictors, pfun = .y))
# ALE --
# Recall, that the grid size is important and critical for ALE computations, if grid is
# set to a value smaller than nrows(observed.data) then we have multiple data points within
# each interval (which is desirable). The trade-off between smoothness and number of data
# points is shortly elaborated on in the thesis.
ale <- map2(.x = models, .y = pfuns, 
            .f = ~ale_decomposition(model = .x, mediatypes = mediatypes, 
                                    data = observed.data[, c(target, predictors)], 
                                    target = target, pfun = .y, grid = 30))

# -----------------------------------------------

# Append to output ----
# Here, we append the contributions to the output data frame and name the columns accordingly
output_names <- c("ols_poly.media.1", "ols_poly.media.2",
                  "ols_multi.media.1", "ols_multi.media.2",
                  "svr_poly.media.1", "svr_poly.media.2",
                  "svr_multi.media.1", "svr_multi.media.2",
                  "gam.media.1", "gam.media.2",
                  "gam_2.media.1", "gam_2.media.2")

wfd <- reduce(wfd, cbind)
colnames(wfd) <- paste0(output_names, ".wfd")

colnames(wfd_tvem) <- c("tvem.media.1.wfd", "tvem.media.2.wfd")

shap <- reduce(shap, cbind)
colnames(shap) <- paste0(output_names, ".shap")

ale <- reduce(ale, cbind)
colnames(ale) <- paste0(output_names, ".ale")

append <- list(wfd, wfd_tvem, shap, ale) %>% reduce(cbind)
output <- bind_cols(output, append)

# -----------------------------------------------

# Model validation ----
# Compute the R2 for each modeling approach
models <- list(ols_poly, ols_multi, svr_poly, svr_multi, gam, gam_2, tvem)
model_names <- c("ols_poly", "ols_multi", "svr_poly", "svr_multi", "gam", "gam_2","tvem")
pfuns <- list(predict, pred_ols_multi, pred_svr_poly, pred_svr_multi, predict, predict, pred_tvem)

validation <- map2(.x = models, .y = pfuns, 
                   .f = ~data.frame(y = observed.data$revenue,
                                    y_hat = .y(.x, observed.data)))

# R2 function
r_squared <- function(y, y_hat) {
  tss <- sum((y - mean(y, na.rm = T))^2)
  rss <- sum((y - y_hat)^2)
  r2 <- 1 - rss/tss
  return(r2)
}

validation <- map(.x = validation, .f = ~r_squared(.x$y, .x$y_hat))
validation <- reduce(validation, cbind)
colnames(validation) <- model_names
