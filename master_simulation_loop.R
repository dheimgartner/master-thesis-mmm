# Script description ----
# Master script from which we run all the subscripts. Here, we first generate n data sets
# according to the simulation specification and subsequently run the analysis by "looping"
# over these n data sets. Finally, we evaluate the approaches by aggregating over all
# n repetitions. The same routine can be applied to the extreme case (robustness checks)
# with alternative AMSS specifications. Set simulation_X_extreme scripts to T. For 
# memory efficiency, we store the output "out" of each iteration in the "/Data/simulation.list" 
# folder and load it individually in the second analysis loop where we again store each output 
# "out" as an .rds file in the "/Data/master.list" folder.
#
# Requires: Even if we repeatedly simulate data with the same simulation specifications
# (and as a consequence the observed data changes and so does the model fit) the ground
# truth does not change. We first run the scripts until observed_data_and_ground_truth
# ONCE, s.t. the data frame true.contributions is generated. Once this file exists in
# the Data folder, the observed_data_and_ground_truth script does not need to be rerun.

# Choose either regular or extreme
true.contribution <- readRDS(paste0(getwd(), "/Data/true.contribution.regular.rds"))

# -----------------------------------------------

# Libraries ----
library(tidyverse)
library(data.table)
library(gdata)
library(gridExtra)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(parallel)
library(foreach)
library(doParallel)
library(progress)
library(ggpubr)

# Allocate functions
select <- dplyr::select
map <- purrr::map
rename <- dplyr::rename

# -----------------------------------------------

# Iteration 1 ----
# Generate Data ----
# Run scripts n times and for each iteration store sim.data, observed.data, observed.data.2
# and output in master.list
n <- 500

# Progress bar
pb <- progress_bar$new(format = "  downloading [:bar] :elapsedfull",
                       total = n, clear = FALSE, width = 100)

# Initiate parallel cluster
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

for (i in 1:n) {
  pb$tick()
  
  # ---------------------------------------------
  
  # simulation_1 (S) ----
  if (T) {
    source(paste0(getwd(), "/simulation_1.R"))
  }
  
  # simulation_1_extreme (S) ----
  if (F) {
    source(paste0(getwd(), "/simulation_1_extreme.R"))
  }
  
  # ---------------------------------------------
  
  # observed_data_and_ground_truth (S) ----
  # Ground truth needs only be computed once (in the first iteration or seperately).
  # Importantly the conditionals (if statements) need to be adjusted accordingly.
  # If observed_data_and_ground_truth is not run, then the true.contribution.rds file
  # needs to be loaded before the previous iteration!
  if (F) {
    source(paste0(getwd(), "/observed_data_and_ground_truth.R"))
  }
  
  # ---------------------------------------------
  
  # Initialize output df ----
  # If observed_data_and_ground_truth not run
  if (T) {
    output <-
      true.contribution %>%
      bind_cols(observed.data) %>%
      select(c(media.1.spend, media.2.spend, market.rate, roas.media.1, roas.media.2,
               media.1.true.contribution, media.2.true.contribution))
  }
  
  # ---------------------------------------------
  
  # simulation_2 (S) ----
  if (T) {
    source(paste0(getwd(), "/simulation_2.R"))
  }
  
  # simulation_2_extreme (S) ----
  if (F) {
    source(paste0(getwd(), "/simulation_2_extreme.R"))
  }
  
  # ---------------------------------------------
  
  # Save ----
  if (F) {
    saveRDS(sim.data, paste0(getwd(), "/Data/sim.data.rds"))
    saveRDS(observed.data, paste0(getwd(), "/Data/observed.data.rds"))
    saveRDS(observed.data.2, paste0(getwd(), "/Data/observed.data.2.rds"))
    saveRDS(output, paste0(getwd(), "/Data/output.rds"))
    saveRDS(true.contribution, paste0(getwd(), "/Data/true.contribution.extreme.rds"))
  }
  
  # Read ----
  if (F) {
    sim.data <- readRDS(paste0(getwd(), "/Data/sim.data.rds"))
    observed.data <- readRDS(paste0(getwd(), "/Data/observed.data.rds"))
    observed.data.2 <- readRDS(paste0(getwd(), "/Data/observed.data.2.rds"))
    output <- readRDS(paste0(getwd(), "/Data/output.rds"))
    true.contribution <- readRDS(paste0(getwd(), "/Data/true.contribution.rds"))
  }
  
  # ---------------------------------------------
  
  # Append to simulation.list ----
  out <- list(sim.data = sim.data, 
              observed.data = observed.data,
              observed.data.2 = observed.data.2,
              output = output)
  
  saveRDS(out, paste0(getwd(), "/Data/simulation.list.regular/out", i, ".rds"))
  
  # ---------------------------------------------
  
  # Remove specific variables in the environment before new iteration
  rm(list = gdata::keep(i, n, true.contribution, select, map, rename, pb, cl))

  # End loop here. This generates n of each data sets (observed.data, observed.data.2, output)
}

# Stop the parallel cluster and the progress bar
stopCluster(cl)
pb$terminate()

# -----------------------------------------------

# Iteration 2 ----
# Analyse Data ----
# From here implement a structure that uses the out of the first loop

# Progress bar
pb <- progress_bar$new(format = "  downloading [:bar] :elapsedfull",
                       total = n, clear = FALSE, width = 100)

# Initiate parallel cluster
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

for (i in 1:n) {
  pb$tick()
  
  # ---------------------------------------------
  
  # Extract ----
  # Extract the data frames from the simulation.list
  out <- readRDS(paste0(getwd(), "/Data/simulation.list.regular/out", i, ".rds"))
  observed.data <- out$observed.data
  observed.data.2 <- out$observed.data.2
  output <- out$output
  
  # ---------------------------------------------
  
  # modeling_and_decomposition (S) ----
  if (T) {
    source(paste0(getwd(), "/modeling_and_decomposition.R"))
  }
  
  # ---------------------------------------------
  
  # curve_fitting (S) ----
  # Define input parameters for the curve_fitting script: fit ("logis" or "hill") and
  # int (number of datapoints on the roi curve domain)
  fit <- "logis"
  int <- 100
  
  if (T) {
    source(paste0(getwd(), "/curve_fitting.R"))
  }
  
  # ---------------------------------------------
  
  # Append to master.list ----
  out <- list(params.media.1 = params.media.1.long, 
              params.media.2 = params.media.2.long,
              curves.media.1 = curves.media.1.long, 
              curves.media.2 = curves.media.2.long,
              R2 = validation)
  
  saveRDS(out, paste0(getwd(), "/Data/master.list.regular/out", i, ".rds"))
  # master.list[[i]] <- out
  
  # ---------------------------------------------
  
  # Remove specific variables in the environment before new iteration
  rm(list = gdata::keep(i, n, select, map, rename, pb, cl))

  # End loop here. This analyses the n data sets and generates n of each data frames
  # (params.media.1, params.media.2, curves.media.1, curves.media.2 and R2) which
  # can be aggregated in the results script and final sections of this script.
}

# Stop the parallel cluster and the progress bar
stopCluster(cl)
pb$terminate()

# -----------------------------------------------

# Create master.list ----
# Import out from data folder and store in master.list of length n
if (T) {
  master.list <- vector(mode = "list", length = n)
  
  for (i in 1:n) {
    master.list[[i]] <- readRDS(paste0(getwd(), "/Data/master.list.regular/out", i, ".rds"))
  }
}

# -----------------------------------------------

# Aggregation ----
# The remainder of the script aggregates the data contained in the master.list and hence
# the final insights are derived. Further the results are structured and the plots are
# formatted. This section corresponds to the Results, Discussion and Robustness Checks
# Section in the thesis.

# results (S) ----
source(paste0(getwd(), "/results.R"))

# results.R
plot_boxplot
plot_boxplot_both
plot_boxplot_media_1
plot_boxplot_media_2
plot_curves_media_1
plot_curves_media_2
plot_mape
plot_not_conv

not.conv

kable(not.conv %>% arrange(media, modeling, decomposition), "latex", booktabs = T, caption = "Unsuccessful approximation") %>%
  kable_styling("center")

# -----------------------------------------------

# curve_plots (S) ----
# This script allows us to select ROI plots for a more detailed discussion
source(paste0(getwd(), "/curve_plots.R"))

# f.ex.
gam.shap
gam.shap.2

# You can arrange plots
ggarrange(gam.shap, gam.shap.2, gam_2.shap, gam_2.shap.2, nrow = 1)
ggarrange(ols_multi.shap, ols_multi.shap.2, svr_multi.shap, svr_multi.shap.2, nrow = 2, ncol = 2)

# -----------------------------------------------

# validation ----
# rbind all R2 from master_list!!
validation <- map(master.list, ~.x[["R2"]]) %>% reduce(rbind) %>% as.data.frame()
validation <- colMeans(validation) %>% round(digits = 2)
validation
