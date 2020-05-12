# Script description ----
# Leverage sim.data and generate observed.data tibble with ground truth. In particular
# the ground_truth_function is the core of the script which computes true ROI for each
# observation (making use of the CalculateROAS function in the AMSS package).
#
# Requires: sim.data which contains all the data form the AMSS simulation. In particular
# all the population segments are kept track of. Also the observed.data is required,
# containing the surface variables available to the modeler.

# -----------------------------------------------

# Libraries ----
library(tidyverse)
library(data.table)
library(amss)

select <- dplyr::select

# -----------------------------------------------

# Compute ground truth ----
# calculateROAS --
# Get weekly ROAS: here we both define a column containing the whole sampling
# distributions and a column containing the mean of the samples as our ROAS estimate.

# The ground_truth function takes sim.data (sim object), observed.data (data frame), 
# m.name and appends roas.m.name and m.name.true.contribution to the observed.data.
# Returns the observed.data tibble and the plot. n specifies the time window and is
# later set to 0 in our case.
ground_truth_function <- function(simulated, observed, m.name, n) {
  
  # Build variable names and enquo() / sym()
  m.name <- enquo(m.name)
  m.name.spend <- paste0(quo_name(m.name), ".spend") %>%
    rlang::sym()
  roas.m.name <- paste0("roas.", quo_name(m.name)) %>%
    rlang::sym()
  m.name.true.contribution <- paste0(quo_name(m.name), ".true.contribution") %>%
    rlang::sym()
  
  # Main trick of function
  observed <-
    observed %>%
    mutate(!! roas.m.name := map2(.x = time.index, .y = !! m.name.spend,
                                  .f = ~ifelse(.y == 0 || .x >= observed$time.index[nrow(observed) - n], 0,
                                               CalculateROAS(
                                                 simulated,
                                                 media.names = quo_name(m.name),
                                                 t.start = .x,
                                                 t.end = .x + n,
                                                 min.reps = 2,
                                                 max.coef.var = 0.01,
                                                 max.margin.error = 0.01,
                                                 max.time = 1,
                                                 verbose = TRUE))) %>% unlist())
  
  # Maybe add roas.media.margin.error and roas.media.coef.var; but if no warning
  # is issued, then this implies that both measures are below the specified value
  
  # Define true contribution --
  # Here we define (later for each channel) a column with the true contribution, being
  # the channel.ROAS * channel.spend
  observed <-
    observed %>%
    mutate(!! m.name.true.contribution := (!! m.name.spend) * (!! roas.m.name))
  
  return(observed)
}

# -----------------------------------------------

# Apply function for all channels ----
# m.name can both be supplied as arg or string.
media.channels <- c("media.1", "media.2")

observed.data <-
  ground_truth_function(sim.data, observed.data, media.1, n = 0)

observed.data <-
  ground_truth_function(sim.data, observed.data, media.2, n = 0)

# -----------------------------------------------

# Initialize "output" tibble and true.contribution ----
output <-
  observed.data %>%
  select(c(media.1.spend, media.2.spend, market.rate, roas.media.1, roas.media.2,
           media.1.true.contribution, media.2.true.contribution))

true.contribution <-
output %>%
  select(c(roas.media.1, roas.media.2,
         media.1.true.contribution, media.2.true.contribution))


# # Save ----
# saveRDS(observed.data, paste0(getwd(), "/Data/observed.data.rds"))
# saveRDS(output, paste0(getwd(), "/Data/output.rds"))
