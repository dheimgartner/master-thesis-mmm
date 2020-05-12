# Script description ----
# Define different media channels to be sourced in simulation scripts (simulation_1 and
# simulation_2). For an extensive description of all the possible parameters and their meanings,
# refer to simulation.R script or the relevant sections in the Thesis. To simplify the
# choice of hill parameter values the hill function is explored and plotted towards the
# end of the section.
#
# Requires: The budget and fligthing patterns extracted from real data and located in the
# data folder.

# -----------------------------------------------

# Media.1 ----
# budget.index --
media.1.budget.index <- rep(1:n.years, each = 52)

# budget --
# media.1.budget <- rep(c(545e5, 475e5, 420e5, 455e5), length = n.years)
media.1.budget <-
  readRDS(paste0(getwd(), "/Data/yearly.budget.rds"))[,1] %>% 
  unlist() %>% as.numeric()

# flighting --
media.1.flighting <- readRDS(paste0(getwd(), "/Data/spending.rds"))[[1]]

# audience.membership --
media.1.audience.membership <- list(activity = rep(0.5, 3),
                                    favorability = rep(0.5, 5),
                                    loyalty = rep(0.5, 3))

# unit.cost --
media.1.unit.cost <- 0.1

# hill.ec --
media.1.hill.ec <- 0.8

# hill.slope --
media.1.hill.slope <- 0.3

# transition.matrices --
# media.activity.trans.mat --
# 3 state spaces: inactive, exploratory, purchase
media.1.activity.trans.mat <- matrix(
  c(0.50, 0.30, 0.20, # reads: 30% chance of converting an inactive individual to exploration and 20% to purchase
    0.00, 0.70, 0.30, # here for individuals already in explorative state
    0.00, 0.00, 1.00),
  nrow = length(kActivityStates), byrow = TRUE)

# media.favorability.trans.mat --
# 5 state spaces: unaware, unfavorable, neutral, somewhat favorable, favorable
media.1.favorability.trans.mat <- matrix(
  c(0.4,  0.0,  0.4, 0.2, 0.0,
    0.0,  0.9,  0.1, 0.0, 0.0,
    0.0,  0.0,  0.5, 0.4, 0.1,
    0.0,  0.0,  0.0, 0.8, 0.2,
    0.0,  0.0,  0.0, 0.0, 1.0),
  nrow = length(kFavorabilityStates), byrow = TRUE)

# media.loyalty.trans.mat --
# 3 state spaces: switcher, loyal, competitor-loyal
media.1.loyalty.trans.mat <- matrix(
  c(0.50, 0.25, 0.25,
    0.00, 1.00, 0.00,
    0.30, 0.00, 0.70),
  nrow = length(kLoyaltyStates), byrow = TRUE)

# params.media --
# stack all above defined parameters in list
params.media.1 <- list(
  audience.membership = media.1.audience.membership,
  budget = media.1.budget,
  budget.index = media.1.budget.index,
  flighting = media.1.flighting,
  unit.cost = media.1.unit.cost,
  hill.ec = media.1.hill.ec,
  hill.slope = media.1.hill.slope,
  transition.matrices = list(
    activity = media.1.activity.trans.mat,
    favorability = media.1.favorability.trans.mat,
    loyalty = media.1.loyalty.trans.mat))

# -----------------------------------------------

# Media.2 ----
# budget.index --
media.2.budget.index <- rep(1:n.years, each = 52)

# budget --
media.2.budget <- 
  readRDS(paste0(getwd(), "/Data/yearly.budget.rds"))[,2] %>% 
  unlist() %>% as.numeric()

# flighting --
media.2.flighting <- readRDS(paste0(getwd(), "/Data/spending.rds"))[[2]]
  

# audience.membership --
media.2.audience.membership <- list(favorability = rep(0.5, 5),
                                    availability = rep(0.5, 3))

# unit.cost --
media.2.unit.cost <- 0.2

# hill.ec --
media.2.hill.ec <- 3

# hill.slope --
media.2.hill.slope <- 6

# media.favorability.trans.mat --
# 5 state spaces: unaware, unfavorable, neutral, somewhat favorable, favorable
media.2.favorability.trans.mat <- matrix(
  c(0.2,  0.0,  0.5, 0.3, 0.0,
    0.0,  0.8,  0.2, 0.0, 0.0,
    0.0,  0.0,  0.5, 0.5, 0.0,
    0.0,  0.0,  0.0, 0.7, 0.3,
    0.0,  0.0,  0.0, 0.0, 1.0),
  nrow = length(kFavorabilityStates), byrow = TRUE)

# media.availability.trans.mat --
# 3 state spaces: low, average, high
media.2.availability.trans.mat <- matrix(
  c(0.25, 0.50, 0.25,
    0.00, 0.50, 0.50,
    0.00, 0.00, 1.00),
  nrow = length(kLoyaltyStates), byrow = TRUE)

# params.media --
# stack all above defined parameters in list
params.media.2 <- list(
  audience.membership = media.2.audience.membership,
  budget = media.2.budget,
  budget.index = media.2.budget.index,
  flighting = media.2.flighting,
  unit.cost = media.2.unit.cost,
  hill.ec = media.2.hill.ec,
  hill.slope = media.2.hill.slope,
  transition.matrices = list(
    favorability = media.2.favorability.trans.mat,
    availability = media.2.availability.trans.mat))

# -----------------------------------------------

if (F) {

# Plot hill function ----
hill_function <- function(f, hill.ec, hill.slope) {
  y <- 1 / (1 + (f / hill.ec)^(-hill.slope))
  return(y)
}

# We might want to define the respective hill transformations over different
# frequency scales. In the checks script, we can see, that media
# frequencies have completely different scales given the specified population 
# migration processes, budget restrictions and flighting/spending functions...
# In order to impose the response curve being realized in the simulated data and
# still pay tribute to VIF and spending patterns, we might simply want to check
# out media frequency ranges and define the hill function over it.
ec.1a <- 0.8
sl.1a <- 0.3
ec.1b <- 1
sl.1b <- 0.75
ec.2a <- 3
sl.2a <- 6
ec.2b <- 5
sl.2b <- 4

freq <- seq(0, 7, 0.01)

effect.1a <- hill_function(f = freq, hill.ec = ec.1a, hill.slope = sl.1a)
effect.1b <- hill_function(f = freq, hill.ec = ec.1b, hill.slope = sl.1b)
effect.2a <- hill_function(f = freq, hill.ec = ec.2a, hill.slope = sl.2a)
effect.2b <- hill_function(f = freq, hill.ec = ec.2b, hill.slope = sl.2b)
effect <- tibble(frequency = freq, "media 1" = effect.1a, "media 2" = effect.2a,
                 media.1b = effect.1b, media.2b = effect.2b)

ggplot(effect %>% pivot_longer(cols = c("media 1", "media 2", media.1b, media.2b), names_to = "channel", values_to = "effect"), 
       aes(x = frequency, y = effect, col = channel)) + geom_line()

plot_hill <-
  ggplot(effect %>% pivot_longer(cols = c("media 1", "media 2"), names_to = "channel", values_to = "effect"),
         aes(x = frequency, y = effect, col = channel)) + geom_line() + theme_bw()
plot_hill
}