# Script descritpion ----
# simulation_2 simulates data for the first period of the thesis" second part.
# There is no need to compute ground truths for this period as the model comparison
# is based on the second period. The script follows very closely the one simulation_1
# and media_params but imposes a different audience size which leads to a structural break
# between simulation_1 and simulation_2. observed.data.2 data frame is the output of
# this script which together with observed.data constitutes a MMM dataset with
# structural change according to our definition of such a change...

# -----------------------------------------------

# Libraries ----
library(tidyverse)
library(data.table)
library(amss)

# -----------------------------------------------

# DefaultNatMigModule ----
# Simulation window ----
# 4 years weekly data (later a burn in period is dropped)
n.years <- 4
time.n <- n.years * 52

# Population ----
# a.k.a. market size
population <- 9e6

# market.rate.trend ----
market.rate.trend <- 1

# market.rate.seas ----
market.rate.nonoise <- readRDS(paste0(getwd(), "/Data/seasontrend.sim.elg.n.rds"))

# With some added noise (leverage SimulateAR1 function)
market.rate.seas <- pmax(
  0, pmin(1,
          market.rate.nonoise *
            SimulateAR1(length(market.rate.nonoise), 1, 0.1, 0)))
# plot(market.rate.seas, type = "l")

# ggplot(tibble(nonoise = market.rate.nonoise, noise = market.rate.seas, index = 1:length(market.rate.nonoise)) %>% 
#          pivot_longer(cols = c(nonoise, noise)), aes(x = index, y = value, col = name)) + geom_line()

# sat.decay ----
sat.decay <- 1

# Natural transition matrices and initial conditions ----
# activity.transition ----
# 3 state spaces: inactive, exploratory, purchase
activity.transition <- matrix(
  c(0.45, 0.30, 0.25,  # migration originating from inactive state
    0.45, 0.30, 0.25,  # exploratory state
    0.45, 0.30, 0.25), # purchase state
  nrow = length(kActivityStates), byrow = TRUE)

prop.activity <- c(0.45, 0.30, 0.25)

# favorability.transition ----
# 5 state spaces: unaware, unfavorable, neutral, somewhat favorable, favorable
favorability.transition <- matrix(
  c(0.03, 0.07, 0.50, 0.30, 0.10,  # migration from the unaware state
    0.03, 0.07, 0.50, 0.30, 0.10,  # negative state
    0.03, 0.07, 0.50, 0.30, 0.10,  # neutral state
    0.03, 0.07, 0.50, 0.30, 0.10,  # somewhat favorable state
    0.03, 0.07, 0.50, 0.30, 0.10), # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

prop.favorability <- c(0.03, 0.07, 0.50, 0.30, 0.10)

# loyalty.transition ----
# 3 state spaces: switcher, loyal, competitor-loyal
loyalty.transition <- matrix(
  c(0.5, 0.3, 0.2,
    0.5, 0.3, 0.2,
    0.5, 0.3, 0.2),
  nrow = length(kLoyaltyStates), byrow = TRUE)

prop.loyalty <- c(0.5, 0.3, 0.2)

# availability.transition ----
# 3 state spaces: low, average, high
availability.transition <- matrix(
  c(0.3, 0.4, 0.3,
    0.3, 0.4, 0.3,
    0.3, 0.4, 0.3),
  nrow = length(kAvailabilityStates), byrow = TRUE)

prop.availability <- c(0.3, 0.4, 0.3)

# nat.mig.params ----
# Stands for natural migration parameters
# Stack above parameters in list
nat.mig.params <- list(
  population = population,
  market.rate.trend = market.rate.trend,
  market.rate.seas = market.rate.seas,
  prop.activity = prop.activity,
  prop.favorability = prop.favorability,
  prop.loyalty = prop.loyalty,
  prop.availability = prop.availability,
  transition.matrices = list(
    activity = activity.transition,
    favorability = favorability.transition,
    loyalty = loyalty.transition,
    availability = availability.transition))

# -----------------------------------------------

# DefaultSalesModule ----
# price ----
price <- 5250

# advertiser.demand.intercept ----
advertiser.demand.intercept <- 
  list(favorability = c(0.015, 0, 0.2, 0.3, 0.9),
       loyalty = c(0.5, 1, 0),
       availability = c(0.1, 0.5, 1))

# sales.params ----
# stack above parameters in list
sales.params <- list(
  advertiser.demand.intercept = advertiser.demand.intercept,
  price = price)

# -----------------------------------------------

# DefaultTraditionalMediaModule ----
# Media.1 ----
# budget.index --
media.1.budget.index <- rep(1:n.years, each = 52)

# budget --
media.1.budget <-
  readRDS(paste0(getwd(), "/Data/yearly.budget.rds"))[,1] %>% 
  unlist() %>% as.numeric()

# flighting --
media.1.flighting <- readRDS(paste0(getwd(), "/Data/spending.rds"))[[1]]

# audience.membership --
media.1.audience.membership <- list(activity = rep(0.2, 3),
                                    favorability = rep(0.2, 5),
                                    loyalty = rep(0.2, 3))

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
media.2.audience.membership <- list(favorability = rep(0.2, 5),
                                    availability = rep(0.2, 3))

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

# SimulateAMSS ----
sim.data.2 <- SimulateAMSS(
  time.n = time.n,
  nat.mig.module = DefaultNatMigModule,
  nat.mig.params = nat.mig.params,
  media.names = c("media.1", "media.2"),
  media.modules = c(
    `DefaultTraditionalMediaModule`,
    `DefaultTraditionalMediaModule`),
  media.params = list(params.media.1, params.media.2),
  sales.module = DefaultSalesModule,
  sales.params = sales.params)

# -----------------------------------------------

# Define observed.data.2 ----
# Specify burn in and final year ----
n.years <- 4
burn.in.length <- 52
final.year.end <- n.years * 52
final.year.start <- final.year.end - 51

# Restructure and save intermediate ----
# It is recommended to drop an initial time window (here one year); again, probably not
# necessary since without lagged effects, the economy starts in the equilibrium...
observed.data.2 <- sim.data.2$data
observed.data.2 <- observed.data.2[(burn.in.length + 1):final.year.end, ]

# Add seasonality as variable
# We can add the seasonality component: assuming a perfect time series decomposition
observed.data.2 <- data.table(observed.data.2)
observed.data.2[,
              market.rate :=
                market.rate.nonoise[(burn.in.length + 1):final.year.end]]
observed.data.2 <- as_tibble(observed.data.2)

# -----------------------------------------------

# Combine observed.data and observed.data.2 ----
observed.data.2 <-
observed.data.2 %>%
  bind_rows(observed.data)
observed.data.2$time.index <- 1:nrow(observed.data.2)

# Plot --
if (F) {
plot_struc_break <-
ggplot(observed.data.2, 
       aes(x = time.index, y = revenue)) +
  geom_line() +
  labs(x = "time", y = "sales") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_vline(xintercept = 157, col = "red", linetype = "dotted") +
  theme_classic()
}