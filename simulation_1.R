# Script description ----
# chronological workbook to go through the AMSS package. Explains the main ingrediants
# to simulate data with the default modules. For each function we print the description
# and list all the parameters with relevant descriptions. It is recommended to go through
# this script chronologically together with the description in the Thesis (sections
# Introduction to the Aggregate Marketing System Simulator and the Data section elaborating
# on the exact simulation specification).

# -----------------------------------------------

# Libraries ----
library(tidyverse)
library(data.table)
library(amss)

# -----------------------------------------------

# DefaultNatMigModule ----
# This function models natural consumer behavior in the absence of marketing
# interventions. In particular, it models changes in consumer mindset over time
# that are outside of advertiser control, such as seasonal changes.

# Simulation window ----
# 4 years weekly data (later a burn in period is dropped)
n.years <- 4
time.n <- n.years * 52

# Population ----
# a.k.a. market size
population <- 9e6

# market.rate.trend ----
# The trend in market size, written as the proportion of the population to be
# considered potentially in the market, pending seasonal adjustments. If a vector,
# should match time.n in length. Defaults to 1, for full population participation
# in market.
market.rate.trend <- 1

# market.rate.seas ----
# A sinusoidal pattern with trend
# Can later be added to observed data: assuming perfect time decomposition...

# We match seasontrend from real data (extracted with the prophet decomposition).
# The extracted seasontrend pattern has to be adjusted s.t. range [0, 1]
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
# Single numeric value between 0 and 1, representing the geometric decay rate at
# which satiated individuals become unsatiated. Defaults to 1 for satiation lasting
# 1 time period for all individuals.
sat.decay <- 1

# Natural transition matrices and initial conditions ----
# Below we define the activity, favorability, loyalty and availability transition matrices. 
# Each matrice has dimension state.space x state.space. A row-identical matrix implies no lagged effects.
# Also, we pin down the initial conditions / proportions for each of the categories.

# In words of the AMSS authors:
# list of matrices for each dimension of population segmentation THAT MAY BE
# AFFECTED by marketing interventions. A named list with members "activity",
# "favorability", "loyalty", and "availability" is expected. By default, any
# missing members will have no effect. The transition matrices represent natural
# migration in these dimensions, and control how quickly the population returns to
# its equilibrium allocation across segments after marketing interventions.

# Equilibrium allocations are defined via the prop.parameter and hence are
# specified by a proportion belonging to each state!

# activity.transition ----
# 3 state spaces: inactive, exploratory, purchase
activity.transition <- matrix(
  c(0.45, 0.30, 0.25,  # migration originating from inactive state
    0.45, 0.30, 0.25,  # exploratory state
    0.45, 0.30, 0.25), # purchase state
  nrow = length(kActivityStates), byrow = TRUE)

# Vector of nonnegative values summing to 1, representing the proportion of the
# population to be assigned to each activity state, given they are "responsive,"
# i.e., "in.market" and "unsatiated."
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

# Vector of nonnegative values summing to 1, representing the proportion of the
# population to be assigned to each favorability state, given they are not "loyal."
prop.favorability <- c(0.03, 0.07, 0.50, 0.30, 0.10)

# loyalty.transition ----
# 3 state spaces: switcher, loyal, competitor-loyal
loyalty.transition <- matrix(
  c(0.5, 0.3, 0.2,
    0.5, 0.3, 0.2,
    0.5, 0.3, 0.2),
  nrow = length(kLoyaltyStates), byrow = TRUE)

# Vector of nonnegative values summing to 1, representing the proportion of the
# population to be assigned to each loyalty state.
prop.loyalty <- c(0.5, 0.3, 0.2)

# availability.transition ----
# 3 state spaces: low, average, high
availability.transition <- matrix(
  c(0.3, 0.4, 0.3,
    0.3, 0.4, 0.3,
    0.3, 0.4, 0.3),
  nrow = length(kAvailabilityStates), byrow = TRUE)

# Vector of nonnegative values summing to 1, representing the proportion of the
# population to be assigned to each availability state.
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
# Simulate consumer purchase behavior, and thus the advertiser"s and its 
# competitors" sales.

# price ----
# Numeric vector of product price over time. If the vector is shorter than the
# number of timepoints, it is repeated as necessary.
price <- 5250 # SEK

# mean.price ----
# Numeric scaler, the mean of price over time. Defaults to zero.

# advertiser.demand.intercept ----
# List of numeric vectors corresponding to each brand state (favorability, loyalty,
# and availability). The product of multiplicands corresponding to a particular
# segment with "purchase" activity state is the probability consumers in that
# segment will purchase the advertiser"s product if the price is mean.price and
# there is no competition. Missing members of the list have no effect on the
# calculation (are set to 1).
advertiser.demand.intercept <- 
  list(favorability = c(0.01, 0, 0.2, 0.3, 0.9),
       loyalty = c(0.5, 1, 0),
       availability = c(0.1, 0.5, 1))

# advertiser.demand.slope ----
# List of numeric vectors corresponding to each brand state (favorability, loyalty,
# and availability). The product of multiplicands corresponding to a particular
# segment with "purchase" activity state is the linear decrease in the probability
# consumers in that segment will purchase the advertiser"s product when the price
# increases by 1, when there is no competition. Missing members of the list have no
# effect on the calculation.

# competitor.demand.max ----
# List of numeric vectors corresponding to each brand state (favorability, loyalty,
# and availability). The product of multiplicands corresponding to a particular 
# segment with "purchase" activity state is the probability consumers in that 
# segment will purchase a competitor"s product when advertiser"s product is too 
# expensive to be a feasible choice. Missing members of the list have no effect on 
# the calculation.

# competitor.demand.replacement ----
# List of numeric vectors corresponding to each brand state (favorability, loyalty, 
# and availability). The product of multiplicands corresponding to a particular
# segment specifies the degree to which advertiser and competitor sales are
# replacements for each other. At 1, competitor sales are unaffected by advertiser
# pricing, and competitor sales replace advertiser sales to the greatest degree
# possible. At 0, advertiser sales are unaffected by the presence of the competitor,
# and advertiser sales replace competitor sales to the greatest degree possible.
# Thus, a reasonble interpretation of consumer loyalty might set this parameter to
# list(loyalty = c(0.5, 0.1, 0.9). Missing members of the list have no effect on the
# calculation.

# purchase.quantity.intercept ----
# Numeric, at least 1. Represents the average number of units bought by each
# consumer purchasing from the advertiser"s brand, if price is mean.price.

# purchase.quantity.slope ----
# Numeric, generally >= 0. Represents the decrease in the average purchase quantity
# per consumer purchasing from the advertiser"s brand given a unit increase in price.
# Missing members of the list have no effect on the calculation.

# purchase.quantity.competitor ----
# Average number of units bought by consumers purchasing a comeptitor"s product.
# Must be at the least the default value of 1.

# unit.cost ----
# Numeric greater than 0, cost of goods sold, for one unit of the advertiser"s product.

# advertiser.transitions ----
# List of transition matrices for each brand state, specifying post-purchase changes
# in consumer mindset for those who purchased the advertiser"s brand. A named list
# with members "favorability", "loyalty", and "availability" is expected. Any
# missing members will have no effect. The default value, list() results in no
# post-purchase migration.

# competitor.transitions ----
# List of transition matrices for each brand state, specifying post-purchase changes
# in consumer mindset for those who purchased a competitor"s brand. A named list
# with members "favorability", "loyalty", and "availability" is expected. Any
# missing members will have no effect. The default value, list() results in no
# post-purchase migration.

# sales.params ----
# Stack above parameters in list
sales.params <- 
  list(advertiser.demand.intercept = advertiser.demand.intercept,
       price = price)

# -----------------------------------------------

# The DefaultTraditionalMediaModule is described here for completeness. The relevant
# parameters are sourced from the media_params script in our case.

# # DefaultTraditionalMediaModule ----
# # we focus on the traditional media module, since the architecture allows for
# # specifying shape effects via a hill transformation schedule. it is recommended
# # to further explore the DefaultSearchMediaModule for more specific modeling of
# # social media platforms or search engine advertising...
# ?DefaultTraditionalMediaModule
# # Simulate the behavior of a traditional media channel, and generate associated 
# # observable variables such as media volume and spend.
# 
# # budget.index ----
# # vector specifying budget period each time point belongs to. For example,
# # rep(1:4, each = 52) would correspond to 4 years of yearly budget periods
# budget.index <- rep(1:n.years, each = 52)
# 
# # budget ----
# # vector specifying the target spend for each budget period. For example, given the
# # example budget.index from above, budget = rep(1e6, 4) would specify a budget of
# # 1 million for each year.
# budget <- rep(c(545e5, 475e5, 420e5, 455e5), length = n.years)
# 
# # flighting ----
# # specifies the relative amount to be spent on each time point within a budget 
# # period. For example, in a budget period of two weeks, flighting = c(1,2) specifies 
# # that twice 1/3 of the budget should be spent in the first week, and 2/3 in the second.
# media.flighting <-
#   pmax(0,
#        market.rate.seas +
#          SimulateAR1(length(market.rate.seas), -0.7, 0.7, -0.7))
# media.flighting <- media.flighting[c(6:length(media.flighting), 1:5)]
# 
# # audience.membership ----
# # list of multipliers used to calculate probability of audience membership. Each 
# # element of the list corresponds to a specific dimension of population segmentation. 
# # Multipliers corresponding to each dimension are multiplied to derive audience 
# # membership probability for each segment. A named list with members "activity", 
# # "favorability", "loyalty", and "availability" is expected. Each member is a 
# # numeric vector containing the multipliers to use for each state in the dimension. 
# # For example, if member "activity" is c(1, 0.5, 0.7), a multiplier of 0.7 should 
# # be used for all segments with activity state "purchase." By default, any missing
# # members will have no effect.
# audience.membership <- list(activity = rep(0.4, 3))
# 
# # unit.cost ----
# # positive numeric specifying expected unit cost per exposure.
# unit.cost <- 0.005
# 
# # effectiveness.function ----
# # vectorized function mapping frequency to media effect (relative to transition 
# # matrices specifying maximum effect). The range of the function should be bounded
# # between 0 and 1. Given the default value of NULL, the module will used the Hill 
# # transformation with parameters hill.ec and hill.slope.
# 
# # hill.ec ----
# # parameter controlling the scaling of frequency vs. effect. This is the EC50 of
# # the Hill transformation.
# hill.ec <- 1.56
# 
# # hill.slope ----
# # parameter controlling the scaling of frequency vs. effect. This is the maximum
# # slope of the Hill transformation.
# hill.slope <- 1
# 
# # transition.matrices ----
# # list of transition matrices for each dimension of population segmentation that
# # may be affected by marketing interventions. A named list with members "activity", 
# # "favorability", "loyalty", and "availability" is expected. By default, any missing
# # members will have no effect.
# 
# # media.activity.trans.mat ----
# # 3 state spaces: inactive, exploratory, purchase
# media.activity.trans.mat <- matrix(
#   c(1.00, 0.00, 0.00,  # migration originating from the inactive state
#     0.00, 1.00, 0.00,  # exploratory state
#     0.00, 0.00, 1.00), # purchase state
#   nrow = length(kActivityStates), byrow = TRUE)
# 
# # media.favorability.trans.mat ----
# # 5 state spaces: unaware, unfavorable, neutral, somewhat favorable, favorable
# media.favorability.trans.mat <- matrix(
#   c(0.4,  0.0,  0.4, 0.2, 0.0,  # migration from the unaware state
#     0.0,  0.9,  0.1, 0.0, 0.0,  # negative state
#     0.0,  0.0,  0.6, 0.4, 0.0,  # neutral state
#     0.0,  0.0,  0.0, 0.8, 0.2,  # somewhat favorable state
#     0.0,  0.0,  0.0, 0.0, 1.0), # favorable state
#   nrow = length(kFavorabilityStates), byrow = TRUE)
# 
# # media.loyalty.trans.mat ----
# # 3 state spaces: switcher, loyal, competitor-loyal
# 
# # media.availability.trans.mat ----
# # 3 state spaces: low, average, high
# 
# # params.media ----
# # stack all above defined parameters in list
# params.media <- list(
#   audience.membership = audience.membership,
#   budget = budget,
#   budget.index = budget.index,
#   flighting = media.flighting,
#   unit.cost = unit.cost,
#   hill.ec = hill.ec,
#   hill.slope = hill.slope,
#   transition.matrices = list(
#     activity = media.activity.trans.mat,
#     favorability = media.favorability.trans.mat))
#
# cpc.min <- 0.8
# cpc.max <- 1.1

# -----------------------------------------------

# SimulateAMSS ----
# Produces an amss.sim object that contains the simulated data and can be used to
# derive ground truth about the scenario.

# Source media params from script media_channels:
# The above defined media parameters serve only as an illustration purpose; the
# ones relevant for the simulation are sourced from here:
source(paste0(getwd(), "/media_params.R"))

# Call main function of the package (all the parameters are defined above or in
# the script media_params.R). Adjust number of media channels below manually.
sim.data <- SimulateAMSS(
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


# Specify burn in and final year
n.years <- 4
burn.in.length <- 52
final.year.end <- n.years * 52
final.year.start <- final.year.end - 51

# Restructure and save intermediate
# It is recommended to drop an initial time window (here one year); probably not necessary
# in case of no lagged effects as the economy starts in equilibrium. Anyways...
observed.data <- sim.data$data
observed.data <- observed.data[(burn.in.length + 1):final.year.end, ]

# Add seasonality as variable
# We can add the seasonality component: assuming a perfect time series decomposition
observed.data <- data.table(observed.data)
observed.data[,
              market.rate :=
                market.rate.nonoise[(burn.in.length + 1):final.year.end]]
observed.data <- as_tibble(observed.data)

# # Save ----
# saveRDS(sim.data, paste0(getwd(), "/Data/sim.data.rds"))



