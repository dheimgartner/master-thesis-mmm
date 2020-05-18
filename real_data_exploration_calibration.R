# Script description ----
# Explores the real datasets in order to extract calibration metrics. These are:
# media spending patterns, media budgets, etc. The real data set is compared to the
# simulated data set and relevant metrics (s.a. correlation matrices) are computed. Also,
# graphs for comparison illustrations are computed. ggcorrplot is really nice for
# correlation plots!
#
# Requires: A real data set and the observed.data data frame.

# -----------------------------------------------

# Libraries ----
library(tidyverse)
library(readxl)
library(corrplot)
library(ggcorrplot)
library(regclass)
library(prophet)

# Source mmm functions (Nepa's software property)
source(paste0(getwd(),"/mmm_functions.R"))

select <- dplyr::select
slice <- dplyr::slice

# -----------------------------------------------

# # Import data ----
# observed.data <- readRDS(paste0(getwd(), "/Data/observed.data.rds"))

# -----------------------------------------------

# Data prep ----
data.real <- read_csv2(paste0(getwd(), "/Data/real data/data_real.csv"), col_names = T)
summary.data.real <- summary(data.real)
data.real$time.index <- 1:nrow(data.real)

# -----------------------------------------------

# Decomposition ----
# Subsequently we leverage nepa"s function from the the mmm_functions script which in turn
# decomposes the time series with help of the prophet function.

# Recode YearWeek as Week
data.real <- data.real %>% mutate(Week = substr(YearWeek, 5,6)) %>% select(-YearWeek)

# # Seasontrend component for simulation:
# # We need to standardize 0-1 before decomposition (recall from the simulation_1 script,
# # that trend patterns have to be on that scale!)
# # Range function
# range01 <- function(x) {(x - min(x)) / (max(x) - min(x))}
# data.elg.n <- range01(data.real %>% select(-Week)) %>% as_tibble()
# data.real.n$Week <- data.real$Week
# data.real.n$Year <- data.real$Year
# 
# prophet.real.n <- SeasonTrendProphet(data.real.n, "StoreSales", holidays = NULL)
# 
# prophet.real.n <- SeasonTrendProphet(data.real.n, "StoreSales", holidays = NULL)
# data.real.n$seasontrend <- prophet.real.n$trend + prophet.real.n$yearly
# 
# seasontrend.sim.real.n <- as.numeric(append(data.real.n$seasontrend[1:54], data.real.n$seasontrend))
# saveRDS(seasontrend.sim.real.n, paste0(getwd(), "/Data/seasontrend.sim.real.n.rds"))

prophet.real <- SeasonTrendProphet(data.real, "StoreSales", holidays = NULL)

# Add seasontrend as a column in model data
data.real$seasontrend <- prophet.real$trend + prophet.real$yearly

# Remove Week (and WeekNumber)
data.real <- data.real %>% select(-c(Week, WeekNumber))

# -----------------------------------------------

# Exploration ----
# Variables and summary stats --
names(data.real)
summary(data.real)

# Correlation matrix befor decay transformation --
col <- colorRampPalette(c("red", "white", "blue"))(20)

corrplot(cor(data.real), tl.cex = 0.6, col = col)
corrplot(cor(data.real), tl.cex = 0.6, col = col, method = "number")

# VIF values --
real.fit <- lm(StoreSales ~. , data = data.real %>% select(-time.index))
summary(real.fit)
real.vif <- VIF(real.fit)
real.vif


# Select media variables of interest ----
vars.real <- c("Display", "DM", "DM_Insert", "Facebook", "OOH", "Print", "Search", "TVC", "Youtube")

# -----------------------------------------------

# Plots ----
# Sales time-series --
ggplot(data.real, aes(x = time.index, y = StoreSales)) + geom_line() +
  geom_line(aes(x = time.index, y = seasontrend), col = "blue") +
  labs(x = "week", y = "sales")

# Spending patterns --
data.real.long <- data.real %>% pivot_longer(cols = vars.real)
ggplot(data.real.long %>% filter(name != "StoreSales"), aes(x = time.index, y = value, col = name)) + geom_line()

# We might choose these three channels...
ggplot(data.real %>% pivot_longer(cols = c(Display, Print)),
       aes(x = time.index, y = value, col = name)) + geom_line()

# -----------------------------------------------

# Calibration ----
# In particular, we extract yearly budgets and spending patterns for 3 selected
# media channels and make various comparisons. The level and general nature of the
# simulated data is mainly driven by either the population size or the market.rate 
# (both in the simulation script) which is set to be equal to seasontrend (extracted
# from real data with prophet - see above)

# As the simulated data consists of 3 years of weekly data (156 obs) after burn-in
# is droped, we need to adjust the length of the data we use in the simulation...
# Recall that we simulate 4 years of weekly data (208 obs) where the first year is
# later to be dropped. hence we simply duplicate the first year (of real data).

# Choose 2 media channels ----
media.names <- c("Display", "Print")

# -----------------------------------------------

# Yearly budget ----
# Define index
i.1 <- 1:52
i.2 <- 53:105
i.3 <- 106:nrow(data.elg)

budget.1 <-
  data.elg %>% filter(time.index %in% i.1) %>% select(media.names) %>% 
  summarise_all(.funs = ~sum(.x))
budget.2 <-
  data.elg %>% filter(time.index %in% i.2) %>% select(media.names) %>% 
  summarise_all(.funs = ~sum(.x))
yearly.budget <- bind_rows(budget.1, budget.2)

# Duplicate first year (because the burn in is dropped)
yearly.budget <- bind_rows(budget.1, yearly.budget)

# # save
# saveRDS(yearly.budget, paste0(getwd(), "/Data/yearly.budget.rds"))

# -----------------------------------------------

# Spending functions ----
spending <- list()
for (i in 1:length(media.names)) {
  spending[[i]] <- data.elg[, media.names[i]] %>% unlist() %>% as.numeric()
  
  spending.1 <- spending[[i]][i.1] / as.numeric(budget.1[1, media.names[i]])
  spending.2 <- spending[[i]][i.2] / as.numeric(budget.2[1, media.names[i]])
  
  spending[[i]] <- c(spending.1, spending.2)
  
  # Duplicate first year
  spending[[i]] <- append(spending[[i]][1:54], spending[[i]])
}

# # Save
# saveRDS(spending, paste0(getwd(), "/Data/spending.rds"))

# -----------------------------------------------

# Real vs. simulated data ----
# Sales time-series --
# Compare real sales to simulated sales
summary_real_sim <-
summary(observed.data$revenue) / summary(data.elg$StoreSales)
summary_real_sim

data.elg$sales.sim <- observed.data$revenue[3:nrow(observed.data)]

plot_real_sim <-
ggplot(data.elg %>% 
         pivot_longer(cols = c(StoreSales, sales.sim)) %>% 
         mutate(name = ifelse(name == "sales.sim", "simulated", "real")),
       aes(x = time.index, y = value, col = name)) + geom_line() +
  labs(x = "time", y = "sales", col = "dataset") +
  theme_bw()
plot_real_sim


# Spending patterns --
data.elg$media.1 <- observed.data$media.1.spend[3:nrow(observed.data)]
ggplot(data.elg %>% pivot_longer(cols = c(Display, media.1)),
       aes(x = time.index, y = value, col = name)) + geom_line()

data.elg$media.2 <- observed.data$media.2.spend[3:nrow(observed.data)]
ggplot(data.elg %>% pivot_longer(cols = c(Print, media.2)),
       aes(x = time.index, y = value, col = name)) + geom_line()

# Make facet wrap for thesis
plot_spending_patterns <-
  ggplot(data.elg %>% rename("media 1" = media.1, "media 2" = media.2) %>% 
           pivot_longer(cols = c("media 1", "media 2")),
         aes(x = time.index, y = value, col = name)) + 
  geom_line() +
  facet_grid(cols = vars(name)) +
  labs(x = "time", y = "media spend", col = "channel") +
  theme_bw()
plot_spending_patterns

# Corrplots
corr <- round(cor(observed.data %>% 
                    select(revenue, market.rate, media.1.spend, media.2.spend) %>% 
                    rename(sales = revenue, "market rate" = market.rate,
                           "media 1" = media.1.spend, "media.2" = media.2.spend)), 1)

# ggcorrplot is really nice for correlation plots!
plot_corr_sim <-
  ggcorrplot(corr, lab = T, col = c("red", "green", "blue")) +
  ggtitle("simulated data")
plot_corr_sim

corr <- round(cor(data.elg %>% 
                    select(StoreSales, seasontrend, Display, Print) %>%
                    rename(sales = StoreSales, "market rate" = seasontrend, 
                           "media 1" = Display, "media 2" = Print)), 1)
plot_corr_real <-
  ggcorrplot(corr, lab = T, col = c("red", "green", "blue")) +
  ggtitle("real data")
plot_corr_real
