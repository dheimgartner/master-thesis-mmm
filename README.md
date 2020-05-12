# Scripts for Master Thesis
<img src="Figures/emblem.png" width=200 align="right">

**Sales Modeling and Local Factor Decomposition for Optimal Investment Decisions in MMM**

A Monte Carlo Simulation Study


# Workflow
The central file which implements the iterations of the Monte Carlo study is called **master_simulation_loop.R**. It is recommended to open this script and go through the residual ones in chronological order. All the scripts start with a description and mention the required inputs. The master_simulation_loop.R file is run for both the original and alternative simulation specification. The paths and the conditionals have to be adjusted manually (= substitute .regular with .extreme where necessary and set F to T for the respective scripts in order to generate data for the robustness checks...)!

- **master_simulation_loop.R:** 
main file, runs all the other script important for the Monte Carlo study

- **simulation_1.R:** 
AMSS simulation specification *window 1*

- **media_params.R:** 
AMSS media channels specification, sourced by script above

- **simulation_1_extreme.R:** 
AMSS simulation specification for robustness checks *window 1*

- **simulation_2.R:** 
AMSS simulation specification *window 2*

- **simulation_2_extreme.R:** 
AMSS simulation specification for robustness checks *window 2*

- **observed_data_and_ground_truth.R:** 
extracts real factor contributions (ROI-curves)

- **real_data_exploration_calibration.R:** 
explores the real data set and computes calibration metrics, defines flighting patterns, media budget, etc. for simulation specification (not to be run)

- **modeling_and_decomposition.R:** 
fits all the models and decomposes them in order to trace out factor contributions (according to WFD, ALE and SHAP)

- **curve_fitting.R:** 
fits a logistic function to the contribution scatter, stores the xy coordinates for the curve plots and the ROI-curve parameters

- **results.R:** 
produces the final results by aggregating all 500 iterations

- **curve_plots.R:** 
produces individual curve plots for each model, decomposition and media channel which can be combined (f.ex. with help of `ggarrange`)

- **own_functions.R:** 
most important functions


# Reproducability
The full output of the thesis is reproducable with these scripts with the exemption of **real_data_exploration_calibration.R** which requires the `SeasonTrendProphet` function (which is third partie's software property). However, this script is not required for reproducability of the results. It is only required for the calibration process which guided our parameter choice in other scripts.
