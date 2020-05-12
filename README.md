# master-thesis-mmm
All my scripts for the master thesis **"Sales Modeling and Local Factor Decomposition for Optimal Investment Decisions in MMM: A Monte Carlo Simulation Study"**

# workflow
The central file which implements the iterations of the Monte Carlo study is **master_simulation_loop.R**. It is recommended to open this script and go through the residual scripts in chronological order. All the scripts start with a description and mention the required inputs.

# reproducability
The full output of the thesis is reproducable with these scripts with the exemption of **real_data_exploration_calibration.R** which requires the `SeasonTrendProphet` function (which is third parties software property). However, this script is not required for reproducability of the results. It is only required for the calibration process which guided our parameter choice in other scripts.
