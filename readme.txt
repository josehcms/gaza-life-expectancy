Replication materials
---------------------------------------------------------------------------------------------------
### Life expectancy losses in the Gaza Strip during the period October, 2023, to September, 2024
### Michel Guillot, Mohammed Draidi, Valeria Cetorelli, José H C Monteiro Da Silva, Ismail Lubbad
### Lancet 2025; 405: 478–85
### https://doi.org/10.1016/S0140-6736(24)02810-1
---------------------------------------------------------------------------------------------------

This github repository contains 3 directories:
- R: R codes for replicating the analyses and results
- inputs: input datasets
- outputs: outputs from the analysis

The R codes are organized in the following way:
- Functions starting with 0X_ contain basic setup and main functions for performing the analyses:
	- 00_basic_setup.R: to clean the R environment and load packages
	- 01_lifetable_funcs.R: functions for estimating life tables
	- 02_pyl_funcs.R: functions for estimating population exposures (person-years lived)
	- 03_interpolation_funcs.R: functions for interpolation 
	- 04_leecarter_funcs.R: function for mortality projection using the Lee-Carter method

- Functions starting with 1X_ contain the scripts for the estimation of life expectancy losses in the Gaza Strip during the Oct 2023 - Sep 2024 period
	- 10_estimate_prewar_and_war_population_exposures.R: script for estimating population exposures before and during the war
	- 11_estimate_lifetables_2017_2022.R: script for estimating the pre-war life tables
	- 12_leecarter_projection_prewar.R: script for projecting mortality until the period before the war using the Lee-Carter Method
	- 13_estimate_lifetables_war_oct2023_sep2024.R: script for estimating the life tables during the war period
	- 14_montecarlo_simulation_e0_cis.R: script for estimating the confidence intervals of life tables using the Monte Carlo simulation approach
