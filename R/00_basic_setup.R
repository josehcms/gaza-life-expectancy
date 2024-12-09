################################################################################
### Script: Basic setup
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Load packages #-------------------------------------------------------------
rm( list = ls() )    # remove objects from R environment
graphics.off()       # close any graphs open
require(lubridate)   # for dealing with dates
require(openxlsx)    # for saving/reading xlsx files
require(tidyverse)   # for graphs and data wrangling
require(data.table)  # for data wrangling
################################################################################