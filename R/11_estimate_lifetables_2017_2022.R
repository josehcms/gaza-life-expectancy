################################################################################
### Script: Estimate life tables for 2017-2022
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Basic setup and load functions #--------------------------------------------

# basic setup
source( 'R/00_basic_setup.R' )

# life table functions
source( 'R/01_lifetable_funcs.R' )

# path to the main input file 
main_file = 'inputs/input_data_gaza.xlsx' 

# igme file path
igme_file = 'inputs/igme2023_imr_u5mr_palestine.csv'
################################################################################

### Read data #-----------------------------------------------------------------

# read pop and change format to data.table for data analysis
pop_dt = 
  read.xlsx( main_file, 
             sheet = 'Population',
             detectDates = T # for detecting date format
  ) %>% 
  as.data.table

# read deaths and change format to data.table for data analysis
deaths_dt = 
  read.xlsx( main_file,
             sheet = 'VR-Deaths',
             detectDates = T # for detecting date format
  ) %>% 
  as.data.table

# read igme Palestine for under-5 mortality adjustment
igme_dt = fread( igme_file )
################################################################################

### Estimate life tables for 2017-2022 #----------------------------------------

# List years and sexes for estimation
list_years = 2017:2022
list_sex = c( 'Females', 'Males', 'Both' )

# Create empty dataset to store life tables
lt_gaza = data.table()  

# Loop through years and sexes
for( this_year in list_years ){
  for( this_sex in list_sex ){
    
    # create temp datasets with filtered year and sex for each dataset
    temp_pop    = pop_dt[ Year == this_year & Sex == this_sex, ] # population
    temp_deaths = deaths_dt[ Year == this_year & Sex == this_sex, ]  # deaths
    temp_igme   = igme_dt[ Year == this_year & Sex == this_sex, ] # igme
    
    x = temp_pop$AgeStart     # age vector
    nEx = temp_pop$Population # mid-year population and 1-year span = exposure approximates to mid-year pop
    nDx = temp_deaths$Deaths  # death counts 
    
    lt_gaza =
      rbind( 
        lt_gaza,
        # First, the raw VR unadjusted
        data.table(
          lt_label = 'VR', # label
          ref_period = this_year + 0.5, # reference period
          preston_life_table( x = x, nEx = nEx, nDx = nDx, Sex = this_sex ) # run life table
        ),
        # Now, the adjusted by IGME IMR and U5MR values
        data.table(
          lt_label = 'Adjusted', # label
          ref_period = this_year + 0.5, # reference period
          preston_life_table( x = x, nEx = nEx, nDx = nDx, Sex = this_sex, # run life table
                              q0_1 = temp_igme$q0_1,  # adjustment IMR
                              q0_5 = temp_igme$q0_5 ) # adjustment U5MR
        )
      )
  }
  # clean
  rm( temp_pop, temp_deaths, temp_igme, x, nEx, nDx )
}
################################################################################

### Save #----------------------------------------------------------------------

# save only the adjusted version using igme, which is the one we will use
fwrite( lt_gaza[ lt_label == 'Adjusted' ],
        file = 'outputs/02_life_tables_gaza_2017_2022.csv',
        bom = TRUE )

################################################################################
