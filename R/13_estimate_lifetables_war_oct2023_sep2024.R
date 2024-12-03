################################################################################
### Script: Estimate life tables during war period
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Basic setup #---------------------------------------------------------------
# basic setup
source( 'R/00_basic_setup.R' )
# life table functions
source( 'R/01_lifetable_funcs.R' )
################################################################################

### Read data #-----------------------------------------------------------------

# exposures
expos_dt = fread( 'outputs/01_prewar_and_war_population_exposures.csv' )

# prewar mortality = no war scenario
ltnowar = fread( 'outputs/03_life_tables_gaza_projected_oct2022_sep2023.csv' )
################################################################################

### Calculate mortality rates during war for different scenarios #--------------

# Merge exposures and casualties with lee-carter proj nMxs for the period Oct 1 2022 - Oct 1 2023
wardeaths_dt =
  merge(
    expos_dt[ , 
              .( sex = Sex, x = AgeStart, 
                 Scenario1, Scenario2, Scenario3, expos1, expos2, expos3 ) ],
    ltnowar[ , .( sex, x, nMx ) ],
    by = c( 'sex', 'x' )
  )

# deaths by age and sex for the 3 different scenarios
wardeaths_dt[ , deaths1 := expos1 * nMx + Scenario1 ]
wardeaths_dt[ , deaths2 := expos2 * nMx + Scenario2 ]
wardeaths_dt[ , deaths3 := expos3 * nMx + Scenario3 ]

# death rates for the 3 different scenarios
wardeaths_dt[ , nMx1 := deaths1 / expos1 ]
wardeaths_dt[ , nMx2 := deaths2 / expos2 ]
wardeaths_dt[ , nMx3 := deaths3 / expos3 ]
################################################################################

### Life tables for the different war scenarios #-------------------------------
# reference period
date1 = as.Date( '2023-10-01' )
date2 = as.Date( '2024-10-01')
# decimal reference date
date_ref_decimal = ( decimal_date( date1 ) + decimal_date( date2 ) ) / 2 
# date format
date_ref = date_decimal( date_ref_decimal ) %>% as.Date
# Change the format of the input data for life table estimation
lt_war_input_long =
  data.table::melt(
    wardeaths_dt,
    id.vars = c( 'sex', 'x' ),
    measure.vars = c( 'nMx1', 'nMx2', 'nMx3' ),
    value.name = 'nMx',
    variable.name = 'scenario'
  ) %>%
  setorder( scenario, sex, x  )

# change labels of scenarios nMx1, nMx2, nMx3
lt_war_input_long[ ,  scenario := fcase( 
  scenario == 'nMx1', 'Scenario 1: 34,344 casualties',
  scenario == 'nMx2', 'Scenario 2: 41,615 casualties',
  scenario == 'nMx3', 'Scenario 3: 51,615 casualties'
) ]
                       

# List sexes and scenarios for estimation
list_sex = c( 'Females', 'Males', 'Both' )
list_scenarios = c( 'Scenario 1: 34,344 casualties', 
                    'Scenario 2: 41,615 casualties', 
                    'Scenario 3: 51,615 casualties')

# Create empty dataset to store life tables
lt_gaza_war = data.table()  

# Loop through sexes
for( this_scenario in list_scenarios ){
  for( this_sex in list_sex ){
    
    # create temp datasets with filtered scenario and sex
    temp_lt_input = lt_war_input_long[ scenario == this_scenario & sex == this_sex, ] 
    
    x = temp_lt_input$x     # age vector
    nMx = temp_lt_input$nMx # nMx mortality rates
    
    lt_gaza_war =
      rbind( 
        lt_gaza_war,
        # run life table estimation for this scenario and this sex
        data.table(
          lt_label = 'War', # label
          scenario = this_scenario,
          ref_period = round( date_ref_decimal, 2 ), # reference period
          preston_life_table( x = x, nMx = nMx, Sex = this_sex ) # run life table
        )
      )
  }
  # clean
  rm( temp_lt_input, x, nMx )
}
################################################################################

### Save results #--------------------------------------------------------------
fwrite( lt_gaza_war,
        file = 'outputs/04_life_tables_gaza_war_oct2023_sep2024.csv',
        bom = TRUE,
        sep = ';' )
################################################################################