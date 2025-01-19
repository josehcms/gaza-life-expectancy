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

# Merge exposures and casualties with Lee-Carter projection nMxs for the period Oct 1 2022 - Oct 1 2023
wardeaths_dt =
  merge(
    expos_dt[ Sex != 'Both', 
              .( sex = Sex, x = AgeStart, 
                 Scenario1, Scenario2, Scenario3, expos1, expos2, expos3 ) ],
    ltnowar[ sex != 'Both', .( sex, x, nMx ) ],
    by = c( 'sex', 'x' )
  )

# deaths by age and sex for the 3 different scenarios - no war
wardeaths_dt[ , deaths1nowar := expos1 * nMx ]
wardeaths_dt[ , deaths2nowar := expos2 * nMx ]
wardeaths_dt[ , deaths3nowar := expos3 * nMx ]

# deaths by age and sex for the 3 different scenarios - war
wardeaths_dt[ , deaths1war := deaths1nowar + Scenario1 ]
wardeaths_dt[ , deaths2war := deaths2nowar + Scenario2 ]
wardeaths_dt[ , deaths3war := deaths3nowar + Scenario3 ]

# add both sexes
wardeaths_dt =
  rbind(
    wardeaths_dt[ , .( sex, x, 
                       deaths1nowar, deaths2nowar, deaths3nowar, 
                       deaths1war, deaths2war, deaths3war,
                       expos1, expos2, expos3 ) ],
    wardeaths_dt %>% copy %>%
      .[ , sex := 'Both' ] %>%
      .[ , .( deaths1nowar = sum( deaths1nowar ),
              deaths2nowar = sum( deaths2nowar ),
              deaths3nowar = sum( deaths3nowar ),
              deaths1war = sum( deaths1war ),
              deaths2war = sum( deaths2war ),
              deaths3war = sum( deaths3war ),
              expos1 = sum( expos1 ),
              expos2 = sum( expos2 ),
              expos3 = sum( expos3 ) ),
         .( sex, x ) ]
  )


# death rates for the 3 different scenarios
wardeaths_dt[ , nMx1 := deaths1war / expos1 ]
wardeaths_dt[ , nMx2 := deaths2war / expos2 ]
wardeaths_dt[ , nMx3 := deaths3war / expos3 ]
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