################################################################################
### Script: Estimate exposures (oct 2022-Sep 2023, oct 2023-sep 2024)
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Basic setup and load functions #--------------------------------------------
# basic setup
source( 'R/00_basic_setup.R' )

# path to the main input file 
main_file = 'inputs/input_data_gaza.xlsx' 

# population interpolation functions
source( 'R/03_interpolation_funcs.R' )

# person-years lived functions
source( 'R/02_pyl_funcs.R' )

################################################################################

### Read data #-----------------------------------------------------------------
# read population data and change format to data.table for data analysis
pop_dt = 
  read.xlsx( main_file, 
             sheet = 'Population',
             detectDates = T # for detecting date format
  ) %>% 
  as.data.table

# read casualties data and change format to data.table for data analysis
casu_dt = 
  read.xlsx( main_file, 
             sheet = 'Casualties',
             detectDates = T # for detecting date format
  ) %>% 
  as.data.table

# state numbers of scenarios 2 and 3 for casualties
total_scenario2 = 41615
total_scenario3 = 51615

# get proportions of reported data so we can do a pro-rata distribution for each scenario
casu_dt[ Sex != 'Both', CasualtiesProp := Casualties / sum( Casualties ) ] 
casu_dt[ Sex == 'Both', CasualtiesProp := Casualties / sum( Casualties ) ] 

# estimate casualties by age and sex for each scenario
casu_dt[ , Scenario1 := Casualties ]
casu_dt[ , Scenario2 := CasualtiesProp * total_scenario2 ]
casu_dt[ , Scenario3 := CasualtiesProp * total_scenario3 ] 
################################################################################

### Interpolate population for Oct 1st, 2022, 2023, and 2024 #------------------
### These will be used later for calculating exposures

### Create new dataset with the interpolated population for Oct 1, 2022
# this will be used to combine male and female deaths from the Lee-Carter projection
pop_oct1_2022 =
  data.table(
    DataLabel = 'Population (interpolated) by age and sex (Oct 2022) - Gaza',
    Year      = 2022,
    AgeStart  = pop_dt[ Year == 2022, AgeStart ],
    AgeEnd    = pop_dt[ Year == 2022, AgeEnd ],
    AgeGrp    = pop_dt[ Year == 2022, AgeGrp ],
    Sex       = pop_dt[ Year == 2022, Sex ],
    TimeRef   = as.Date( '2022-10-01' ),
    # Linear interpolation between July 1st 2022 and July 1st 2023 to get October 1st 2022 pop
    Population   =  interplin( # x = decimal_date( as.Date( '2023-10-01' ) ), 
      # x1 = decimal_date( as.Date( '2023-07-01' ) ), 
      # x2 = decimal_date( as.Date( '2024-07-01' ) ),
      x = 2022.75,
      x1 = 2022.5,
      x2 = 2023.5,
      y1 = pop_dt[ Year == 2022, Population ],
      y2 = pop_dt[ Year == 2023, Population ] )
  )

### Create new dataset with the interpolated population for Oct 1, 2023
pop_oct1_2023 =
  data.table(
    DataLabel = 'Population (interpolated) by age and sex (Oct 2023) - Gaza',
    Year      = 2023,
    AgeStart  = pop_dt[ Year == 2023, AgeStart ],
    AgeEnd    = pop_dt[ Year == 2023, AgeEnd ],
    AgeGrp    = pop_dt[ Year == 2023, AgeGrp ],
    Sex       = pop_dt[ Year == 2023, Sex ],
    TimeRef   = as.Date( '2023-10-01' ),
    # Linear interpolation between July 1st 2023 and July 1st 2024 to get October 1st 2023 pop
    Population   =  interplin( # x = decimal_date( as.Date( '2023-10-01' ) ), 
      # x1 = decimal_date( as.Date( '2023-07-01' ) ), 
      # x2 = decimal_date( as.Date( '2024-07-01' ) ),
      x = 2023.75,
      x1 = 2023.5,
      x2 = 2024.5,
      y1 = pop_dt[ Year == 2023, Population ],
      y2 = pop_dt[ Year == 2024, Population ] )
  )

### Create new dataset with the interpolated population for Oct 1, 2024
pop_oct1_2024 =
  data.table(
    DataLabel = 'Population (interpolated) by age and sex (Oct 2024) - Gaza',
    Year      = 2024,
    AgeStart  = pop_dt[ Year == 2024, AgeStart ],
    AgeEnd    = pop_dt[ Year == 2024, AgeEnd ],
    AgeGrp    = pop_dt[ Year == 2024, AgeGrp ],
    Sex       = pop_dt[ Year == 2024, Sex ],
    TimeRef   = as.Date( '2024-10-01' ),
    # Linear interpolation between July 1st 2023 and July 1st 2024 to get October 1st 2023 pop
    Population   =  interplin( # x = decimal_date( as.Date( '2024-10-01' ) ),
      x = 2024.75,
      x1 = 2024.5,
      x2 = 2025.5,
      # x1 = decimal_date( as.Date( '2024-07-01' ) ), 
      # x2 = decimal_date( as.Date( '2025-07-01' ) ),
      y1 = pop_dt[ Year == 2024, Population ],
      y2 = pop_dt[ Year == 2025, Population ] )
  )

################################################################################

### Estimate population exposures #---------------------------------------------

# setup exposures dataset
# start with October 1st 2022 pop (Lee-Carter period)
expos_dt = 
  pop_oct1_2022[ , .( Sex, AgeStart, 
                      PopulationOct2022 = Population ) ]

# merge with October 1st 2023 pop
expos_dt = 
  merge(
    expos_dt,
    pop_oct1_2023[ , .( Sex, AgeStart, 
                        PopulationOct2023 = Population ) ],
    by = c( 'Sex', 'AgeStart' )
  )

# merge expos_dt dataset with October 1st 2024 pop data
expos_dt =
  merge(
    expos_dt,
    pop_oct1_2024[ , .( Sex, AgeStart,
                        PopulationOct2024 = Population ) ],
    by = c( 'Sex', 'AgeStart' )
  )

# merge with casualties
expos_dt =
  merge(
    expos_dt,
    casu_dt[ , .( Sex, AgeStart, Scenario1, Scenario2, Scenario3 ) ],
    by = c( 'Sex', 'AgeStart' )
  )

# exposures of Lee-Carter projected period
expos_dt[ , expos0 := pylinear( t1 = 2022.75,
                                t2 = 2023.75,
                                P1 = PopulationOct2022,
                                P2 = PopulationOct2023 ) ]

# Estimate person-years lived under 3 different scenarios
expos_dt[ , expos1 := pylinear( t1 = 2023.75,
                                t2 = 2024.75,
                                P1 = PopulationOct2023,
                                P2 = ( PopulationOct2024 - Scenario1 ) ) ]
expos_dt[ , expos2 := pylinear( t1 = 2023.75,
                                t2 = 2024.75,
                                P1 = PopulationOct2023,
                                P2 = ( PopulationOct2024 - Scenario2 ) ) ]
expos_dt[ , expos3 := pylinear( t1 = 2023.75,
                                t2 = 2024.75,
                                P1 = PopulationOct2023,
                                P2 = ( PopulationOct2024 - Scenario3 ) ) ]

################################################################################

### Save #----------------------------------------------------------------------

# save exposures
fwrite( expos_dt,
        file = 'outputs/01_prewar_and_war_population_exposures.csv',
        bom = TRUE,
        sep = ';' )

# long format
expos_long =
  expos_dt %>%
  data.table::melt(
    id.vars = c( 'Sex', 'AgeStart' ),
    measure.vars = paste0( 'expos', 0:3 ),
    variable.name = 'ref',
    value.name    = 'expos' 
  ) %>%
  .[ , DataLabel := fcase( ref == 'expos0', 'prewar',
                           ref == 'expos1', 'Scenario 1: 34,344 casualties',
                           ref == 'expos2', 'Scenario 2: 41,615 casualties',
                           ref == 'expos3', 'Scenario 3: 51,615 casualties' ) ] %>%
  .[ , TimeRef1 := fcase( ref == 'expos0', as.Date( '2022-10-01' ),
                          ref == 'expos1', as.Date( '2023-10-01' ),
                          ref == 'expos2', as.Date( '2023-10-01' ),
                          ref == 'expos3', as.Date( '2023-10-01' ) ) ] %>%
  .[ , TimeRef2 := fcase( ref == 'expos0', as.Date( '2023-10-01' ),
                          ref == 'expos1', as.Date( '2024-10-01' ),
                          ref == 'expos2', as.Date( '2024-10-01' ),
                          ref == 'expos3', as.Date( '2024-10-01' ) ) ] %>%
  .[ , TimeRef := TimeRef1 + ( TimeRef2 - TimeRef1 ) / 2 ] %>%
  .[ , .( DataLabel, TimeRef, TimeRef1, TimeRef2, AgeStart, Sex, expos ) ]

# combine long with pop data
pop_dt[ , TimeRef1 := as.Date( paste0( Year, '-01-01' ) ) ]
pop_dt[ , TimeRef2 := as.Date( paste0( Year + 1, '-01-01' ) ) ]

expos_long =
  rbind( 
    expos_long,
    pop_dt[ , .( DataLabel = 'midyear estimates', 
                 TimeRef, TimeRef1, TimeRef2, AgeStart, Sex, 
                 expos = Population ) ]
  )

fwrite( expos_long,
        file = 'outputs/01_prewar_and_war_population_exposures_long.csv',
        bom = TRUE )
################################################################################