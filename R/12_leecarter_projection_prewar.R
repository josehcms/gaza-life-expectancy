################################################################################
### Script: Lee-Carter Projection for pre-war period
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Basic setup and load functions #--------------------------------------------

# basic setup
source( 'R/00_basic_setup.R' )

# life table functions
source( 'R/01_lifetable_funcs.R' )

# Lee-Carter function
source( 'R/04_leecarter_funcs.R' )

# population interpolation and person-years lived functions
source( 'R/03_interpolation_funcs.R' )
source( 'R/02_pyl_funcs.R' )
################################################################################

### Lee-Carter projection #-----------------------------------------------------

# read 2017-2022 data
lt_adjusted = fread( 'outputs/02_life_tables_gaza_2017_2022.csv' )

# list sexes for loop
list_sex = c( 'Males', 'Females' )

# projection time, to be added to the last estimate (here: 2022.5)
# we want 2023.25 = 2022.5 + 0.75
projection_times = c( 0.75 ) 

# prepare empty dataset to store the life tables from projections
lt_lcproj = data.table()

# Loop for each sex
for( this_sex in list_sex ){
  # prepare input data for LeeCarter funtion
  # 3 columns: 
  ## t for reference period, 
  ## x for age, 
  ## mx for mortality rates for that time and age
  input_lc_data = 
    lt_adjusted[ sex == this_sex, 
                 .( t = ref_period, x = x, mx = nMx ) ]
  
  # Project Lee-Carter
  leecarter = lee_carter_proj( input_lc_data, projection_times )
  # change data format to data.table
  setDT(leecarter)
  
  # Now, we run life table for projected Lee-Carter nMxs
  
  n_projections = length( projection_times ) # count number of projection periods
  # Loop for each period (here we have only one)
  for( i in 1 : n_projections ){
    
    # temp data for that specific projection period
    temp_lc = leecarter[ t == projection_times[ i ], ]
    
    # save Lee-Carter projection life table estimated with projected nMx and 
    # the Preston et al procedure
    
    lt_lcproj =
      rbind(
        lt_lcproj,
        data.frame(
          lt_label = 'Lee-Carter Projected',
          ref_period = 2022.5 + projection_times[ i ],
          preston_life_table( x = temp_lc$x, 
                              nMx = temp_lc$nMx, 
                              Sex = this_sex ) 
        )
      )
  }
  # clean
  rm( input_lc_data, leecarter, n_projections, temp_lc )
}

################################################################################

### Combine sexes for obtaining both sexes projections #------------------------

### read population exposures
expos_dt = fread( 'outputs/01_prewar_and_war_population_exposures.csv' )
  
# use pre-war exposures and Lee-Carter nMx by sex to get nMx 
# for both sexes for the projected period
mx_lcproj_both =
  merge(
    lt_lcproj[ ref_period == 2023.25, .( sex, x, nMx ) ],
    expos_dt[ , .( sex = Sex, x = AgeStart, expos0 ) ],
    by = c( 'sex', 'x' )
  ) %>%
  .[ , nDx := nMx * expos0 ] %>%
  .[ , sex := 'Both' ] %>%
  .[ , .( nMx = sum( nDx ) / sum( expos0 ) ), .( sex, x ) ]

mx_lcproj = 
  rbind(
    lt_lcproj[ ref_period == 2023.25, .( sex, x, nMx ) ],
    mx_lcproj_both
  ) %>%
  setorder( sex, x )

# combine with life tables by sex from Lee-Carter
lt_lcproj_prewar =
  rbind(
    lt_lcproj[ ref_period == 2023.25 ],
    data.table( 
      lt_label = 'Lee-Carter Projected',
      ref_period = 2023.25, 
      preston_life_table( x = mx_lcproj_both$x, 
                          nMx = mx_lcproj_both$nMx,
                          Sex = 'Both' ) )
  ) %>%
  setorder( ref_period, sex, x )

################################################################################

### Save #----------------------------------------------------------------------

# save only the projected for Oct 1st 2022 - Sep 31st 2023
fwrite( lt_lcproj_prewar,
        file = 'outputs/03_life_tables_gaza_projected_oct2022_sep2023.csv',
        bom = TRUE )

################################################################################
