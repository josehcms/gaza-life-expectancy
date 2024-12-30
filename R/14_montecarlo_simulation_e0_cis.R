################################################################################
### Script: Estimate Monte Carlo 95% CIs for life expectancy
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Basic setup #---------------------------------------------------------------
# basic setup
source( 'R/00_basic_setup.R' )
# life table functions
source( 'R/01_lifetable_funcs.R' )
# path to the main input file 
main_file = 'inputs/input_data_gaza.xlsx' 
################################################################################

### Read and prepare data for MC simulation #-----------------------------------

# read life tables
ltgaza_2017_2022 = fread( 'outputs/02_life_tables_gaza_2017_2022.csv' ) 
ltgaza_prewar    = fread( 'outputs/03_life_tables_gaza_projected_oct2022_sep2023.csv' )
ltgaza_war       = fread( 'outputs/04_life_tables_gaza_war_oct2023_sep2024.csv' )

# Pop Exposures
expos_dt = fread( 'outputs/01_prewar_and_war_population_exposures_long.csv' )

# 2017-2022
mc_input_2017_2022 = 
  ltgaza_2017_2022[ , .( ref_period, sex, x, nMx ) ] %>%
  merge(
    expos_dt[ DataLabel == 'midyear estimates' ] %>%
      .[ , ref_period := year( TimeRef ) + 0.5 ] %>%
      .[ , .( ref_period, x = AgeStart, sex = Sex, nEx = expos ) ],
    by = c( 'ref_period', 'sex', 'x' )
  ) %>%
  .[ , label := 'VR Adjusted' ] %>%
  .[ , .( label, ref_period, sex, x, nMx, nEx, nDx = nMx * nEx ) ]

# pre-war (Lee-Carter projection)
mc_input_prewar = 
  ltgaza_prewar[ , .( ref_period, sex, x, nMx ) ] %>%
  merge(
    expos_dt[ DataLabel == 'prewar' ] %>%
      .[ , ref_period := year( TimeRef ) + 0.25 ] %>%
      .[ , .( ref_period, x = AgeStart, sex = Sex, nEx = expos ) ],
    by = c( 'ref_period', 'sex', 'x' )
  ) %>%
  .[ , label := 'Lee-Carter Projected' ] %>%
  .[ , .( label, ref_period, sex, x, nMx, nEx, nDx = nMx * nEx ) ]

# war period
mc_input_war = 
  ltgaza_war[ , .( ref_period, scenario, sex, x, nMx ) ] %>%
  merge(
    expos_dt[ ! DataLabel %in% c( 'prewar', 'midyear estimates' ) ] %>%
      .[ , scenario := DataLabel ] %>%
      .[ , .( scenario, x = AgeStart, sex = Sex, nEx = expos ) ],
    by = c( 'scenario', 'sex', 'x' )
  ) %>%
  .[ , label := scenario ] %>%
  .[ , .( label, ref_period, sex, x, nMx, nEx, nDx = nMx * nEx ) ]

# merge the three
mc_input = rbind( mc_input_2017_2022, mc_input_prewar, mc_input_war )
################################################################################

### Run Monte Carlo simulations #-----------------------------------------------

set.seed( 13 )

# create id for the estimates for loop
mc_input[ , id := paste0( label, '-', ref_period, '-', sex ) ]

# loop through ids and run 10,000 MC simulations
niter = 10000
e0_mcsim = 
  do.call( rbind,
           lapply( unique( mc_input$id ), 
                   function( i ){
                     # filter data and get essential info
                     temp = mc_input[ id == i ]
                     lt_label = temp$label %>% unique
                     ref_period = temp$ref_period %>% unique
                     sex = temp$sex %>% unique
                     x  = temp$x
                     Dx = temp$nDx
                     Ex = temp$nEx
                     m = length( x )
                     
                     # prepare simulation matrix
                     mxsim_matrix = matrix( NA, nrow = niter, ncol = m )
                     colnames( mxsim_matrix ) = x
                     
                     # for each age group j draw 10000 Poisson realizations with lamba = nDx
                     # nEx is nonrandom
                     for( j in 1:m ){
                       mxsim_matrix[ ,j ] = rpois( niter, lambda = Dx[ j ] ) / Ex[ j ]
                     }
                     
                     # simulate life tables and save e0s for each of the 10000 schedule
                     e0_sim = sapply( 1:niter, 
                                      function( k ){
                                        mx = mxsim_matrix[ k, ]
                                        e0 = preston_life_table( x = x, nMx = mx, Sex = sex )[ 1, 'ex' ]
                                        return( e0 )
                                      } )
                     
                     # output data
                     e0_mcsim =
                       data.table(
                         lt_label   = lt_label,
                         ref_period = ref_period,
                         sex        = sex,
                         e0_low     = quantile( e0_sim, probs = 0.025 ), 
                         e0_med     = median( e0_sim ),
                         e0_high    = quantile( e0_sim, probs = 0.975 ),
                         e0_mean    = mean( e0_sim )
                       )
                     
                     return( e0_mcsim )
                       
                   } ) )

################################################################################

### Merge Monte Carlo results with point estimates and save #-------------------

# point estimates of e0
e0point =
  rbind(
    ltgaza_2017_2022[ x == 0, .( lt_label = 'VR Adjusted', ref_period, sex, e0 = ex ) ],
    ltgaza_prewar[ x == 0, .( lt_label = 'Lee-Carter Projected', ref_period, sex, e0 = ex ) ],
    ltgaza_war[ x == 0, .( lt_label = scenario, ref_period, sex, e0 = ex ) ]
  )

# final dataset with e0 results
e0_mc_final =
  merge(
    e0_mcsim,
    e0point,
    by = c( 'lt_label', 'ref_period', 'sex' )
  ) %>%
  setorder( sex, ref_period, lt_label )

# save
fwrite( e0_mc_final,
        file = 'outputs/05_e0_estimates_cis_montecarlosim.csv',
        bom = TRUE,
        sep = ';' )
################################################################################
