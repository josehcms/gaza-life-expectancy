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
expos_long = fread( 'outputs/01_prewar_and_war_population_exposures_long.csv' )

# prewar mortality = no war scenario
ltnowar = fread( 'outputs/03_life_tables_gaza_projected_oct2022_sep2023.csv' )

# war
ltwar = fread( 'outputs/04_life_tables_gaza_war_oct2023_sep2024.csv' )

################################################################################

mx_nowar = 
  ltnowar[ , .( sex, x, nMx ) ] %>%
  merge( expos_long[ DataLabel == 'prewar', .( x = AgeStart, sex = Sex, expos ) ],
         by = c( 'sex', 'x' ) )

mx_war = 
  ltwar[ , .( scenario, sex, x, nMx ) ] %>%
  merge( expos_long[ ! DataLabel %in% c( 'prewar', 'midyear estimates' ),
                     .( scenario = DataLabel, x = AgeStart, sex = Sex, expos ) ],
         by = c( 'scenario', 'sex', 'x' ) ) %>%
  .[ scenario == 'Scenario 2: 41,615 casualties' ]


# create dataset for saving e0s CIs
mx_mcsim = data.table()
for( this_sex in c( 'Males', 'Females', 'Both') ){
  temp1 = mx_nowar[ sex == this_sex ] %>% setorder( sex, x )
  temp2 = mx_war[ sex == this_sex ] %>% setorder( sex, x )
  
  sex = temp1$sex %>% unique
  x  = temp1$x
  Dx1 = temp1$nMx * temp1$expos
  Ex1 = temp1$expos
  Dx2 = temp2$nMx * temp2$expos
  Ex2 = temp2$expos
  
  m = length( x )
  
  mxsim_matrix1 = matrix( NA, nrow = 10000, ncol = m )
  mxsim_matrix2 = matrix( NA, nrow = 10000, ncol = m )
  colnames( mxsim_matrix1 ) = x
  colnames( mxsim_matrix2 ) = x
  for( j in 1:m ){
    mxsim_matrix1[ ,j ] = rpois( 10000, lambda = Dx1[ j ] ) / Ex1[ j ]
    mxsim_matrix2[ ,j ] = rpois( 10000, lambda = Dx2[ j ] ) / Ex2[ j ]
  }
  
  mxsim_ratios = mxsim_matrix2 / mxsim_matrix1
  mxsim_diffs   = mxsim_matrix2 - mxsim_matrix1
  
  mx_mcsim =
    rbind(
      mx_mcsim,
      data.table(
        sex        = sex,
        x = x,
        mx_rat    = temp2$nMx / temp1$nMx,
        mxrat_low = apply( mxsim_ratios, 2, function( x ) quantile( x, probs = 0.025 ) ),
        mxrat_med = apply( mxsim_ratios, 2, function( x ) quantile( x, probs = 0.50 ) ),
        mxrat_up  = apply( mxsim_ratios, 2, function( x ) quantile( x, probs = 0.975 ) ),
        mx_diff    = temp2$nMx - temp1$nMx,
        mxdiff_low = apply( mxsim_diffs, 2, function( x ) quantile( x, probs = 0.025 ) ),
        mxdiff_med = apply( mxsim_diffs, 2, function( x ) quantile( x, probs = 0.50 ) ),
        mxdiff_up  = apply( mxsim_diffs, 2, function( x ) quantile( x, probs = 0.975 ) )
      )
    )
  
}

mx_mcsim 

ggplot( mx_mcsim[ sex == 'Both' ] ) +
  geom_point( aes( x = x, y = mx_diff ) ) +
  geom_line( aes( x = x, y = mx_diff ) ) +
  geom_ribbon( aes( x = x, xmin = x, xmax = x,
                    y = mxdiff_med, ymin = mxdiff_up, ymax = mxdiff_low ),
               alpha = 0.5 ) +
  scale_y_continuous( name = 'Absolute difference in nMx',
                      breaks = seq( 0, 0.1, 0.01 ) ) +
  scale_x_continuous( name = 'Age',
                      breaks = seq( 0, 80, 5 ) ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.12,
                                      linetype = 'dotted', 
                                      color = 'gray30'),
    panel.grid.major.y = element_line( linewidth = 0.12,
                                       linetype = 'dotted', 
                                       color = 'gray30'),
    axis.text.y = element_text( size = 13, color = 'black' ),
    axis.text.x = element_text( size = 11, color = 'black' ),
    axis.title = element_text( size = 15, color = 'black' ),
    legend.text = element_text( size = 10 ),
    legend.position = 'bottom',
    legend.title = element_blank()
  )


ggsave( '../../paper-outputs/figures/sm_fig1_abschange_both.svg', width = 8, height = 5 )

ggplot( mx_mcsim[ sex != 'Both' ] ) +
  geom_point( aes( x = x, y = mx_diff, color = sex, shape = sex ) ) +
  geom_line( aes( x = x, y = mx_diff, color = sex ) ) +
  geom_ribbon( aes( x = x, xmin = x, xmax = x,
                    y = mxdiff_med, ymin = mxdiff_up, ymax = mxdiff_low, 
                    fill = sex ),
               alpha = 0.5 ) +
  scale_y_continuous( name = 'Absolute difference in nMx' ) +
  scale_x_continuous( name = 'Age',
                      breaks = seq( 0, 80, 5 ) ) +
  scale_shape_manual( name = '', 
                      values = c( 15, 19 ) ) +
  scale_color_manual( name = '',
                      values = c( 'Males' = 'navy', 'Females' = 'tomato3' ) ) +
  scale_fill_manual( name = '',
                     values = c( 'Males' = 'navy', 'Females' = 'tomato3' ) ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.12,
                                      linetype = 'dotted', 
                                      color = 'gray30'),
    panel.grid.major.y = element_line( linewidth = 0.12,
                                       linetype = 'dotted', 
                                       color = 'gray30'),
    axis.text.y = element_text( size = 13, color = 'black' ),
    axis.text.x = element_text( size = 11, color = 'black' ),
    axis.title = element_text( size = 15, color = 'black' ),
    legend.text = element_text( size = 12 ),
    legend.key.size = unit( 0.5, 'in' ),
    legend.position = 'inside',
    legend.position.inside = c( 0.2, 0.85 ),
    legend.background = element_rect( fill = 'transparent', color = NA ),
    legend.direction = 'horizontal',
    legend.title = element_blank()
  )

ggsave( '../../paper-outputs/figures/sm_fig2_abschange_bysex.svg', width = 8, height = 5 )

# Relative change in nMx in Gaza by age (Year 1 / Year 0), for both sexes combined

ggplot( mx_mcsim[ sex == 'Both' ] ) +
  geom_point( aes( x = x, y = mx_rat ) ) +
  geom_line( aes( x = x, y = mx_rat ) ) +
  geom_ribbon( aes( x = x, xmin = x, xmax = x,
                    y = mxrat_med, ymin = mxrat_up, ymax = mxrat_low ),
               alpha = 0.5 ) +
  scale_y_continuous( name = 'Relative change in nMx',
                      breaks = seq( 0, 150, 10 ),
                      limits = c( 0, 70 ) ) +
  scale_x_continuous( name = 'Age',
                      breaks = seq( 0, 80, 5 ) ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.12,
                                      linetype = 'dotted', 
                                      color = 'gray30'),
    panel.grid.major.y = element_line( linewidth = 0.12,
                                       linetype = 'dotted', 
                                       color = 'gray30'),
    axis.text.y = element_text( size = 13, color = 'black' ),
    axis.text.x = element_text( size = 11, color = 'black' ),
    axis.title = element_text( size = 15, color = 'black' ),
    legend.text = element_text( size = 12 ),
    legend.key.size = unit( 0.5, 'in' ),
    legend.position = 'inside',
    legend.position.inside = c( 0.8, 0.85 ),
    legend.background = element_rect( fill = 'transparent', color = NA ),
    legend.direction = 'horizontal',
    legend.title = element_blank()
  )

ggsave( '../../paper-outputs/figures/sm_fig3_relchange_both.svg', width = 8, height = 5 )


# Relative change in nMx in Gaza by age (Year 1 / Year 0), by sex on the same figure

ggplot( mx_mcsim[ sex != 'Both' ] ) +
  geom_point( aes( x = x, y = mx_rat, color = sex, shape = sex ) ) +
  geom_line( aes( x = x, y = mx_rat, color = sex ) ) +
  geom_ribbon( aes( x = x, xmin = x, xmax = x,
                    y = mxrat_med, ymin = mxrat_up, ymax = mxrat_low, 
                    fill = sex ),
               alpha = 0.5 ) +
  scale_y_continuous( name = 'Relative change in nMx',
                      breaks = seq( 0, 1000, 10 ),
                      limits = c( 0, 120 ) ) +
  scale_x_continuous( name = 'Age',
                      breaks = seq( 0, 80, 5 ) ) +
  scale_shape_manual( name = '', 
                      values = c( 15, 19 ) ) +
  scale_color_manual( name = '',
                      values = c( 'Males' = 'navy', 'Females' = 'tomato3' ) ) +
  scale_fill_manual( name = '',
                      values = c( 'Males' = 'navy', 'Females' = 'tomato3' ) ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.12,
                                      linetype = 'dotted', 
                                      color = 'gray30'),
    panel.grid.major.y = element_line( linewidth = 0.12,
                                       linetype = 'dotted', 
                                       color = 'gray30'),
    axis.text.y = element_text( size = 13, color = 'black' ),
    axis.text.x = element_text( size = 11, color = 'black' ),
    axis.title = element_text( size = 15, color = 'black' ),
    legend.text = element_text( size = 12 ),
    legend.key.size = unit( 0.5, 'in' ),
    legend.position = 'inside',
    legend.position.inside = c( 0.8, 0.85 ),
    legend.background = element_rect( fill = 'transparent', color = NA ),
    legend.direction = 'horizontal',
    legend.title = element_blank()
  )

ggsave( '../../paper-outputs/figures/sm_fig4_relchange_bysex.svg', width = 8, height = 5 )
