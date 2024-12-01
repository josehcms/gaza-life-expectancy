################################################################################
### Script: Lee-Carter Projection function (with drift correction)
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Lee-Carter Projection function #--------------------------------------------
lee_carter_proj = 
  function( 
    # mortality rates by age and reference time 
    mxt_df, # data frame with 3 columns: t (time), x (age), and mx (mortality rates)
    projection_times # vector of lengths of time to be added to the last observed data period 
  ){
    
    # sort data
    mxt_df = mxt_df[ order( mxt_df$t, mxt_df$x ) ]
    
    # get x
    x = sort( unique( mxt_df$x ) )
    
    mx_wide =
      reshape( data = mxt_df[ , c( 't', 'x', 'mx' ) ],
               idvar = 't',
               timevar = 'x',
               v.names = 'mx',
               direction = 'wide' ) 
    
    # get last projected year
    t = mx_wide$t
    last_t = max( t )
    # remove first column (year) and save matrix time vs ages with mx values
    mxt = as.matrix( mx_wide[ , -1 ] )
    
    # take the log
    logmxt = log( mxt )
    
    # Lee-Carter model: logmx = ax + bx * kt
    
    # ax: logmx of last year 2022
    ax = logmxt[ nrow( logmxt ), ]
    for( i in 1: length( ax ) ){
      logmxt[ , i ] = logmxt[ , i ] - ax[ i ]
    }
    
    # Singular Value Decomposition
    lc_svd = svd( logmxt )
    
    # bx: normalized vector of first principal component of age
    bx = lc_svd$v[ , 1 ] / sum( lc_svd$v[ , 1 ] )
    
    # kt:
    kt = lc_svd$u[ , 1 ] * sum( lc_svd$v[ , 1 ] ) * lc_svd$d[ 1 ]

    tnorm = t - last_t # normalize years
    model_kt = summary( lm( kt ~ 0 + tnorm ) )
    kt_drift = model_kt$coefficients[ 1, 1 ]
    #sec = model_kt$coefficients[ 1, 2 ]
    #see = model_kt$sigma
    tproj = c( projection_times )
    kt_proj = tproj * kt_drift
    
    # project mortality rates using LC model with projected kts
    mx_proj = data.frame()
    for( j in 1 : length( kt_proj ) ){
      mx_proj =
        rbind(
          mx_proj,
          data.frame(
            ref_period = last_t + tproj[ j ],
            t = tproj[ j ],
            x = x,
            nMx = exp( ax + bx * kt_proj[ j ] )
          )
        )
    }
    
    row.names( mx_proj ) = NULL 
    
    return( mx_proj )
  }

################################################################################