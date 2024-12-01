################################################################################
### Script: Life table functions (following Preston et al 2001, chapter 3)
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Function for estimating 1a0 from 0M1 using CD West life table #------------- 
a0_1_preston = 
  function( M0_1, Sex ){
    
    Sex = tolower( substr( Sex, 1, 1 ) )
    
    if( ! Sex %in% c( 'm', 'f', 'b' ) ){
      stop( 'Provide sex: male or female, or both' )
    } else if( Sex == 'm' ){
      if( M0_1 >= 0.107 ){
        0.330
      } else{
        0.045 + 2.684 * M0_1
      }
      
    } else if( Sex == 'f' ){
      if( M0_1 >= 0.107 ){
        0.350
      } else{
        0.053 + 2.800 * M0_1
      }
      
    } else if( Sex == 'b' ){
      if( M0_1 >= 0.107 ){
        ( 0.330 + 0.350 ) / 2
      } else{
        ( ( 0.045 + 2.684 * M0_1 ) + ( 0.053 + 2.800 * M0_1 ) ) / 2
      }
      
    } 
    
  }
################################################################################

### Function for estimating 1a4 from 0M1 using CD West life table #-------------
a1_4_preston = 
  function( M0_1, Sex ){
    
    Sex = tolower( substr( Sex, 1, 1 ) )
    
    if( ! Sex %in% c( 'm', 'f', 'b' ) ){
      stop( 'Provide sex: male or female or both' )
    } else if( Sex == 'm' ){
      if( M0_1 >= 0.107 ){
        1.352
      } else{
        1.651 - 2.816 * M0_1
      }
    } else if( Sex == 'f' ){
      if( M0_1 >= 0.107 ){
        1.361
      } else{
        1.522 - 1.518 * M0_1
      }
    } else if( Sex == 'b' ){
      if( M0_1 >= 0.107 ){
        ( 1.352 + 1.361 ) / 2
      } else{
        ( ( 1.651 - 2.816 * M0_1 ) + ( 1.522 - 1.518 * M0_1 ) ) / 2
      }
    } 
    
  }
################################################################################

### Main Life table function #--------------------------------------------------

### Function for estimating life table using Preston book's rationale
### Additional parameters q0_1 and q0_5 added for users to adjust in case 
### they want to add other estimates for these measures
preston_life_table = 
  function( x, 
            nMx = NULL, 
            nEx = NULL, 
            nDx = NULL, 
            Sex,
            q0_1 = NULL, q0_5 = NULL,
            l0_radix = 100000 ){
    
    # Number of age groups
    len = length( x )
    
    if( ! is.null( nEx ) & !is.null( nDx ) ){
      # Stop if number of age groups do not match with nEx and nDx vectors
      stopifnot( len == length( nEx ) | len == length( nDx ) )    
      # Calculate nMx
      nMx = nDx / nEx
    } else if( !is.null( nMx ) ){
      stopifnot( len == length( nMx ) ) 
    } else{
      stop( 'You must input either (nMx) or (nDx and nEx)' )
    }
    
    # Calculate width of age groups
    n = c( diff( x ), Inf )
    # Get 1M0 for estimating the 1a0 and 1a4 using the Preston book formula
    M0_1 = nMx[ x == 0 ]
    # Estimate nax, for x < 5 use formula, for open age group 1/nMx, and n/2 for others
    nax = n / 2
    nax[ 1 ]   = a0_1_preston( M0_1, Sex )
    nax[ 2 ]   = a1_4_preston( M0_1, Sex )
    nax[ len ] = 1 / nMx[ len ] 
    # Estimate nqx from nMx, n, and nax
    nqx = numeric( len )
    nqx = ( n * nMx ) / ( 1 + ( n - nax ) * nMx )
    nqx[ len ] = 1
    
    # if we have other 1q0 and 1q4 we want to use:
    if( !is.null( q0_1 ) ){
      nqx[ 1 ] = q0_1
    }
    
    if( !is.null( q0_1 ) & !is.null( q0_5 ) ){
      # 1q4 = ( 5q0 - 1q0 ) / ( 1 - 1q0 )
      nqx[ 2 ] = ( q0_5 - q0_1 ) / ( 1 - q0_1 )
    }
    # Calculate npx
    npx = 1 - nqx
    # Calculate lx starting from radix
    lx = numeric( len )
    lx[ 1 ] = l0_radix
    for( i in 2: len ){
      lx[ i ] = lx[ i - 1 ] * npx[ i - 1 ]
    }
    # Calculate ndx by applying probs of dying to lx
    ndx = lx * nqx 
    # Calculate person-years lived in each age group
    nLx = numeric( len )
    for( i in 1 : ( len - 1 ) ){
      nLx[ i ] = ndx[ i ] * nax[ i ] + lx[ i + 1 ] * n[ i ]
    }
    nLx[ len ] = ndx[ len ] * nax[ len ]
    # Calculate the cumulative person-years lived from that age
    Tx = rev( cumsum( rev( nLx ) ) )
    # Calculate life expectancy
    ex = Tx / lx 
    
    # adjust 1M0 and 4M1 if we used q0_1 or q_5
    if( !is.null( q0_1 ) | !is.null( q0_5 ) ){
      nMx[ 1:2 ] = ndx[ 1:2 ] / nLx[ 1:2 ]
    }
    
    # Prepare output life table
    preston_lt = data.frame( sex = Sex, x, n, nMx, nqx, npx, lx, ndx, nLx, Tx, ex )
    
    return( preston_lt )
  }
################################################################################
