################################################################################
### Script: Functions for estimating person-years lived
### Author: josehcms
### Last updated: 2024-11-30
################################################################################

### Person-years lived estimation based on linear assumption #------------------
pylinear = function( t1, t2, P1, P2 ){
  ( ( P2 + P1 ) * ( t2 - t1 ) ) / 2
}
################################################################################

### Person-years lived estimation based on cnst growth rate assumption #--------
pyconstantr = function( t1, t2, P1, P2 ){
  r = log( P2 / P1 ) / ( t2 - t1 )
  ( P2 - P1 ) / r
}
################################################################################