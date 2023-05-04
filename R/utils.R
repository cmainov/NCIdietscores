#####################################
########## %notin% operator #########
#####################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
`%notin%` <- Negate( `%in%` )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

#####################################
###### list to rbind function #######
#####################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
list_it <- function( list ) {
  do.call( "rbind" , list )
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

###---------------------------------------------------
###   ADD PIPE OPERATOR TO THE PACKAGE
###---------------------------------------------------
#' @importFrom magrittr "%>%"


###################################################
###### coerce variables to numeric function #######
###################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

coerce_numeric <- function( d, vars) {

  these <- which( colnames( d ) %in% vars )

  n <- sum( sapply( d[ these ], function( x ) is.numeric( x)  ) )

  # first check that variables contain only digits, stop if not

  if ( sum( sapply( d[ vars ], function(x) sum( stringr::str_detect( x, '^([A-Za-z\\s]*)$' ), na.rm = T ) ), na.rm = T ) != 0 )
  { stop( "Non-digit characters detected in at least one of the columns specified. Ensure all variables contain only digits before executing the function." ) }

  # coerce
  if (  n < length( these ) ){
    warning( paste0( "At least one of the provided numerator variables was not of object class numeric. ", length( these )-n, " variables were (was)  coerced to numeric." ) )

    d[ vars ] <- data.frame( sapply( d[ vars ], function( x ) as.numeric( as.character ( x)  ) ) )


  }

  return( d )
}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


