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



###########################################################
###### adjust factor levels (10 possible responses) #######
###########################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

adjust_levels_10 <- function( df, item.names ){

  these.cols <- which( colnames( df ) %in% c( str_extract( names( item.names ), "Q\\d$"),
              str_extract( names( item.names ), "Q\\d\\d$") ) )

  for( i in these.cols ){

    # for class character or factor variables
    if( inherits( df[ , i ], "character" ) | inherits( df[ , i ], "factor" ) ){

      if( c("0") %notin% levels( factor( df[ , i ] ) ) & c("10") %in% levels( factor( df[ , i ] ) ) ){

        df[, i ][ df[, i ] == "M" ] <- NA
        df[, i ][ df[, i ] == "E" ] <- NA
        df[, i ] <- as.character( as.numeric( df[, i ] ) - 1 )

      }

    }

    # for class numeric variables
    if( inherits( df[ , i ], "numeric" ) | inherits( df[ , i ], "double") | inherits( df[ , i ], "integer" ) ){

      if( c(0) %notin% levels( factor( df[ , i ] ) ) & c(10) %in% levels( factor( df[ , i ] ) ) ){

        df[, i ] <- as.numeric( df[, i ] )

        df[, i ] <- df[, i ] - 1

      }
    }
  }

  return( df )
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


###########################################################
###### adjust factor levels (10 possible responses) #######
###########################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

adjust_levels_4 <- function( df, item.names ){

  these.cols <- which( colnames( df ) %in% names( item.names )[ c( str_detect( names( item.names ), "A") ) ] )

  for( i in these.cols ){

    # for class character or factor variables
    if( inherits( df[ , i ], "character" ) | inherits( df[ , i ], "factor" ) ){

      if( c("0") %notin% levels( factor( df[ , i ] ) ) & c("4") %in% levels( factor( df[ , i ] ) ) ){

        df[, i ][ df[, i ] == "M" ] <- NA
        df[, i ][ df[, i ] == "E" ] <- NA
        df[, i ] <- as.character( as.numeric( df[, i ] ) - 1 )

      }

    }

    # for class numeric variables
    if( inherits( df[ , i ], "numeric" ) | inherits( df[ , i ], "double") | inherits( df[ , i ], "integer" ) ){

      if( c(0) %notin% levels( factor( df[ , i ] ) ) & c(4) %in% levels( factor( df[ , i ] ) ) ){

        df[, i ] <- as.numeric( df[, i ] )

        df[, i ] <- df[, i ] - 1

      }
    }
  }

  return( df )
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

df.copy <- fv.data.meal.c
df.copy <- adjust_levels_10( df.copy, item.names = item.names )
