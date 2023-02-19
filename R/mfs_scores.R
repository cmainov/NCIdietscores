###---------------------------------------------------------------
###   DIET QUALITY SCORES FOR MULTIFACTOR SCREENER USED IN OPEN
###---------------------------------------------------------------

#' @title Diet Quality Scores for the Multifactor Screener in OPEN
#'
#' @description Calculate % Energy from fat, fiber, and fruit and vegetable MyPyramid equivalents
#' on data collected with the National Cancer Institute's Multifactor Screener from the OPEN Study.
#'
#' @details
#' Implements the scoring procedures for data obtained from the National Cancer Institute (NCI)
#' Multifactor Screener from the OPEN Study. Computes % energy from fat,
#' fiber, and intake equivalents of fruits and vegetables. For detailed
#' description of the screener, please refer to the NCI's documentation (see below).
#' .
#'
#' @seealso
#' \href{https://epi.grants.cancer.gov/diet/screeners/OPEN.pdf}{Screener Documentation}
#' \href{https://epi.grants.cancer.gov/past-initiatives/open/multifactor/open_multifactor_datadic.pdf}{Data Dictionary}
#' \href{https://epi.grants.cancer.gov/diet/shortreg/instruments/multifactor-screener-in-open-self-report-version.pdf}{The Screener}
#'
#' @usage mfs_scores( df,
#' default.names = TRUE,
#' item.names = list( HQ1 = "HQ1", HQ2 = "HQ2",
#'                    HQ3 = "HQ3", HQ4 = "HQ4",
#'                    HQ5 = "HQ5", HQ6 = "HQ6",
#'                    HQ7 = "HQ7", HQ8 = "HQ8",
#'                    HQ9 = "HQ9", HQ10 = "HQ10",
#'                    HQ11 = "HQ11", HQ12 = "HQ12",
#'                    HQ13 = "HQ13", HQ14 = "HQ14",
#'                    HQ15 = "HQ15", HQ16 = "HQ16",
#'                    HQ2A = "HQ2A" ),
#' age.col = "AGE",
#' sex.col = "SEX" )
#'
#' @param df A \code{data.frame} or \code{tibble} containing the columns (dietary items, sex, and age) for computing the scores.
#' @param default.names A logical. Defaulted to \code{TRUE} and establishes whether default survey item names (see data dictionary above) are used. If user-specified names are used, set to \code{FALSE} and specify the column names in \code{item.names}.
#' @param item.names A named list containing the user-specified column names (character vectors) for the survey items in the \code{df}. Ignored if \code{default.names} is \code{TRUE}. Must follow format used in \code{usage}.
#' @param age.col A character vector specifying the name of the age column in the \code{df}. Defaulted to "AGE".
#' @param sex.col A character vector specifying the name of the sex column in the \code{df}. Ensure levels of this variable are coded numerically ("1" = male, "2" = female) or as "male" "female" as computation of the scores is contingent on this variable. Defaulted to "SEX".
#'
#' @return Object of class \code{data.frame} containing the original user-supplied data with the
#' dietary screener scores appended.
#'
#' @export


source( "R/utils.R" )

mfs_scores <- function( df,
                        default.names = TRUE,
                        item.names = list( HQ1 = "HQ1", HQ2 = "HQ2",
                                              HQ3 = "HQ3", HQ4 = "HQ4",
                                              HQ5 = "HQ5", HQ6 = "HQ6",
                                              HQ7 = "HQ7", HQ8 = "HQ8",
                                              HQ9 = "HQ9", HQ10 = "HQ10",
                                              HQ11 = "HQ11", HQ12 = "HQ12",
                                              HQ13 = "HQ13", HQ14 = "HQ14",
                                              HQ15 = "HQ15", HQ16 = "HQ16",
                                              HQ2A = "HQ2A" ),
                        age.col = "AGE",
                        sex.col = "SEX" ) {

  ### checks

  ## diet column names checks
  if ( !default.names & is.null( item.names) ) stop( "Error: user-specified list of column names empty when checking `default.names = T`." )
  if ( ( !default.names ) & length( item.names ) < 17 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( ( !default.names ) & length( item.names ) < 17 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( sum( c( paste0( "HQ", 1:16 ), "HQ2A" ) %notin% names( item.names ) )  > 0 ) stop( "Error: list of user-specified column names not in proper format. See default values for `item.names` in the documentation for an example." )

  ## sex column name checks
  if ( is.null( df[[sex.col]]) ) stop( "Error: input to `sex.col` not detected in the provided dataset." )
  if ( is.null( df[[age.col]]) ) stop( "Error: input to `age.col` not detected in the provided dataset." )

  # check level-coding for sex column
  sex.levs <- levels( as.factor( df[[sex.col]] ) )

  levs.12 <- sum( sex.levs %notin% c( "1", "2" ) ) > 0

  # check "male"/"female"
  levs.mf <- sum(str_detect(  sex.levs, regex( "male", ignore_case = T ) ) ) +
    sum( str_detect( sex.levs, regex( "female", ignore_case = T ) ) )

  # if there is "male" or "female" detected in the dataset, convert to numeric
  if ( levs.mf > 0 ){

    df[[sex.col]] <- ifelse( str_detect(  df[[sex.col]], regex( "male", ignore_case = T ) ), 1, df[[sex.col]] )
    df[[sex.col]] <- ifelse( str_detect(  df[[sex.col]], regex( "female", ignore_case = T ) ), 1, df[[sex.col]] )

  }

  # now, ensure it's 1's and 2'sl if not, give error
  sex.levs.new <- levels( as.factor( df[[sex.col]] ) )
  levs.12 <- sum( sex.levs.new %notin% c( "1", "2" ) ) > 0

  if( levs.12 ) stop( 'Error: ensure `sex.col` is a variable with levels coded as "male" or "female" or "1", "2".' )


  ### numeric variable classes

  ## ensure variable classes are numeric ##

  # run coerce_numeric and loop through all variables required and that matched by the two input arguments
  if( default.names ) v <- c( paste0( "HQ", 1:16 ), "HQ2A", sex.col, age.col )
  if ( !default.names ) v <- c( unlist( item.names ), sex.col, age.col )

  df <- coerce_numeric( d = df, vars = v ) # coerce to numeric



  ## Make a copy of the original dataset to append at the end
  d.copy <- df

  ## (2.1) Do unit conversions ##

  # dietary frequency column names
  if( default.names ) c.nms <- paste0( "HQ", 1:16 )

  if( !default.names ) c.nms <- unlist( item.names )[1:16] # not including milk type variable name

  # loop through columns and convert to servings
  for( i in 1:length( c.nms ) ){

    cn <- c.nms[i] # column name

    # mapping
    df[[ cn ]] <- ifelse( df[[ cn ]] == 1, 0, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 2, 0.67, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 3, 0.214, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 4, 0.5, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 5, 0.786, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 6, 1, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 7, 2, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 9, 4.5, df[[ cn ]] )

  }


  ## (2.1) First change milk variable to four separate milk variables (skim, 1%,  2%, and whole) ##

  # check to see that all subjects who responded to milk question also responded to milk type
  if( default.names ) milk.miss <- sum( is.na( df$HQ2A ) & !is.na( df$HQ2 ) )

  if( !default.names ) milk.miss <- sum( is.na( df[[ item.names[["HQ2A"]] ]] ) & !is.na( df[[ item.names[["HQ2"]] ]] ) )

  if ( milk.miss > 0 ) warning( "Warning: observations with missing milk type detected. Will set milk type to '1%' for these observations. If this is not desired, manually change the entries and re-run." )


  # expand the milk type columns

  if( default.names ) { # if we use the default column names

    df <- df %>% # for those that have a milk entry that is not missing, they will get "0" for all other milk types not the ones that they consume
      mutate( HQ2A = ifelse( is.na( HQ2A ) & !is.na( HQ2 ), 3, HQ2A ), # set those w/ missing milk type to 1%--see warning message above

              # now create the milk-specific columns (4 columns total)
              milk.skim = ifelse( HQ2A == 5, HQ2, 1 ), # if a subject respond in the affirmative for a given milk type, they get their value of `HQ2` in the milk type column, otherwise they get a "1" for "never"
              milk.skim = ifelse( HQ2A == 4, HQ2, 1 ),
              milk.one = ifelse( HQ2A == 3, HQ2, 1  ),
              milk.two = ifelse( HQ2A == 2, HQ2, 1  ),
              milk.whole = ifelse( HQ2A == 1, HQ2, 1  ) )
  }

  if( !default.names ) { # if we don't use the default column names

    milk.var <- item.names[["HQ2A"]]
    milk.type.used <- item.names[["HQ2"]]

    df <- df %>% # for those that have a milk entry that is not missing, they will get "0" for all other milk types not the ones that they consume
      mutate( HQ2A = ifelse( is.na( HQ2A ) & !is.na( HQ2 ), 3, HQ2A ), # set those w/ missing milk type to 1%--see warning message above

              # now create the milk-specific columns (4 columns total)
              milk.skim = ifelse( milk.var == 5, milk.var, 1 ), # if a subject respond in the affirmative for a given milk type, they get their value of `HQ2` in the milk type column, otherwise they get a "1" for "never"
              milk.skim = ifelse( milk.var == 4, milk.var, 1 ),
              milk.one = ifelse( milk.var == 3, milk.var, 1  ),
              milk.two = ifelse( milk.var == 2, milk.var, 1  ),
              milk.whole = ifelse( milk.var == 1, milk.var, 1  ) )
  }

  ## --------- End Subsection --------- ##


  ## (1.3) Rename diet variables to user-friendly names ##

  # these names also match the names on the sex/age conversion tables #

  if( default.names ) {

    df <- df %>%
      rename( cold.cereals = HQ1,
              bacon.sausage = HQ3,
              hot.dogs = HQ4,
              bread = HQ5,
              juice = HQ6,
              fruit = HQ7,
              regular.fat = HQ8,
              salad = HQ9,
              potatoes = HQ10,
              white.potatoes = HQ11,
              beans = HQ12,
              vegetables = HQ13,
              pasta = HQ14,
              nuts = HQ15,
              chips = HQ16 )

  }

  if( !default.names ) {

    df <- df %>%
      rename( cold.cereals = item.names[["HQ1"]],
              bacon.sausage = item.names[["HQ3"]],
              hot.dogs = item.names[["HQ4"]],
              bread = item.names[["HQ5"]],
              juice = item.names[["HQ6"]],
              fruit = item.names[["HQ7"]],
              regular.fat = item.names[["HQ8"]],
              salad = item.names[["HQ9"]],
              potatoes = item.names[["HQ10"]],
              white.potatoes = item.names[["HQ11"]],
              beans = item.names[["HQ12"]],
              vegetables = item.names[["HQ13"]],
              pasta = item.names[["HQ14"]],
              nuts = item.names[["HQ15"]],
              chips = item.names[["HQ16"]] )

  }




  ## (4.2) Adjust fruit/veg frequency of food intake by gender/age specific factors ##

  # this generates results for 1/2 cup pyramid serving units (predfv7 predfv6) #

  # we will use the 4th HTML table in `sysdata.rda` (i.e., tbl.4)

  for( i in 1:nrow( df ) ){  # loop on subject

    for( j in 2:length( age.lst) ){ # loop on age which determines which columns of reference table to use

      ## inner loops will be determined based on which rows to use from the table

      # males inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 1 ){

        for( g in 3:9){

          df[ i, paste0( tbl.4$`Food Group`[g],".m" ) ] <-
            df[ i, which(colnames( df ) == tbl.4$`Food Group`[g]) ]*as.numeric( tbl.4[g,j] )

        }

      }

      # females inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 2 ){

        for( g in 11:17){

          df[ i, paste0( tbl.4$`Food Group`[g],".m" ) ] <-
            df[ i, which(colnames( df ) == tbl.4$`Food Group`[g]) ]*as.numeric( tbl.4[g,j] )

        }
      }
    }
  }

  # end loop

  ## --------- End Subsection --------- ##


  ## (4.3) Compute 1/2 cup pyramid serving units variables ##

  df <- df %>%
    mutate( fv7 = fruit.m + vegetables.m + juice.m + potatoes.m + white.potatoes.m + salad.m + beans.m,
            fv6 = fruit.m + vegetables.m + juice.m + white.potatoes.m + salad.m + beans.m, # remove fried potatoes
            sqfv7 = sqrt( fv7 ),
            sqfv6 = sqrt( fv6 ),
            ## create predicted outcomes ##
            predfv7ps = ifelse( SEX == 1, 0.90679 + 0.75856*sqfv7,
                                ifelse( SEX == 2, 0.81956 + 0.73086*sqfv7, NA ) ),
            predfv6ps = ifelse( SEX == 1, 0.94077 + 0.73906*sqfv6,
                                ifelse( SEX == 2, 0.81626 + 0.73022*sqfv6, NA ) ) )

  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ### (5.0) Compute `pred.fiber` and `pred.pcf` (Age and Sex-Specific) ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (5.1) Age-sex adjustment

  # it will use tables 2 (for males) and 3 (for females) to make the conversions from `df.list`

  for( i in 1:nrow( df ) ){  # loop on subject

    for( j in 2:length( age.lst) ){ # loop on age which determines which columns of reference table to use

      ## inner loops will be determined based on which rows to use from the table

      # males inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 1 ){

        for( g in 3:20){ # loop on all food items this time
          df[ i, paste0( tbl.2$`Food Group`[g],"_a" ) ] <-
            df[ i, which(colnames( df ) == tbl.2$`Food Group`[g]) ]*as.numeric( tbl.2[g,j] )

        }

      }

      # females inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 2 ){

        for( g in 3:20){

          df[ i, paste0( tbl.3$`Food Group`[g],"_a" ) ] <-
            df[ i, which(colnames( df ) == tbl.3$`Food Group`[g]) ]*as.numeric( tbl.3[g,j] )

        }
      }
    }
  }

  ## --------- End Subsection --------- ##


  ## (5.2) Use table  6 from `df.list` to create predicted fiber intake and % from fat ##

  df <- df %>% # copy data before looping and alternating

    # initialize variables with intercept values
    mutate( pred.fiber = ifelse( SEX == 1, as.numeric( tbl.6[2,3] ),
                                 ifelse( SEX == 2, as.numeric( tbl.6[2,5] ), NA ) ),
            pred.pcf = ifelse( SEX == 1, as.numeric( tbl.6[2,2] ),
                               ifelse( SEX == 2, as.numeric( tbl.6[2,4] ), NA ) ) )

  for( i in 1:nrow( df ) ){  # loop on subject



    for( g in 3:20){ # loop on all food items this time

      ## males
      if( df[ i, sex.col ] == 1 ){

        # predicted fiber
        df[ i, "pred.fiber" ] <-
          df[ i, which(colnames( df ) == paste0( tbl.6$`Parameter`[g], "_a" ) ) ]*
          as.numeric( tbl.6[g,3] ) + df[ i, "pred.fiber" ]

        # predicted % from fat
        df[ i, "pred.pcf" ] <-
          df[ i, which(colnames( df ) == paste0( tbl.6$`Parameter`[g], "_a" ) ) ]*
          as.numeric( tbl.6[g,2] ) + df[ i, "pred.pcf" ]
      }

      ## females
      if( df[ i, sex.col ] == 2 ){

        # predicted fiber
        df[ i, "pred.fiber" ] <-
          df[ i, which(colnames( df ) == paste0( tbl.6$`Parameter`[g], "_a" ) ) ]*
          as.numeric( tbl.6[g,5] ) + df[ i, "pred.fiber" ]

        # predicted % from fat
        df[ i, "pred.pcf" ] <-
          df[ i, which(colnames( df ) == paste0( tbl.6$`Parameter`[g], "_a" ) ) ]*
          as.numeric( tbl.6[g,4] ) + df[ i, "pred.pcf" ]
      }
    }
  }


  ### (6.0) Adjustment of Food Items by Age/Gender Factors ###
  ## Creates cup-equivalent pyramid serving units (`raw.pred.fv7.ce`, `raw.pred.fv6.ce`, `pred.fv7.ce`, `pred.fv6.ce`) ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (6.1) Create age category variable
  for ( i in 1:nrow( df ) ){

    for ( j in 2:length( age.lst ) ){

      if( df[i, age.col] %in% age.lst[[j]] ){

        df[i, "age.cat"] <- j-1

      }

    }
  }

  ## --------- End Subsection --------- ##


  ## (6.2) F/V cup equivalent adjustments, Merge and create Variables ##

  # merge portion size adjustment variables to working dataset and multiply to
  # estimate cup equivalents of F & V (`raw.pred.fv7.ce`, `raw.pred.fv6.ce`) and
  # adjust the estimates using regression coefficients from CSFII 94-96 (stored in `fvcupadj`)
  # (create: `pred.fv7.ce`, `pred.fv6.ce`)

  # this adjustment uses table 8, `fvcupadj` (i.e., `tbl.8`)

  # specify join columns for age and gender
  sex.join.cols <- "gender"
  names( sex.join.cols ) <- sex.col # approach when using a named object inside the `by` argument of `left_join`

  df <- df %>%
    left_join( ., fvcupadj, by = c( sex.join.cols, "age.cat" = "AgeGrp" ) ) %>%

    mutate( raw.pred.fv7.ce = juice*FVCAFrtJ + fruit*FVCAFruit + potatoes*FVCAFrFry
            + white.potatoes*FVCAOthPot + beans*FVCADrBean + salad*FVCASalad
            + vegetables*FVCAOthVeg,

            raw.pred.fv6.ce = juice*FVCAFrtJ + fruit*FVCAFruit
            + white.potatoes*FVCAOthPot + beans*FVCADrBean + salad*FVCASalad
            + vegetables*FVCAOthVeg, # excludes french fries

            # adjust using regression coefficients from CSFII 94-96

            pred.fv7.ce = ifelse( SEX == 1,
                                  ( 0.666228 + 0.770652*( sqrt( raw.pred.fv7.ce ) ) )^2,
                                  ifelse( SEX == 2,
                                          ( 0.611844 + 0.733890*( sqrt(raw.pred.fv7.ce) ) )^2, NA ) ),
            pred.fv6.ce = ifelse( SEX == 1,
                                  ( 0.706696 + 0.742255*( sqrt( raw.pred.fv6.ce ) ) )^2,
                                  ifelse( SEX == 2,
                                          ( 0.616033 + 0.727761*( sqrt(raw.pred.fv6.ce ) ) )^2, NA ) ) )



  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (7.0) Return Final Dataset  ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (7.1) Columns to return in final dataset ##
  d.out <- cbind( d.copy,
                      df[ ,c( "pred.fiber", "pred.pcf",
                              "pred.fv7.ce", "pred.fv6.ce",
                              "raw.pred.fv7.ce", "raw.pred.fv6.ce",
                              "predfv7ps", "predfv6ps" ) ] ) %>% data.frame()

  ## --------- End Subsection --------- ##


  # print summary stats for the appended columns
  print( summary( df[ ,c( "pred.fiber", "pred.pcf",
                          "pred.fv7.ce", "pred.fv6.ce",
                          "raw.pred.fv7.ce", "raw.pred.fv6.ce",
                          "predfv7ps", "predfv6ps" ) ] ) )
  return( d.out )

}

mfs_scores(diet.data, default.names = F )
