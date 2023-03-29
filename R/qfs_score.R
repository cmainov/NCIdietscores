###---------------------------------------------------------------
###  PERCENT ENERGY FROM FAT FROM THE QUICK FOOD SCAN
###---------------------------------------------------------------

#' @title Percent Energy from fat from the NCI's Quick Food Scan
#'
#' @description Calculate age & sex-adjusted % Energy from fat,
#' on data collected with the National Cancer Institute's Quick Food Scan screener.
#'
#' @details
#' Implements scoring procedures for data obtained from the National Cancer Institute (NCI)
#' Quick Food Scan screener. Computes % energy from fat.
#'
#' Citation:
#' The Quick Food Scan. Epidemiology and Genomics Research
#' Program. National Cancer Institute. https://epi.grants.cancer.gov/diet/screeners/files.
#'
#' @import dplyr
#' @import stringr
#' @import rlang
#'
#' @seealso
#' \itemize{
#' \item \href{https://epi.grants.cancer.gov/diet/screeners/fat/}{Screener Documentation}
#' \item \href{https://epi.grants.cancer.gov/diet/shortreg/instruments/percent-energy-from-fat-screener.pdf}{The Screener}
#' \item \href{https://epi.grants.cancer.gov/diet/screeners/files.html#fat}{Original SAS Code from the NCI}
#' }
#'
#' @usage qfs_scores( df, default.names = TRUE,
#' item.names = list( CEREAL = "CEREAL", MILK.SKIM = "SKIMMILK",
#'                    EGGS = "EGGS", SAUSAGE = "SAUSAGE",
#'                    MARG.BUTTER = "MARGBR", CITRUS.JUICE = "CITJUICE",
#'                    FRUIT = "FRUIT", HOTDOG = "HOTDOG",
#'                    CHEESE = "CHEESE", FRIED.POTATOES = "FRIEDPOT",
#'                    MARG.BUTTER.ON.VEG = "MARGVEG", MAYO = "MAYO",
#'                    DRESSING = "SALDRS", RICE = "RICE",
#'                    MARGRICE = "MARGRICE", RED.FAT.MARG = "LOFATMRG",
#'                    FAT.SUBJECTIVE = "ALLFAT" ),
#' age.col = "AGE",
#' sex.col = "SEX" )
#'
#'
#' @param df A \code{data.frame} or \code{tibble} containing the columns (dietary items, sex, and age) for computing the scores.
#' @param default.names A logical. Defaulted to \code{TRUE} and establishes whether default survey item names (see data dictionary above) are used. If user-specified names are used, set to \code{FALSE} and specify the column names in \code{item.names}.
#' @param item.names A named \code{list} containing the user-specified column names (character vectors) for the survey items in the \code{df}. Ignored if \code{default.names} is \code{TRUE}. Must follow format used in \code{usage}.
#' @param age.col A character vector specifying the name of the age column in the \code{df}. Defaulted to "AGE".
#' @param sex.col A character vector specifying the name of the sex column in the \code{df}. Ensure levels of this variable are either coded numerically ("1" = male, "2" = female) or as "male" "female" as computation of the scores is contingent on this variable. Defaulted to "get( sex.col )".
#'
#' @return Object of class \code{data.frame} containing the original user-supplied data with the
#' age & sex-adjusted % energy from fat appended. Column names and descriptions are as follows:
#'
#' `pred.pcf`: Predicted percentage of calories from fat (\%)
#'
#' @examples
#' library( NCIdietscores )
#'
#' # using default diet item names
#'
#' qfs_scores( short.data )
#'
#' # user-specified diet item names but using default names in `item.names`
#'
#' qfs_scores( short.data, default.names = FALSE )
#'
#' # user specified names
#'
#' d.user <- setNames( short.data,
#'                     c( "cold.cereals", "milk",
#'                        "eggs", "bacon",
#'                        "butter", "cit.juice",
#'                        "fruit", "hot.dog",
#'                        "cheese", "potatoes",
#'                        "marg.veg", "mayonnaise",
#'                        "dressing", "rice",
#'                        "marg.on.rice", "red.marg",
#'                        "fat.subj", "SEX", "AGE" ) )
#'
#' # run `qfs_scores` without specifying column names, throws error
#' \dontrun{
#'
#'   qfs_scores( df = d.user, default.names = FALSE )
#'
#' }
#'
#'
#' # run `qfs_scores`  specifying column names in incorrect format, error thrown
#'
#' \dontrun{
#'   cls.list <- list( "cold.cereals", "milk",
#'                     "eggs", "bacon",
#'                     "butter", "cit.juice",
#'                     "fruit", "hot.dog",
#'                     "cheese", "potatoes",
#'                     "marg.veg", "mayonnaise",
#'                     "dressing", "rice",
#'                     "marg.on.rice", "red.marg",
#'                     "fat.subj" )
#'
#'   qfs_scores( df = d.user,
#'               default.names = FALSE,
#'               item.names = cls.list )
#' }
#'
#'
#' # run `qfs_scores`  specifying column names, no error
#'
#' cls.list <- list( CEREAL = "cold.cereals", MILK.SKIM = "milk",
#'                   EGGS = "eggs", SAUSAGE = "bacon",
#'                   MARG.BUTTER = "butter", CITRUS.JUICE = "cit.juice",
#'                   FRUIT = "fruit", HOTDOG = "hot.dog",
#'                   CHEESE = "cheese", FRIED.POTATOES = "potatoes",
#'                   MARG.BUTTER.ON.VEG = "marg.veg", MAYO = "mayonnaise",
#'                   DRESSING = "dressing", RICE = "rice",
#'                   MARGRICE = "marg.on.rice", RED.FAT.MARG = "red.marg",
#'                   FAT.SUBJECTIVE = "fat.subj" )
#'
#' qfs_scores( df = d.user,
#'             default.names = FALSE,
#'             item.names = cls.list )
#'
#'
#' # specify own names for sex and age columns
#'
#' d.user.age.sex <- short.data
#'
#' colnames( d.user.age.sex )[ colnames( d.user.age.sex ) == "SEX" ] <- "subject.sex"
#' colnames( d.user.age.sex )[ colnames( d.user.age.sex ) == "AGE" ] <- "subject.age"
#' colnames( d.user )[ colnames( d.user ) == "SEX" ] <- "subject.sex"
#' colnames( d.user )[ colnames( d.user ) == "AGE" ] <- "subject.age"
#'
#'
#' qfs_scores( df = d.user.age.sex, sex.col = "subject.sex", age.col = "subject.age" )
#'
#' qfs_scores( df = d.user,
#'             default.names = FALSE,
#'             item.names = cls.list,
#'             sex.col = "subject.sex",
#'             age.col = "subject.age" )
#'
#'
#' ## more errors: ##
#'
#' # incorrect data types
#' \dontrun{
#'
#'   qfs_scores( df = list( short.data ) )
#'   qfs_scores( df = short.data, age.col = 3 )
#'   qfs_scores( df = short.data, sex.col = 7 )
#'
#' }
#'
#' # incorrect formatting of data frequencies
#' \dontrun{
#'   short.data.format <- short.data
#'
#'   short.data.format[1:16][ short.data.format[1:16] == 1 ] <- "Never"
#'   short.data.format[1:16][ short.data.format[1:16] == 2 ] <- "Less than once per month"
#'   short.data.format[1:16][ short.data.format[1:16] == 3 ] <- "1-3 times per month"
#'   short.data.format[1:16][ short.data.format[1:16] == 4 ] <- "1-2 times per week"
#'   short.data.format[1:16][ short.data.format[1:16] == 5 ] <- "3-4 times per week"
#'   short.data.format[1:16][ short.data.format[1:16] == 6 ] <- "5-6 times per week"
#'   short.data.format[1:16][ short.data.format[1:16] == 7 ] <- "1 times per day"
#'   short.data.format[1:16][ short.data.format[1:16] == 8 ] <- "2 or more times per day"
#'   short.data.format[1:16][ short.data.format[1:16] == 9 ] <- "4 or more times per day"
#'
#'   qfs_scores( df = short.data.format )
#' }
#'
#' @export


qfs_scores <- function( df,
                        default.names = TRUE,
                        item.names = list( CEREAL = "CEREAL", MILK.SKIM = "SKIMMILK",
                                           EGGS = "EGGS", SAUSAGE = "SAUSAGE",
                                           MARG.BUTTER = "MARGBR", CITRUS.JUICE = "CITJUICE",
                                           FRUIT = "FRUIT", HOTDOG = "HOTDOG",
                                           CHEESE = "CHEESE", FRIED.POTATOES = "FRIEDPOT",
                                           MARG.BUTTER.ON.VEG = "MARGVEG", MAYO = "MAYO",
                                           DRESSING = "SALDRS", RICE = "RICE",
                                           MARGRICE = "MARGRICE", RED.FAT.MARG = "LOFATMRG",
                                           FAT.SUBJECTIVE = "ALLFAT" ),
                        age.col = "AGE",
                        sex.col = "SEX" ) {

  ### (1.0) Function Checks ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (1.1) Argument types and entries  ##

  # levels of the diet columns
  def.names <- c( "CEREAL", "SKIMMILK", "EGGS", "SAUSAGE", "MARGBR", "CITJUICE", "FRUIT", "HOTDOG",
                  "CHEESE", "FRIEDPOT", "MARGVEG", "MAYO", "SALDRS", "RICE", "MARGRICE", "LOFATMRG",
                  "ALLFAT" ) # default names

  list.item.nms <- c( "CEREAL", "MILK.SKIM", "EGGS", "SAUSAGE", "MARG.BUTTER", "CITRUS.JUICE", "FRUIT", "HOTDOG",
                     "CHEESE", "FRIED.POTATOES", "MARG.BUTTER.ON.VEG", "MAYO",
                     "DRESSING", "RICE", "MARGRICE", "RED.FAT.MARG",
                     "FAT.SUBJECTIVE" )

  # class checks
  if ( !inherits( item.names, "list" ) ) stop( "Error: `item.names` must be a list" )
  if ( sum( class( df ) %notin% c( "data.frame", "tbl", "tbl_df" ) ) >= 1 ) stop( "Error: `df` must be an object of class `data.frame` or `tibble`." )
  if ( !inherits( age.col, "character" ) | !inherits( sex.col, "character" ) ) stop( "Error: `age.col` and `sex.col` must be objects of class `character`." )

  # diet column names checks
  if ( !default.names & is.null( item.names) ) stop( "Error: user-specified list of column names empty when checking `default.names = T`." )
  if ( ( !default.names ) & length( item.names ) < 17 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( ( !default.names ) & length( item.names ) < 17 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( sum( list.item.nms %notin%  names( item.names ) )  > 0 ) stop( "Error: list of user-specified column names not in proper format. See default values for `item.names` in the documentation for an example." )

  if ( !default.names){
    if ( sum( c( item.names, sex.col, age.col  ) %notin% colnames( df )  ) > 0 ) stop( "Error: column names of `df` not detected in `item.names`, `age.col`, or `sex.col`. See default values for `item.names` in the documentation for an example and check entries for `age.col` and `sex.col`." )
  }

  # sex column name checks
  if ( is.null( df[[sex.col]]) ) stop( "Error: input to `sex.col` not detected in the provided dataset." )
  if ( is.null( df[[age.col]]) ) stop( "Error: input to `age.col` not detected in the provided dataset." )

  ## --------- End Subsection --------- ##


  ## (1.2) Check levels of `SEX` column ##

  # levels of sex column
  sex.levs <- levels( as.factor( df[[sex.col]] ) )

  # condition: check if "male"/"female" character/not numeric
  levs.mf <- sum(str_detect(  sex.levs, regex( "male", ignore_case = T ) ) ) +
    sum( str_detect( sex.levs, regex( "female", ignore_case = T ) ) )

  # execute: if there is "male" or "female" detected in the dataset, convert to numeric
  if ( levs.mf > 0 ){

    df[[sex.col]] <- ifelse( str_detect(  df[[sex.col]], regex( "female", ignore_case = T ) ), 2, df[[sex.col]] )
    df[[sex.col]] <- ifelse( str_detect(  df[[sex.col]], regex( "male", ignore_case = T ) ), 1, df[[sex.col]] )

  }

  # execute: now, ensure it's 1's and 2's if not, give error
  sex.levs.new <- levels( as.factor( df[[sex.col]] ) ) # reassign levels of new `SEX` variable

  # condition: check if new variable is numeric with assigned levels ,if not throw error
  levs.12 <- sum( sex.levs.new %notin% c( "1", "2" ) ) > 0

  if( levs.12 ) stop( 'Error: ensure `sex.col` is a variable with levels coded as "male" or "female" or "1", "2".' )

  ## --------- End Subsection --------- ##


  ## (1.3) Check levels of diet frequency column ##


  if ( default.names ) these.diet <- c( def.names, sex.col, age.col )
  if ( !default.names) these.diet <- unlist( item.names )

  # check data types of diet frequency columns
  if (sum( sapply( df[these.diet], function(x) sum( stringr::str_detect( x, "^([A-Za-z\\s]*)$" ),
                                                    na.rm = T ) ), na.rm = T ) != 0 ){

    stop( "Error: non-digit levels of the diet frequency column. Levels for these columns should be numeric. See data dictionary.")
  }

  ## --------- End Subsection --------- ##


  ## (1.3) ensure variable classes are numeric  ##

  # condition: assign an object, `v`, with the column names depending on option for`default.names` argument
  if( default.names ) v <- c( def.names, sex.col, age.col )
  if ( !default.names ) v <- c( unlist( item.names ), sex.col, age.col )

  # execute: run coerce_numeric and loop through all variables required and that matched by the two input arguments
  df <- coerce_numeric( d = df, vars = v )

  ## --------- End Subsection --------- ##


  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (2.0) Make a Copy of the Input Data for Final Merge  ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  d.copy <- df

  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (3.0) Do Unit Conversions to Daily Averages ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ## (3.1) Map responses to daily averages  ##

  # condition: assign an object with the dietary column names for subsequent loop
  if( default.names )  c.nms <- c( def.names )[ !def.names %in% c( "LOFATMRG", "ALLFAT" ) ]
  if( !default.names ) c.nms <- c( unlist( item.names ) )[ !c( unlist( item.names ) ) %in% c( "LOFATMRG", "ALLFAT" ) ]

  # execute: loop through columns to convert
  for( i in 1:length( c.nms ) ){

    cn <- c.nms[i] # assign column name for each iteration

    # map subject response to daily average (from Table 2-1 in the screener documentation)
    df[[ cn ]] <- ifelse( df[[ cn ]] == 1, 0, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 2, 0.018, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 3, 0.066, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 4, 0.214, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 5, 0.499, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 6, 0.784, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 7, 1, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 8, 2, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 9, NA, df[[ cn ]] )

  }

  ## --------- End Subsection --------- ##


  ## (3.3) Rename diet variables to user-friendly names ##

  # these names also match the names on the sex/age conversion tables #
  if( default.names ) {

    df <- df %>%
      rename( f2 = CEREAL,
              f3 = SKIMMILK,
              f4 = EGGS,
              f5 = SAUSAGE,
              f6 = MARGBR,
              f7 = CITJUICE,
              f8 = FRUIT,
              f9 = HOTDOG,
              f10 = CHEESE,
              f11 = FRIEDPOT,
              f12 = MARGVEG,
              f13 = MAYO,
              f14 = SALDRS,
              f15 = RICE,
              f16 = MARGRICE,
              lo.fat.mrg = LOFATMRG

      )

  }

  if( !default.names ) {

    df <- df %>%
      rename( f2 = item.names[["CEREAL"]],
              f3 = item.names[["MILK.SKIM"]],
              f4 = item.names[["EGGS"]],
              f5 = item.names[["SAUSAGE"]],
              f6 = item.names[["MARG.BUTTER"]],
              f7 = item.names[["CITRUS.JUICE"]],
              f8 = item.names[["FRUIT"]],
              f9 = item.names[["HOTDOG"]],
              f10 = item.names[["CHEESE"]],
              f11 = item.names[["FRIED.POTATOES"]],
              f12 = item.names[["MARG.BUTTER.ON.VEG"]],
              f13 = item.names[["MAYO"]],
              f14 = item.names[["DRESSING"]],
              f15 = item.names[["RICE"]],
              f16 = item.names[["MARGRICE"]],
              lo.fat.mrg = item.names[["RED.FAT.MARG"]]

              )
  }

  ## --------- End Subsection --------- ##


  ## (3.4) Margarine/butter fat computations ##
  df <- df %>%
    mutate( tot.fat = f6 + f12 + f16,
            fat.real = ifelse( lo.fat.mrg %in% c( 1, 2), 1,
                               ifelse( lo.fat.mrg == 3, 0.75,
                                       ifelse( lo.fat.mrg == 4, 0.5,
                                               ifelse( lo.fat.mrg == 5, 0.25,
                                                       ifelse( lo.fat.mrg == 6, 0,
                                                               ifelse( is.na( lo.fat.mrg), NA, 0 )))))),
            diet.fat = ifelse( lo.fat.mrg %in% c( 1, 2), 1,
                               ifelse( lo.fat.mrg == 3, 0.25,
                                       ifelse( lo.fat.mrg == 4, 0.5,
                                               ifelse( lo.fat.mrg == 5, 0.75,
                                                       ifelse( lo.fat.mrg == 6, 1,
                                                               ifelse( is.na( lo.fat.mrg), NA, 0 )))))),
            reg.fat = fat.real*tot.fat
    )

  ## --------- End Subsection --------- ##

  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (4.0) Estimate % Energy from Fat ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (4.1) Sex/Age adjustment loop ##

  # we will use the 10th and 11th HTML table in `sysdata.rda` (i.e., tbl.10.11)
  for( i in 1:nrow( df ) ){  # loop on subject

    for( j in 2:length( age.lst ) ){ # loop on age which determines which columns of reference table to use

      ## inner loops will be determined based on which rows to use from the table

      # males inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 1 ){

        for( g in c( 2:7, 9:15 ) ){

          df[ i, paste0( tbl.10.11$fd[g],".a" ) ] <-
            df[ i, which(colnames( df ) == tbl.10.11$fd[g]) ]*as.numeric( tbl.10.11[g,(j+1)] ) # j+1 given the additional sex column

        }

      }

      # females inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 2 ){

        for( g in c( 17:22, 24:30 ) ){

          df[ i, paste0( tbl.10.11$fd[g],".a" ) ] <-
            df[ i, which(colnames( df ) == tbl.10.11$fd[g]) ]*as.numeric( tbl.10.11[g,(j+1)] ) # j+1 given the additional sex column

        }
      }
    }
  }

  ## --------- End Subsection --------- ##



  ## (4.1) Multiply adjusted estimates by regression coefficients


    # we use the 13th HTML table for this final set of computations (i.e., `tbl.13`

    df <- df %>%

      # initialize variables with intercept values
        mutate(
            pred.pcf = ifelse( get( sex.col ) == 1, as.numeric( tbl.13[1,2] ),
                               ifelse( get( sex.col ) == 2, as.numeric( tbl.13[1,3] ), NA ) )
            )

  # add regression coefficient*intake iteratively
  for( i in 1:nrow( df ) ){  # loop on subject


    for( g in c( 2:14 ) ){ # loop on all food items in inner loop

      ## males ##
      if( df[ i, sex.col ] == 1 ){

        # predicted % from fat
        df[ i, "pred.pcf" ] <-
          df[ i, which(colnames( df ) == paste0( tbl.13$`Parameter`[g] ) ) ]*
          as.numeric( tbl.13[g,2] ) + df[ i, "pred.pcf" ]

      }

      ## females ##
      if( df[ i, sex.col ] == 2 ){

        # predicted % from fat
        df[ i, "pred.pcf" ] <-
          df[ i, which(colnames( df ) == paste0( tbl.13$`Parameter`[g] ) ) ]*
          as.numeric( tbl.13[g,3] ) + df[ i, "pred.pcf" ]

      }
    }
  }

  ## --------- End Subsection --------- ##


  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (7.0) Return Final Dataset  ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (7.1) Columns to return in final dataset ##
  d.out <- cbind( d.copy,
                  df[ ,c( "pred.pcf" ) ] ) %>% data.frame()

  ## --------- End Subsection --------- ##


  ## (7.2) Print summary stats for the appended columns ##
  print( summary( df[ ,c( "pred.pcf" ) ] ) )

  ## --------- End Subsection --------- ##

  return( d.out )

}
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


