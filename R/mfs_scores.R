###---------------------------------------------------------------
###   DIET QUALITY SCORES FOR MULTIFACTOR SCREENER USED IN OPEN
###---------------------------------------------------------------

#' @title Diet Quality Scores for the Multifactor Screener in OPEN
#'
#' @description Calculate age & sex-adjusted % Energy from fat, fiber, and fruit and vegetable MyPyramid equivalents
#' on data collected with the National Cancer Institute's Multifactor Screener from the OPEN Study.
#' For an example of how the data should be structured prior to feeding it to the function, see
#' \code{data( mfs.data )} and \code{help( data( mfs.data ) )}.
#'
#' @details
#' Implements the scoring procedures for data obtained from the National Cancer Institute (NCI)
#' Multifactor Screener from the OPEN Study. Computes % energy from fat,
#' fiber, and intake equivalents of fruits and vegetables (F & V). For a detailed
#' description of the screener, please refer to the NCI's documentation (see below).
#'
#' Citation:
#' The Multifactor screener in the
#' Observing Protein & Energy Nutrition (OPEN) Study. Epidemiology and Genomics Research
#' Program. National Cancer Institute. https://epi.grants.cancer.gov/diet/screeners/files.
#' Updated November 20, 2019.
#'
#' @import dplyr
#' @import stringr
#' @import rlang
#'
#' @seealso
#' \itemize{
#' \item \href{https://epi.grants.cancer.gov/diet/screeners/OPEN.pdf}{Screener Documentation}
#' \item \href{https://epi.grants.cancer.gov/past-initiatives/open/multifactor/open_multifactor_datadic.pdf}{Data Dictionary}
#' \item \href{https://epi.grants.cancer.gov/diet/shortreg/instruments/multifactor-screener-in-open-self-report-version.pdf}{The Screener}
#' \item \href{https://epi.grants.cancer.gov/diet/screeners/sas-program-open.zip}{Original SAS Code from the NCI}
#' }
#'
#' @usage mfs_scores( df,
#' default.names = TRUE,
#' item.names = list(
#'                    HQ1 = "HQ1", HQ2 = "HQ2",
#'                    HQ3 = "HQ3", HQ4 = "HQ4",
#'                    HQ5 = "HQ5", HQ6 = "HQ6",
#'                    HQ7 = "HQ7", HQ8 = "HQ8",
#'                    HQ9 = "HQ9", HQ10 = "HQ10",
#'                    HQ11 = "HQ11", HQ12 = "HQ12",
#'                    HQ13 = "HQ13", HQ14 = "HQ14",
#'                    HQ15 = "HQ15", HQ16 = "HQ16",
#'                    HQ2A = "HQ2A"
#'                    ),
#' age.col = "AGE",
#' sex.col = "SEX" )
#'
#' @param df A \code{data.frame} or \code{tibble} containing the columns (dietary items, sex, and age) for computing the scores.
#' @param default.names A logical. Defaulted to \code{TRUE} and establishes whether default survey item names (see data dictionary above) are used. If user-specified names are used, set to \code{FALSE} and specify the column names in \code{item.names}.
#' @param item.names A named \code{list} containing the user-specified column names (character vectors) for the survey items in the \code{df}. Ignored if \code{default.names} is \code{TRUE}. Must follow format used in \code{usage}.
#' @param age.col A character vector specifying the name of the age column in the \code{df}. Defaulted to "AGE".
#' @param sex.col A character vector specifying the name of the sex column in the \code{df}. Ensure levels of this variable are either coded numerically ("1" = male, "2" = female) or as "male" "female" as computation of the scores is contingent on this variable. Defaulted to "get( sex.col )".
#'
#' @return Object of class \code{data.frame} containing the original user-supplied data with the
#' age & sex-adjusted dietary screener scores appended. Column names and descriptions are as follows:
#'
#' `pred.fiber`: Predicted predicted fiber intake (cube rooted; cube to get back estimate in g)
#' `pred.pcf`: Predicted percentage of calories from fat (\%)
#' `pred.fv7.ps`: Predicted F & V pyramid serving units, including french fries, adjusted for age and sex
#' `pred.fv6.ps`: Predicted F & V pyramid serving units, excluding french fries, adjusted for age and sex
#' `raw.pred.fv7.ce`: Predicted F & V cup equivalents, including french fries, not adjusted for age and sex
#' `raw.pred.fv6.ce`: Predicted F & V cup equivalents, excluding french fries, not adjusted for age and sex
#' `pred.fv7.ce`: Predicted F & V cup equivalents, including french fries, adjusted for age and sex
#' `pred.fv6.ce`: Predicted F & V cup equivalents, excluding french fries, adjusted for age and sex
#'
#'
#' @examples
#' library( NCIdietscores )
#'
#' # using default diet item names
#'
#' mfs_scores( mfs.data )
#'
#' # user-specified diet item names but using default names in `item.names`
#'
#' mfs_scores( mfs.data, default.names = FALSE )
#'
#' # user specified names
#'
#' d.user <- setNames( mfs.data,
#'                     c( "cold.cereals", "milk", "bacon.sausage", "hot.dogs",
#'                        "bread", "juice", "fruit", "regular.fat", "salad", "potatoes",
#'                        "white.potatoes", "beans", "vegetables", "pasta", "nuts", "chips",
#'                        "milk.type", "SEX", "AGE" ) )
#'
#' # run `mfs_scores` without specifying column names, throws error
#' \dontrun{
#'
#'   mfs_scores( df = d.user, default.names = FALSE )
#'
#' }
#'
#'
#' # run `mfs_scores`  specifying column names in incorrect format, error thrown
#'
#' \dontrun{
#'   cls.list <- list( "cold.cereals", "milk",
#'                     "bacon.sausage", "hot.dogs",
#'                     "bread", "juice",
#'                     "fruit", "regular.fat",
#'                     "salad", "potatoes",
#'                     "white.potatoes", "beans",
#'                     "vegetables", "pasta",
#'                     "nuts", "chips",
#'                     "milk.type" )
#'
#'   mfs_scores( df = d.user,
#'               default.names = FALSE,
#'               item.names = cls.list )
#' }
#'
#'
#' # run `mfs_scores`  specifying column names, no error
#'
#' cls.list <- list( HQ1 = "cold.cereals", HQ2 = "milk",
#'                   HQ3 = "bacon.sausage", HQ4 = "hot.dogs",
#'                   HQ5 = "bread", HQ6 = "juice",
#'                   HQ7 = "fruit", HQ8 = "regular.fat",
#'                   HQ9 = "salad", HQ10 = "potatoes",
#'                   HQ11 = "white.potatoes", HQ12 = "beans",
#'                   HQ13 = "vegetables", HQ14 = "pasta",
#'                   HQ15 = "nuts", HQ16 = "chips",
#'                   HQ2A = "milk.type" )
#'
#' mfs_scores( df = d.user,
#'             default.names = FALSE,
#'             item.names = cls.list )
#'
#'
#' # specify own names for sex and age columns
#'
#' d.user.age.sex <- mfs.data
#'
#' colnames( d.user.age.sex )[ colnames( d.user.age.sex ) == "SEX" ] <- "subject.sex"
#' colnames( d.user.age.sex )[ colnames( d.user.age.sex ) == "AGE" ] <- "subject.age"
#' colnames( d.user )[ colnames( d.user ) == "SEX" ] <- "subject.sex"
#' colnames( d.user )[ colnames( d.user ) == "AGE" ] <- "subject.age"
#'
#'
#' mfs_scores( df = d.user.age.sex, sex.col = "subject.sex", age.col = "subject.age" )
#'
#' mfs_scores( df = d.user,
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
#'   mfs_scores( df = list( mfs.data ) )
#'   mfs_scores( df = mfs.data, age.col = 3 )
#'   mfs_scores( df = mfs.data, sex.col = 7 )
#'
#' }
#'
#' # incorrect formatting of data frequencies
#' \dontrun{
#' mfs.data.format <- mfs.data
#'
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 1 ] <- "Never"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 3 ] <- "1-2 times per week"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 2 ] <- "1-3 times last month"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 4 ] <- "3-4 times per week"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 5 ] <- "5-6 times per week"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 6 ] <- "1 time per day"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 7 ] <- "2 times per day"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 8 ] <- "3 times per day"
#' mfs.data.format[1:16][ mfs.data.format[1:16] == 9 ] <- "4 or more times per day"
#'
#' mfs_scores( df = mfs.data.format )
#' }
#'
#' @export

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




  ### (1.0) Function Checks ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (1.1) Argument types and entries  ##

  # class checks
  if ( !inherits( item.names, "list" ) ) stop( "Error: `item.names` must be a list" )
  if ( sum( class( df ) %notin% c( "data.frame", "tbl", "tbl_df" ) ) >= 1 ) stop( "Error: `df` must be an object of class `data.frame` or `tibble`." )
  if ( !inherits( age.col, "character" ) | !inherits( sex.col, "character" ) ) stop( "Error: `age.col` and `sex.col` must be objects of class `character`." )

  # diet column names checks
  if ( !default.names & is.null( item.names) ) stop( "Error: user-specified list of column names empty when checking `default.names = T`." )
  if ( ( !default.names ) & length( item.names ) < 17 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( ( !default.names ) & length( item.names ) < 17 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( sum( c( paste0( "HQ", 1:16 ), "HQ2A" ) %notin% names( item.names ) )  > 0 ) stop( "Error: list of user-specified column names not in proper format. See default values for `item.names` in the documentation for an example." )

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

  # levels of the diet columns
  if ( !default.names) these.diet <- unlist( item.names )
  if ( default.names) these.diet <- c( paste0( "HQ", 1:16 ), "HQ2A" )

  # check data types of diet frequency columns
  if (sum( sapply( df[these.diet], function(x) sum( stringr::str_detect( x, "^([A-Za-z\\s]*)$" ),
                                                    na.rm = T ) ), na.rm = T ) != 0 ){

    stop( "Error: non-digit levels of the diet frequency column. Levels for these columns should be numeric. See data dictionary.")
  }

  ## --------- End Subsection --------- ##


  ## (1.3) ensure variable classes are numeric  ##

  # condition: assign an object, `v`, with the column names depending on option for`default.names` argument
  if( default.names ) v <- c( paste0( "HQ", 1:16 ), "HQ2A", sex.col, age.col )
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

  ## (3.2) Modify milk variables to four separate milk variables (skim, 1%,  2%, and whole) ##

  # condition: check to see that all subjects who responded to milk question also responded to milk type
  if( default.names ) milk.miss <- sum( is.na( df$HQ2A ) & !is.na( df$HQ2 ) )
  if( !default.names ) milk.miss <- sum( is.na( df[[ item.names[["HQ2A"]] ]] ) & !is.na( df[[ item.names[["HQ2"]] ]] ) )

  # execute: if condition is true, coerce to 1% milk and throw warning
  if ( milk.miss > 0 ) warning( "Warning: observations with missing milk type detected. Will set milk type to '1%' for these observations. If this is not desired, manually change the entries and re-run." )


  # expand the milk type columns
  if( default.names ) {

    df <- df %>% # for those that have a milk entry that is not missing, they will get "0" for all other milk types not the ones that they consume
      mutate( HQ2A = ifelse( is.na( HQ2A ) & !is.na( HQ2 ), 3, HQ2A ), # set those w/ missing milk type to 1%--see warning message above

              # now create the milk-specific columns (4 columns total)
              milk.skim = ifelse( HQ2A == 5, HQ2, 0 ), # if a subject respond in the affirmative for a given milk type, they get their value of `HQ2` in the milk type column, otherwise they get a "1" for "never"
              milk.skim = ifelse( HQ2A == 4, HQ2, 0 ),
              milk.one = ifelse( HQ2A == 3, HQ2, 0  ),
              milk.two = ifelse( HQ2A == 2, HQ2, 0  ),
              milk.whole = ifelse( HQ2A == 1, HQ2, 0  ) )
  }

  if( !default.names ) {

    milk.var <- item.names[["HQ2"]]
    milk.type.used <- item.names[["HQ2A"]]

    df <- df %>% # for those that have a milk entry that is not missing, they will get "0" for all other milk types not the ones that they consume
      mutate( !!milk.type.used := ifelse( is.na( get( milk.type.used ) ) & !is.na( get( milk.var ) ), 3, get( milk.type.used ) ), # set those w/ missing milk type to 1%--see warning message above

              # now create the milk-specific columns (4 columns total)
              milk.skim = ifelse( get( milk.type.used ) %in% c(4,5), get( milk.var ), 0 ), # if a subject respond in the affirmative for a given milk type, they get their value of `HQ2` in the milk type column, otherwise they get a "1" for "never"
              milk.one = ifelse( get( milk.type.used ) == 3, get( milk.var ), 0  ),
              milk.two = ifelse( get( milk.type.used ) == 2, get( milk.var ), 0  ),
              milk.whole = ifelse( get( milk.type.used ) == 1, get( milk.var ), 0  ) )
  }

  ## --------- End Subsection --------- ##


  ## (3.1) Map responses to daily averages  ##

  # condition: assign an object with the dietary column names for subsequent loop
  if( default.names )  c.nms <- c( paste0( "HQ", c(1,3:16) ), "milk.whole", "milk.two", "milk.one", "milk.skim" )
  if( !default.names ) c.nms <- c( unlist( item.names )[c(1,3:16)], "milk.whole", "milk.two", "milk.one", "milk.skim" )

  # execute: loop through columns to convert
  for( i in 1:length( c.nms ) ){

    cn <- c.nms[i] # assign column name for each iteration

    # map subject response to daily average (from Table 2-1 in the screener documentation)
    df[[ cn ]] <- ifelse( df[[ cn ]] == 1, 0, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 2, 0.067, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 3, 0.214, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 4, 0.5, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 5, 0.786, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 6, 1, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 7, 2, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 8, 3, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 9, 4.5, df[[ cn ]] )

  }

  ## --------- End Subsection --------- ##




  ## (3.3) Rename diet variables to user-friendly names ##

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

  ## --------- End Subsection --------- ##




  ### (4.0) Adjust Fruit/Veg Frequency Sex/Age-Specific Factors  ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ## (4.1) Sex/Age adjustment loop ##

  # we will use the 4th HTML table in `sysdata.rda` (i.e., tbl.4)
  for( i in 1:nrow( df ) ){  # loop on subject

    for( j in 2:length( age.lst ) ){ # loop on age which determines which columns of reference table to use

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



  ## --------- End Subsection --------- ##


  ## (4.2) Compute pyramid serving units variables ##

  df <- df %>%
    mutate( fv7 = fruit.m + vegetables.m + juice.m + potatoes.m + white.potatoes.m + salad.m + beans.m,
            fv6 = fruit.m + vegetables.m + juice.m + white.potatoes.m + salad.m + beans.m, # remove fried potatoes
            sqfv7 = sqrt( fv7 ),
            sqfv6 = sqrt( fv6 ),
            ## create predicted outcomes ##
            pred.fv7.ps = ifelse( get( sex.col ) == 1, 0.90679 + 0.75856*sqfv7,
                                ifelse( get( sex.col ) == 2, 0.81956 + 0.73086*sqfv7, NA ) ),
            pred.fv6.ps = ifelse( get( sex.col ) == 1, 0.94077 + 0.73906*sqfv6,
                                ifelse( get( sex.col ) == 2, 0.81626 + 0.73022*sqfv6, NA ) ) )

  ## --------- End Subsection --------- ##


  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (5.0) Compute `pred.fiber` and `pred.pcf` (Age and Sex-Specific) ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (5.1) Age-sex adjustment loop ##

  # it will use tables (tbl.2) 2 (for males) and 3 (tbl.3) (for females) to make the conversions from `sysdata.rda`
  for( i in 1:nrow( df ) ){  # loop on subject

    for( j in 2:length( age.lst) ){ # loop on age which determines which columns of reference table to use

      ## inner loops will be determined based on which rows to use from the table

      # males inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 1 ){

        for( g in 3:20){ # loop on all food items in inner loop
          df[ i, paste0( tbl.2$`Food Group`[g],"_a" ) ] <-
            df[ i, which(colnames( df ) == tbl.2$`Food Group`[g]) ]*as.numeric( tbl.2[g,j] )

        }

      }

      # females inner loop
      if( df[ i, age.col ] %in% age.lst[[j]] & df[ i, sex.col ] == 2 ){

        for( g in 3:20){ # loop on all food items in inner loop

          df[ i, paste0( tbl.3$`Food Group`[g],"_a" ) ] <-
            df[ i, which(colnames( df ) == tbl.3$`Food Group`[g]) ]*as.numeric( tbl.3[g,j] )

        }
      }
    }
  }

  ## --------- End Subsection --------- ##


  ## (5.2) Use table  6 from `df.list` to create predicted fiber intake and % from fat ##

  df <- df %>%

    # initialize variables with intercept values
    mutate( pred.fiber = ifelse( get( sex.col ) == 1, as.numeric( tbl.6[2,3] ),
                                 ifelse( get( sex.col ) == 2, as.numeric( tbl.6[2,5] ), NA ) ),
            pred.pcf = ifelse( get( sex.col ) == 1, as.numeric( tbl.6[2,2] ),
                               ifelse( get( sex.col ) == 2, as.numeric( tbl.6[2,4] ), NA ) ) )

  # add regression coefficient*intake iteratively
  for( i in 1:nrow( df ) ){  # loop on subject



    for( g in 3:20){ # loop on all food items in inner loop

      ## males ##
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

      ## females ##
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

  ## --------- End Subsection --------- ##


  # ---------------------------------------------------------------------------------------------------------------------------------------------------------





  ### (6.0) Sex/Age-Adjusted Cup Equivalents of Fruit & Veg ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (6.1) Create age category variable ##

  for ( i in 1:nrow( df ) ){

    for ( j in 2:length( age.lst ) ){

      if( df[ i, age.col ] %in% age.lst[[j]] ){

        df[ i, "age.cat" ] <- j - 1

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

  # subjects with anty missing values will get NA for this score
  df <- df %>%
    left_join( ., fvcupadj, by = c( sex.join.cols, "age.cat" = "AgeGrp" ) ) %>%

    mutate( raw.pred.fv7.ce = juice*FVCAFrtJ + fruit*FVCAFruit + potatoes*FVCAFrFry
            + white.potatoes*FVCAOthPot + beans*FVCADrBean + salad*FVCASalad
            + vegetables*FVCAOthVeg, # includes french fries

            raw.pred.fv6.ce = juice*FVCAFrtJ + fruit*FVCAFruit
            + white.potatoes*FVCAOthPot + beans*FVCADrBean + salad*FVCASalad
            + vegetables*FVCAOthVeg, # excludes french fries

            # adjust using sex-soecific regression coefficients from CSFII 94-96
            pred.fv7.ce = ifelse( get( sex.col ) == 1,
                                  ( 0.666228 + 0.770652*( sqrt( raw.pred.fv7.ce ) ) )^2,
                                  ifelse( get( sex.col ) == 2,
                                          ( 0.611844 + 0.733890*( sqrt(raw.pred.fv7.ce) ) )^2, NA ) ),
            pred.fv6.ce = ifelse( get( sex.col ) == 1,
                                  ( 0.706696 + 0.742255*( sqrt( raw.pred.fv6.ce ) ) )^2,
                                  ifelse( get( sex.col ) == 2,
                                          ( 0.616033 + 0.727761*( sqrt(raw.pred.fv6.ce ) ) )^2, NA ) ) )

  ## --------- End Subsection --------- ##


  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (7.0) Return Final Dataset  ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (7.1) Columns to return in final dataset ##
  d.out <- cbind( d.copy,
                      df[ ,c( "pred.fiber", "pred.pcf",
                              "pred.fv7.ce", "pred.fv6.ce",
                              "raw.pred.fv7.ce", "raw.pred.fv6.ce",
                              "pred.fv7.ps", "pred.fv6.ps" ) ] ) %>% data.frame()

  ## --------- End Subsection --------- ##


  ## (7.2) Print summary stats for the appended columns ##
  print( summary( df[ ,c( "pred.fiber", "pred.pcf",
                          "pred.fv7.ce", "pred.fv6.ce",
                          "raw.pred.fv7.ce", "raw.pred.fv6.ce",
                          "pred.fv7.ps", "pred.fv6.ps" ) ] ) )

  ## --------- End Subsection --------- ##

  return( d.out )

}

