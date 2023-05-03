###------------------------------------------------------------------------
###   SCORING FOR FRUIT & VEGETABLE SCREENER USED IN EATS (ALL-DAY SCREENER)
###------------------------------------------------------------------------

#' @title Scores for the Fruit & Vegetable Intake Day Screener in the Eating at America's Table Study (EATS)
#'
#' @description Calculate MyPyramid cup equivalents and MyPyramid servings of fruit & vegetable intake
#' on data collected with the National Cancer Institute's Fruit & Vegetable Intake All-Day Screener in the
#' EATS.
#'
#' @details
#' Implements the scoring procedures for data obtained from the National Cancer Institute (NCI)
#' Fruit & Vegetable Intake All-Day Screener from the EATS. MyPyramid cup equivalents and MyPyramid
#' servings of fruit & vegetable intake. For a detailed description of the screener, please refer
#' to the NCI's documentation (see below).
#'
#' Citation:
#' Thompson FE, Subar AF, Smith AF, et al. Fruit and vegetable assessment: performance of 2 new
#' short instruments and a food frequency questionnaire. J Am Diet Assoc. 2002;102(12):1764-1772.
#' doi:10.1016/s0002-8223(02)90379-2
#'
#' @import dplyr
#' @import stringr
#' @import rlang
#'
#' @seealso
#' \itemize{
#' \item \href{https://epi.grants.cancer.gov/diet/screeners/fruitveg/}{Screener Documentation}
#' \item \href{https://epi.grants.cancer.gov/diet/screeners/fruitveg/scoring/}{Scoring Procedures}
#' \item \href{https://epi.grants.cancer.gov/diet/shortreg/instruments/eats_all-day.pdf}{The Screener}
#' \item \href{https://epi.grants.cancer.gov/diet/screeners/sas-program-eats-allday.zip}{Original SAS Code from the NCI}
#' }
#'
#' @usage fvs_scores_day( df,
#' default.names = TRUE,
#' item.names = list( Q1 = "Q1", Q1A = "Q1A",
#'                    Q2 = "Q2", Q2A1 = "Q2A1",
#'                    Q2A2 = "Q2A2", Q3 = "Q3",
#'                    Q3A = "Q3A", Q4 = "Q4",
#'                    Q4A = "Q4A", Q5 = "Q5",
#'                    Q5A = "Q5A", Q6 = "Q6",
#'                    Q6A = "Q6A", Q7 = "Q7",
#'                    Q7A = "Q7A", Q8 = "Q8",
#'                    Q8A = "Q8A", Q9 = "Q9",
#'                    Q9A = "Q9A", Q10 = "Q10" ) )
#'
#' @param df A \code{data.frame} or \code{tibble} containing the columns (dietary items) for computing the scores.
#' @param default.names A logical. Defaulted to \code{TRUE} and establishes whether default survey item names (see NCI SAS code linked above) are used. If user-specified names are used, set to \code{FALSE} and specify the column names in \code{item.names}.
#' @param item.names A named \code{list} containing the user-specified column names (character vectors) for the survey items in the \code{df}. Ignored if \code{default.names} is \code{TRUE}. Must follow format used in \code{usage}.
#'
#' @return Object of class \code{data.frame} containing the original user-supplied data with the
#' MyPyramid cup equivalents and MyPyramid servings of fruit & vegetable intake. Column names and descriptions
#' are as follows:
#'
#' `frt.veg.ce`: Estimated daily average MyPyramid cup equivalents of fruits & vegetables
#' `frt.veg.ps`: Estimated daily average MyPyramid servings of fruits & vegetables
#'
#' @examples
#' library( NCIdietscores )
#'
#' # using default diet item names
#'
#' fvs_scores_day( fv.data.day )
#'
#' # user-specified diet item names but using default names in `item.names`
#'
#' fvs_scores_day( fv.data.day, default.names = FALSE )
#'
#' # user specified names
#'
#' d.user <- setNames( fv.data.day,
#'                     c( "juice", "veg.mix", "juice.size", "fruit",
#'                        "fruit.size.a", "fruit.size.b", "salad", "salad.size", "frfy", "frfy.size",
#'                        "oth.pot", "oth.pot.size", "beans", "beans.size", "oth.veg", "oth.veg.size",
#'                        "tom", "tom.size", "veg.soup", "veg.soup.size" ) )
#'
#' # run `fvs_scores_day` without specifying column names, throws error
#' \dontrun{
#'
#'   fvs_scores_day( df = d.user, default.names = FALSE )
#'
#' }
#'
#'
#' # run `fvs_scores_day`  specifying column names in incorrect format, error thrown
#'
#' \dontrun{
#'   cls.list <- list( "juice", "veg.mix", "juice.size", "fruit",
#'                     "fruit.size.a", "fruit.size.b", "salad", "salad.size", "frfy", "frfy.size",
#'                     "oth.pot", "oth.pot.size", "beans", "beans.size", "oth.veg", "oth.veg.size",
#'                     "tom", "tom.size", "veg.soup", "veg.soup.size" )
#'
#'   fvs_scores_day( df = d.user,
#'                   default.names = FALSE,
#'                   item.names = cls.list )
#' }
#'
#'
#' # run `fvs_scores_day`  specifying column names, no error
#'
#' cls.list <- list( Q1 = "juice", Q1A = "juice.size",
#'                   Q2 = "fruit", Q2A1 = "fruit.size.a",
#'                   Q2A2 = "fruit.size.b", Q3 = "salad",
#'                   Q3A = "salad.size", Q4 = "frfy",
#'                   Q4A = "frfy.size", Q5 = "oth.pot",
#'                   Q5A = "oth.pot.size", Q6 = "beans",
#'                   Q6A = "beans.size", Q7 = "oth.veg",
#'                   Q7A = "oth.veg.size", Q8 = "tom",
#'                   Q8A = "tom.size", Q9 = "veg.soup",
#'                   Q9A = "veg.soup.size", Q10 = "veg.mix" )
#'
#' fvs_scores_day( df = d.user,
#'                 default.names = FALSE,
#'                 item.names = cls.list )
#'
#'
#' # Set "M" and "E" entries to missing before using function
#'
#' fv.data.day.me <- fv.data.day
#' fv.data.day.me[ fv.data.day.me == "M" ] <- NA
#' fv.data.day.me[ fv.data.day.me == "E" ] <- NA
#'
#' fvs_scores_day( fv.data.day.me )
#'
#'
#'
#' ## more errors: ##
#'
#' # incorrect data types
#' \dontrun{
#'
#'   fvs_scores_day( df = list( fv.data.day ) )
#'
#' }
#'
#' # incorrect formatting of data frequencies
#' \dontrun{
#'   fv.data.day.format <- fv.data.day
#'
#'   nms <- paste0( "Q", 1:10 )
#'
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 0 ] <- "Never"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 1 ] <- "1-3 times last month"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 3 ] <- "1-2 times per week"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 2 ] <- "3-4 times per week"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 4 ] <- "5-6 times per week"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 5 ] <- "1 time per day"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 6 ] <- "2 times per day"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 7 ] <- "3 times per day"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 8 ] <- "4 times per day"
#'   fv.data.day.format[nms][ fv.data.day.format[nms] == 9 ] <- "5 or more times per day"
#'
#'   fvs_scores_day( df = fv.data.day.format )
#' }
#'
#' @export

fvs_scores_day <- function( df,
                        default.names = TRUE,
                        item.names = list( Q1 = "Q1", Q1A = "Q1A",
                                           Q2 = "Q2", Q2A1 = "Q2A1",
                                           Q2A2 = "Q2A2", Q3 = "Q3",
                                           Q3A = "Q3A", Q4 = "Q4",
                                           Q4A = "Q4A", Q5 = "Q5",
                                           Q5A = "Q5A", Q6 = "Q6",
                                           Q6A = "Q6A", Q7 = "Q7",
                                           Q7A = "Q7A", Q8 = "Q8",
                                           Q8A = "Q8A", Q9 = "Q9",
                                           Q9A = "Q9A", Q10 = "Q10" ) ) {

  # copy dataset
  df.copy <- df


  ### (1.0) Function Checks ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ## (1.1) Argument types and entries  ##

  # class checks
  if ( !inherits( item.names, "list" ) ) stop( "Error: `item.names` must be a list" )
  if ( sum( class( df ) %notin% c( "data.frame", "tbl", "tbl_df" ) ) >= 1 ) stop( "Error: `df` must be an object of class `data.frame` or `tibble`." )

  # diet column names checks
  if ( !default.names & is.null( item.names) ) stop( "Error: user-specified list of column names empty when checking `default.names = T`." )
  if ( ( !default.names ) & length( item.names ) < 20 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( ( !default.names ) & length( item.names ) < 20 ) stop( "Error: user-specified list of column names is less than the sufficient length." )
  if ( sum( c( paste0( "Q", 1:10 ), "Q2A1", "Q2A2" ) %notin% names( item.names ) )  > 0 ) stop( "Error: list of user-specified column names not in proper format. See default values for `item.names` in the documentation for an example." )

  # levels of the diet columns
  def.names <- c( paste0( "Q", 1:10 ), paste0( "Q", c(1,3:9), "A" ), "Q2A1", "Q2A2" ) # default names

  if( default.names ) v <- c( def.names )
  if ( !default.names ) v <- c( unlist( item.names ) )

  ## (1.2) Set "M" and "E" entries to missing ##
  df.copy[ v ][ df.copy[ v ] == "M" ] <- NA
  df.copy[ v ][ df.copy[ v ] == "E" ] <- NA

  ## --------- End Subsection --------- ##


  ## (1.3) ensure variable classes are numeric  ##

  # execute: run coerce_numeric and loop through all variables required and that matched by the two input arguments
  df.copy <- coerce_numeric( d = df.copy, vars = v )

  ## --------- End Subsection --------- ##


  ### (2.0) Do Unit Conversions to Daily Averages ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ## (2.1) Frequency response conversions ##

  # condition: assign an object with the dietary column names for subsequent loop
  if( default.names )  c.nms <- c( paste0( "Q", c(1:10) ) )
  if( !default.names ) {

    c.nms <- vector()
    for( i in seq_along( paste0( "Q", 1:10 ) ) ){

      c.nms[i] <- unlist( item.names[[ paste0( "Q", i ) ]] )

    }
  }

  # execute: loop through columns to convert
  for( i in 1:length( c.nms ) ){

    cn <- c.nms[i] # assign column name for each iteration

    # map subject response (from SAS code provided by NCI)
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 0, 0, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 1, 0.067, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 2, 0.214, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 3, 0.5, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 4, 0.786, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 5, 1, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 6, 2, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 7, 3, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 8, 4, df.copy[[ cn ]] )
    df.copy[[ cn ]] <- ifelse( df.copy[[ cn ]] == 9, 5, df.copy[[ cn ]] )

  }

  ## --------- End Subsection --------- ##


  ## (2.2) Portion size response conversions (pyramid cup equivalents) ##

  df.ce <- df.copy

  # Q1A
  if( default.names ) og <- str_extract( "Q1A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q1"]] # frequency item name if default names not used

  df.ce[, paste0( "Q1A", "N" ) ] <- ifelse( df.ce[, item.names[["Q1A"]] ] == 0, 0.5,
                                            ifelse( df.ce[, item.names[["Q1A"]] ] == 1, 1,
                                                    ifelse( df.ce[, item.names[["Q1A"]] ] == 2, 1.625,
                                                            ifelse( df.ce[, item.names[["Q1A"]] ] == 3, 2.5,
                                                                    ifelse( df.ce[, og ] == 0, 0, df.ce[, og ] ) ) ) ) )

  # Q2A1, Q2A2, Q3A, Q8A
  these.1 <- c( "Q2A1", "Q2A2", "Q3A", "Q8A" )
  these.og <- str_extract( these.1, "Q\\d" )

  for( i in seq_along( these.1 ) ){

    if( default.names ) og <- str_extract( these.1[i], "Q\\d" ) # frequency item name if default names used
    if( !default.names ) og <- unlist( item.names[[ these.og[i] ]] )

    df.ce[, paste0( these.1[i], "N" ) ] <- ifelse( df.ce[, item.names[[ these.1[i] ]] ] == 0, 0.25,
                                                   ifelse( df.ce[, item.names[[ these.1[i] ]] ] == 1, 0.5,
                                                           ifelse( df.ce[, item.names[[ these.1[i] ]] ] == 2, 1,
                                                                   ifelse( df.ce[, item.names[[ these.1[i] ]] ] == 3, 1.5,
                                                                           ifelse( df.ce[, og ] == 0, 0, df.ce[, og ] ) ) ) ) )
  }


  # Q4A
  if( default.names ) og <- str_extract( "Q4A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q4"]] # frequency item name if default names not used

  df.ce[, paste0( "Q4A", "N" ) ] <- ifelse( df.ce[, item.names[["Q4A"]] ] == 0, 0.2,
                                            ifelse( df.ce[, item.names[["Q4A"]] ] == 1, 0.5,
                                                    ifelse( df.ce[, item.names[["Q4A"]] ] == 2, 0.75,
                                                            ifelse( df.ce[, item.names[["Q4A"]] ] == 3, 1.3,
                                                                    ifelse( df.ce[, og ] == 0, 0, df.ce[, og ] ) ) ) ) )

  # Q5A
  if( default.names ) og <- str_extract( "Q5A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q5"]] # frequency item name if default names not used

  df.ce[, paste0( "Q5A", "N" ) ] <- ifelse( df.ce[, item.names[["Q5A"]] ] == 0, 0.25,
                                            ifelse( df.ce[, item.names[["Q5A"]] ] == 1, 0.75,
                                                    ifelse( df.ce[, item.names[["Q5A"]] ] == 2, 1.2,
                                                            ifelse( df.ce[, item.names[["Q5A"]] ] == 3, 2.0,
                                                                    ifelse( df.ce[, og ] == 0, 0, df.ce[, og ] ) ) ) ) )

  # Q6A
  if( default.names ) og <- str_extract( "Q6A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q6"]] # frequency item name if default names not used

  df.ce[, paste0( "Q6A", "N" ) ] <- ifelse( df.ce[, item.names[["Q6A"]] ] == 0, 0.25,
                                            ifelse( df.ce[, item.names[["Q6A"]] ] == 1, 0.75,
                                                    ifelse( df.ce[, item.names[["Q6A"]] ] == 2, 1.25,
                                                            ifelse( df.ce[, item.names[["Q6A"]] ] == 3, 2.0,
                                                                    ifelse( df.ce[, og ] == 0, 0, df.ce[, og ] ) ) ) ) )

  # Q7A
  if( default.names ) og <- str_extract( "Q7A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q7"]] # frequency item name if default names not used

  df.ce[, paste0( "Q7A", "N" ) ] <- ifelse( df.ce[, item.names[["Q7A"]] ] == 0, 0.25,
                                            ifelse( df.ce[, item.names[["Q7A"]] ] == 1, 0.75,
                                                    ifelse( df.ce[, item.names[["Q7A"]] ] == 2, 1.5,
                                                            ifelse( df.ce[, item.names[["Q7A"]] ] == 3, 2.25,
                                                                    ifelse( df.ce[, og ] == 0, 0, df.ce[, og ] ) ) ) ) )

  # Q9A
  if( default.names ) og <- str_extract( "Q9A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q9"]] # frequency item name if default names not used

  df.ce[, paste0( "Q9A", "N" ) ] <- ifelse( df.ce[, item.names[["Q9A"]] ] == 0, 0.3,
                                            ifelse( df.ce[, item.names[["Q9A"]] ] == 1, 1.0,
                                                    ifelse( df.ce[, item.names[["Q9A"]] ] == 2, 1.6,
                                                            ifelse( df.ce[, item.names[["Q9A"]] ] == 3, 2.25,
                                                                    ifelse( df.ce[, og ] == 0, 0, df.ce[, og ] ) ) ) ) )

  ## --------- End Subsection --------- ##


  ## (2.3) Pyramid cup equivalents of fruit & veg calculation ##

  df.ce <- df.ce %>%
    mutate( JUICE = get( item.names[["Q1"]] ) * Q1AN,
            FRUITA = get( item.names[["Q2"]] ) * Q2A1N,
            FRUITB = get( item.names[["Q2"]] ) * Q2A2N,
            FRUIT = rowMeans( cbind( FRUITA, FRUITB ), na.rm = T ),
            LSALAD = get( item.names[["Q3"]] ) * Q3AN,
            FRFRY = get( item.names[["Q4"]] ) * Q4AN,
            WHPOT = get( item.names[["Q5"]] ) * Q5AN,
            DRBEAN = get( item.names[["Q6"]] ) * Q6AN,
            OTHVEG = get( item.names[["Q7"]] ) * Q7AN,
            TOMSAUCE = get( item.names[["Q8"]] ) * Q8AN,
            VEGSOUP = get( item.names[["Q9"]] ) * Q9AN,

            # final sum
            frt.veg.ce = rowSums( cbind( JUICE, FRUIT, LSALAD, FRFRY, WHPOT, DRBEAN, OTHVEG, TOMSAUCE, VEGSOUP ),
                               na.rm = T ) ) # note that any NAs in any of the columns are being set to ZERO before summing

  ## --------- End Subsection --------- ##


  ## (2.4) Portion size response conversions (pyramid servings) ##

  df.ps <- df.copy

  # Q1A
  if( default.names ) og <- str_extract( "Q1A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q1"]] # frequency item name if default names not used

  df.ps[, paste0( "Q1A", "N" ) ] <- ifelse( df.ps[, item.names[["Q1A"]] ] == 0, 0.75,
                                              ifelse( df.ps[, item.names[["Q1A"]] ] == 1, 1.33,
                                                      ifelse( df.ps[, item.names[["Q1A"]] ] == 2, 2.17,
                                                              ifelse( df.ps[, item.names[["Q1A"]] ] == 3, 3.33,
                                                                      ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  # Q2A1, Q2A2
  these.2 <- c( "Q2A1", "Q2A2" )
  these.og.2 <- str_extract( these.2, "Q\\d" )

  for( i in seq_along( these.2 ) ){

    if( default.names ) og <- str_extract( these.1[i], "Q\\d" ) # frequency item name if default names used
    if( !default.names ) og <- unlist( item.names[[ these.og.2[i] ]] )

    df.ps[, paste0( these.2[i], "N" ) ] <- ifelse( df.ps[, item.names[[ these.2[i] ]] ] == 0, 0.75,
                                 ifelse( df.ps[, item.names[[ these.2[i] ]] ] == 1, 1,
                                         ifelse( df.ps[, item.names[[ these.2[i] ]] ] == 2, 2,
                                                 ifelse( df.ps[, item.names[[ these.2[i] ]] ] == 3, 2.5,
                                                         ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )
  }

  # Q3A
  if( default.names ) og <- str_extract( "Q3A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q3"]] # frequency item name if default names not used

  df.ps[, paste0( "Q3A", "N" ) ] <- ifelse( df.ps[, item.names[["Q3A"]] ] == 0, 0.5,
                                            ifelse( df.ps[, item.names[["Q3A"]] ] == 1, 1,
                                                    ifelse( df.ps[, item.names[["Q3A"]] ] == 2, 2,
                                                            ifelse( df.ps[, item.names[["Q3A"]] ] == 3, 3,
                                                                    ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  # Q4A
  if( default.names ) og <- str_extract( "Q4A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q4"]] # frequency item name if default names not used

  df.ps[, paste0( "Q4A", "N" ) ] <- ifelse( df.ps[, item.names[["Q4A"]] ] == 0, 1.25,
                                         ifelse( df.ps[, item.names[["Q4A"]] ] == 1, 2.3,
                                                 ifelse( df.ps[, item.names[["Q4A"]] ] == 2, 3.1,
                                                         ifelse( df.ps[, item.names[["Q4A"]] ] == 3, 4.8,
                                                                 ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  # Q5A
  if( default.names ) og <- str_extract( "Q5A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q5"]] # frequency item name if default names not used

  df.ps[, paste0( "Q5A", "N" ) ] <- ifelse( df.ps[, item.names[["Q5A"]] ] == 0, 0.8,
                                         ifelse( df.ps[, item.names[["Q5A"]] ] == 1, 1.5,
                                                 ifelse( df.ps[, item.names[["Q5A"]] ] == 2, 2.4,
                                                         ifelse( df.ps[, item.names[["Q5A"]] ] == 3, 3.5,
                                                                 ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  # Q6A
  if( default.names ) og <- str_extract( "Q6A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q6"]] # frequency item name if default names not used

  df.ps[, paste0( "Q6A", "N" ) ] <- ifelse( df.ps[, item.names[["Q6A"]] ] == 0, 0.75,
                                         ifelse( df.ps[, item.names[["Q6A"]] ] == 1, 1.5,
                                                 ifelse( df.ps[, item.names[["Q6A"]] ] == 2, 2.5,
                                                         ifelse( df.ps[, item.names[["Q6A"]] ] == 3, 3.5,
                                                                 ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  # Q7A
  if( default.names ) og <- str_extract( "Q7A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q7"]] # frequency item name if default names not used

  df.ps[, paste0( "Q7A", "N" ) ] <- ifelse( df.ps[, item.names[["Q7A"]] ] == 0, 0.75,
                                         ifelse( df.ps[, item.names[["Q7A"]] ] == 1, 1.5,
                                                 ifelse( df.ps[, item.names[["Q7A"]] ] == 2, 3,
                                                         ifelse( df.ps[, item.names[["Q7A"]] ] == 3, 4.5,
                                                                 ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  # Q8A
  if( default.names ) og <- str_extract( "Q8A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q8"]] # frequency item name if default names not used

  df.ps[, paste0( "Q8A", "N" ) ] <- ifelse( df.ps[, item.names[["Q8A"]] ] == 0, 0.36,
                                            ifelse( df.ps[, item.names[["Q8A"]] ] == 1, 0.72,
                                                    ifelse( df.ps[, item.names[["Q8A"]] ] == 2, 1.45,
                                                            ifelse( df.ps[, item.names[["Q8A"]] ] == 3, 1.7,
                                                                    ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  # Q9A
  if( default.names ) og <- str_extract( "Q9A", "Q\\d" ) # frequency item name if default names used
  if( !default.names ) og <- item.names[["Q9"]] # frequency item name if default names not used

  df.ps[, paste0( "Q9A", "N" ) ] <- ifelse( df.ps[, item.names[["Q8A"]] ] == 0, 0.75,
                                         ifelse( df.ps[, item.names[["Q8A"]] ] == 1, 1.36,
                                                 ifelse( df.ps[, item.names[["Q8A"]] ] == 2, 2.27,
                                                         ifelse( df.ps[, item.names[["Q8A"]] ] == 3, 3.2,
                                                                 ifelse( df.ps[, og ] == 0, 0, df.ps[, og ] ) ) ) ) )

  ## --------- End Subsection --------- ##


  ## (2.5) Pyramid servings of fruit & veg calculation ##

  df.ps <- df.ps %>%
    mutate( JUICE = get( item.names[["Q1"]] ) * Q1AN,
            FRUITA = get( item.names[["Q2"]] ) * Q2A1N,
            FRUITB = get( item.names[["Q2"]] ) * Q2A2N,
            FRUIT = rowMeans( cbind( FRUITA, FRUITB ), na.rm = T ), # this ensures averages and missing are accounted for
            LSALAD = get( item.names[["Q3"]] ) * Q3AN,
            FRFRY = get( item.names[["Q4"]] ) * Q4AN,
            WHPOT = get( item.names[["Q5"]] ) * Q5AN,
            DRBEAN = get( item.names[["Q6"]] ) * Q6AN,
            OTHVEG = get( item.names[["Q7"]] ) * Q7AN,
            TOMSAUCE = get( item.names[["Q8"]] ) * Q8AN,
            VEGSOUP = get( item.names[["Q9"]] ) * Q9AN,

            # final sum
            frt.veg.ps = rowSums( cbind( JUICE, FRUIT, LSALAD, FRFRY, WHPOT, DRBEAN, OTHVEG, TOMSAUCE, VEGSOUP ),
                                  na.rm = T ) ) # note that any NAs in any of the columns are being set to ZERO before summing

  # ---------------------------------------------------------------------------------------------------------------------------------------------------------




  ### (3.0) Return Final Dataset  ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (3.1) Columns to return in final dataset ##

  d.out <- setNames( bind_cols( data.frame( df ),
                  data.frame( df.ce[ , c( "frt.veg.ce" ) ] ),
                  data.frame( df.ps[ , c( "frt.veg.ps" ) ] ) ) %>% data.frame(),
                  c( colnames( df ), "frt.veg.ce", "frt.veg.ps" ) )

  ## --------- End Subsection --------- ##


  ## (3.2) Print summary stats for the appended columns ##

  print( summary( d.out[ ,c( "frt.veg.ce", "frt.veg.ps" ) ] ) )

  ## --------- End Subsection --------- ##

  return( d.out )

}


