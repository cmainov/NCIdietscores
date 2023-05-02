###------------------------------------------------------------------------
###   SCORING FOR FRUIT & VEGETABLE SCREENER USED IN EATS (DAY SCREENER)
###------------------------------------------------------------------------

#' @title Scores for the Fruit & Vegetable Intake Screeners in the Eating at America's Table Study (EATS)
#'
#' @description Calculate MyPyramid cup equivalents and MyPyramid servings of fruit & vegetable intake
#' on data collected with the National Cancer Institute's Fruit & Vegetable Intake Screeners in the
#' EATS.
#'
#' @details
#' Implements the scoring procedures for data obtained from the National Cancer Institute (NCI)
#' Fruit & Vegetable Intake Screeners from the EATS. MyPyramid cup equivalents and MyPyramid
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
#' `frt.veg.ce`: Estimated MyPyramid cup equivalents of fruit & vegetable intake
#' `frt.veg.ps`: Estimated MyPyramid servings of fruit & vegetable intake


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

  # levels of the diet columns
  def.names <- c( paste0( "Q", 1:10 ), paste0( "Q", c(1,3:9), "A" ), "Q2A1", "Q2A2" ) # default names


  ## (1.2) Set "M" and "E" entries to missing ##
  df.copy[ def.names ][ df.copy[ def.names ] == "M" ] <- NA
  df.copy[ def.names ][ df.copy[ def.names ] == "E" ] <- NA

  ## --------- End Subsection --------- ##


  ## (1.3) ensure variable classes are numeric  ##

  # condition: assign an object, `v`, with the column names depending on option for`default.names` argument
  if( default.names ) v <- c( def.names, sex.col, age.col )
  if ( !default.names ) v <- c( unlist( item.names ), sex.col, age.col )

  # execute: run coerce_numeric and loop through all variables required and that matched by the two input arguments
  df.copy <- coerce_numeric( d = df.copy, vars = v )

  ## --------- End Subsection --------- ##


  ### (2.0) Do Unit Conversions to Daily Averages ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ## (2.1) Frequency response conversions ##

  # condition: assign an object with the dietary column names for subsequent loop
  if( default.names )  c.nms <- c( def.names )[ !def.names %in% c( paste0( "Q", c(1,3:9), "A" ), "Q2A1", "Q2A2" ) ]
  if( !default.names ) c.nms <- c( unlist( item.names ) )[ !c( unlist( item.names ) ) %in% c( "Q2A1", "Q2A2" ) ]

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
  df.ce[, paste0( "Q1A", "N" ) ] <- ifelse( df.ce[, "Q1A" ] == 0, 0.5,
                                            ifelse( df.ce[, "Q1A" ] == 1, 1,
                                                    ifelse( df.ce[, "Q1A" ] == 2, 1.625,
                                                            ifelse( df.ce[, "Q1A" ] == 3, 2.5,
                                                                    ifelse( df.ce[, "Q1" ] == 0, 0, df.ce[, "Q1A" ] ) ) ) ) )

  # Q2A1, Q2A2, Q3A, Q8A
  these.1 <- c( "Q2A1", "Q2A2", "Q3A", "Q8A" )

  for( i in seq_along( these.1 ) ){

    df.ce[, paste0( these.1[i], "N" ) ] <- ifelse( df.ce[, these.1[i] ] == 0, 0.25,
                                                   ifelse( df.ce[, these.1[i] ] == 1, 0.5,
                                                           ifelse( df.ce[, these.1[i] ] == 2, 1,
                                                                   ifelse( df.ce[, these.1[i] ] == 3, 1.5,
                                                                           ifelse( df.ce[, str_extract( these.1[i], "Q\\d" ) ] == 0, 0, df.ce[, these.1[i] ] ) ) ) ) )
  }


  # Q4A
  df.ce[, paste0( "Q4A", "N" ) ] <- ifelse( df.ce[, "Q4A" ] == 0, 0.2,
                                            ifelse( df.ce[, "Q4A" ] == 1, 0.5,
                                                    ifelse( df.ce[, "Q4A" ] == 2, 0.75,
                                                            ifelse( df.ce[, "Q4A" ] == 3, 1.3,
                                                                    ifelse( df.ce[, "Q4" ] == 0, 0, df.ce[, "Q4A" ] ) ) ) ) )

  # Q5A
  df.ce[, paste0( "Q5A", "N" ) ] <- ifelse( df.ce[, "Q5A" ] == 0, 0.25,
                                            ifelse( df.ce[, "Q5A" ] == 1, 0.75,
                                                    ifelse( df.ce[, "Q5A" ] == 2, 1.2,
                                                            ifelse( df.ce[, "Q5A" ] == 3, 2.0,
                                                                    ifelse( df.ce[, "Q5" ] == 0, 0, df.ce[, "Q5A" ] ) ) ) ) )

  # Q6A
  df.ce[, paste0( "Q6A", "N" ) ] <- ifelse( df.ce[, "Q6A" ] == 0, 0.25,
                                            ifelse( df.ce[, "Q6A" ] == 1, 0.75,
                                                    ifelse( df.ce[, "Q6A" ] == 2, 1.25,
                                                            ifelse( df.ce[, "Q6A" ] == 3, 2.0,
                                                                    ifelse( df.ce[, "Q6" ] == 0, 0, df.ce[, "Q6A" ] ) ) ) ) )

  # Q7A
  df.ce[, paste0( "Q7A", "N" ) ] <- ifelse( df.ce[, "Q7A" ] == 0, 0.25,
                                            ifelse( df.ce[, "Q7A" ] == 1, 0.75,
                                                    ifelse( df.ce[, "Q7A" ] == 2, 1.5,
                                                            ifelse( df.ce[, "Q7A" ] == 3, 2.25,
                                                                    ifelse( df.ce[, "Q7" ] == 0, 0, df.ce[, "Q7A" ] ) ) ) ) )

  # Q9A
  df.ce[, paste0( "Q9A", "N" ) ] <- ifelse( df.ce[, "Q9A" ] == 0, 0.3,
                                            ifelse( df.ce[, "Q9A" ] == 1, 1.0,
                                                    ifelse( df.ce[, "Q9A" ] == 2, 1.6,
                                                            ifelse( df.ce[, "Q9A" ] == 3, 2.25,
                                                                    ifelse( df.ce[, "Q9" ] == 0, 0, df.ce[, "Q9A" ] ) ) ) ) )

  ## --------- End Subsection --------- ##


  ## (2.3) Pyramid cup equivalents of fruit & veg calculation ##

  df.ce <- df.ce %>%
    mutate( JUICE = Q1 * Q1AN,
            FRUITA = Q2 * Q2A1N,
            FRUITB = Q2 * Q2A2N,
            FRUIT = ( FRUITA + FRUITB ) / 2,
            LSALAD = Q3 * Q3AN,
            FRFRY = Q4 * Q4AN,
            WHPOT = Q5 * Q5AN,
            DRBEAN = Q6 * Q6AN,
            OTHVEG = Q7 * Q7AN,
            TOMSAUCE = Q8 * Q8AN,
            VEGSOUP = Q9 * Q9AN,

            # final sum
            frt.veg.ce = rowSums( cbind( JUICE, FRUIT, LSALAD, FRFRY, WHPOT, DRBEAN, OTHVEG, TOMSAUCE, VEGSOUP ),
                               na.rm = T ) ) # note that any NAs in any of the columns are being set to ZERO before summing

  ## --------- End Subsection --------- ##


  ## (2.4) Portion size response conversions (pyramid servings) ##

  df.ps <- df.copy

  # Q1A
  df.ps[, paste0( "Q1A", "N" ) ] <- ifelse( df.ps[, "Q1A" ] == 0, 0.75,
                                              ifelse( df.ps[, "Q1A" ] == 1, 1.33,
                                                      ifelse( df.ps[, "Q1A" ] == 2, 2.17,
                                                              ifelse( df.ps[, "Q1A" ] == 3, 3.33,
                                                                      ifelse( df.ps[, "Q1" ] == 0, 0, df.ps[, "Q1A" ] ) ) ) ) )

  # Q2A1, Q2A2, Q3A, Q8A
  these.2 <- c( "Q2A1", "Q2A2" )

  for( i in seq_along( these.2 ) ){

    df.ps[, paste0( these.2[i], "N" ) ] <- ifelse( df.ps[, these.2[i] ] == 0, 0.75,
                                 ifelse( df.ps[, these.2[i] ] == 1, 1,
                                         ifelse( df.ps[, these.2[i] ] == 2, 2,
                                                 ifelse( df.ps[, these.2[i] ] == 3, 2.5,
                                                         ifelse( df.ps[, str_extract( these.2[i], "Q\\d" ) ] == 0, 0, df.ps[, these.2[i] ] ) ) ) ) )
  }

  # Q3A
  df.ps[, paste0( "Q3A", "N" ) ] <- ifelse( df.ps[, "Q3A" ] == 0, 0.5,
                                            ifelse( df.ps[, "Q3A" ] == 1, 1,
                                                    ifelse( df.ps[, "Q3A" ] == 2, 2,
                                                            ifelse( df.ps[, "Q3A" ] == 3, 3,
                                                                    ifelse( df.ps[, "Q3" ] == 0, 0, df.ps[, "Q3A" ] ) ) ) ) )

  # Q4A
  df.ps[, paste0( "Q4A", "N" ) ] <- ifelse( df.ps[, "Q4A" ] == 0, 1.25,
                                         ifelse( df.ps[, "Q4A" ] == 1, 2.3,
                                                 ifelse( df.ps[, "Q4A" ] == 2, 3.1,
                                                         ifelse( df.ps[, "Q4A" ] == 3, 4.8,
                                                                 ifelse( df.ps[, "Q4" ] == 0, 0, df.ps[, "Q4A" ] ) ) ) ) )

  # Q5A
  df.ps[, paste0( "Q5A", "N" ) ] <- ifelse( df.ps[, "Q5A" ] == 0, 0.8,
                                         ifelse( df.ps[, "Q5A" ] == 1, 1.5,
                                                 ifelse( df.ps[, "Q5A" ] == 2, 2.4,
                                                         ifelse( df.ps[, "Q5A" ] == 3, 3.5,
                                                                 ifelse( df.ps[, "Q5" ] == 0, 0, df.ps[, "Q5A" ] ) ) ) ) )

  # Q6A
  df.ps[, paste0( "Q6A", "N" ) ] <- ifelse( df.ps[, "Q6A" ] == 0, 0.75,
                                         ifelse( df.ps[, "Q6A" ] == 1, 1.5,
                                                 ifelse( df.ps[, "Q6A" ] == 2, 2.5,
                                                         ifelse( df.ps[, "Q6A" ] == 3, 3.5,
                                                                 ifelse( df.ps[, "Q6" ] == 0, 0, df.ps[, "Q6A" ] ) ) ) ) )

  # Q7A
  df.ps[, paste0( "Q7A", "N" ) ] <- ifelse( df.ps[, "Q7A" ] == 0, 0.75,
                                         ifelse( df.ps[, "Q7A" ] == 1, 1.5,
                                                 ifelse( df.ps[, "Q7A" ] == 2, 3,
                                                         ifelse( df.ps[, "Q7A" ] == 3, 4.5,
                                                                 ifelse( df.ps[, "Q7" ] == 0, 0, df.ps[, "Q7A" ] ) ) ) ) )

  # Q8A
  df.ps[, paste0( "Q8A", "N" ) ] <- ifelse( df.ps[, "Q8A" ] == 0, 0.36,
                                            ifelse( df.ps[, "Q8A" ] == 1, 0.72,
                                                    ifelse( df.ps[, "Q8A" ] == 2, 1.45,
                                                            ifelse( df.ps[, "Q8A" ] == 3, 1.7,
                                                                    ifelse( df.ps[, "Q8" ] == 0, 0, df.ps[, "Q8A" ] ) ) ) ) )

  # Q9A
  df.ps[, paste0( "Q9A", "N" ) ] <- ifelse( df.ps[, "Q9A" ] == 0, 0.75,
                                         ifelse( df.ps[, "Q9A" ] == 1, 1.36,
                                                 ifelse( df.ps[, "Q9A" ] == 2, 2.27,
                                                         ifelse( df.ps[, "Q9A" ] == 3, 3.2,
                                                                 ifelse( df.ps[, "Q9" ] == 0, 0, df.ps[, "Q9A" ] ) ) ) ) )

  ## --------- End Subsection --------- ##


  ## (2.5) Pyramid servings of fruit & veg calculation ##

  df.ps <- df.ps %>%
    mutate( JUICE = Q1 * Q1AN,
            FRUITA = Q2 * Q2A1N,
            FRUITB = Q2 * Q2A2N,
            FRUIT = ( FRUITA + FRUITB ) / 2,
            LSALAD = Q3 * Q3AN,
            FRFRY = Q4 * Q4AN,
            WHPOT = Q5 * Q5AN,
            DRBEAN = Q6 * Q6AN,
            OTHVEG = Q7 * Q7AN,
            TOMSAUCE = Q8 * Q8AN,
            VEGSOUP = Q9 * Q9AN,

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

  print( summary( d.out[ ,c( "frt.veg.ce", "frt.veg.ps" )] ) )

  ## --------- End Subsection --------- ##

  return( d.out )

}


