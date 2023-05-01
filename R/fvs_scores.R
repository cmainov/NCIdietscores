


fvs_scores <- function( df,
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
                                           Q9A = "Q9A", Q10 = "Q10" ),
                        age.col = "AGE",
                        sex.col = "SEX" ) {


  ### (1.0) Function Checks ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (1.1) Argument types and entries  ##

  # levels of the diet columns
  def.names <- c( paste0( "Q", 1:10 ), paste0( "Q", c(1,3:9), "A" ), "Q2A1", "Q2A2" ) # default names

  list.item.nms <- c( "CEREAL", "MILK.SKIM", "EGGS", "SAUSAGE", "MARG.BUTTER", "CITRUS.JUICE", "FRUIT", "HOTDOG",
                      "CHEESE", "FRIED.POTATOES", "MARG.BUTTER.ON.VEG", "MAYO",
                      "DRESSING", "RICE", "MARGRICE", "RED.FAT.MARG",
                      "FAT.SUBJECTIVE" )

  ## (1.2) Set "M" and "E" entries to missing ##
  df[ def.names ][ df[ def.names ] == "M" ] <- NA
  df[ def.names ][ df[ def.names ] == "E" ] <- NA

  ## --------- End Subsection --------- ##


  ## (1.3) ensure variable classes are numeric  ##

  # condition: assign an object, `v`, with the column names depending on option for`default.names` argument
  if( default.names ) v <- c( def.names, sex.col, age.col )
  if ( !default.names ) v <- c( unlist( item.names ), sex.col, age.col )

  # execute: run coerce_numeric and loop through all variables required and that matched by the two input arguments
  df <- coerce_numeric( d = df, vars = v )

  ## --------- End Subsection --------- ##


  ### (3.0) Do Unit Conversions to Daily Averages ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ## (3.1) Frequency response conversions ##

  # condition: assign an object with the dietary column names for subsequent loop
  if( default.names )  c.nms <- c( def.names )[ !def.names %in% c( paste0( "Q", c(1,3:9), "A" ), "Q2A1", "Q2A2" ) ]
  if( !default.names ) c.nms <- c( unlist( item.names ) )[ !c( unlist( item.names ) ) %in% c( "Q2A1", "Q2A2" ) ]

  # execute: loop through columns to convert
  for( i in 1:length( c.nms ) ){

    cn <- c.nms[i] # assign column name for each iteration

    # map subject response (from SAS code provided by NCI)
    df[[ cn ]] <- ifelse( df[[ cn ]] == 0, 0, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 1, 0.067, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 2, 0.214, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 3, 0.5, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 4, 0.786, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 5, 1, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 6, 2, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 7, 3, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 8, 4, df[[ cn ]] )
    df[[ cn ]] <- ifelse( df[[ cn ]] == 9, 5, df[[ cn ]] )

  }

  ## --------- End Subsection --------- ##


  ## (3.2) Portion size response conversions ##


  # Q1A
  df[, paste0( "Q1A", "N" ) ] <- ifelse( df[, "Q1A" ] == 0, 0.5,
                                              ifelse( df[, "Q1A" ] == 1, 1,
                                                      ifelse( df[, "Q1A" ] == 2, 1.625,
                                                              ifelse( df[, "Q1A" ] == 3, 2.5,
                                                                      ifelse( df[, "Q1" ] == 0, 0, df[, "Q1A" ] ) ) ) ) )

  # Q2A1, Q2A2, Q3A, Q8A
  these.1 <- c( "Q2A1", "Q2A2", "Q3A", "Q8A" )

  for( i in seq_along( these.1 ) ){

    df[, paste0( these.1[i], "N" ) ] <- ifelse( df[, these.1[i] ] == 0, 0.25,
                                 ifelse( df[, these.1[i] ] == 1, 0.5,
                                         ifelse( df[, these.1[i] ] == 2, 1,
                                                 ifelse( df[, these.1[i] ] == 3, 1.5,
                                                         ifelse( df[, str_extract( these.1[i], "Q\\d" ) ] == 0, 0, df[, these.1[i] ] ) ) ) ) )
  }


}
