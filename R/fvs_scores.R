


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


  ### (3.0) Do Unit Conversions to Daily Averages ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------


  ## (3.1) Map responses to daily averages  ##

  # condition: assign an object with the dietary column names for subsequent loop
  if( default.names )  c.nms <- c( def.names )[ !def.names %in% c( paste0( "Q", c(1,3:9), "A" ), "Q2A1", "Q2A2" ) ]
  if( !default.names ) c.nms <- c( unlist( item.names ) )[ !c( unlist( item.names ) ) %in% c( "Q2A1", "Q2A2" ) ]

  # execute: loop through columns to convert
  for( i in 1:length( c.nms ) ){

    cn <- c.nms[i] # assign column name for each iteration

    # map subject response to daily average (from Table 2-1 in the screener documentation)
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
