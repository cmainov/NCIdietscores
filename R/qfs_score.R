


qfs_scores <- function( df,
                        default.names = TRUE,
                        item.names = list( CEREAL = "CEREAL", MILK.SKIM = "SKIMMILK",
                                           EGGS = "EGGS", SAUSAGE = "SAUSAGE",
                                           MARG.BUTTER = "MARGBR", CITRUS.JUICE = "CITJUICE",
                                           FRUIT = "FRUIT", HOTDOG = "HOTDOG",
                                           CHEESE = "CHEESE", FRIED.POTATOES = "FRIEDPOT",
                                           MARG.BUTTER.ON.VEG = "MARGVEG", MAYO = "MAYO",
                                           DRESSING = "SALDRS", RICE = "RICE",
                                           MARG.BUTTER.ON.RICE = "MARGRICE", RED.FAT.MARG = "LOFATMRG",
                                           FAT.SUBJECTIVE = "ALLFAT" ),
                        age.col = "AGE",
                        sex.col = "SEX" ) {


  ### (1.0) Function Checks ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  ## (1.1) Argument types and entries  ##

  # class checks
  if ( class( item.names ) != "list" ) stop( "Error: `item.names` must be a list" )
  if ( sum( class( df ) %notin% c( "data.frame", "tbl", "tbl_df" ) ) >= 1 ) stop( "Error: `df` must be an object of class `data.frame` or `tibble`." )
  if ( class( age.col ) != "character" | class( sex.col ) != "character" ) stop( "Error: `age.col` and `sex.col` must be objects of class `character`." )

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
  def.names <- c( "CEREAL", "SKIMMILK", "EGGS", "SAUSAGE", "MARGBR", "CITJUICE", "FRUIT", "HOTDOG",
                  "CHEESE", "FRIEDPOT", "MARGVEG", "MAYO", "SALDRS", "RICE", "MARGRICE", "LOFATMRG",
                  "ALLFAT " ) # default names

  if ( default.names ) v <- c( def.names, sex.col, age.col )
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




  ## (3.1) Map responses to daily averages  ##

  # condition: assign an object with the dietary column names for subsequent loop
  if( default.names )  c.nms <- c( def.names )
  if( !default.names ) c.nms <- c( unlist( item.names ) )

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
              f3 = item.names[["SKIMMILK"]],
              f4 = item.names[["EGGS"]],
              f5 = item.names[["SAUSAGE"]],
              f6 = item.names[["MARGBR"]],
              f7 = item.names[["CITJUICE"]],
              f8 = item.names[["FRUIT"]],
              f9 = item.names[["HOTDOG"]],
              f10 = item.names[["CHEESE"]],
              f11 = item.names[["FRIEDPOT"]],
              f12 = item.names[["MARGVEG"]],
              f13 = item.names[["MAYO"]],
              f14 = item.names[["SALDRS"]],
              f15 = item.names[["RICE"]],
              f16 = item.names[["MARGRICE"]],
              lo.fat.mrg = f16 = item.names[["LOFATMRG"]]

              )
  }

  ## Fat amounts ##
  df <- df %>%
    mutate( tot.fat = f6 + f12 + f16,
            fat.real = ifelse( lo.fat.mrg %in% c( 1, 2), 1,
                               ifelse( lo.fat.mrg == 3, 0.75,
                                       ifelse( lo.fat.mrg == 4, 0.5,
                                               ifelse( lo.fat.mrg == 5, 0.25,
                                                       ifelse( lo.fat.mrg == 6, 0, NA ))))),
            diet.fat = ifelse( lo.fat.mrg %in% c( 1, 2), 1,
                               ifelse( lo.fat.mrg == 3, 0.25,
                                       ifelse( lo.fat.mrg == 4, 0.5,
                                               ifelse( lo.fat.mrg == 5, 0.75,
                                                       ifelse( lo.fat.mrg == 6, 1, NA ))))),
            reg.fat = fat.real*tot.fat
    )


  ### Estimate % Energy from Fat ###








