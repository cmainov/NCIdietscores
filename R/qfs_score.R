


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


