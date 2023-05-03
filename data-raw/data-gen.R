require( rvest )
require( tidyverse )

# below creates /data-raw package and lists it in the .Rbuildignore (run before creating this script)
# usethis::use_data_raw( "data" )

### (1.0) Web Scrape for HTML Tables Containing Adjustment factors ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## (1.1) OPEN Multifactor Screener: Web scrape the link below for the HTML tables that have all the conversion factors ##

url <-  "https://epi.grants.cancer.gov/past-initiatives/open/multifactor/scoring.html#scoring"

# webscrape the URL for HTML tables
df.list <- url %>%
  read_html() %>%
  html_nodes( "table" ) %>%
  html_table( fill = T )

## --------- End Subsection --------- ##


## (1.2) Convert food group names to those present in working dataset ##
for( i in 2:length(df.list)){
  if( i %in% 2:5){
    df.list[[i]]$`Food Group` <- ifelse( str_detect(df.list[[i]]$`Food Group`, "juice"), "juice",
                                         ifelse(str_detect(df.list[[i]]$`Food Group`, "cereals"), "cold.cereals",
                                                ifelse(str_detect(df.list[[i]]$`Food Group`, "Bacon"), "bacon.sausage",
                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "dogs"), "hot.dogs",
                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "Fruit"), "fruit",
                                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "dressing"), "regular.fat",
                                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "Salad \\("), "salad",
                                                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "bread"), "bread",
                                                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "Fried"), "potatoes",
                                                                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "white pot"), "white.potatoes",
                                                                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "beans"), "beans",
                                                                                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "vegetable"), "vegetables",
                                                                                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "Pasta"), "pasta",
                                                                                                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "Nuts"), "nuts",
                                                                                                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "2%"), "milk.two",
                                                                                                                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "Chips"), "chips",
                                                                                                                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "1%"), "milk.one",
                                                                                                                                                                       ifelse( str_detect(df.list[[i]]$`Food Group`, "Skim"), "milk.skim",
                                                                                                                                                                               ifelse( str_detect(df.list[[i]]$`Food Group`, "Whole milk"), "milk.whole", df.list[[i]]$`Food Group` ) ))))))))))))))))))
  }

  if( i %in% 6:8){
    df.list[[i]]$Parameter <- ifelse( str_detect(df.list[[i]]$Parameter, "juice"), "juice",
                                      ifelse(str_detect(df.list[[i]]$Parameter, "cereals"), "cold.cereals",
                                             ifelse(str_detect(df.list[[i]]$Parameter, "Bacon"), "bacon.sausage",
                                                    ifelse( str_detect(df.list[[i]]$Parameter, "dogs"), "hot.dogs",
                                                            ifelse( str_detect(df.list[[i]]$Parameter, "Fruit"), "fruit",
                                                                    ifelse( str_detect(df.list[[i]]$Parameter, "dressing"), "regular.fat",
                                                                            ifelse( str_detect(df.list[[i]]$Parameter, "Salad (P3)"), "salad",
                                                                                    ifelse( str_detect(df.list[[i]]$Parameter, "bread"), "bread",
                                                                                            ifelse( str_detect(df.list[[i]]$Parameter, "Fried"), "potatoes",
                                                                                                    ifelse( str_detect(df.list[[i]]$Parameter, "white pot"), "white.potatoes",
                                                                                                            ifelse( str_detect(df.list[[i]]$Parameter, "beans"), "beans",
                                                                                                                    ifelse( str_detect(df.list[[i]]$Parameter, "vegetable"), "vegetables",
                                                                                                                            ifelse( str_detect(df.list[[i]]$Parameter, "Pasta"), "pasta",
                                                                                                                                    ifelse( str_detect(df.list[[i]]$Parameter, "Nuts"), "nuts",
                                                                                                                                            ifelse( str_detect(df.list[[i]]$Parameter, "2%"), "milk.two",
                                                                                                                                                    ifelse( str_detect(df.list[[i]]$Parameter, "Chips"), "chips",
                                                                                                                                                            ifelse( str_detect(df.list[[i]]$Parameter, "1%"), "milk.one",
                                                                                                                                                                    ifelse( str_detect(df.list[[i]]$Parameter, "Skim"), "milk.skim",
                                                                                                                                                                            ifelse( str_detect(df.list[[i]]$Parameter, "Whole milk"), "milk.whole", df.list[[i]]$Parameter ) ))))))))))))))))))

  }
}

## --------- End Subsection --------- ##


## (1.3) Fat Screener: Web scrape the link below for the HTML tables that have all the conversion factors ##

url.2 <-  "https://epi.grants.cancer.gov/diet/screeners/fat/scoring.html"

# webscrape the URL for HTML tables
df.list.2 <- url.2 %>%
  read_html() %>%
  html_nodes( "table" ) %>%
  html_table( fill = T )

## --------- End Subsection --------- ##


## (1.4) Assign each element of the list as a separate object to then add to the .rda file ##

# first, combine the two lists of HTML tables
table.list <- c( df.list, df.list.2 )

# extract all conversion tables from list
for( i in 1:length( table.list ) ){

  assign( paste0( "tbl.", i ), table.list[[i]] )

}

# we will also column bind tables 10 and 11 since they are part of the same table, just split up
tbl.10.11 <- setNames( cbind( tbl.10, tbl.11 ),
                       c( "age", "f3",
                          "f5", "f2", "f8", "f13",
                          "f14", "age.2", "f4", "f7",
                          "f9", "f10", "f11", "reg.fat", "f15" ) ) %>%
  t(.) %>%
  data.frame() %>%
  mutate( fd = rownames( . ))

# column names
c.nm <- tbl.10.11[1,]

# rearrange table into desired format
tbl.10.11 <- rbind( setNames( tbl.10.11[c( 17, 1:8 )],
                              c( "fd", "sex", c.nm[2:8] ) ), setNames( tbl.10.11[c( 17, 9:16 )],
                                                                       c( "fd", "sex", c.nm[10:16] ) ) )


# rename rows of table 13
tbl.13[ , 1 ] <- paste0( c( "intercept", "f2", "f3", "f4", "f5", "f7",
                            "f8", "f9", "f10", "f11", "f13", "f14", "f15", "reg.fat" ), ".a" )

## --------- End Subsection --------- ##


## (1.4) Age list ##

age.lst <- list( c( 0:17 ),
                 c( 18:27 ),
                 c( 28:37 ),
                 c( 38:47 ),
                 c( 48:57 ),
                 c( 58:67 ),
                 c( 68:77 ),
                 c( 78:99 ) )

## --------- End Subsection --------- ##


## (6.2) Input F/V cup equivalent adjustments, from fran.predict.nhis.fortim.cupeq.txt provided by Lisa Kahle 06/15/2007 ##

fvcupadj <- tibble::tribble(
  ~"gender", ~"AgeGrp", ~"FVCAFruit", ~"FVCAFrtJ", ~"FVCAFrFry", ~"FVCAOthPot", ~"FVCASalad", ~"FVCAOthVeg", ~"FVCADrBean",
  1, 1, 0.999580, 1.499160, 0.721125, 1.000400, 0.272700, 0.387675, 0.717550,
  1, 2, 0.933450, 1.250580, 0.727700, 1.140030, 0.353970, 0.473920, 0.551540,
  1, 3, 0.867300, 1.000980, 0.641000, 0.999600, 0.377235, 0.499840, 0.566720,
  1, 4, 0.867300, 1.000980, 0.641000, 0.999600, 0.374963, 0.500240, 0.612360,
  1, 5, 0.867300, 1.000176, 0.548055, 0.999490, 0.416640, 0.499905, 0.500250,
  1, 6, 0.774916, 0.750735, 0.480750, 0.833175, 0.375000, 0.460585, 0.502285,
  1, 7, 0.657060, 0.750735, 0.499980, 0.754400, 0.411323, 0.416899, 0.575360,
  2, 1, 0.749235, 1.124370, 0.509595, 0.782020, 0.306788, 0.364468, 0.492150,
  2, 2, 0.867300, 1.000960, 0.455110, 0.876945, 0.286335, 0.395882, 0.341550,
  2, 3, 0.844838, 1.000176, 0.448700, 0.771260, 0.416625, 0.404303, 0.430530,
  2, 4, 0.789970, 0.938130, 0.448700, 0.771260, 0.499950, 0.408330, 0.345763,
  2, 5, 0.742350, 0.764776, 0.394856, 0.749700, 0.397688, 0.416913, 0.430685,
  2, 6, 0.712640, 0.750728, 0.444260, 0.771260, 0.312469, 0.436560, 0.430530,
  2, 7, 0.620475, 0.750434, 0.444260, 0.644235, 0.374963, 0.452214, 0.500400
)

## --------- End Subsection --------- ##


## (1.6) Generate test data for mfs_scores ##

# set seed for reproducibility
set.seed = 23

# initialize matrix of NAs to store data
diet.data <- matrix( NA, nrow = 45, ncol = 16 )

# loop through the number of columns and generate a discrete random variable of length 45 bounded below at 1 and above at 9
for( i in 1:16 ){

  diet.data[ , i ] <- sample( 1:9, size = 45, replace = TRUE )

}

# coerce to dataframe and set column names according to survey item
diet.data <- setNames( data.frame( diet.data ), paste0( "HQ", 1:16 ) )

# milk type variable
diet.data$HQ2A <- sample( 1:5, size = 45, replace = TRUE )

# sex variable
diet.data$SEX <- sample( 1:2, size = 45, replace = TRUE )

# age variable
diet.data$AGE <- sample( 18:99, size = 45, replace = TRUE )

## --------- End Subsection --------- ##


## (1.7) Generate test data for qfs_scores ##

# set seed for reproducibility
set.seed = 23

# initialize matrix of NAs to store data
short.data <- matrix( NA, nrow = 45, ncol = 15 )

# loop through the number of columns and generate a discrete random variable of length 45 bounded below at 1 and above at 9
for( i in 1:15 ){

  short.data[ , i ] <- sample( 1:8, size = 45, replace = TRUE )

}

# coerce to dataframe and set column names according to survey item
short.data <- setNames( data.frame( short.data ),
                        c( "CEREAL", "SKIMMILK",
                           "EGGS", "SAUSAGE",
                           "MARGBR", "CITJUICE",
                           "FRUIT", "HOTDOG",
                           "CHEESE", "FRIEDPOT",
                           "MARGVEG", "MAYO",
                           "SALDRS", "RICE",
                           "MARGRICE" ) )

# Fat intake questions
short.data$LOFATMRG <- sample( 1:6, size = 45, replace = TRUE )
short.data$ALLFAT <- sample( 1:3, size = 45, replace = TRUE )

# sex variable
short.data$SEX <- sample( 1:2, size = 45, replace = TRUE )

# age variable
short.data$AGE <- sample( 18:99, size = 45, replace = TRUE )

## --------- End Subsection --------- ##


## (1.5) Fruit & Vegetable Day Screener Data ##

# initialize matrix of NAs to store data (first dataset will be for primary questions on the survey (i.e. Q1:10))
fv.data.day.q <- matrix( NA, nrow = 45, ncol = 10 )

# set seed for reproducibility
set.seed = 87

# loop through the number of columns and generate a discrete random variable of length 45 bounded below at 0 and above at 9
for( i in 1:10 ){

  fv.data.day.q[ , i ] <- sample( c(0:9, "M", "E"), size = 45, replace = TRUE ) # "M" and "E" are for missing and error, respectively (see NCI SAS code for this screener)

}


# initialize matrix of NAs to store data (second dataset will be for sub questions on the survey (i.e. Q1A, Q2A...))
fv.data.day.a <- matrix( NA, nrow = 45, ncol = 10 )

# set seed for reproducibility
set.seed = 87

# loop through the number of columns and generate a discrete random variable of length 45 bounded below at 0 and above at 3
for( i in 1:10 ){

  fv.data.day.a[ , i ] <- sample( c(0:3, "M", "E"), size = 45, replace = TRUE ) # "M" and "E" are for missing and error, respectively (see NCI SAS code for this screener)

}

# column bind the two matrices into a single dataset
fv.data.day <- cbind(
  setNames( data.frame( fv.data.day.q ),
                 c( paste0( "Q", c(1:10) ) ) ),
       setNames( data.frame( fv.data.day.a ),
          c( paste0( "Q", c(1,3:9), "A" ), "Q2A1", "Q2A2" ) )
) %>%
  select( order( colnames( . ) ) ) # arrange columns alphabetically by column name


## --------- End Subsection --------- ##


## (1.6) Fruit & Vegetable By-Meal Screener Data ##


# initialize matrix of NAs to store data (first dataset will be for primary questions on the survey (i.e. Q1:10))
fv.data.meal.q <- matrix( NA, nrow = 45, ncol = 14 )

# set seed for reproducibility
set.seed = 81

# loop through the number of columns and generate a discrete random variable of length 45 bounded below at 0 and above at 9
for( i in 1:14 ){

  fv.data.meal.q[ , i ] <- sample( c(0:9, "M", "E"), size = 45, replace = TRUE ) # "M" and "E" are for missing and error, respectively (see NCI SAS code for this screener)

}



# initialize matrix of NAs to store data (second dataset will be for sub questions on the survey (i.e. Q1A, Q2A...))
fv.data.meal.a <- matrix( NA, nrow = 45, ncol = 16 )

# set seed for reproducibility
set.seed = 81

# loop through the number of columns and generate a discrete random variable of length 45 bounded below at 0 and above at 3
for( i in 1:16 ){

  fv.data.meal.a[ , i ] <- sample( c(0:3, "M", "E"), size = 45, replace = TRUE ) # "M" and "E" are for missing and error, respectively (see NCI SAS code for this screener)

}

# column bind the two matrices into a single dataset
fv.data.meal <- cbind(
  setNames( data.frame( fv.data.meal.q ),
            c( paste0( "Q", c(1:14) ) ) ),
  setNames( data.frame( fv.data.meal.a ),
            c( paste0( "Q", c(1,2,3,4,5,7,9,11:13), "A" ),
               paste0( "Q", c(6,8,10), "A1" ),
               paste0( "Q", c(6,8,10), "A2" ) ) )
) %>%
  select( order( colnames( . ) ) ) # arrange columns alphabetically by column name


## --------- End Subsection --------- ##


## (1.6) Save developer data as internal data using `use_data` ##

usethis::use_data( tbl.1, tbl.2,
                   tbl.3, tbl.4,
                   tbl.5, tbl.6,
                   tbl.7, tbl.8,
                   tbl.9, tbl.10,
                   tbl.10.11,
                   tbl.11, tbl.12,
                   tbl.13, age.lst,
                   fvcupadj,
                   internal = TRUE,
                   overwrite = TRUE ) # save each table as internal data to the package

## --------- End Subsection --------- ##


## (1.7) Save example data for export ##

usethis::use_data( diet.data, short.data, fv.data.day, fv.data.meal,
                   overwrite = TRUE ) # this creates the `/data` folder and stores the individual published datasets in that directory

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




