### Load the packages and functions ############################################
################################################################################

# packages
library(tidyverse)
library(purrr)
library(tidyr)
library(imputeTS)
library(MASS)
library(data.table)
library(sf)

# load necessary functions - written in a separate script
source("code/functions.R")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Select the routes to be considered in the analysis #########################
################################################################################

# Both directions to be considered interstates but not for non-interstates 
# I-15 and US-89
routes <- c("0015P", "0015N", "0089") 

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare AADT data ##########################################################
################################################################################

# import raw data
aadt <- read.csv(("data/raw/aadt/AADT_Unrounded.csv"))

# sort by start mile point for each route
aadt <- aadt %>% arrange(ROUTE_NAME, START_ACCU)

# rename some columns
aadt <- aadt %>% rename("LABEL" = "ROUTE_NAME")

# for interstates: use directional AADT (i.e., total/2)
aadt <- aadt %>%
  mutate(across(contains("AADT"), ~if_else(LABEL == "0015PM", ./2, ./1)))
aadt$LABEL[aadt$LABEL == "0015PM"] <- list(c("0015P", "0015N"))
aadt <- aadt %>% unnest(LABEL)

# for non-interstates: keep total AADT
aadt$LABEL[aadt$LABEL == "0089PM"] <- "0089"

# filter data for I-15 and US-89 only
aadt <- aadt %>%
  dplyr::filter(LABEL %in% routes)

# create a column of new start mile points based on return_range function and
# explode rows to each start mile points
aadt <- aadt %>%
  group_by(LABEL) %>%
  rowwise() %>%
  mutate(START = return_range(START_ACCU, END_ACCUM, 0.5)) %>%
  unnest(START)
rm(return_range)

# create a column of end miles
aadt <- aadt %>%
  group_by(LABEL) %>%
  mutate(END = ifelse(row_number() == max(row_number()),
    END_ACCUM, lead(START)
  )) %>%
  ungroup()

# join start and end miles
aadt$RANGE <- as.factor(paste0("(", aadt$START, ",", aadt$END, "]"))

# list of start and end mile points
# to be used to get other data frames to the same range of mile points
bin_breaks <- aadt %>%
  group_by(LABEL) %>%
  summarize(BREAKS = as.list(sort(unique(c(START, END))), collapse = ", ")) 
bin_breaks <- as.list(unstack(bin_breaks, BREAKS~LABEL))
  
# keep the required columns only
aadt <- aadt[, c(
  "LABEL", "AADT2019", "AADT2018", "AADT2017",
  "AADT2016", "START", "END", "RANGE"
)]

# convert aadt of different years to long format
aadt <- melt(setDT(aadt),
  id.vars = c("LABEL", "START", "END", "RANGE"),
  variable.name = c("YEAR")
)

# rename some columns
aadt <- aadt %>% rename("AADT" = "value")

# reformat YEAR column to numeric
aadt$YEAR <- as.numeric(gsub("[a-zA-Z]", "", aadt$YEAR))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare skid data ##########################################################
################################################################################

# skid data is available as a geodatabase

# view all layers in the skid geodatabase
st_layers(dsn = "data/raw/skid/skid.gdb")

# import interstate data from the geodatabase for years 2015 to 2019
skid15_int <- st_read("data/raw/skid/skid.gdb", layer = "Skid2015_Interstate")
skid16_int <- st_read("data/raw/skid/skid.gdb", layer = "Skid2016_Interstate")
skid17_int <- st_read("data/raw/skid/skid.gdb", layer = "Skid2017_Interstate")
skid18_int <- st_read("data/raw/skid/skid.gdb", layer = "Skid2018_Interstate")
skid19_int <- st_read("data/raw/skid/skid.gdb", layer = "Skid2019_Interstate")

# bring interstate data frames to same format
# year
skid15_int <- skid15_int %>% rename("YEAR" = "Year_")
skid16_int <- skid16_int %>% rename("YEAR" = "Year_")
skid17_int <- skid17_int %>% rename("YEAR" = "Year_")
skid18_int <- skid18_int %>% rename("YEAR" = "Year_")
skid19_int <- skid19_int %>% rename("YEAR" = "Year")


# route
skid16_int$Route <- as.numeric(skid16_int$Route)

# label
skid15_int <- skid15_int %>% rename("LABEL" = "Route1")
skid16_int <- skid16_int %>% rename("LABEL" = "Route_1")
skid17_int <- skid17_int %>% rename("LABEL" = "Route_1")
skid18_int <- skid18_int %>% rename("LABEL" = "Route_1")
skid19_int <- skid19_int %>% rename("LABEL" = "Label")

# SN_40
skid15_int <- skid15_int %>% rename("SN_40" = "SN40")

# collection_date
skid15_int$DATE <- as.Date(as.POSIXct(skid15_int$Collection,
  format = "%Y-%m-%d %H:%M:%S"
))
skid16_int$DATE <- as.Date(skid16_int$Collection_Date,
  format = "%B %d, %Y"
)
skid17_int$DATE <- as.Date(skid17_int$Collection_Date,
  format = "%d-%b-%y"
)
skid18_int$DATE <- as.Date(skid18_int$Collection_Date,
  format = "%d-%b-%y"
)
skid19_int$DATE <- as.Date(skid19_int$Collection,
  format = "%d-%b-%y"
)

# drop unnecessary columns
skid15_int$Collection <- NULL
skid16_int$Collection_Date <- NULL
skid17_int$Collection_Date <- NULL
skid17_int$Field10 <- NULL
skid18_int$Collection_Date <- NULL
skid19_int$Collection <- NULL

# bin all interstate data frames
skid_int <- bind_rows(
  skid15_int, skid16_int, skid17_int, skid18_int,
  skid19_int
)
rm(skid15_int)
rm(skid16_int)
rm(skid17_int)
rm(skid18_int)
rm(skid19_int)

# intestate = 1 for all observations
skid_int$Interstate <- 1

# plot interstate data frame
# plot(skid_int)

# import non-interstate data from the geodatabase for years 2015 to 2019
skid15_oth <- st_read("data/raw/skid/skid.gdb",
  layer = "Skid2015_NonInterstate"
)
skid16_oth <- st_read("data/raw/skid/skid.gdb",
  layer = "Skid2016_NonInterstate"
)
skid17_oth <- st_read("data/raw/skid/skid.gdb",
  layer = "Skid2017_NonInterstate"
)
skid18_oth <- st_read("data/raw/skid/skid.gdb",
  layer = "Skid2018_NonInterstate"
)
skid19_oth <- st_read("data/raw/skid/skid.gdb",
  layer = "Skid2019_NonInterstate"
)

# bring interstate data frames to same format
# year
skid15_oth <- skid15_oth %>% rename("YEAR" = "Year_")
skid16_oth <- skid16_oth %>% rename("YEAR" = "Year_")
skid17_oth <- skid17_oth %>% rename("YEAR" = "Year_")
skid18_oth <- skid18_oth %>% rename("YEAR" = "Year_")
skid19_oth <- skid19_oth %>% rename("YEAR" = "Year")


# route
skid16_oth$Route <- as.numeric(gsub("[a-zA-Z]", "", skid16_oth$Route))
skid18_oth$Route <- as.numeric(gsub("[a-zA-Z]", "", skid18_oth$Route))
skid19_oth$Route <- as.numeric(gsub("[a-zA-Z]", "", skid19_oth$Route))

# label
skid15_oth <- skid15_oth %>% rename("LABEL" = "Route1")
skid16_oth <- skid16_oth %>% rename("LABEL" = "Route_1")
skid17_oth <- skid17_oth %>% rename("LABEL" = "Route_1")
skid18_oth <- skid18_oth %>% rename("LABEL" = "Route_1")
skid19_oth <- skid19_oth %>% rename("LABEL" = "Label")

# SN_40
skid15_oth <- skid15_oth %>% rename("SN_40" = "SN40")

# collection_date
skid15_oth$DATE <- as.Date(as.POSIXct(skid15_oth$Collection,
  format = "%Y-%m-%d %H:%M:%S"
))
skid16_oth$DATE <- as.Date(skid16_oth$Collection_Date,
  format = "%B %d, %Y"
)
skid17_oth$DATE <- as.Date(skid17_oth$Collection_Date,
  format = "%d-%b-%y"
)
skid18_oth$DATE <- as.Date(skid18_oth$Collection_Date,
  format = "%d-%b-%y"
)
skid19_oth$DATE <- as.Date(skid19_oth$Collection,
  format = "%d-%b-%y"
)

# non_interstate
skid19_oth <- skid19_oth %>% rename("Non_Interstate" = "Non_Inters")

# drop unnecessary columns
skid15_oth$Collection <- NULL
skid16_oth$Collection_Date <- NULL
skid17_oth$Collection_Date <- NULL
skid17_oth$Field10 <- NULL
skid18_oth$Collection_Date <- NULL
skid19_oth$Collection <- NULL

# bind all interstate data frames
skid_oth <- bind_rows(
  skid15_oth, skid16_oth, skid17_oth, skid18_oth,
  skid19_oth
)
rm(skid15_oth)
rm(skid16_oth)
rm(skid17_oth)
rm(skid18_oth)
rm(skid19_oth)

# non_intestate = 1 for all observations
skid_oth$Non_Interstate <- 1

# plot interstate data frame
# plot(skid_oth)

# merge interstate and non-interstate data
skid <- bind_rows(skid_int, skid_oth)
rm(skid_int)
rm(skid_oth)

# Interstate = 1 for interstates and = 0 for non-interstates
skid$Interstate[is.na(skid$Interstate)] <- 0
skid$Non_Interstate <- NULL

# plot skid data
# plot(skid)

# for non-interstates: aggregate the observations of both direction
skid$LABEL[skid$LABEL == "0089P" | skid$LABEL == "0089N"] <- "0089"

# skid data for selected routes for year 2016-2019
skid <- skid %>%
  dplyr::filter(LABEL %in% routes) %>%
  dplyr::filter(YEAR >= 2016 & YEAR <= 2020)
skid <- data.frame(skid)

# aggregate skid number to required range of mile points for all routes
for (i in 1:length(routes)) {
skid1 <- skid[skid$LABEL == routes[i],] %>%
  group_by(
    RANGE = cut(Mile, breaks = bin_breaks[[routes[i]]], dig.lab = -1),
    YEAR, LABEL
  ) %>%
  summarize(SN = mean(SN_40))
if (i == 1) {skid2 <- skid1} 
  else {skid2 <- rbind(skid2, skid1)}
}
skid <- skid2
rm(i)
rm(skid1)
rm(skid2)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare crash data #########################################################
################################################################################

# import raw data for years 2016 to 2019
crash <- read.csv(("data/raw/crash/Crashes_2016_2019.csv"))

# rename some columns
crash <- crash %>% rename("LABEL" = "Full.Route.Name")
crash <- crash %>% rename("YEAR" = "Year")

# # extract route number from label
# crash$RT_NUM <- str_extract_all(crash$LABEL, "\\d+")

# for non-interstates: aggregate the observations of both direction
crash$LABEL[crash$LABEL == "0089P" | crash$LABEL == "0089N"] <- "0089"

# crash data for selected routes for years 2016-2020
crash <- crash %>%
  dplyr::filter(LABEL %in% routes) %>%
  dplyr::filter(YEAR >= 2016 & YEAR <= 2020)

# aggregate to required range of mile points
for (i in 1:length(routes)) {
  crash1 <- crash[crash$LABEL == routes[i],] %>%
    group_by(
      RANGE = cut(Milepoint, breaks = bin_breaks[[routes[i]]], dig.lab = -1),
      YEAR, LABEL
    ) %>%
    summarize(
      COUNT_TOT = n(),
      COUNT_DRY = length(Crash.ID[Roadway.Surface.Condition == "Dry"]),
      COUNT_WET = length(Crash.ID[Roadway.Surface.Condition != "Dry"]),
      COUNT_PDO = length(Crash.ID[Crash.Severity == "No injury/PDO"]),
      COUNT_INJ = length(Crash.ID[Crash.Severity != "No injury/PDO"])
    )
  if (i == 1) {crash2 <- crash1} 
  else {crash2 <- rbind(crash2, crash1)}
}
rm(i)
crash <- crash2
rm(crash1)
rm(crash2)

# remove  bin breaks
rm(bin_breaks)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Join all data sets #########################################################
################################################################################

# join all data sets
df <- list(aadt, skid, crash) %>%
  reduce(full_join, by = c("RANGE", "YEAR", "LABEL"))
rm(aadt)
rm(skid)
rm(crash)

# remove observations with missing AADT
df <- df[!is.na(df$AADT), ]

# count missing values
sum(is.na(df$AADT))
sum(is.na(df$SN))
sum(is.na(df$COUNT_TOT))

# impute missing SN values by linear interpolation
df <- df %>% arrange(LABEL, YEAR, START)
df$SN <- na_interpolation(df$SN, option = "linear")

# replace missing count values by 0
df <- df %>% mutate(across(starts_with("COUNT_"), ~ ifelse(is.na(.x), 0, .x)))

# calculate length of segments
df$LENGTH <- df$END - df$START

# total crash count per mile
df$COUNT_MILE <- df$COUNT_TOT / df$LENGTH

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Some visualizations ########################################################
################################################################################

# select route
df1 <- df
df <- df1[df1$LABEL != "0089",]

# correlation between SN and crash count
cor(df$SN, df$COUNT_MILE)
plot(df$SN, df$COUNT_MILE)
abline(lm(df$COUNT_MILE ~ df$SN), col = 4, lwd = 3)

# correlation between SN and AADT
cor(df$SN, df$AADT)
plot(df$SN, df$AADT)
abline(lm(df$AADT ~ df$SN), col = 4, lwd = 3)


# correlation between AADT and crash count
cor(df$AADT, df$COUNT_MILE)
plot(df$AADT, df$COUNT_MILE)
abline(lm(df$COUNT_MILE ~ df$AADT), col = 4, lwd = 3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Crash count models #########################################################
################################################################################

# total crashes
# poisson model
poi_tot <- glm(COUNT_TOT ~ SN + log(AADT) + offset(log(LENGTH)),
               data = df,
               family = poisson()
)
summary(poi_tot)

# negative binomial model
nb_tot <- glm.nb(COUNT_TOT ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
summary(nb_tot)

# dry crashes
# poisson model
poi_dry <- glm(COUNT_DRY ~ SN + log(AADT) + offset(log(LENGTH)),
               data = df,
               family = poisson()
)
summary(poi_dry)

# negative binomial model
nb_dry <- glm.nb(COUNT_DRY ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
summary(nb_dry)

# wet crashes
# poisson model
poi_wet <- glm(COUNT_WET ~ SN + log(AADT) + offset(log(LENGTH)),
               data = df,
               family = poisson()
)
summary(poi_wet)

# negative binomial model
nb_wet <- glm.nb(COUNT_WET ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
summary(nb_wet)

# PDO crashes
# poisson model
poi_pdo <- glm(COUNT_PDO ~ SN + log(AADT) + offset(log(LENGTH)),
               data = df,
               family = poisson()
)
summary(poi_pdo)

# negative binomial model
nb_pdo <- glm.nb(COUNT_PDO ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
summary(nb_pdo)

# injury-related/fatal crashes
# poisson model
poi_inj <- glm(COUNT_INJ ~ SN + log(AADT) + offset(log(LENGTH)),
               data = df,
               family = poisson()
)
summary(poi_inj)

# negative binomial model
nb_inj <- glm.nb(COUNT_INJ ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
summary(nb_inj)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Crash modification factors #################################################
################################################################################

# define function that returns CMF
return_cmf <- function(beta, test, base) exp(beta*(test-base))

# create a dataframe of CMF for different crash types
cmf <- data.frame(test_SN = seq(0, 100, 1), base_SN = 40)
cmf <- cmf %>% 
  mutate(tot = return_cmf(nb_tot[["coefficients"]][["SN"]],test_SN, base_SN),
         dry = return_cmf(nb_dry[["coefficients"]][["SN"]],test_SN, base_SN),
         wet = return_cmf(nb_wet[["coefficients"]][["SN"]],test_SN, base_SN),
         pdo = return_cmf(nb_pdo[["coefficients"]][["SN"]],test_SN, base_SN),
         inj = return_cmf(nb_inj[["coefficients"]][["SN"]],test_SN, base_SN))

# convert dataframe to long format
cmf <- melt(setDT(cmf), id.vars = c("test_SN", "base_SN"), variable.name = "cmf")
cmf <- cmf %>% rename("crash_type" = "cmf", "cmf" = "value")

# plot
ggplot(cmf, aes(col = crash_type)) +
  geom_point(aes(test_SN, cmf)) +
  scale_color_brewer(palette = "RdYlBu") + 
  xlab("SN (base = 40)") + 
  ylab("CMF") +
  ggtitle("Skid number crash modification factors for different crash types") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0.2, 2.8), breaks = seq(0.2, 2.8, 0.2)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
    panel.background = element_rect(fill = "grey90"),
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
  )

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

