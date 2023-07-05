### Load packages and import prepared data #####################################
################################################################################

# load packages
library(tidyverse)
library(purrr)
library(tidyr)
library(imputeTS)
library(MASS)
library(data.table)
library(sf)

# import prepared data
df <- readRDS("./data/processed/prepared_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
### Some visualizations ########################################################
################################################################################

# select route: I-15
df_15 <- df[df$LABEL == "0015N" | df$LABEL == "0015P", ]

# correlation between SN and crash count
cor(df_15$SN, df_15$COUNT_MILE)
plot(df_15$SN, df_15$COUNT_MILE)
abline(lm(df_15$COUNT_MILE ~ df_15$SN), col = 4, lwd = 3)

# correlation between SN and AADT
cor(df_15$SN, df_15$AADT)
plot(df_15$SN, df_15$AADT)
abline(lm(df_15$AADT ~ df_15$SN), col = 4, lwd = 3)

# correlation between AADT and crash count
cor(df_15$AADT, df_15$COUNT_MILE)
plot(df_15$AADT, df_15$COUNT_MILE)
abline(lm(df_15$COUNT_MILE ~ df_15$AADT), col = 4, lwd = 3)

# select route: US-89
df_89 <- df[df$LABEL == "0089", ]

# correlation between SN and crash count
cor(df_89$SN, df_89$COUNT_MILE)
plot(df_89$SN, df_89$COUNT_MILE)
abline(lm(df_89$COUNT_MILE ~ df_89$SN), col = 4, lwd = 3)

# correlation between SN and AADT
cor(df_89$SN, df_89$AADT)
plot(df_89$SN, df_89$AADT)
abline(lm(df_89$AADT ~ df_89$SN), col = 4, lwd = 3)

# correlation between AADT and crash count
cor(df_89$AADT, df_89$COUNT_MILE)
plot(df_89$AADT, df_89$COUNT_MILE)
abline(lm(df_89$COUNT_MILE ~ df_89$AADT), col = 4, lwd = 3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
