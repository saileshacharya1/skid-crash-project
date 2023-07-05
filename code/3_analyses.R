### Load packages/functions and import prepared da##############################
################################################################################

# load packages
library(tidyverse)
library(purrr)
library(tidyr)
library(imputeTS)
library(MASS)
library(data.table)
library(sf)

# load necessary functions - written in a separate script
source("code/1_utils.R")

# import prepared data
df <- readRDS("./data/processed/prepared_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Crash count models - combined for interstates and non-interstates ##########
################################################################################

## total crashes--------
# poisson model
poi_tot <- glm(COUNT_TOT ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df,
  family = poisson()
)
sink("./outputs/models/1.poi_tot.txt")
print(summary(poi_tot))


# negative binomial model
nb_tot <- glm.nb(COUNT_TOT ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
sink("./outputs/models/2.nb_tot.txt")
print(summary(nb_tot))
## --------


## dry crashes--------
# poisson model
poi_dry <- glm(COUNT_DRY ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df,
  family = poisson()
)
sink("./outputs/models/3.poi_dry.txt")
print(summary(poi_dry))

# negative binomial model
nb_dry <- glm.nb(COUNT_DRY ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
sink("./outputs/models/4.nb_dry.txt")
print(summary(nb_dry))
## --------


## wet crashes--------
# poisson model
poi_wet <- glm(COUNT_WET ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df,
  family = poisson()
)
sink("./outputs/models/5.poi_wet.txt")
print(summary(poi_wet))

# negative binomial model
nb_wet <- glm.nb(COUNT_WET ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
sink("./outputs/models/6.nb_wet.txt")
print(summary(nb_wet))
## --------


## PDO crashes--------
# poisson model
poi_pdo <- glm(COUNT_PDO ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df,
  family = poisson()
)
sink("./outputs/models/7.poi_pdo.txt")
print(summary(poi_pdo))

# negative binomial model
nb_pdo <- glm.nb(COUNT_PDO ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
sink("./outputs/models/8.nb_pdo.txt")
print(summary(nb_pdo))
## --------


## injury-related/fatal crashes--------
# poisson model
poi_inj <- glm(COUNT_INJ ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df,
  family = poisson()
)
sink("./outputs/models/9.poi_inj.txt")
print(summary(poi_inj))

# negative binomial model
nb_inj <- glm.nb(COUNT_INJ ~ SN + log(AADT) + offset(log(LENGTH)), data = df)
sink("./outputs/models/10.nb_inj.txt")
print(summary(nb_inj))
closeAllConnections()
## --------

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Crash count models - for interstates #######################################
################################################################################
# only negative binomial models are fitted #

# subset to interstates dataframe
df_15 <- df[df$LABEL == "0015N" | df$LABEL == "0015P", ]

# total crashes
int_tot <- glm.nb(COUNT_TOT ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_15
)
sink("./outputs/models/11.int_tot.txt")
print(summary(int_tot))

# dry crashes
int_dry <- glm.nb(COUNT_DRY ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_15
)
sink("./outputs/models/12.int_dry.txt")
print(summary(int_dry))

# wet crashes
int_wet <- glm.nb(COUNT_WET ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_15
)
sink("./outputs/models/13.int_wet.txt")
print(summary(int_wet))

# PDO crashes
int_pdo <- glm.nb(COUNT_PDO ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_15
)
sink("./outputs/models/14.int_pdo.txt")
print(summary(int_pdo))

# injury-related/fatal crashes
int_inj <- glm.nb(COUNT_INJ ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_15
)
sink("./outputs/models/15.int_inj.txt")
print(summary(int_inj))
closeAllConnections()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Crash count models - for non-interstates ###################################
################################################################################
# only negative binomial models are fitted #

# subset to non-interstates dataframe
df_89 <- df[df$LABEL == "0089", ]

# total crashes
non_tot <- glm.nb(COUNT_TOT ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_89
)
sink("./outputs/models/16.non_tot.txt")
print(summary(non_tot))

# dry crashes
non_dry <- glm.nb(COUNT_DRY ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_89
)
sink("./outputs/models/17.non_dry.txt")
print(summary(int_dry))

# wet crashes
non_wet <- glm.nb(COUNT_WET ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_89
)
sink("./outputs/models/18.non_wet.txt")
print(summary(non_wet))

# PDO crashes
non_pdo <- glm.nb(COUNT_PDO ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_89
)
sink("./outputs/models/19.non_pdo.txt")
print(summary(non_pdo))

# injury-related/fatal crashes
non_inj <- glm.nb(COUNT_INJ ~ SN + log(AADT) + offset(log(LENGTH)),
  data = df_89
)
sink("./outputs/models/20.non_inj.txt")
print(summary(non_inj))
closeAllConnections()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Crash modification factors - combined for interstates and non-interstates ##
################################################################################

# create a dataframe of CMF for different crash types
cmf <- data.frame(test_SN = seq(0, 100, 1), base_SN = 40)
cmf <- cmf %>%
  mutate(
    tot = return_cmf(nb_tot[["coefficients"]][["SN"]], test_SN, base_SN),
    dry = return_cmf(nb_dry[["coefficients"]][["SN"]], test_SN, base_SN),
    wet = return_cmf(nb_wet[["coefficients"]][["SN"]], test_SN, base_SN),
    pdo = return_cmf(nb_pdo[["coefficients"]][["SN"]], test_SN, base_SN),
    inj = return_cmf(nb_inj[["coefficients"]][["SN"]], test_SN, base_SN)
  )

# convert dataframe to long format
cmf <- melt(setDT(cmf),
  id.vars = c("test_SN", "base_SN"),
  variable.name = "cmf"
)
cmf <- cmf %>% rename("crash_type" = "cmf", "cmf" = "value")

# plot
cmf <- ggplot(cmf, aes(col = crash_type)) +
  geom_point(aes(test_SN, cmf)) +
  scale_color_brewer(palette = "RdYlBu") +
  xlab("SN (base = 40)") +
  ylab("CMF") +
  ggtitle("Skid number CMF") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0.2, 2.8), breaks = seq(0.2, 2.8, 0.2)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
    panel.background = element_rect(fill = "grey90"),
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
  )

# save the plot
ggsave(
  filename = "./outputs/plots/1.cmf.jpeg",
  plot = cmf, width = 6.5, height = 8.5, unit = "in", dpi = 1000
)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Crash modification factors - for interstates ###############################
################################################################################

# create a dataframe of CMF for different crash types
cmf <- data.frame(test_SN = seq(0, 100, 1), base_SN = 40)
cmf <- cmf %>%
  mutate(
    tot = return_cmf(int_tot[["coefficients"]][["SN"]], test_SN, base_SN),
    dry = return_cmf(int_dry[["coefficients"]][["SN"]], test_SN, base_SN),
    wet = return_cmf(int_wet[["coefficients"]][["SN"]], test_SN, base_SN),
    pdo = return_cmf(int_pdo[["coefficients"]][["SN"]], test_SN, base_SN),
    inj = return_cmf(int_inj[["coefficients"]][["SN"]], test_SN, base_SN)
  )

# convert dataframe to long format
cmf <- melt(setDT(cmf),
  id.vars = c("test_SN", "base_SN"),
  variable.name = "cmf"
)
cmf <- cmf %>% rename("crash_type" = "cmf", "cmf" = "value")

# plot
cmf <- ggplot(cmf, aes(col = crash_type)) +
  geom_point(aes(test_SN, cmf)) +
  scale_color_brewer(palette = "RdYlBu") +
  xlab("SN (base = 40)") +
  ylab("CMF") +
  ggtitle("Skid number CMF for interstates") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0.2, 2.8), breaks = seq(0.2, 2.8, 0.2)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
    panel.background = element_rect(fill = "grey90"),
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
  )

# save the plot
ggsave(
  filename = "./outputs/plots/2.cmf_interstates.jpeg",
  plot = cmf, width = 6.5, height = 8.5, unit = "in", dpi = 1000
)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Crash modification factors - for non-interstates ###########################
################################################################################

# create a dataframe of CMF for different crash types
cmf <- data.frame(test_SN = seq(0, 100, 1), base_SN = 40)
cmf <- cmf %>%
  mutate(
    tot = return_cmf(non_tot[["coefficients"]][["SN"]], test_SN, base_SN),
    dry = return_cmf(non_dry[["coefficients"]][["SN"]], test_SN, base_SN),
    wet = return_cmf(non_wet[["coefficients"]][["SN"]], test_SN, base_SN),
    pdo = return_cmf(non_pdo[["coefficients"]][["SN"]], test_SN, base_SN),
    inj = return_cmf(non_inj[["coefficients"]][["SN"]], test_SN, base_SN)
  )

# convert dataframe to long format
cmf <- melt(setDT(cmf),
  id.vars = c("test_SN", "base_SN"),
  variable.name = "cmf"
)
cmf <- cmf %>% rename("crash_type" = "cmf", "cmf" = "value")

# plot
cmf <- ggplot(cmf, aes(col = crash_type)) +
  geom_point(aes(test_SN, cmf)) +
  scale_color_brewer(palette = "RdYlBu") +
  xlab("SN (base = 40)") +
  ylab("CMF") +
  ggtitle("Skid number CMF for interstates") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_y_continuous(limits = c(0.2, 2.8), breaks = seq(0.2, 2.8, 0.2)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.25),
    panel.background = element_rect(fill = "grey90"),
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
  )

# save the plot
ggsave(
  filename = "./outputs/plots/3.cmf_non_interstates.jpeg",
  plot = cmf, width = 6.5, height = 8.5, unit = "in", dpi = 1000
)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
