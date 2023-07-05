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
cmf <- melt(setDT(cmf), id.vars = c("test_SN", "base_SN"),
  variable.name = "cmf")
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