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
library(ggplot2)

# import prepared data
df <- readRDS("./data/processed/prepared_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Some visualizations ########################################################
################################################################################

## I-15--------
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
## --------


## I-15--------
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
## --------

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Summary of crashes #########################################################
################################################################################

# prepare a summary table
crash <- df %>%
  group_by(LABEL, YEAR) %>%
  summarize(
    dry = sum(COUNT_DRY),
    wet = sum(COUNT_WET),
    pdo = sum(COUNT_PDO),
    inj = sum(COUNT_INJ),
    tot = sum(COUNT_TOT),
  )

# reshape data to long format
crash <- gather(crash, key = crash_type, value = count, dry:tot, na.rm = TRUE)

# factor with the desired order of levels
crash$crash_type <- factor(crash$crash_type,
  levels = c("wet", "inj", "pdo", "dry", "tot"),
  labels = c(
    "Wet",
    "Injury-related",
    "Property damage only",
    "Dry",
    "All-type"
  )
)

# rename highway labels
crash$LABEL <- ifelse(crash$LABEL == "0015P", "(a) I-15 NB",
  ifelse(crash$LABEL == "0015N", "(b) I-15 SB",
    ifelse(crash$LABEL == "0089", "(c) US-89", crash$LABEL)
  )
)

# create the plot
plot <- ggplot(crash, aes(x = YEAR, y = count, fill = crash_type)) +
  geom_bar(
    position = position_dodge(width = 0.8),
    stat = "identity", width = 0.7
  ) +
  facet_wrap(~LABEL, ncol = 1, scales = "free_x", strip.position = "bottom") +
  labs(x = "", y = "# of crashes", fill = "Crash type:") +
  scale_fill_manual(values = c(
    "Dry" = "blue",
    "Wet" = "green",
    "Property damage only" = "orange",
    "Injury-related" = "red",
    "All-type" = "gray"
  )) +
  scale_x_continuous(
    breaks = c(2016, 2017, 2018, 2019),
    expand = c(0.01, 0.01)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 10),
    strip.text = element_text(face = "bold", size = 12),
    strip.placement = "outside",
    legend.position = "top",
    legend.direction = "horizontal",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
  )

# save the plot
ggsave(
  filename = "./outputs/plots/4.crash_statistics.jpeg",
  plot = plot, width = 6.5, height = 6.5, unit = "in", dpi = 2000
)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
