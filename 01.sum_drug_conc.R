# 01.sum_drug_conc.R
#
# Author: Bob Verity
# Date: 2023-03-31
#
# Purpose:
# Reads in raw Gaussian quadrature data and sums to produce drug concentration
# curves in each age-nutrition group.
#
# ------------------------------------------------------------------

library(tidyverse)

# read in raw data
dat_raw <- readRDS("data/quadrature_pk_abd.rds")

# sum data over Gaussian quadrature groups
data_summed <- dat_raw %>%
  dplyr::select(time, drug_value.conc, group, age, weighting, pop_prop) %>%
  group_by(group, time) %>%
  summarise(drug_conc = sum(drug_value.conc * weighting),
            age = age[1],
            pop_prop = pop_prop[1])

# plot the drug concentration over groups
ggplot(data = data_summed, aes(x = time, y = drug_conc, colour = group)) +
  geom_line() + theme_bw()

# read in trial data and get maximum time
df_trial <- read.csv("data/cisse_data.csv")
max_time <- max(df_trial$time.1)

# trim the data to the maximum time that will be needed
data_trimmed <- data_summed %>%
  dplyr::filter(time <= max_time)

# save data to file
if (FALSE) {
  saveRDS(data_trimmed, file = "data/drug_conc_summed.rds")
}
