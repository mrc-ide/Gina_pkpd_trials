# 02.simulate_data.R
#
# Author: Bob Verity
# Date: 2023-03-31
#
# Purpose:
# Simulates triel data in the same format as observed data, conditional on
# certain model parameters.
#
# ------------------------------------------------------------------

# read in drug concentration data
drug_conc <- readRDS("data/drug_conc_summed.rds")

# read in real trial data
df_trial <- read.csv("data/cisse_data.csv")

# get trial properties
# TODO - might want to drop or change this once you know the format that your simulation function will take
N_control <- df_trial$n_patients[df_trial$treat_arm == 1][1]
N_treat <- df_trial$n_patients[df_trial$treat_arm == 2][1]
max_time <- max(df_trial$time.1)
time_schedule_control <- df_trial %>%
  dplyr::filter(treat_arm == 1) %>%
  pull(time.1)
time_schedule_treat <- df_trial %>%
  dplyr::filter(treat_arm == 2) %>%
  pull(time.1)

# define parameters
FOI <- 0.01 / 24 # FOI of 0.08 is daily prob. infection
min_prob <- 0.05
half_point <- 1
k <- 3 # hill function power parameter

# convert drug concentration to probability of protection
drug_conc <- drug_conc %>%
  mutate(prob_susceptible = min_prob + (1 - min_prob) / (1 + (drug_conc / half_point)^k))

# plot probability of protection
# TODO - once you've summed over age-groups in the previous script, this should no longer be broken down into different colours
ggplot(data = drug_conc, aes(x = time, y = 1 - prob_susceptible, colour = group)) +
  geom_line() + theme_bw() +
  ylim(c(0, 1)) + ylab("Prob. protection")

# simulate the control arm
# TODO - this exponential method is technically correct, but to make more
# general we should just have one function that we use for both control and
# intervention arms
time_infection <- NULL
time_next_infection <- 0
N_control_remaining <- N_control
for (i in 1:1000) {
  time_next_infection <- time_next_infection + rexp(1, rate = N_control_remaining * FOI)
  time_infection <- c(time_infection, time_next_infection)
  N_control_remaining <- N_control_remaining - 1
  if ((N_control_remaining == 0) || (time_next_infection > max_time)) {
    break
  }
}

# count up infections in each time interval
df_sim_control <- df_trial %>%
  dplyr::filter(treat_arm == 1)
df_sim_control$n_patients <- NA
df_sim_control$n_infected <- NA
N_remaining <- N_control
new_infections <- 0
for (i in 1:nrow(df_sim_control)) {
  N_remaining <- N_remaining - new_infections
  time0 <- df_sim_control$time[i]
  time1 <- df_sim_control$time.1[i]
  new_infections <- sum((time_infection > time0) & (time_infection <= time1))
  
  df_sim_control$n_infected[i] <- sum((time_infection > time0) & (time_infection <= time1))
  df_sim_control$n_patients[i] <- N_remaining
}

# plot KM curve
ggplot() + theme_bw() +
  geom_step(aes(x = time.1, y = 1 - n_patients / N_control), data = df_sim_control) +
  ylim(c(0, 1))


