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
half_point <- 1.5
k <- 2 # hill function power parameter

# convert drug concentration to probability of protection
drug_conc <- drug_conc %>%
  dplyr::filter(time %in% seq(0, max(df_trial$time.1), by = 1)) %>% # ensure it's only hourly values so that this matches the input data
  mutate(prob_susceptible = min_prob + (1 - min_prob) / (1 + (drug_conc / half_point)^k))

# plot probability of protection
# ensure that the test parameters show efficacy wanes before subsequent doses
ggplot(data = drug_conc, aes(x = time/24, y = 1 - prob_susceptible)) +
  geom_line() + theme_bw() +
  ylim(c(0, 1)) + ylab("Prob. protection") + 
  geom_hline(aes(yintercept = 1 - min_prob), lty = 2, lwd = 0.4) + 
  geom_vline(aes(xintercept = 28), lty = 2, lwd = 0.4)
  
#######################################################################################
## create a function to simulate test data for an arm of a trial
simulate_trial_arm <- function(real_data, drug_arm, prob_susceptible, hourly_foi) {
  #' input: real data to gain time steps, which treatment arm, vector of hourly probability susceptibilities (drug_conc$prob_susceptible or equivalent)
  #' input ctd: ths is a vector of 1s for the placebo arm and an hourly foi 
  #' process: extract time steps, initialise the number of patients, simulate time to next infections and then determine infections in time int from data
  #' output: a simulated dataframe for that treatment arm with the same data format as the original data
  ## initialise
  treat_data <- real_data %>%
    dplyr::filter(treat_arm == drug_arm)
  max_time <- treat_data$time.1[nrow(treat_data)]
  times <- c(treat_data$time, max_time)
  N_patients <- treat_data$n_patients[1]
  N_remaining <- N_patients
  time_infection <- NULL
  time_next_infection <- 0
  # this loop works fine
  for(i in 1:max_time) {
    time_next_infection <- time_next_infection + rexp(1, rate = N_remaining * hourly_foi[i] * prob_susceptible[i])
    time_infection <- c(time_infection, time_next_infection)
    N_control_remaining <- N_control_remaining - 1
    if ((N_control_remaining == 0) || (time_next_infection > max_time)) {
      break
    }
  }
  # time_infection is a vector of the times of every infection
  N_remaining <- N_patients
  new_infections <- 0
  df_sim <- treat_data
  df_sim$n_patients <- NA
  df_sim$n_infected <- NA
  for (i in 1:nrow(df_sim)) {
    N_remaining <- N_remaining - new_infections
    time0 <- df_sim$time[i]
    time1 <- df_sim$time.1[i]
    new_infections <- sum((time_infection > time0) & (time_infection <= time1))
    ## simulated df
    df_sim$n_infected[i] <- sum((time_infection > time0) & (time_infection <= time1))
    df_sim$n_patients[i] <- N_remaining
    df_sim$treat_arm <- as.factor(drug_arm)
  }
  
  df_sim <- df_sim %>%
    dplyr::select(c(n_patients, n_infected, time, time.1, treat_arm))
  return(df_sim)
}

prob_susceptible_control <- rep(1, max(df_trial$time.1)+1)
prob_susceptible_sp <- drug_conc$prob_susceptible
length(prob_susceptible_control) == length(prob_susceptible_control) # sanity check

## test function
set.seed(1)
# simulate the control arm
df_sim_control <- simulate_trial_arm(real_data = df_trial, drug_arm = 1, 
                                     prob_susceptible = prob_susceptible_control, hourly_foi = hourly_foi)
df_sim_sp <- simulate_trial_arm(real_data = df_trial, drug_arm = 2, 
                                     prob_susceptible = prob_susceptible_sp, hourly_foi = hourly_foi)

# df_sim <- rbind(df_sim_control, df_sim_sp)
# df_sim$treat_arm <- as.factor(df_sim$treat_arm)

# plot KM curve
ggplot() + theme_bw() +
  geom_step(aes(x = time.1/24, y = 1 - n_patients / N_control, col = treat_arm), data = df_sim_control) +
  geom_step(aes(x = time.1/24, y = 1 - n_patients / N_treat, col = treat_arm), data = df_sim_sp) +
  scale_color_discrete() +
  ylim(c(0, 1)) + labs(x = "time", y = "proportion infected")


