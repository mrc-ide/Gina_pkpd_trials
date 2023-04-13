## TODO: code that converts the raw kaplan-meier into the data format we desire
km_raw <- read.csv("data/cisse_km_raw.csv")

km_to_trial <- function(km_data, drug_arm, N_patients) {
  #' input: raw data, treatment arm (0 = placebo, 1 = drug) and initial members of patients
  #' process: converts from the raw and manually cleaned kaplan meier data into a format used by the mcmc
  #' output: a dataframe with the data in the exact form that the mcmc requires
  
  df_km <- km_data %>%
    dplyr::filter(treat_arm == drug_arm) %>%
    dplyr::mutate(hour = week * 7 * 24) %>%
    dplyr::mutate(cum_infected = round(frac_inf*N_patients)) 
  
  df <- data.frame(n_patients = rep(N_patients, length(unique(df_km$week))-1))
  df$n_inf <- rep(0, nrow(df))
  df$time <- rep(0, nrow(df))
  df$time.1 <- rep(0, nrow(df))
  df$treat_arm <- rep(drug_arm, nrow(df))
  
  time_vec <- round(unique(df_km$hour))
  
  for(i in 1:(nrow(df))) {
    df$time[i] <- time_vec[i]
    df$time.1[i] <- time_vec[i+1]
    df$cumu_inf[i] <- df_km$cum_infected[round(df_km$hour) == df$time.1[i]][1]
  }
  
  df$n_inf[1] <- df$cumu_inf[1]
  for(i in 2:nrow(df)) {
    df$n_inf[i] <- df$cumu_inf[i] - df$cumu_inf[i-1]
    df$n_patients[i] <- df$n_patients[i-1]-df$n_inf[i-1]
  }
  df <- df %>%
    dplyr::select(n_patients, n_inf, time, time.1, treat_arm)
  return(df)
}

placebo <- km_to_trial(km_raw, drug_arm = 0, N_patients = 542)
sp <- km_to_trial(km_raw, drug_arm = 1, N_patients = 546)

trial <- rbind(placebo, sp)

write.csv(trial, "data/cisse_data.csv")
