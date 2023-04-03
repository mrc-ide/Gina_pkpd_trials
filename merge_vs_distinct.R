# merge_vs_distinct.R
#
# Author: Bob Verity
# Date: 2023-04-03
#
# Purpose:
# Explore by simulation whether two approaches to modelling structured survival
# are the same.
#
# Method1: merge groups
# We have a population of size N in which individuals are split into two groups
# - a fast-decaying group (group1) and a slow-decaying group (group2. Every
# individual has an independent probability p_group1 of being in group1, but we
# don't ever draw from this probability, i.e. we don't know which group
# individuals fall in. Instead, we do a weighted sum over the survival curves,
# so the probability of being susceptible (i.e. alive) over time is given by
# p_group1*exp(-lambda_1*t) + (1 - p_group1)*exp(-lambda_2*t). We draw
# binomially from this probability at the first time point, and for the
# surviving individuals we calculate the probability of decaying by time t2
# *conditional* on being alive at time t1, and draw from this binomial
# probability.
#
# Method2: distinct groups
# Unlike method1, we draw the group of every individual at the start. We then
# draw from a simple Poisson process for individuals in each group given their
# group-specific rates. The total number alive at each time point is the sum
# over the two groups.
#
# Conclusion:
# The two process appear to be identical, at least by visual inspection based on
# simulations.
#
# ------------------------------------------------------------------

library(tidyverse)
set.seed(1)

# define two groups. Group one is "fast", group 2 is "slow". Also set the total
# sample size and the probability of being in either of these groups
lambda_1 <- 1
lambda_2 <- 0.1
N <- 100
p_group1 <- 0.25

# set the time at which we make the first and second samples
t1 <- 2
t2 <- 4

# set the number of draws we will make from the process
reps <- 1e4

#################################
#### METHOD 1 - MERGE GROUPS ####
#################################

# calculate the probability of still being susceptible at times t1 and t2
p_sus_t1 <- p_group1*exp(-lambda_1*t1) + (1 - p_group1)*exp(-lambda_2*t1)
p_sus_t2 <- p_group1*exp(-lambda_1*t2) + (1 - p_group1)*exp(-lambda_2*t2)

# calculate the probability of being susceptible at time t2 as a proportion of
# those who were susceptible at time t1
p_sus_t2_conditional <- p_sus_t2 / p_sus_t1

# draw the number of susceptibles at time t1, and the number still susceptible
# at time t2
n1 <- rbinom(reps, N, p_sus_t1)
n2 <- rbinom(reps, n1, p_sus_t2_conditional)


####################################
#### METHOD 1 - DISTINCT GROUPS ####
####################################

# calculate the probability of being susceptible by time t1 in each of the
# groups
p_sus_t1_g1 <- exp(-lambda_1*t1)
p_sus_t1_g2 <- exp(-lambda_2*t1)

# calculate the probability of remaining susceptible between times t1 and t2 in
# each of the groups
p_sus_t2_g1 <- exp(-lambda_1*(t2 - t1))
p_sus_t2_g2 <- exp(-lambda_2*(t2 - t1))

# draw the number of individuals in each group
n_g1 <- rbinom(reps, N, p_group1)
n_g2 <- N - n_g1

# draw the number of susceptibles at time t1 in each group, and at time t2
n1_g1 <- rbinom(reps, n_g1, p_sus_t1_g1)
n1_g2 <- rbinom(reps, n_g2, p_sus_t1_g2)
n2_g1 <- rbinom(reps, n1_g1, p_sus_t2_g1)
n2_g2 <- rbinom(reps, n1_g2, p_sus_t2_g2)

# combine groups
n1B <- n1_g1 + n1_g2
n2B <- n2_g1 + n2_g2


#########################
#### COMPARE RESULTS ####
#########################

# get results in long format
df_long <- rbind(data.frame(method = "Merged", n1 = n1, n2 = n2),
                 data.frame(method = "Distinct", n1 = n1B, n2 = n2B))

# scatterplot results
df_long %>%
  ggplot() + theme_bw() +
  geom_point(aes(x = n1, y = n2), col = "#00000010") +
  facet_wrap(~method)

df_long %>%
  ggplot() + theme_bw() +
  geom_point(aes(x = n1, y = n2, col = method, size = method)) +
  scale_size_manual(values = c(1,2))
