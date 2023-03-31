
# NOTE
# THIS SCRIPT IS JUST A PLACEHOLDER AT THE MOMEMT. WILL EVENTIALLY HOLD CODE FOR RUNNING MCMC

# install a specific version of drjacoby
#devtools::install_github("mrc-ide/drjacoby", ref = "version1.3")
library(drjacoby)

# define data
df_data <- read.csv("data/cisse_data.csv")

# define parameters dataframe
df_params <- define_params(name = "mu", min = 0, max = 1e3,
                           name = "sigma", min = 0, max = Inf)

# define MCMC parameters
burnin <- 1e2
samples <- 1e3
chains <- 1

# source C++ likelihood and prior
Rcpp::sourceCpp("src/pkpd_mod1.cpp")

# run MCMC
mcmc <- run_mcmc(data = as.list(df_data),
                 df_params = df_params,
                 misc = list(),
                 loglike = "loglike",
                 logprior = "logprior",
                 burnin = burnin,
                 samples = samples,
                 chains = chains)

plot_par(mcmc, show = "mu")

