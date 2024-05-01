# fit model script
pacman::p_load(tidyverse, here, cmdstanr, rstan, posterior, brms)

fit_model <- function(df, stan_filepath){
    

    # prepare the data
    data <- list("ntrials" = length(df$trial),
                 "nfeatures" = 5, 
                 "dangerous" = df$dangerous,
                 "choices" = df$choices, 
                 "stimuli" = df[,c("eyes", "legs", "spots", "arms", "color")]     
                 "w_prior_values" = c(1, 1, 1, 1, 1), # consider changing
                 "c_prior_values" = c(0, 1) # consider changing          
                                  )
    # compile the model
    model <- cmdstan_model(stan_filepath, cpp_options = list(stan_threads = TRUE))
    
    # fit the model
    samples <- model$sample(
        data = data,
        seed = 420,
        threads_per_chain = 4,
        iter_warmup = 1000,
        iter_sampling = 2000,
        refresh = 500,
        max_treedepth = 10,
        adapt_delta = 0.99
    )
    
    return(samples)
}

# stan file path
stan_filepath <- here::here("stan", "GCM.stan")

# read data
df <- read_csv(here::here("data", "simulated_data.csv"))

# subset the data to just a single simulation
df <- df %>% filter(agent_number == 1 & agent_types == "both_good")

# fit model
samples <- fit_model(df, here::here("stan", "GCM.stan"))

