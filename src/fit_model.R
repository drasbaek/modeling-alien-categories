# fit model script
pacman::p_load(tidyverse, here, cmdstanr, rstan, posterior, brms)

fit_model <- function(df, stan_filepath, ){
    

    # prepare the data
    data <- list("ntrials" = length(df$trial),
                 "nfeatures" = 
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