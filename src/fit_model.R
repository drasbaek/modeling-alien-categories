# fit model script
pacman::p_load(tidyverse, here, cmdstanr, rstan, posterior, brms, stringr)

fit_model <- function(df, stan_filepath){
    
    # prepare the data
    data <- list("ntrials" = length(df$trial),
                 "nfeatures" = 5, 
                 "dangerous" = df$dangerous,
                 "choices" = df$choices, 
                 "stimuli" = as.matrix(df[,c("eyes", "legs", "spots", "arms", "color")]),    
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

### SIMULATED DATA ### 
# read data
df <- read_csv(here::here("data", "simulated_data.csv"))

# make six subsets of the data (two agent_type = both_good, one_good, neutral)
selected_types <- c("both_good", "one_good", "neutral")
selected_c <- c(0.5, 4)

# fit model for each subset
for (type in selected_types){
    for (c in selected_c){
        # subset the data (always take the first agent)
        df_subset <- df %>% filter(agent_number == 1 & agent_types == type & true_c == c)
        
        # fit model
        samples <- fit_model(df_subset, stan_filepath)
        
        # save samples
        file_path <- here::here("data", "simulated_samples", paste0("samples_", type, "_c_", c, ".rds"))
        samples$save_object(file_path)
    }
}


### ALIEN DATA ###
# read csv 
real_df <- read_csv(here::here("data", "AlienData.txt"))

# filter to only include session 1 (where eyes and spots indicate dangerous), condition 2 (where task was solved alone) and 96 trials (ignoring the stimulus that has pt at the end of their feature string)
real_df <- real_df %>% filter(session == 1 & condition == 2 & trial <= 96)

# rm .jpg 
real_df$stimulus <- gsub(".jpg", "", real_df$stimulus)

split_columns <- str_split_fixed(real_df$stimulus, "", 5)

# make into df
split_df <- as.data.frame(split_columns)

colnames(split_df) <- c("eyes", "legs", "spots", "arms", "color")

# bind to real_df
real_df <- cbind(real_df, split_df)

# derive choices
real_df$choices <- ifelse(real_df$response %in% c(3, 4), 1, 0)

# fit model
n_subjects <- length(unique(real_df$subject))
for (subject in 1:n_subjects){
    # subset the data
    df_subset <- real_df %>% filter(subject == subject)
    
    # fit model
    samples <- fit_model(df_subset, stan_filepath)
    
    # save samples
    file_path <- here::here("data", "real_samples", paste0("samples_subject_", subject, ".rds"))
    samples$save_object(file_path)
}
