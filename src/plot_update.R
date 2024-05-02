pacman::p_load(tidyverse, here, cmdstanr, posterior, ggpubr)

plot_prior_posterior_update <- function(samples, params){
    
    draws_df <- as_draws_df(samples$draws())

    # restructure df


    # plot 
    plot <- ggplot(draws_df, aes(x = , y = )) + 
        geom_density_ridges(scale = 0.6)

}

final_df <- data.frame()
selected_types <- c("both_good", "one_good", "neutral")
selected_c <- c(0.3, 5)
for (type in selected_types){
    for (c in selected_c){
        # read samples
        samples <- readRDS(here::here("data", "simulated_samples", paste0("samples_", type, "_c_", as.character(c), ".rds")))

        # bind to dataframe
        df <- as_draws_df(samples$draws())

        # add type and c
        df$agent_type <- rep(type, nrow(df))

        df$true_c <- rep(c, nrow(df))

        # add to final df
        final_df <- rbind(final_df, df)
    }
}


readRDS(here::here("data", "simulated_samples", "samples_both_good_c_5.rds"))

# rbind
df <- bind_rows(all_samples)


samples <- readRDS(here::here("..", "data", "simulated_samples", "samples_both_good_c_0.3.rds"))
test <- as_draws_df(samples$draws())
print(test["w[1]"])

