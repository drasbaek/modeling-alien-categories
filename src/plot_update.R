pacman::p_load(tidyverse, here, cmdstanr, posterior, ggpubr, ggridges)

plot_prior_posterior_update <- function(samples, params){
    
    draws_df <- as_draws_df(samples$draws())
    # plot 
    plot <- ggplot(draws_df, aes(x = , y = )) + 
        geom_density_ridges(scale = 0.6)

}

final_df <- data.frame()
selected_types <- c("both_good", "one_good", "neutral")
selected_c <- c(0.3, 5)
for (type in selected_types){
    for (c in selected_c){
        # define file path
        file_path <- here::here("data", "simulated_samples", paste0("samples_", as.character(type), "_c_", as.character(c), ".rds"))
        
        # read samples
        samples <- readRDS(file_path)

        # bind to dataframe
        df <- as_draws_df(samples$draws())

        # add type and c
        df$agent_type <- rep(type, nrow(df))

        df$true_c <- rep(c, nrow(df))

        # add to final df
        final_df <- rbind(final_df, df)
    }
}


subset_df <- final_df[, c("agent_type", "true_c", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]")] 
subset_df <- subset_df %>% 
    pivot_longer(cols=c("w[1]", "w[2]", "w[3]", "w[4]", "w[5]"), names_to = "w_number", values_to = "w_value") 

subset_df$true_c <- as.factor(subset_df$true_c)

# plot with geom ridge
plot <- ggplot(subset_df, aes(x = w_value, y = w_number, fill = w_number)) +
    geom_density_ridges(scale = 0.9, rel_min_height = 0.01) +
    theme_minimal() +
    labs(x = "Weight Value") +
    theme(legend.position = "top") +
    scale_fill_manual(values = c("w[1]" = "#9758C3", "w[2]" = "#E357AC", "w[3]" = "#FF6D8A", "w[4]" = "#FF986B", "w[5]" = "#FFC95C")) +
    facet_grid(~agent_type) + 
    # custom legend title
    guides(fill = guide_legend(title = "Weight Parameter")) +    
    # reduce white space between y axis and first dist
    scale_y_discrete(expand = c(0, 0.10)) +
    theme_bw() + 
    # remove y axis title and axis text
    theme(axis.title.y = element_blank(), axis.text.y = element_blank()) 

# save plot 
ggsave(here::here("plots", "weights_prior_posterior_update.png"), plot, width = 10, height = 10, units = "in", dpi = 300)
