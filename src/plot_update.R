pacman::p_load(tidyverse, here, cmdstanr, posterior, ggpubr, ggridges, ggh4x)

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

# PLOTTING WEIGHTS # 
weight_df <- final_df[, c("agent_type", "true_c", "w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "w_prior[1]", "w_prior[2]", "w_prior[3]", "w_prior[4]", "w_prior[5]")]

weight_df <- weight_df %>% 
    pivot_longer(cols=c("w[1]", "w[2]", "w[3]", "w[4]", "w[5]", "w_prior[1]", "w_prior[2]", "w_prior[3]", "w_prior[4]", "w_prior[5]"), names_to = "w_number", values_to = "w_value") 

# split w_number col 
weight_df$distribution <- ifelse(grepl("prior", weight_df$w_number), "prior", "posterior")
weight_df$w_number <- gsub("_prior", "", weight_df$w_number)

# ensure that distribution is a factor with prior as the first level
weight_df$distribution <- factor(weight_df$distribution, levels = c("prior", "posterior"))

weight_df$true_c <- as.factor(weight_df$true_c)

# change factor levels for agent type 
weight_df$agent_type <- factor(weight_df$agent_type, levels = c("both_good", "one_good", "neutral"))

# add the true weights (based on actual values used in simulate.R)
weight_df <- weight_df %>%
  mutate(true_w = case_when(
    (w_number == "w[1]" | w_number == "w[3]") & agent_type == "both_good" ~ 0.5,
    w_number %in% c("w[2]", "w[4]", "w[5]") & agent_type == "both_good" ~ 0,
    w_number == "w[1]" & agent_type == "one_good" ~ 0.5,
    w_number %in% c("w[2]", "w[3]", "w[4]", "w[5]") & agent_type == "one_good" ~ 0.125,
    agent_type == "neutral" ~ 0.2,
    TRUE ~ NA_real_  # For handling NA if there was
  ))

# set facet labels
agent_type_labs <- c("Eyes-Spots Sensitive", "Eyes Sensitive", "Ignorant")
names(agent_type_labs) <- c("both_good", "one_good", "neutral")

weight_labs <- c("Eyes", "Legs", "Spots", "Arms", "Color")
names(weight_labs) <- c("w[1]", "w[2]", "w[3]", "w[4]", "w[5]")

scaling_labs <- c("Low Scaling", "High Scaling")
names(scaling_labs) <- c("0.3", "5")

plot <- ggplot(weight_df, aes(w_value, fill=distribution)) +
    geom_density(alpha=0.6) +
    labs(x = "Weight Value", fill = "Distribution") +
    # add true val 
    geom_vline(aes(xintercept = true_w), linetype = "dashed", color = "black") +
    # legend values and labels
    scale_fill_manual(values = c("prior" = "lightgrey", "posterior" = "#0f5bea"), labels=c("Prior", "Posterior")) +
    # do all facets 
    facet_nested(agent_type + true_c ~ w_number, switch="y", labeller = labeller(agent_type=agent_type_labs, w_number=weight_labs, true_c=scaling_labs)) + 
    # scale y 
    scale_y_discrete(expand = c(0, 0.10)) +
    # add theme + custom theme elements
    theme_bw() + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
          panel.spacing = unit(1, "lines"), strip.text = element_text(size = 13, face="bold"), strip.background = element_rect(fill="lightgrey"),
          legend.text = element_text(size = 14), legend.title = element_text(size = 14), legend.position = "bottom",
          axis.title.x = element_text(size = 14), axis.title = element_text(size = 14))

# save plot 
ggsave(here::here("plots", "weights_prior_posterior_update.png"), plot, width = 10, height = 10, units = "in", dpi = 300)

subset_df
