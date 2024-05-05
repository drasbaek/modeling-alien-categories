pacman::p_load(tidyverse, here)

# load the data
file_path <- here::here("data", "simulated_data.csv")
simulated_data <- read.csv(file_path)

# calculate mean accuracy across agent type, trial and c
grouped_data <- simulated_data %>%
  group_by(agent_types, trial, true_c) %>%
  summarise(mean_accuracy = mean(accuracy), sd_accuracy = sd(accuracy))

grouped_data$agent_types <- factor(grouped_data$agent_types, levels = c("both_good", "one_good", "neutral"))
agent_type_labs <- c("Eyes-Spots Sensitive", "Eyes Sensitive", "Ignorant")

# plot the data
plot <- ggplot(grouped_data, aes(x=trial, y=mean_accuracy, color=agent_types)) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  geom_smooth(se=FALSE) +
  labs(x="Trial", y="Mean Accuracy") +

  # scale fill discrete
  scale_color_discrete(name = "Weights", labels = agent_type_labs) +
  # facet wrap, put all plots side by side
  facet_wrap(~true_c) +
  
  # add lim to y
  ylim(0.20, 0.80) +

  theme_bw()+ 
  theme(legend.position = "bottom", legend.text = element_text(size = 14), legend.title = element_blank(), axis.title.x = element_text(size = 14), axis.title = element_text(size = 14), 
        panel.spacing = unit(1, "lines"), strip.text = element_text(size = 13, face="bold"), strip.background = element_rect(fill="lightgrey"))

# save 
ggsave(here::here("plots", "simulated_data.png"), plot, width=10, height=7, units="in", dpi=300)
