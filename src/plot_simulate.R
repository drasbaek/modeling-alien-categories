pacman::p_load(tidyverse, here)

# load the data
file_path <- here::here("data", "simulated_data.csv")
simulated_data <- read.csv(file_path)

# calculate mean accuracy across agent type, trial and c
grouped_data <- simulated_data %>%
  group_by(agent_types, trial, true_c) %>%
  summarise(mean_accuracy = mean(accuracy), sd_accuracy = sd(accuracy))

# plot the data
plot <- ggplot(grouped_data, aes(x=trial, y=mean_accuracy, color=agent_types)) +
  geom_smooth() +
  labs(title="Simulated Data", x="Trial", y="Mean Accuracy") +

  # facet wrap, put all plots side by side
  facet_wrap(~true_c) +
  
  # add lim to y
  ylim(0.20, 0.80) +

  theme_bw()

# save 
ggsave(here::here("plots", "simulated_data.png"), plot, width=10, height=6, units="in", dpi=300)
