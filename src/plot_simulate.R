pacman::p_load(tidyverse, here)

# load the data
file_path <- here::here("data", "simulated_data.csv")
simulated_data <- read.csv(file_path)

# calculate cumulative accuracy for each agent in dataframe (denoted by agent number)


# calculate cumulative accuracy within each agent number
grouped_data <- simulated_data %>%
  group_by(agent_types, agent_number) %>% 
  mutate(cumulative_accuracy = cumsum(accuracy)/1:n())  %>% 
  ungroup()

# take means
grouped_data <- grouped_data %>%
  group_by(agent_types, trial) %>%
  summarise(mean_accuracy = mean(accuracy), mean_cumulative_accuracy = mean(cumulative_accuracy), sd_cum_acc = sd(cumulative_accuracy))

# plot the data
plot <- ggplot(grouped_data, aes(x=trial, y=mean_cumulative_accuracy, color=agent_types)) +
  geom_line() +
  geom_point() +
  labs(title="Simulated Data", x="Trial", y="Accuracy") +
  # add shaded area for standard deviation
  geom_ribbon(aes(ymin=mean_cumulative_accuracy-sd_cum_acc, ymax=mean_cumulative_accuracy+sd_cum_acc), alpha=0.2) +

  # facet wrap, put all plots side by side
  facet_wrap(~agent_types) +
  
  # add lim to y
  ylim(0.20, 0.80) +

  theme_bw()

# save 
ggsave(here::here("plots", "simulated_data.png"), plot, width=10, height=6, units="in", dpi=300)
