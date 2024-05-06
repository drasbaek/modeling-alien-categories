pacman::p_load(tidyverse, here)

# load txt
df <- read_csv(here::here("data", "AlienData.txt"))

# filter to only include session 1 (where eyes and spots indicate dangerous), condition 1 (where task was solved alone) and 96 trials (ignoring the stimulus that has pt at the end of their feature string)
df <- df %>% filter(session == 1 & condition == 2 & trial <= 96)

# add a column to indicate whether responses were correct on dangerous condition
df$dangerous_accuracy <- ifelse(df$dangerous == 1 & df$response %in% c(3, 4) | df$dangerous == 0 & df$response %in% c(1, 2), 1, 0)

cumsum_df <- df %>%
  group_by(subject) %>% 
  mutate(cumulative_dangerous_accuracy = cumsum(dangerous_accuracy)/1:n())  %>% 
  ungroup() 
  

grouped_df <- cumsum_df %>%
    group_by(trial) %>%
    summarise(mean_dangerous_accuracy = mean(cumulative_dangerous_accuracy),
              sd_dangerous_accuracy = sd(cumulative_dangerous_accuracy)) 

# do the plot
plot <- ggplot() +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  geom_line(data=cumsum_df, aes(x=trial, y=cumulative_dangerous_accuracy, group = subject), alpha=0.1) +
  geom_smooth(data=cumsum_df, aes(x=trial, y=cumulative_dangerous_accuracy), se=FALSE, color="#0f5bea") +
  geom_ribbon(data=grouped_df, aes(x=trial, y=mean_dangerous_accuracy, ymin = mean_dangerous_accuracy - sd_dangerous_accuracy, ymax = mean_dangerous_accuracy + sd_dangerous_accuracy), alpha=0.1) +
  labs(x="Trial", y="Cumulative Accuracy") +
  coord_cartesian(xlim = c(1, 96), ylim = c(0.20, 1)) +
  theme_bw()

# save
ggsave(here::here("plots", "alien_data_cumulative.png"), plot, width=10, height=6, units="in", dpi=300)

# subset by five subjects
subset_df <- cumsum_df %>% 
  filter(subject %in% c(3, 10, 14, 15, 19))

subset_df$subject <- as.factor(subset_df$subject)

# do the plot but without grouped_df 
plot <- ggplot(data=subset_df, aes(x=trial, y=cumulative_dangerous_accuracy, color = subject)) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  geom_line(size=1) +
  scale_color_brewer(palette="Paired") +
  labs(x="Trial", y="Cumulative Accuracy") +
  coord_cartesian(xlim = c(1, 96), ylim = c(0.20, 1)) +
  theme_bw()+ 
  # custom legend title 
  theme(legend.position = "bottom", legend.text = element_text(size = 14), legend.title = element_blank(), axis.title.x = element_text(size = 14), axis.title = element_text(size = 14), 
        panel.spacing = unit(1, "lines"), strip.text = element_text(size = 13, face="bold"), strip.background = element_rect(fill="lightgrey"))

ggsave(here::here("plots", "performance_subset_participants.png"), plot, width=10, height=6, units="in", dpi=300)