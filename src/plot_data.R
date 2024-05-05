pacman::p_load(tidyverse, here)

# load txt
df <- read_csv(here::here("data", "AlienData.txt"))

# filter to only include session 1 (where eyes and spots indicate dangerous) and condition 2 (where task was solved alone)
df <- df %>% filter(session == 1 & condition == 2)

df

print(colnames(df))

print(df)
