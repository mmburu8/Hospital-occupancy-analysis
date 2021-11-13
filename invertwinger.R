library(tidyverse)
install.packages("ggbump")
library(ggbump)
admit <- read_csv("final_admission.csv")
invertwing <- admit %>%
  filter((year == 2021) & (indicator == "Sum week hospital occupancy")) %>%
  select(country, Weeks, value, year)
# calculate cummulative sum
invertwing$csum <- ave(invertwing$value, invertwing$country, FUN= cumsum)
invertwing
invertwing_rank <- invertwing %>%
  group_by(Weeks) %>%
  arrange(Weeks, desc(csum), desc(value), country) %>%
  mutate(ranking = row_number(), Weeks = as.numeric(Weeks)) %>%
  as.data.frame()
invertwing_rank <- invertwing_rank %>%
  filter(Weeks > 32)
# visualization
ggplot(data = invertwing_rank, aes(x = Weeks, y = ranking, color = country))+
  geom_bump(size = 1.5)
warnings()

invertwing_rank
