library(tidyverse)
# read csv
admit <- read.csv("final_admission.csv")
# filter to get 2020 and Sum week hospital occupancy
medecin <- admit %>%
  filter(year == 2020 & indicator == "Sum week ICU occupancy")
# group by country and sum by value
(
  total_country <- medecin %>%
    group_by(month) %>%
    mutate(tot_victim = sum(value)) %>%
    arrange(desc(tot_victim))
)
top_five <- total_country %>%
  filter(
    country == "France" |
      country == "Italy" |
      country == "Germany" |
      country == "Spain" |
      country == "Romania"
  )
top_five
# visualization
ggplot(data = top_five) +
  geom_smooth(mapping = aes(x = month, y = tot_victim, color = country), se = FALSE)+
  ggtitle("Line graph of number of people in ICU weekly due to COVID-19 infections") + 
  xlab("Weeks in 2020") + ylab("Number of patients in ICU")
