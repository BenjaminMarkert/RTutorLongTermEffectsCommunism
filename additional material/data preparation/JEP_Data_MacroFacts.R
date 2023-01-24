library(dplyr)
library(haven)

data <- read_dta("old data/JEP_Data_MacroFacts.dta")

data = data %>% mutate(Country_group = case_when(West == 1 ~ "West",
                                            EU_east == 1 ~ "EU East",
                                            non_EU_east == 1 ~ "Non-EU East"),
                Country_group2 = case_when(West == 1 ~ "West",
                                           East == 1 ~ "East"))
data = data %>%
  select(-c(ID, West, EU_east, non_EU_east, East, Region))


saveRDS(data, file = "JEP_Data_MacroFacts.rds")
