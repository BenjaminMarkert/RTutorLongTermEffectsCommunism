library(dplyr)
library(haven)

data <- read_dta("old data/JEP_Preferences_Data.dta")

data = data %>%
  mutate(cohort = case_when(birthyear < 1945 ~ "Born before 1945",
                            birthyear >= 1945 & birthyear <= 1959 ~ "Born 1945 - 1959",
                            birthyear > 1959 & birthyear <= 1974 ~ "Born 1960 - 1974",
                            birthyear > 1974 ~ "Born after 1974"),
         cohort_numeric = case_when(birthyear < 1945 ~ 1,
                                    birthyear >= 1945 & birthyear <= 1959 ~ 2,
                                    birthyear > 1959 & birthyear <= 1974 ~ 3,
                                    birthyear > 1974 ~ 4),
         countryg = case_when(EU15 == 1 ~ "West",
                              East_EU == 1 ~ "EU East",
                              East_nonEU == 1 ~ "Non-EU East"),
         east_west = ifelse(East2 == 1, "East", "West"),
         east_west_ger = ifelse(GermanyEast == 1, "East Germany", "West Germany"),
         women_work_index7 = (women_work_1 + women_work_2 + women_work_3 + women_work_4 + women_work_5 + women_work_6 + women_work_7)/7) %>%
  mutate(birthyear10 = birthyear/10,
         East2_birthyr10 = East2 * birthyear10,
         unemployed_All = unemployed,
         unemployed_All = ifelse(is.na(unemployed_All), unemployedlooking, unemployed_All),
         unemployed_All = ifelse(is.na(unemployed_All), activelylook_nosuitable, unemployed_All)) 

data = data %>%
  select(-c(women_work_1,women_work_2,women_work_3,women_work_4,women_work_5,women_work_6,women_work_7,unemployed,unemployedlooking,activelylook_nosuitable))

data = data %>% rename(prefer_market = prefermarket_omitDK, prefer_democracy = preferdemocracy_omitDK, weight_LITS = weight_LITS_each_one, weight_ISSP = weight_ISSP_each_one)

saveRDS(data, file = "JEP_Preferences_Data.rds")
