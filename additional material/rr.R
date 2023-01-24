library(dplyr)
library(tidyr)
library(lfe)
library(stargazer)
library(TAM)

regression_results = function(){
  datapref <- readRDS("data/JEP_Preferences_Data.rds")
  # support for market economy
  datareg1 <- datapref %>%
    filter(countryname != "Germany") %>%
    drop_na(prefer_market, birthyear10, East2_birthyr10, male, unemployed_All, countryyear, countryname, weight_LITS)
  reg1_3 <- felm(prefer_market ~ East2_birthyr10 + birthyear10 + male + unemployed_All | countryyear | 0 | countryname , data = datareg1, weights = datareg1$weight_LITS)
  # support for democracy
  datareg2 <- datapref %>%
    filter(countryname != "Germany") %>%
    drop_na(prefer_democracy, birthyear10, East2_birthyr10, male, unemployed_All, countryyear, countryname, weight_LITS)
  reg2_2 <- felm(prefer_democracy ~ East2_birthyr10 + birthyear10 + male + unemployed_All | countryyear | 0 | countryname , data = datareg2, weights = datareg2$weight_LITS)
  # support for redistribution (social ineq. module)
  datareg3_1 <- datapref %>%
    filter(countryname != "Germany") %>%
    drop_na(govreduceinequality_socineq, birthyear10, East2_birthyr10, male, unemployed_All, countryyear, countryname, weight_ISSP) %>%
    mutate(govreduceinequality_socineq_sd = (govreduceinequality_socineq - weighted_mean(govreduceinequality_socineq,weight_ISSP)) / weighted_sd(govreduceinequality_socineq,weight_ISSP))
  reg3_1 <- felm(govreduceinequality_socineq_sd ~ East2_birthyr10 + birthyear10 + male + unemployed_All | countryyear | 0 | countryname, data = datareg3_1, weight = datareg3_1$weight_ISSP)
  # support for redistribution (role of gov. module)
  datareg3_2 <- datapref %>%
    filter(countryname != "Germany") %>%
    drop_na(govreduceinequality_roleofgov, birthyear10, East2_birthyr10, male, unemployed_All, countryyear, countryname, weight_ISSP) %>%
    mutate(govreduceinequality_roleofgov_sd = (govreduceinequality_roleofgov - weighted_mean(govreduceinequality_roleofgov,weight_ISSP)) / weighted_sd(govreduceinequality_roleofgov,weight_ISSP))
  reg3_2 <- felm(govreduceinequality_roleofgov_sd ~ East2_birthyr10 + birthyear10 + male + unemployed_All | countryyear | 0 | countryname, data = datareg3_2, weight = datareg3_2$weight_ISSP)
  # support for gender equality
  datareg4 <- datapref %>%
    filter(countryname != "Germany") %>%
    drop_na(women_work_index7, birthyear10, East2_birthyr10, male, unemployed_All, countryyear, countryname, weight_ISSP) %>%
    mutate(women_work_index7_sd = (women_work_index7 - weighted_mean(women_work_index7,weight_ISSP)) / weighted_sd(women_work_index7,weight_ISSP))
  reg4 <- felm(women_work_index7_sd ~ East2_birthyr10 + birthyear10 + male + unemployed_All | countryyear | 0 | countryname, data = datareg4, weight = datareg4$weight_ISSP)
  # regression table
  stargazer(reg1_3,
            reg2_2,
            reg3_1,
            reg3_2,
            reg4,
            type = "html",
            title = "Regression Results",
            dep.var.labels = c("Support for <br>Market Economy (%)", "Support for <br>Democracy (%)", "Support for <br>Redistribution <br>(social ineq. module)", "Support for <br>Redistribution <br>(role of gov. module)", "Support for <br>Gender Equality <br>(index)"),
            column.labels = c("LITS data", "ISSP data"),
            column.separate = c(2,3),
            covariate.labels = c("EastxBirthYear<sup>-10<sup>", "BirthYear<sup>-10<sup>", "Male", "Unemployed"),
            keep.stat = c("rsq", "n"),
            add.lines=list(c('CountryxYear fixed effects', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                           c('Mean dep. var.', 
                             round(weighted_mean(datareg1$prefer_market,datareg1$weight_LITS),2), 
                             round(weighted_mean(datareg2$prefer_democracy,datareg2$weight_LITS),2), 
                             round(weighted_mean(datareg3_1$govreduceinequality_socineq_sd,datareg3_1$weight_ISSP),2), 
                             round(weighted_mean(datareg3_2$govreduceinequality_roleofgov_sd,datareg3_2$weight_ISSP),2), 
                             round(weighted_mean(datareg4$women_work_index7_sd,datareg4$weight_ISSP),2)),
                           c('Sd dep. var.', 
                             round(weighted_sd(datareg1$prefer_market,datareg1$weight_LITS),2), 
                             round(weighted_sd(datareg2$prefer_democracy,datareg2$weight_LITS),2), 
                             round(weighted_sd(datareg3_1$govreduceinequality_socineq_sd,datareg3_1$weight_ISSP),2), 
                             round(weighted_sd(datareg3_2$govreduceinequality_roleofgov_sd,datareg3_2$weight_ISSP),2), 
                             round(weighted_sd(datareg4$women_work_index7_sd,datareg4$weight_ISSP),2))))
}
