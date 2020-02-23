library(violinplotter)
str(dummy_data)
OUT_1 = violinplotter(formula = RESPONSE_1 ~ STRATUM*TREATMENT, data=dummy_data, REGRESSX=c(TRUE, FALSE, FALSE))
OUT_2 = violinplotter(formula = RESPONSE_2 ~ STRATUM*TREATMENT, data=dummy_data)
