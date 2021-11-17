library(violinplotter)
str(dummy_data)
OUT_1 = violinplotter(formula = RESPONSE_1 ~ STRATUM*TREATMENT, data=dummy_data, CATEGORICAL=c(FALSE,TRUE,TRUE), REGRESS=c(TRUE, FALSE, FALSE), HSD=c(TRUE, TRUE, FALSE))
OUT_2 = violinplotter(formula = RESPONSE_2 ~ STRATUM*TREATMENT, data=dummy_data)
