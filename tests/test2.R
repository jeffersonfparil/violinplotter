library(violinplotter)
str(dummy_data)
OUT_1 = violinplotter(formula = RESPONSE_1 ~ STRATUM*TREATMENT, data=dummy_data, XCATEGOR=c(FALSE,TRUE,TRUE), REGRESSX=c(TRUE, FALSE, FALSE), HSD=c(TRUE, TRUE, FALSE))
OUT_2 = violinplotter(formula = RESPONSE_2 ~ STRATUM*TREATMENT, data=dummy_data)
