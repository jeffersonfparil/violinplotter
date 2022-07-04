### Generate dummy_data_out.png

install.packages("remotes")
remotes::install_github("jeffersonfparil/violinplotter")
library(violinplotter)

png("dummy_data_out.png", width=1500, heigh=700)
par(mfrow=c(1,3), cex=1.25)
violinplotter(RESPONSE_1 ~ STRATUM, REGRESS=TRUE, SHOW_MEANS=FALSE, SHOW_SAMPLE_SIZE=TRUE, ALPHA=0.01, data=dummy_data)
violinplotter(RESPONSE_1 ~ TREATMENT, data=dummy_data)
violinplotter(RESPONSE_1 ~ STRATUM:TREATMENT, SHOW_MEANS=FALSE, MANN_WHITNEY=FALSE, data=dummy_data)
dev.off()
