### Test using dummy data from the data directory

library(violinplotter)

data = read.csv("data/beauty_dummy_data.csv")
str(data)
formula=beauty ~ food*people
data=data
TITLE=""
XLAB=""
YLAB=""
VIOLIN_COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe")
ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26")
XCATEGOR=TRUE
LOGX=FALSE
LOGX_BASE=1
HSDX=TRUE
ALPHA=0.05
REGRESSX=FALSE
OUT = violinplotter(formula=formula, data=data)

