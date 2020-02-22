# violinplotter
## R package for plotting and comparing means with violin plots

[](https://travis-ci.com/jeffersonfparil/violinplotter.svg?branch=master)

Just another violin plotter with mean comparison bars and optional HSD grouping and regression line.

## Usage
```
violinplotter(dat, response_var, explanatory_var, title="", xlab="", ylab="", COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XTICKS=TRUE, LOG=FALSE, BASE=1, HSD=TRUE, REGRESS=FALSE)
```

## Manual
Download the repository:
```
wget https://github.com/jeffersonfparil/violinplotter/archive/master.zip
echo -e "print(.libPaths()[1])" > libpath.r
YOUR_RLIB_PATH=$(Rscript libpath.r | cut -d' ' -f2)
R CMD INSTALL -l ${YOUR_RLIB_PATH} violinplotter-master.zip
# library(violinplotter)
# ?violinplotter
```

## Examples
```
library(violinplotter)
x1 = rep(rep(rep(letters[1:5], each=5), times=5), times=5)
x2 = rep(rep(letters[6:10], each=5*5), times=5)
x3 = rep(letters[11:15], each=5*5*5)
y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1)) ### x3 is the variable affecting y (see each=5*5*5)
data = data.frame(x1, x2, x3, y)
formula = y ~ x1 + x2 + x3 + (x2:x3)
VIOLIN_COLOURS = RColorBrewer::brewer.pal(9, "GnBu")[c(2,3,4,5,6,7)]
ERROR_BAR_COLOURS = c("#636363", "#1c9099", "#de2d26")
TITLE = "" ### string or vector of strings
XLAB = "" ### string or vector of strings
YLAB = ""
XCATEGOR = FALSE ### Is the explanatory variable strictly categorical? ### logical or vector of logicals
LOGX=FALSE  ### logical or vector of logicals
LOGX_BASE=10 ### numeric or vector of numerics
HSDX=TRUE
ALPHA=0.05
REGRESSX=TRUE
OUT = violinplotter(formula=formula, data=data, TITLE=TITLE, XLAB=XLAB, YLAB=YLAB, VIOLIN_COLOURS=VIOLIN_COLOURS, ERROR_BAR_COLOURS=ERROR_BAR_COLOURS, XCATEGOR=XCATEGOR, LOGX=LOGX, LOGX_BASE=LOGX_BASE, HSDX=HSDX, ALPHA=ALPHA, REGRESSX=REGRESSX)

```
