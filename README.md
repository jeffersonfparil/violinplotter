# violinplotter
## R package for plotting and comparing means with violin plots

![](https://travis-ci.com/jeffersonfparil/violinplotter.svg?branch=master)

Just another violin plotter with mean comparison bars and optional HSD grouping and regression line.

## Usage

```r
violinplotter(dat, response_var, explanatory_var, title="", xlab="", ylab="", COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XTICKS=TRUE, LOG=FALSE, BASE=1, HSD=TRUE, REGRESS=FALSE)
```

## Manual

Download the repository:

```shell
wget https://github.com/jeffersonfparil/violinplotter/archive/master.zip
cd violinplotter/
```

Inside the violinplotter/ directory build and install

```r
### build the package into the base directory (one level lower)
devtools::build()
### move to lower directory
setwd("..")
### identify the name of the most recent build
build_name = system("ls | grep violinplotter_ | tail -n1", intern=TRUE)
### install
install.packages(build_name)
```

## Examples

```r
library(violinplotter)
x1 = rep(rep(rep(letters[1:5], each=5), times=5), times=5)
x2 = rep(rep(letters[6:10], each=5*5), times=5)
x3 = rep(letters[11:15], each=5*5*5)
y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1)) ### x3 is the variable affecting y (see each=5*5*5)
data = data.frame(x1, x2, x3, y)
OUT = violinplotter(formula=y ~ x1 + x2 + x3 + (x2:x3), data=data, ALPHA=0.05)
```
