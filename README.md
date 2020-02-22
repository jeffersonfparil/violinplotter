# violinplotter
## R package for plotting and comparing means with violin plots

|                                                        **Lab Webiste**                                                        |                                                               **Build Status**                                                                |                                                                              **Documentation**                                                                               |
|:-----------------------------------------------------------------------------------------------------------------------------:|:---------------------------------------------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| <a href="https://adaptive-evolution.biosciences.unimelb.edu.au/"><img src="misc/adaevo_lab_unimelb_2020.png" width="150"></a> | [![Build Status](https://travis-ci.com/jeffersonfparil/violinplotter.svg?branch=master)](https://travis-ci.com/jeffersonfparil/violinplotter) | <a href="https://github.com/jeffersonfparil/violinplotter/wiki" target="_blank"><img src="https://img.shields.io/badge/docs-latest-blue.svg" alt="Latest documentation"></a> |

Just another violin plotter with mean comparison bars and optional HSD grouping and regression line

## Usage

```r
violinplotter(formula, data=NULL, TITLE="", XLAB="", YLAB="", VIOLIN_COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XCATEGOR=TRUE, LOGX=FALSE, LOGX_BASE=1, HSDX=TRUE, ALPHA=0.05, REGRESSX=FALSE)
?violinplotter ### for more information
```

## Installation

[Download the repository as a zip file](https://github.com/jeffersonfparil/violinplotter/archive/master.zip).
Unzip, and navigate inside the *violinplotter-master/* directory.
Open R (inside the *violinplotter-master/* directory), build and install via:

```r
### build the package into the base directory (one level lower)
devtools::build()
### move to lower directory
setwd("..")
### identify the name of the most recent build
build_name = system("ls | grep violinplotter_*.tar.gz | tail -n1", intern=TRUE)
### install
install.packages(build_name)
```

## Examples

Simulated dataset:

```r
library(violinplotter)
x1 = rep(rep(rep(letters[1:5], each=5), times=5), times=5)
x2 = rep(rep(letters[6:10], each=5*5), times=5)
x3 = rep(letters[11:15], each=5*5*5)
y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1)) ### x3 is the variable affecting y (see each=5*5*5)
data = data.frame(x1, x2, x3, y)
OUT = violinplotter(formula=y ~ x1 + x2 + x3 + (x2:x3), data=data, ALPHA=0.05)
```

Dummy dataset:

```r
library(violinplotter)
data(dummy_data)
str(dummy_data)
BEAUTY = violinplotter(formula = beauty ~ food*people, data=dummy_data)
FEET_QUALITY = violinplotter(formula = feet_quality ~ food*people, data=dummy_data)
```
