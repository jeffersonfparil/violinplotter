pkgname <- "violinplotter"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('violinplotter')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("violinplotter")
### * violinplotter

flush(stderr()); flush(stdout())

### Name: violinplotter
### Title: Plotting and Comparing Means with Violin Plots
### Aliases: violinplotter

### ** Examples

x1 = rep(rep(rep(c(1:5), each=5), times=5), times=5)
x2 = rep(rep(letters[6:10], each=5*5), times=5)
x3 = rep(letters[11:15], each=5*5*5)
y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1))
formula = log(y) ~ exp(x1) + x2 + x3 + (x2:x3)
test1 = violinplotter(formula=formula)
test2 = violinplotter(formula=formula, PLOT_BARS=c("ci", "stdev"))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
