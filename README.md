# violin_plotter.R
## R package for plotting and comparing means with violin plots

Just another violin plotter with mean comparison bars and optional HSD grouping and regression line.

## Usage
violinplot_func(dat, response_var, explanatory_var, title="", xlab="", ylab="", COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XTICKS=TRUE, LOG=FALSE, BASE=1, HSD=TRUE, REGRESS=FALSE)
## Parameters
- formula: R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
- data: data.frame containing the response and explantory variables which forms the formula above [default=NULL]
- TITLE: string or vector of strings corresponding to all the explanatory terms including additive and interaction terms in the formula [default=""]
- XLAB string or vector of strings specifing the x-axis labels [default=column names of the explanatory variables (and their combinations) from data]
- YLAB string or vector of strings specifing the y-axis labels [default=column names of the response variable from data]
- VIOLIN_COLOURS vector of colors of the violin plots which are repeated if the length is less than the number of explanatory factor levels [default=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe")]
- ERROR_BAR_COLOURS vector of colours of standard deviation, standard error and 95 percent confidence interval error bars (error bar selection via leaving one of the three colors empty) [default=c("#636363", "#1c9099", "#de2d26")]
- XCATEGOR logical or vector of logicals referring to whether the explanatory variable/s is/are strictly categorcial [default=TRUE]
- LOGX logical or vector of logicals referring to whether to transform the explanatory variable/s into the logarithm scale [default=FALSE]
- LOGX_BASE numeric or vector of numerics referring to the logarithm base to transform the explanatory variable/s with [default=1]
- HSDX logical or vector of logicals referring to whether to perform Tukey's Honest Significance Grouping [default=TRUE]
- ALPHA numeric significance level for the analysis of variance F-test and Tukey's mean comparison [default=0.05]
- REGRESSX logical or vector of logicals referring to whether to regress the response variable against the explantory variable/s [default=FALSE]
## Output
- Violin plot/s with optional error bars, mean comparison grouping/s, and regression line/s
- Mean comparison grouping/s based on Tukey's Hones significant difference and regression line statistics, if applicable

## Examples
x1 = rep(rep(rep(letters[1:5], each=5), times=5), times=5)
x2 = rep(rep(letters[6:10], each=5*5), times=5)
x3 = rep(letters[11:15], each=5*5*5)
y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1)) ### x3 is the variable affecting y (see each=5*5*5)
data = data.frame(x1, x2, x3, y)
formula = y ~ x1 + x2 + x3 + (x2:x3)
OUT = violin_plotter(formula=formula, data=data)
