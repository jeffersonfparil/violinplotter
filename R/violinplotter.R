#' Violin plotter with mean comparison bars and optional HSD grouping and regression line
#'
#' @usage violinplotter(dat, response_var, explanatory_var, title="", xlab="", ylab="", COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XTICKS=TRUE, LOG=FALSE, BASE=1, HSD=TRUE, REGRESS=FALSE)
#'
#' @param formula R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
#' @param data data.frame containing the response and explantory variables which forms the formula above [default=NULL]
#' @param TITLE string or vector of strings corresponding to violin plot title/s [default: combinations of the "response variable name X explantory variable" from the dataframe column names]
#' @param XLAB string or vector of strings specifying the x-axis labels [default: column names of the explanatory variables (and their combinations) from data]
#' @param YLAB string or vector of strings specifying the y-axis labels [default: column names of the response variable from data]
#' @param VIOLIN_COLOURS vector of colors of the violin plots which are repeated if the length is less than the number of explanatory factor levels [default=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe")]
#' @param ERROR_BAR_COLOURS vector of colours of standard deviation, standard error and 95 percent confidence interval error bars (error bar selection via leaving one of the three colors empty) [default=c("#636363", "#1c9099", "#de2d26")]
#' @param XCATEGOR logical or vector of logicals referring to whether the explanatory variable/s is/are strictly categorical [default=TRUE]
#' @param LOGX logical or vector of logicals referring to whether to transform the explanatory variable/s into the logarithm scale [default=FALSE]
#' @param LOGX_BASE numeric or vector of numerics referring to the logarithm base to transform the explanatory variable/s with [default=1]
#' @param HSDX logical or vector of logicals referring to whether to perform Tukey's Honest Significance Grouping [default=TRUE]
#' @param ALPHA numeric significance level for the analysis of variance F-test and Tukey's mean comparison [default=0.05]
#' @param REGRESSX logical or vector of logicals referring to whether to regress the response variable against the explantory variable/s [default=FALSE]
#'
#' @return Violin plot/s with optional error bars, mean comparison grouping/s, and regression line/s
#' @return Mean comparison grouping/s based on Tukey's Hones significant difference and regression line statistics, if applicable
#'
#' @examples
#' x1 = rep(rep(rep(letters[1:5], each=5), times=5), times=5)
#' x2 = rep(rep(letters[6:10], each=5*5), times=5)
#' x3 = rep(letters[11:15], each=5*5*5)
#' y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1)) ### x3 is the variable affecting y (see each=5*5*5)
#' data = data.frame(x1, x2, x3, y)
#' formula = y ~ x1 + x2 + x3 + (x2:x3)
#' OUT = violinplotter(formula=formula, data=data)

violinplotter = function(formula, data=NULL, TITLE="", XLAB="", YLAB="", VIOLIN_COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XCATEGOR=TRUE, LOGX=FALSE, LOGX_BASE=1, HSDX=TRUE, ALPHA=0.05, REGRESSX=FALSE){
  ### FOR TESTING: load the parsing, plotting, HSD, and regressing functions
  # source("parse_formula.R")
  # source("plot_violin_1x.R")
  # source("mean_comparison_HSD.R")
  # source("plot_regression_line.R")

  ### parse the formula and generate the dataframe with explicit interaction terms if expressed in the formula
  df = parse_formula(formula=formula, data=data, IMPUTE=FALSE, IMPUTE_METHOD=mean)
  response_var = df[,1]
  explanatory_var = df[,2:ncol(df)]
  response_var_name = colnames(df)[1]
  explanatory_var_names = colnames(df)[2:ncol(df)]

  ### set axes labels and titles
  if (YLAB==""){
    YLAB = response_var_name
  }
  if ( (XLAB=="") | (length(XLAB) != ncol(df)-1) ){
    XLAB = explanatory_var_names
  }
  if ( (TITLE=="") | (length(TITLE) != ncol(df)-1) ){
    TITLE = paste0(YLAB, "\nX\n", XLAB)
  }

  ### Do we have to transform the explanatory variable/s into the log space?
  if (length(LOGX) != ncol(df)-1) {
    LOGX = rep(LOGX, times=ncol(df)-1)
  }
  if (length(LOGX_BASE) != ncol(df)-1) {
    LOGX_BASE = rep(LOGX_BASE, times=ncol(df)-1)
  }

  ### Are the explanatory variable/s are strictly categorical?
  if (length(XCATEGOR) != ncol(df)-1) {
    XCATEGOR = rep(XCATEGOR, times=ncol(df)-1)
  }

  ### Do we have to perform Tukey's hones significant difference test across the explanatory variable/s?
  if (length(HSDX) != ncol(df)-1) {
    HSDX = rep(HSDX, times=ncol(df)-1)
  }

   ### Do we have to regress the response variable against the explanatory variable/s?
  if (length(REGRESSX) != ncol(df)-1) {
    REGRESSX = rep(REGRESSX, times=ncol(df)-1)
  }

  ### define the layout of the plot space
  MFROW = c(1)
  for (i in 2:length(explanatory_var_names)){
    if ( (length(explanatory_var_names) %% i)==0 ){
      MFROW = c(MFROW, i)
    }
  }

  ### iterate across explanatory variables defined by the formula
  OUT = list()
  par(mfrow=MFROW[c(ceiling(length(MFROW)/2), (floor(length(MFROW)/2)+1))])
  for (i in 1:length(explanatory_var_names)){
    # i = 4
    print("======================================================")
    print(paste0("Violin Plotting: ", explanatory_var_names[i]))
    print("======================================================")
    VIOPLOT_LETTERS2NUMS = plot_violin_1x(dat=df,
                                          response_variable_name=response_var_name,
                                          explanatory_variable_name=explanatory_var_names[i],
                                          title=TITLE[i],
                                          xlab=XLAB[i],
                                          ylab=YLAB,
                                          COLOURS=VIOLIN_COLOURS,
                                          BAR_COLOURS=ERROR_BAR_COLOURS,
                                          XTICKS=XCATEGOR[i],
                                          LOG=LOGX[i],
                                          BASE=LOGX_BASE[i])
    if (HSDX[i]==TRUE){
      print("======================================================")
      print(paste0("HSD Grouping: ", explanatory_var_names[i]))
      print("======================================================")
      HSD_out = mean_comparison_HSD(formula=formula,
                                    data=data,
                                    explanatory_variable_name=explanatory_var_names[i],
                                    alpha=ALPHA,
                                    LOG=LOGX[i],
                                    BASE=LOGX_BASE[i],
                                    PLOT=TRUE)
    } else {HSD_out = NULL}
    if (REGRESSX[i]==TRUE){
      print("======================================================")
      print(paste0("LiLnear Regressing: ", explanatory_var_names[i]))
      print("======================================================")
      REGRESS_out = plot_regression_line(dat=df,
                                        response_variable_name=response_var_name,
                                        explanatory_variable_name=explanatory_var_names[i],
                                        LOG=LOGX[i],
                                        BASE=LOGX_BASE[i],
                                        PLOT=TRUE,
                                        LINE_COL="gray")
    } else {REGRESS_out = NULL}
    OUT[[i]] = c(HSD_out=HSD_out, REGRESS_out=REGRESS_out)
  }
  return(OUT)
}
