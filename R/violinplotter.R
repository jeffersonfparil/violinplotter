#' Plotting and Comparing Means with Violin Plots
#'
#' @usage violinplotter(formula, data=NULL, TITLE="", XLAB="", YLAB="",
#'  VIOLIN_COLOURS=c("#e0f3db","#ccebc5","#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe"),
#'  PLOT_BARS=TRUE,
#'  ERROR_BAR_COLOURS=c("#636363","#1c9099","#de2d26"),
#'  SHOW_SAMPLE_SIZE=TRUE,
#'  XCATEGOR=TRUE, LOGX=FALSE, LOGX_BASE=10, HSDX=TRUE,
#'  ALPHA=0.05, REGRESSX=FALSE)
#'
#' @param formula R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
#' @param data data.frame containing the response and explanatory variables which forms the formula above [default=NULL]
#' @param TITLE string or vector of strings corresponding to violin plot title/s [default: combinations of the "response variable name X explanatory variable" from the dataframe column names]
#' @param XLAB string or vector of strings specifying the x-axis labels [default: column names of the explanatory variables (and their combinations) from data]
#' @param YLAB string or vector of strings specifying the y-axis labels [default: column names of the response variable from data]
#' @param VIOLIN_COLOURS vector or list of vectors of colors of the violin plots which are repeated if the length is less than the number of explanatory factor levels or less than the number of explanatory factors in the case of a list [default=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe")]
#' @param PLOT_BARS logical (i.e. TRUE or FALSE) to plot all or none of the bars; or vector strings which bars to plot (e.g. "stdev", "sterr", "ci") [default=TRUE=c("stdev", "sterr", "ci")]
#' @param ERROR_BAR_COLOURS vector of colors of standard deviation, standard error and 95 percent confidence interval error bars (error bar selection via leaving one of the three colors empty) [default=c("#636363", "#1c9099", "#de2d26")]
#' @param SHOW_SAMPLE_SIZE logical referring to whether or not to show the sample sizes for each category [default=TRUE]
#' @param XCATEGOR logical or vector of logicals referring to whether the explanatory variable/s is/are strictly categorical [default=TRUE]
#' @param LOGX logical or vector of logicals referring to whether to transform the explanatory variable/s into the logarithm scale [default=FALSE]
#' @param LOGX_BASE numeric or vector of numerics referring to the logarithm base to transform the explanatory variable/s with [default=1]
#' @param HSDX logical or vector of logicals referring to whether to perform Tukey's Honest Significance Grouping [default=TRUE]
#' @param ALPHA numeric significance level for the analysis of variance F-test and Tukey's mean comparison [default=0.05]
#' @param REGRESSX logical or vector of logicals referring to whether to regress the response variable against the explanatory variable/s [default=FALSE]
#'
#' @return Violin plot/s with optional error bars, mean comparison grouping/s, and regression line/s
#' @return Mean comparison grouping/s based on Tukey's Hones significant difference and regression line statistics, if applicable
#'
#' @examples
#' x1 = rep(rep(rep(c(1:5), each=5), times=5), times=5)
#' x2 = rep(rep(letters[6:10], each=5*5), times=5)
#' x3 = rep(letters[11:15], each=5*5*5)
#' y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1))
#' formula = log(y) ~ exp(x1) + x2 + x3 + (x2:x3)
#' test1 = violinplotter(formula=formula)
#' test2 = violinplotter(formula=formula, PLOT_BARS=c("ci", "stdev"))
#'
#' @importFrom grDevices rgb
#'
#' @export
violinplotter = function(formula, data=NULL, TITLE="", XLAB="", YLAB="", VIOLIN_COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), PLOT_BARS=TRUE, ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), SHOW_SAMPLE_SIZE=TRUE, XCATEGOR=TRUE, LOGX=FALSE, LOGX_BASE=10, HSDX=TRUE, ALPHA=0.05, REGRESSX=FALSE){
  ### FOR TESTING: load the parsing, plotting, HSD, and regressing functions
  # source("parse_formula.R")
  # source("plot_violin_1x.R")
  # source("mean_comparison_HSD.R")
  # source("plot_regression_line.R")
  # source("violinplotter.R")
  # x1 = rep(rep(rep(c(1:5), each=5), times=5), times=5)
  # x2 = rep(rep(letters[6:10], each=5*5), times=5)
  # x3 = rep(letters[11:15], each=5*5*5)
  # y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1))
  # formula = log(y) ~ exp(x1) + x2 + x3 + (x2:x3)
  # data = NULL
  # TITLE=""; XLAB=""; YLAB=""; VIOLIN_COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe")
  # PLOT_BARS=TRUE; ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26");
  # PLOT_BARS=FALSE; ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26");
  # PLOT_BARS="sterr"; ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26");
  # PLOT_BARS=c("ci", "sterr"); ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26");
  # PLOT_BARS=c(); ERROR_BAR_COLOURS=c("#636363", "#1c9099", "#de2d26");
  # XCATEGOR=TRUE; LOGX=FALSE; LOGX_BASE=1; HSDX=TRUE; ALPHA=0.05; REGRESSX=FALSE
  # XCATEGOR=FALSE; LOGX=FALSE; LOGX_BASE=1; HSDX=TRUE; ALPHA=0.05; REGRESSX=TRUE
  # SHOW_SAMPLE_SIZE=TRUE

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
  if ( (sum(TITLE==rep("", times=length(TITLE)))==length(TITLE)) | (length(TITLE) != ncol(df)-1) ){
    TITLE = paste0(YLAB, "\nX\n", XLAB)
  }

  ### What are the violin plot colours we're using for each explanatory variable?
  if (is.list(VIOLIN_COLOURS)==FALSE) {
    VIOLIN_COLOURS = rep(list(VIOLIN_COLOURS), times=length(explanatory_var_names))
  } else if (length(VIOLIN_COLOURS) < length(explanatory_var_names)){
    VIOLIN_COLOURS = rep(VIOLIN_COLOURS, times=(length(explanatory_var_names)-length(VIOLIN_COLOURS)+1))
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
  XCATEGOR[LOGX] = FALSE ### automatically convert factors into non-strictly categorcialy if they were set to be log-transformed!

  ### Which statistical bars do we plot?
  ERROR_BAR_COLOURS=rep(ERROR_BAR_COLOURS, times=3)[1:3] ### to make sure we have 3 error bar colours
  orig_ERROR_BAR_COLOURS = ERROR_BAR_COLOURS
  ERROR_BAR_COLOURS = rep(c(rgb(1,0,0,alpha=0)), times=3)
  if (length(PLOT_BARS)==1){
    if (PLOT_BARS==TRUE){
      ERROR_BAR_COLOURS = orig_ERROR_BAR_COLOURS
    }
  }
  if (sum(PLOT_BARS=="stdev")>0){
    ERROR_BAR_COLOURS[1] = orig_ERROR_BAR_COLOURS[1]
  } 
  if (sum(PLOT_BARS=="sterr")>0){
    ERROR_BAR_COLOURS[2] = orig_ERROR_BAR_COLOURS[2]
  } 
  if (sum(PLOT_BARS=="ci")>0){
    ERROR_BAR_COLOURS[3] = orig_ERROR_BAR_COLOURS[3]
  } 

  ### Do we have to perform Tukey's hones significant difference test across the explanatory variable/s?
  if (length(HSDX) != ncol(df)-1) {
    HSDX = rep(HSDX, times=ncol(df)-1)
  }

   ### Do we have to regress the response variable against the explanatory variable/s?
  if (length(REGRESSX) != ncol(df)-1) {
    REGRESSX = rep(REGRESSX, times=ncol(df)-1)
  }

  ### iterate across explanatory variables defined by the formula
  OUT = list()
  if (length(explanatory_var_names) > 1){
    ### define layout if we have more than one explanatory variable
    ### otherwise don't set the layout so the user can define their own layout and print multiple plots
    orig_par = par(no.readonly=TRUE)
    on.exit(par(orig_par))
    m = length(explanatory_var_names)
    par(mfrow=c(round(sqrt(m)), (floor(sqrt(m)) + ceiling(sqrt(m) %% floor(sqrt(m))))))
  }
  for (i in 1:length(explanatory_var_names)){
    # i = 4
    message("======================================================")
    message(paste0("Violin Plotting: ", explanatory_var_names[i]))
    message("======================================================")
    VIOPLOT_LETTERS2NUMS = plot_violin_1x(dat=df,
                                          response_variable_name=response_var_name,
                                          explanatory_variable_name=explanatory_var_names[i],
                                          title=TITLE[i],
                                          xlab=XLAB[i],
                                          ylab=YLAB,
                                          COLOURS=VIOLIN_COLOURS[[i]],
                                          BAR_COLOURS=ERROR_BAR_COLOURS,
                                          CI=(1-ALPHA)*100,
                                          XTICKS=XCATEGOR[i],
                                          LOG=LOGX[i],
                                          BASE=LOGX_BASE[i])
    if (HSDX[i]==TRUE){
      message("======================================================")
      message(paste0("HSD Grouping: ", explanatory_var_names[i]))
      message("======================================================")
      HSD_out = mean_comparison_HSD(formula=formula,
                                    data=data,
                                    explanatory_variable_name=explanatory_var_names[i],
                                    alpha=ALPHA,
                                    LOG=LOGX[i],
                                    BASE=LOGX_BASE[i],
                                    PLOT=TRUE,
                                    SHOW_SAMPLE_SIZE=SHOW_SAMPLE_SIZE)
    } else {HSD_out = NULL}
    if (REGRESSX[i]==TRUE){
      message("======================================================")
      message(paste0("Linear Regressing: ", explanatory_var_names[i]))
      message("======================================================")
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
