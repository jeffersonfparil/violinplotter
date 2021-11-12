# Show means and sample sizes
#
# @usage show_means_samle_sizesfunction(formula, data=NULL, explanatory_variable_name, SHOW_MEANS=TRUE, SHOW_SAMPLE_SIZE=TRUE)
#
# @param formula R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
# @param data data.frame containing the response and explanatory variables which forms the formula above [default=NULL]
# @param explanatory_variable_name string referring to the variable name of the explanatory variable whose class means will be compared [mandatory]
# @param SHOW_SAMPLE_SIZE logical referring to whether or not to show the sample sizes of each factor level into an existing plot [default=FALSE]
# @param SHOW_MEANS logical referring to whether or not to show the means [default=TRUE]
#
# @return Tukey's honest significant difference grouping table with response variable categorical means, grouping, level names and corresponding numeric counterparts
# @return Appends honest significant difference grouping letters into an existing plot
#
# @examples
# x1 = rep(rep(rep(c(1:5), each=5), times=5), times=5)
# x2 = rep(rep(letters[6:10], each=5*5), times=5)
# x3 = rep(letters[11:15], each=5*5*5)
# y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1))
# data = data.frame(x1, x2, x3, y)
# formula = y ~ x1 + x2 + x3 + (x2:x3)
# DF = parse_formula(formula=formula, data=data)
# plot_violin_1x(dat=DF, response_variable_name="y", explanatory_variable_name="x3")
# show_means_samle_sizesfunction(formula, data=NULL, explanatory_variable_name, SHOW_MEANS=TRUE, SHOW_SAMPLE_SIZE=TRUE)
#
#' @importFrom graphics text
#
show_means_sample_sizes = function(formula, data=NULL, explanatory_variable_name, SHOW_MEANS=TRUE, SHOW_SAMPLE_SIZE=TRUE){
   ### FOR TESTING:
  # data=NULL; explanatory_variable_name="x3"; alpha=0.05; LOG=FALSE; BASE=10; PLOT=FALSE; SHOW_SAMPLE_SIZE=FALSE
  ### parse the formula and generate the dataframe with explicit interaction terms if expressed in the formula
  df = parse_formula(formula=formula, data=data, IMPUTE=FALSE, IMPUTE_METHOD=mean)
  response_var = df[,1]; response_var_name = colnames(df)[1]
  ### computate the means per explanatory variable level
  means = eval(parse(text=paste0("aggregate(",  response_var_name, "~ `", explanatory_variable_name, "`, data=df, FUN=mean)")))
  colnames(means) = c("LEVELS", "MEANS")
  means = means[order(means$MEANS, decreasing=TRUE), ]
  ### prepare the grouping list
  GROUPING_LIST = eval(parse(text=paste0("list('LEVEL_", paste(as.character(means$LEVELS), collapse="'=c(), 'LEVEL_"), "'=c())")))
  GROUPING_LIST = as.matrix(lapply(GROUPING_LIST, FUN=paste, collapse=""))
  GROUPING_LIST = data.frame(LEVELS=gsub("LEVEL_", "", as.character(rownames(GROUPING_LIST))), GROUPING=as.character(GROUPING_LIST[,1]))
  ### prepare the explanatory variable names and corresponding numbers
  x_levels = eval(parse(text=paste0("levels(as.factor(df$`", explanatory_variable_name, "`))")))
  x_numbers = tryCatch(as.numeric(gsub("_", "-", as.character(x_levels))),
                  warning=function(e){as.numeric(as.factor(x_levels))})
  X_LEVELS_AND_NUMBERS = data.frame(LEVELS=x_levels, NUMBERS=x_numbers)
  ### merge and append the grouping letters together with the means
  MERGE_GROUPING_DF = merge(merge(GROUPING_LIST, X_LEVELS_AND_NUMBERS, by="LEVELS"), means, by="LEVELS")
  ### show means and/or sample sizes
  if(SHOW_MEANS){
    text(x=MERGE_GROUPING_DF$NUMBERS, y=max(response_var)+(sd(response_var)/4), lab=paste0("(", round(MERGE_GROUPING_DF$MEANS,2), ")"))
  }
  if(SHOW_SAMPLE_SIZE){
    sample_sizes = table(eval(parse(text=paste0("df$`", explanatory_variable_name, "`"))))
    SAMPLE_SIZES = as.data.frame(sample_sizes)
    colnames(SAMPLE_SIZES) = c("LEVELS", "SAMPLE_SIZES")
    MERGE_GROUPING_DF = merge(MERGE_GROUPING_DF, SAMPLE_SIZES, by="LEVELS")
    text(x=MERGE_GROUPING_DF$NUMBERS, y=max(response_var)+(sd(response_var)/16), lab=paste0("(n=", MERGE_GROUPING_DF$SAMPLE_SIZES, ")"))
  }
  ### output
  return(MERGE_GROUPING_DF)
}