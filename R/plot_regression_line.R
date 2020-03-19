#' Regress the response variable against one explanatory variable
#'
#' @usage plot_regression_line(dat, response_variable_name, explanatory_variable_name,
#'                      LOG=FALSE, BASE=10, PLOT=TRUE, LINE_COL="gray")
#'
#' @param dat dataframe where the response and explanatory variables including interaction terms if applicable are explicitly written into columns (output of the parse_formula() function) [mandatory]
#' @param response_variable_name string referring to the variable name of the response variable [mandatory]
#' @param explanatory_variable_name string referring to the variable name of the explanatory variable [mandatory]
#' @param LOG logical referring to whether to transform the explanatory variable into the logarithm scale [default=FALSE]
#' @param BASE numeric referring to the logarithm base to transform the explanatory variable with [default=1]
#' @param PLOT logical referring to whether to plot the regression line into the existing plot [default=FALSE]
#' @param LINE_COL string referring to the color or the regression line [default="gray"]
#'
#' @return Linear regression statistics (completely fixed linear model): intercept, slope and coefficient of determination adjusted-R^2
#'
#' @examples
#' x1 = rep(rep(rep(c(1:5), each=5), times=5), times=5)
#' x2 = rep(rep(letters[6:10], each=5*5), times=5)
#' x3 = rep(letters[11:15], each=5*5*5)
#' y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1))
#' data = data.frame(x1, x2, x3, y)
#' formula = y ~ x1 + x2 + x3 + (x2:x3)
#' DF = parse_formula(formula=formula, data=data)
#' plot_violin_1x(dat=DF, response_variable_name="y", explanatory_variable_name="x1")
#' HSD = mean_comparison_HSD(formula, data=data, explanatory_variable_name="x1", PLOT=TRUE)
#' REGRESS = plot_regression_line(dat=DF, response_variable_name="y",
#'                                explanatory_variable_name="x1")
#'
#' @importFrom stats lm
#' @importFrom graphics lines legend
#'
#' @export
plot_regression_line = function(dat, response_variable_name, explanatory_variable_name, LOG=FALSE, BASE=10, PLOT=TRUE, LINE_COL="gray"){
  x_levels = eval(parse(text=paste0("levels(as.factor(dat$`", explanatory_variable_name, "`))")))
  x_numbers = tryCatch(as.numeric(gsub("_", "-", as.character(x_levels))),
                  warning=function(e){as.numeric(as.factor(x_levels))})
  eval(parse(text=paste0("levels(dat$`", explanatory_variable_name, "`) = x_numbers")))
  x = eval(parse(text=paste0("as.numeric(as.character(dat$`", explanatory_variable_name, "`))")))
  y = eval(parse(text=paste0("dat$`", response_variable_name, "`")))
  if (LOG==TRUE){
    if (sum(is.na(suppressWarnings(log(x, base=BASE))))==0){
      x = log(x, base=BASE)
    } else {
      x = log(x+abs(min(x))+1, base=BASE)
    }
  }
  mod = lm(y ~ x)
  b0 = mod$coefficients[1]
  b1 = mod$coefficients[2]
  r2adj = summary(mod)$adj.r.squared
  regress_out = c(b0, b1, r2adj); names(regress_out) = c("intercept", "slope", "R2adj")
  x_new = seq(from=min(x)-sd(x), to=max(x)+sd(x), length.out=100)
  y_pred = mod$coefficients[1] + (mod$coefficients[2] * x_new)
  lines(x=x_new, y=y_pred, lty=2, lwd=2, col=LINE_COL)
  legend("bottomright", legend=paste0(c("y-intercept=", "slope=", "R2_adjusted="), round(regress_out,3)), cex=0.75)
  return(regress_out)
}
