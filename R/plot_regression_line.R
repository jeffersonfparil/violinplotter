plot_regression_line = function(dat, response_variable_name, explanatory_variable_name, LOG=FALSE, BASE=10, x_levels=x_levels, x_numbers=x_numbers, PLOT=TRUE, LINE_COL="gray") {
  # dat=df; response_variable_name=response_var_name; explanatory_variable_name = explanatory_var_names[i]; LINE_COL="gray"
  dat_defined_x_levels = eval(parse(text=paste0("levels(dat$`", explanatory_variable_name, "`)")))
  if (sum( dat_defined_x_levels == x_levels ) == length(x_levels)) {
    eval(parse(text=paste0("levels(dat$`", explanatory_variable_name, "`) = x_numbers")))
    x = eval(parse(text=paste0("as.numeric(as.character(dat$`", explanatory_variable_name, "`))")))
    y = eval(parse(text=paste0("dat$`", response_variable_name, "`")))
    mod = lm(y ~ x)
    b0 = mod$coefficients[1]
    b1 = mod$coefficients[2]
    r2adj = summary(mod)$adj.r.squared
    regress_out = c(b0, b1, r2adj); names(regress_out) = c("intercept", "slope", "R2adj")
    x_new = seq(from=min(x)-sd(x), to=max(x)+sd(x), length.out=100)
    y_pred = mod$coefficients[1] + (mod$coefficients[2] * x_new)
    lines(x=x_new, y=y_pred, lty=2, lwd=2, col=LINE_COL)
    legend("bottomright", legend=paste0(c("y-intercept=", "slope=", "R2_adjusted="), round(regress_out,2)), cex=0.75)
  } else {
    stop("The levels of the explanatory variable does not match x_levels. Check if you can actually convert this explanatory variable into a continuous numeric variable.")
  }
  return(regress_out)
}

