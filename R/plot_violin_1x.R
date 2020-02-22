#' One explanatory variable at a time violin plotter
#'
#' @usage plot_violin_1x(dat, response_variable_name, explanatory_variable_name, title="", xlab="", ylab="", COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XTICKS=TRUE, LOG=FALSE, BASE=10)
#'
#' @param dat ataframe where the response and explanatory variables including interaction terms if applicable are explicitly written into columns (output of the parse_formula() function) [mandatory]
#' @param response_variable_name string referring to the variable name of the response variable [mandatory]
#' @param explanatory_variable_name string referring to the variable name of the explanatory variable [mandatory]
#' @param title string corresponding to the explanatory term including additive and interaction terms in the formula [default=""]
#' @param xlab string specifing the x-axis label [default=""]
#' @param ylab string specifing the y-axis label [default=""]
#' @param COLOURS vector of colors of the violin plots which are repeated if the length is less than the number of explanatory factor levels [default=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe")]
#' @param BAR_COLOURS vector of colours of standard deviation, standard error and 95 percent confidence interval error bars (error bar selection via leaving one of the three colors empty) [default=c("#636363", "#1c9099", "#de2d26")]
#' @param XTICKS logical referring to whether the explanatory variable is strictly categorcial [default=TRUE]
#' @param LOG logical referring to whether to transform the explanatory variable into the logarithm scale [default=FALSE]
#' @param BASE numeric referring to the logarithm base to transform the explanatory variable with [default=1]
#'
#' @return Return 0 if successful
#'
#' @examples
#' x1 = rep(rep(rep(letters[1:5], each=5), times=5), times=5)
#' x2 = rep(rep(letters[6:10], each=5*5), times=5)
#' x3 = rep(letters[11:15], each=5*5*5)
#' y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1)) ### x3 is the variable affecting y (see each=5*5*5)
#' data = data.frame(x1, x2, x3, y)
#' formula = y ~ x1 + x2 + x3 + (x2:x3)
#' DF = parse_formula(formula=formula, data=data)
#' plot_violin_1x(dat=DF, response_variable_name="y", explanatory_variable_name="x3")

plot_violin_1x = function(dat, response_variable_name, explanatory_variable_name, title="", xlab="", ylab="", COLOURS=c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe"), BAR_COLOURS=c("#636363", "#1c9099", "#de2d26"), XTICKS=TRUE, LOG=FALSE, BASE=10){
  ### extract the dependent or response or y variable, as well as the independent or explanatory or x variable
  x = eval(parse(text=paste0("dat$`", explanatory_variable_name, "`"))) ### numeric or categorical
  x = as.factor(gsub("-", "", x)) ### remove "-" because it will conflict with the string splitting if performing TukeyHSD()
  y = eval(parse(text=paste0("dat$`", response_variable_name, "`")))    ### numeric
  # y = rnorm(length(y)) ### null test
  ### merge them into a data frame for ease of handling
  ### while converting the x variable into both categorical and numeric variables
  df = tryCatch(
          data.frame(y=y, x_categorical=as.factor(as.character(x)), x_numeric=as.numeric(as.character(x))),
          warning=function(e){
            data.frame(y=y, x_categorical=as.factor(as.character(x)), x_numeric=as.numeric(x))
          }
        )
  df = df[complete.cases(df), ]
  ### transform the x axis into log-scale for ease of viewing
  if (LOG==TRUE){
    df$x_numeric=log(df$x_numeric, base=BASE)
    df$x_categorical=as.factor(round(df$x_numeric, 2))
  }
  ### extract the levels and unique values of the x variable
  x_levels = levels(df$x_categorical)
  x_numbers = tryCatch(
                as.numeric(as.character(x_levels)),
                warning=function(e){
                  as.numeric(as.factor(x_levels))
                }
              )
  ### calculate the summary statistics of the x and y vairables
  x_min = min(df$x_numeric)
  x_max = max(df$x_numeric)
  x_sd = sd(df$x_numeric)
  y_min = min(df$y)
  y_max = max(df$y)
  y_sd = sd(df$y)
  ### calculate the maximum interval between two levels of the x variable (divide by 2)
  max_x_interval = min(x_numbers[order(x_numbers)][2:length(x_numbers)] - x_numbers[order(x_numbers)][1:length(x_numbers)-1]) / 2
  ### repeat violin plot colours to fit the number of explanatory variable levels
  COLOURS = rep(COLOURS, times=ceiling(length(x_levels)/length(COLOURS)))
  ### define las: i.e. the orientation of the x-axis tick labels
  ### as well as the plot margins and the x-axis label
  max_nchar = max(unlist(lapply(x_levels, FUN=nchar)))
  if (max_nchar > 10){
    las = 2
    par(mar=c(max_nchar*0.70, 5, 7, 2))
    xlab=""
  } else {
    las =1
    par(mar=c(5, 5, 7, 2))
  }
  ### initialize the plot with or without the x-axis
  if (XTICKS==TRUE){
    ### for strictly categorical explanatory variable
    plot(x=c(x_min-max_x_interval, x_max+max_x_interval), y=c(y_min-y_sd, y_max+y_sd), type="n", main=title, xlab=xlab, ylab=ylab, las=las, xaxt="n")
    axis(side=1, at=x_numbers, labels=x_levels, las=las)
  } else {
    ### for continuous explanatory variable
    plot(x=c(x_min-max_x_interval, x_max+max_x_interval), y=c(y_min-y_sd, y_max+y_sd), type="n", main=title, xlab=xlab, ylab=ylab, las=las)
  }
  ### iteratively plot the density of each explanatory variable level
  for (i in 1:length(x_levels)){
    # i = 1
    subdat = droplevels(subset(df, x_categorical==x_levels[i]))
    ### calculate the summary statistics of the response variable (y): mean, standard deviation, standard error, and 95% confidence interval
    mu = mean(subdat$y)
    sigma = sd(subdat$y)
    se = sd(subdat$y)/sqrt(nrow(subdat)-1)
    ci = qnorm(((0.95)/2)+0.50) * se
    ### calculate the density with binning adjustment proportional the number of observations divded by 1x10^5
    d = density(subdat$y, adjust=max(c(1, nrow(subdat)/1e5)))
    ### restrict the range of the response variable to the input dataframe
    d$y = d$y[(d$x >= min(df$y)) & (d$x <= max(df$y))]
    d$x = d$x[(d$x >= min(df$y)) & (d$x <= max(df$y))]
    ### tranform the density (d$y) into the 0.00 to 1.00 range (in preparation to be mutiplied withe maximum interval variable: "max_x_interval")
    d.y_min = min(d$y)
    d.y_max = max(d$y)
    d$y = (d$y - d.y_min) / (d.y_max - d.y_min)
    ### define the x-axis points of the polygon defined as the density values left and right of the explanatory variable (df$x) level or value
    poly_x = c(x_numbers[i]-rev(d$y*max_x_interval), x_numbers[i]+(d$y*max_x_interval))
    ### define the y-axis points of the polygon defined as the range of values of the response variable (df$y)
    poly_y = c(rev(d$x), d$x)
    ### draw the polygon
    polygon(x=poly_x, y=poly_y, border=NA, col=COLOURS[i])
    ### plot the summary statistics
    arrows(x0=x_numbers[i], y0=mu+sigma, y1=mu-sigma, angle=90, code=3, lwd=2, length=0.1, col=BAR_COLOURS[1])
    arrows(x0=x_numbers[i], y0=mu+se, y1=mu-se, angle=90, code=3, lwd=2, length=0.1, col=BAR_COLOURS[2])
    arrows(x0=x_numbers[i], y0=mu+ci, y1=mu-ci, angle=90, code=3, lwd=2, length=0.1, col=BAR_COLOURS[3])
    points(x=x_numbers[i], y=mu, pch=20)
    # print(x_levels[i])
    # print(mean(subdat$y))
  }
  ### plot grid lines
  grid()
  ### show the summary statistics legend
  legend("bottomright", inset=c(0, 1), xpd=TRUE, horiz=TRUE, bty="n", col=unlist(BAR_COLOURS), cex=0.75, lty=1, lwd=2, legend=c("Standard Deviation", "Standard Error", "95% Confidence Interval"))
  ### return the levels and unique values of the x variable
  return(0)
}