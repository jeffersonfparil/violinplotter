# Tukey's mean comparison and grouping using fixed effect linear modeling
#
# @usage mean_comparison_HSD(formula, data=NULL, explanatory_variable_name, alpha=0.05,
#                    LOG=FALSE, BASE=10, PLOT=FALSE)
#
# @param formula R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
# @param data data.frame containing the response and explanatory variables which forms the formula above [default=NULL]
# @param explanatory_variable_name string referring to the variable name of the explanatory variable whose class means will be compared [mandatory]
# @param alpha numeric significance level for Tukey's mean comparison [default=0.05]
# @param LOG logical referring to whether to transform the explanatory variable into the logarithm scale [default=FALSE]
# @param BASE numeric referring to the logarithm base to transform the explanatory variable with [default=1]
# @param PLOT logical referring to whether or not to plot the mean comparison grouping letters into an existing plot [default=FALSE]
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
# formula = y ~ x1 + (x2:x3)
# DF = parse_formula(formula=formula, data=data)
# plot_violin_1x(dat=DF, response_variable_name="y", explanatory_variable_name="x2:x3")
# HSD = mean_comparison_HSD(formula, data=data, explanatory_variable_name="x2:x3", PLOT=TRUE)
#
#' @importFrom stats aov anova sd
#' @importFrom graphics text
#' @importFrom stats as.formula terms.formula
#
mean_comparison_HSD = function(formula, data=NULL, explanatory_variable_name, alpha=0.05, LOG=FALSE, BASE=10, PLOT=FALSE) {
  ### FOR TESTING:
  # data=data.frame(x1=rep(rep(rep(c(1:5), each=5), times=5), times=5), x2=rep(rep(letters[6:10], each=5*5), times=5), x3=rep(letters[11:15], each=5*5*5), y=rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1)))
  # formula=y ~ x1 + (x2:x3); explanatory_variable_name="x2:x3"; alpha=0.05; LOG=FALSE; BASE=10; PLOT=FALSE; SHOW_SAMPLE_SIZE=FALSE
  ### parse the formula and generate the dataframe with explicit interaction terms if expressed in the formula
  df = parse_formula(formula=formula, data=data, IMPUTE=FALSE, IMPUTE_METHOD=mean)
  response_var = df[,1]; response_var_name = colnames(df)[1]
  ### rename interaction effects and their levels by replacing ":" with "_"
  colnames(df) = gsub(":", "_", colnames(df))
  explanatory_variable_name = gsub(":", "_", explanatory_variable_name)
  terms = attr(terms.formula(formula), "term.labels")
  formula = paste0(unlist(strsplit(format(formula), "~"))[1], "~", paste(terms, collapse="+"))
  formula = as.formula(gsub(":", "_", format(formula)))
  eval(parse(text=paste0("df$`", explanatory_variable_name, "` = gsub(':', '_', df$`", explanatory_variable_name, "`)")))
  ### linear modelling
  mod = aov(formula, data=df)
  anova_table = as.data.frame(anova(mod))
  if (anova_table$Pr[rownames(anova_table) == explanatory_variable_name] < alpha){
    message(paste0(explanatory_variable_name, " has a significant effect on the response variable!"))
  } else {
    message(paste0(explanatory_variable_name, " has a no significant effect on the response variable!"))
    return(0)
  }
  ### computate the means per explanatory variable level
  means = eval(parse(text=paste0("aggregate(",  response_var_name, "~ `", explanatory_variable_name, "`, data=df, FUN=mean)")))
  colnames(means) = c("LEVELS", "MEANS")
  means = means[order(means$MEANS, decreasing=TRUE), ]
  ### compute the HSD pairwise comparison
  tryCatch(eval(parse(text=paste0("mod$model$`", explanatory_variable_name, "` = as.factor(mod$model$`", explanatory_variable_name, "`)"))),
    error=function(e){})
  hsd = suppressWarnings(eval(parse(text=paste0("as.data.frame(TukeyHSD(mod, conf.level=", 1.00-alpha, ")$`", explanatory_variable_name, "`)"))))
  ### add "LEVEL_" string to allow for explanatory variable that are originally numeric to be easily set as list names
  factor_labels = matrix(paste0("LEVEL_", unlist(strsplit(rownames(hsd), "-"))), ncol=2, byrow=TRUE)
  hsd$factor1 = factor_labels[,1]
  hsd$factor2 = factor_labels[,2]
  factors_all = paste0("LEVEL_", as.character(means$LEVELS))
  ### initialize the list of HSD grouping of each response variable level
  GROUPING_LIST = eval(parse(text=paste0("list('LEVEL_", paste(as.character(means$LEVELS), collapse="'=c(), 'LEVEL_"), "'=c())")))
  ### generate the vector of letters and numbers for grouping
  letters_vector = c(letters, LETTERS)
  if (length(letters_vector) < length(GROUPING_LIST)){
    letters_vector = c(letters_vector, 1:(length(GROUPING_LIST)-length(letters_vector)))
  } else {
    letters_vector = letters_vector[1:length(GROUPING_LIST)]
  }
  ### find the centres
  letter_counter = 1
  i = 1
  GROUPING_LIST[[i]] = letters_vector[letter_counter]
  while (i<length(factors_all)){
    f1 = factors_all[i]
    for (j in (i+1):length(factors_all)){
      # j = 2
      f2 = factors_all[j]
      p = hsd$`p adj`[((hsd$factor1==f1)&(hsd$factor2==f2)) | ((hsd$factor1==f2)&(hsd$factor2==f1))]
      if (p < alpha){
        letter_counter = letter_counter + 1
        GROUPING_LIST[[j]] = letters_vector[letter_counter]
        i = j
        break
      }
    }
    i = j
  }
  ### apped letter of centres where they are not significantly different
  for (f in names(GROUPING_LIST)[unlist(lapply(GROUPING_LIST, FUN=function(x){!is.null(x)}))]){
    # f = names(GROUPING_LIST)[unlist(lapply(GROUPING_LIST, FUN=function(x){!is.null(x)}))][3]
    subhsd = hsd[(hsd$factor1==f) | (hsd$factor2==f), ]
    idx = subhsd$`p adj` >= alpha
    if (sum(idx) > 0){
      subhsd = subhsd[idx, ]
      subhsd = unique(c(subhsd$factor1, subhsd$factor2))
      for (g in subhsd[subhsd != f]){
        # g = subhsd[subhsd != f][1]
        l = eval(parse(text=paste0("GROUPING_LIST$", f)))
        eval(parse(text=paste0("GROUPING_LIST$", g, " = paste0(GROUPING_LIST$", g, ",'", l, "')")))
      }
    }
  }
  ### prepare the grouping list
  GROUPING_LIST = as.matrix(lapply(GROUPING_LIST, FUN=paste, collapse=""))
  GROUPING_LIST = data.frame(LEVELS=gsub("LEVEL_", "", as.character(rownames(GROUPING_LIST))), GROUPING=as.character(GROUPING_LIST[,1]))
  ### prepare the explanatory variable names and corresponding numbers
  x_levels = eval(parse(text=paste0("levels(as.factor(df$`", explanatory_variable_name, "`))")))
  x_numbers = tryCatch(as.numeric(gsub("_", "-", as.character(x_levels))),
                  warning=function(e){as.numeric(as.factor(x_levels))})
  if (LOG==TRUE){
    ### transform the level names into the corresponding level names we used previously (x_levels and x_numbers) because we will be merging dataframes below
    if(sum(is.na(suppressWarnings(log(x_numbers, base=BASE)))) == 0){
      x_numbers = log(x_numbers, base=BASE)
    } else {
      x_numbers = log(x_numbers + (abs(min(x_numbers)) + 1), base=BASE)
    }
  }
  X_LEVELS_AND_NUMBERS = data.frame(LEVELS=x_levels, NUMBERS=x_numbers)
  ### merge and append the grouping letters together with the means
  MERGE_GROUPING_DF = merge(merge(GROUPING_LIST, X_LEVELS_AND_NUMBERS, by="LEVELS"), means, by="LEVELS")
  ### plot the HSD grouping letters
  if(PLOT){
    text(x=MERGE_GROUPING_DF$NUMBERS, y=max(response_var)+sd(response_var), lab=as.character(MERGE_GROUPING_DF$GROUPING))
  }
  ### output
  return(MERGE_GROUPING_DF)
}
