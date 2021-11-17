# Mann-Whitney mean comparison - a non-parametric alternative to HSD when the treatments are non-normal and/or with non-homogenous variance
#
# @usage mean_comparison_Mann_Whitney(formula, data=NULL, explanatory_variable_name, alpha=0.05,
#                    LOG=FALSE, BASE=10, PLOT=FALSE)
#
# @param formula R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
# @param data data.frame containing the response and explanatory variables which forms the formula above [default=NULL]
# @param explanatory_variable_name string referring to the variable name of the explanatory variable whose class means will be compared [mandatory]
# @param alpha numeric significance level for Mann-Whitney mean comparison [default=0.05]
# @param LOG logical referring to whether to transform the explanatory variable into the logarithm scale [default=FALSE]
# @param BASE numeric referring to the logarithm base to transform the explanatory variable with [default=1]
# @param PLOT logical referring to whether or not to plot the mean comparison grouping letters into an existing plot [default=FALSE]
#
# @return Mann-Whitney's pairwise mean comparison grouping table with response variable categorical means, grouping, level names and corresponding numeric counterparts
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
# MW = mean_comparison_Mann_Whitney(formula, data=data, explanatory_variable_name="x3", PLOT=TRUE)
#
#' @importFrom stats wilcox.test
#' @importFrom graphics text
#
mean_comparison_Mann_Whitney = function(formula, data=NULL, explanatory_variable_name, alpha=0.05, LOG=FALSE, BASE=10, PLOT=FALSE) {
  ### FOR TESTING:
  # data=NULL; explanatory_variable_name="x3"; alpha=0.05; LOG=FALSE; BASE=10; PLOT=FALSE; SHOW_SAMPLE_SIZE=FALSE
  ### parse the formula and generate the dataframe with explicit interaction terms if expressed in the formula
  df = parse_formula(formula=formula, data=data, IMPUTE=FALSE, IMPUTE_METHOD=mean)
  response_var = df[,1]; response_var_name = colnames(df)[1]
  ### computate the means per explanatory variable level
  means = eval(parse(text=paste0("aggregate(",  response_var_name, "~ `", explanatory_variable_name, "`, data=df, FUN=mean)")))
  colnames(means) = c("LEVELS", "MEANS")
  means = means[order(means$MEANS, decreasing=TRUE), ]
  ### prepare the explanatory variable names and corresponding numbers
  x_levels = eval(parse(text=paste0("levels(as.factor(df$`", explanatory_variable_name, "`))")))
  x_numbers = tryCatch(as.numeric(gsub("_", "-", as.character(x_levels))),
                  warning=function(e){as.numeric(as.factor(x_levels))})
  ### compute the Mann-Whitney pairwise mean comparison
  factor1 = c()
  factor2 = c()
  pvalue = c()
  for (i in 1:(length(x_levels)-1)){
      level_1 = x_levels[i]
      idx_1 = eval(parse(text=paste0("df$`", explanatory_variable_name, "` == '", level_1, "'")))
      y_1 = response_var[idx_1]
      for (j in (i+1):length(x_levels)){
          level_2 = x_levels[j]
          idx_2 = eval(parse(text=paste0("df$`", explanatory_variable_name, "` == '", level_2, "'")))
          y_2 = response_var[idx_2]
          ### Mann-Whitney test using the wilcox.text() function
          mann_whitney_out = stats::wilcox.test(x=y_1, y=y_2, altenative="two.sided")
          factor1 = c(factor1, level_1)
          factor2 = c(factor2, level_2)
          pvalue = c(pvalue, mann_whitney_out$p.value)          
      }
  }
  ### add "LEVEL_" string to allow for explanatory variable that are originally numeric to be easily set as list names
  mw = data.frame(factor1=paste0("LEVEL_", factor1), factor2=paste0("LEVEL_", factor2), p=pvalue)
  factors_all = paste0("LEVEL_", as.character(means$LEVELS))
  ### initialize the list of Mann-Whitney test grouping of each response variable level
  GROUPING_LIST = eval(parse(text=paste0("list('LEVEL_", paste(as.character(means$LEVELS), collapse="'=c(), 'LEVEL_"), "'=c())")))
  ### generate the vector of letters and numbers for grouping
  letters_vector = c(letters, LETTERS, 1:(nrow(mw)^2))
  ### iterate across response variable level
  letter_counter = 1
  for (f in factors_all){
    # f = factors_all[1]
    ### subset the current factor level
    submw = mw[(mw$factor1==f) | (mw$factor2==f), ]
    ### identify the factor levels that are not significantly from the current factor level: f
    nonsigfactors = unique(c(submw$factor1[submw$p > alpha], submw$factor2[submw$p > alpha]))
    nonsigfactors = nonsigfactors[!(nonsigfactors %in% f)]
    ### define the current letter grouping
    letter_add = letters_vector[letter_counter]
    new_letter_bool = 0 ### for testing if we need a new letter
    ### iterate across non-significantly different factor levels to the current factor
    for (g in nonsigfactors){
      # g = nonsigfactors[1]
      f_letters = eval(parse(text=paste0("GROUPING_LIST$`", f, "`"))) ### currect factor grouping
      g_letters = eval(parse(text=paste0("GROUPING_LIST$`", g, "`"))) ### grouping of the non-siginificantly different factor level
      ### if we have all significantly different means at the start
      if (is.na(g)){
        eval(parse(text=paste0("GROUPING_LIST$`", f, "` = c(", "GROUPING_LIST$`", g, "`, '", letter_add, "')")))
        new_letter_bool = new_letter_bool + 1
      } else if ( !((sum(f_letters %in% g_letters)>0) | (sum(g_letters %in% f_letters)>0)) | is.null(f_letters) ) {
        ### test if the current factor level is the same as the non-siginificantly different factor level or if we are at the start
        eval(parse(text=paste0("GROUPING_LIST$`", g, "` = c(", "GROUPING_LIST$`", g, "`, '", letter_add, "')")))
        new_letter_bool = new_letter_bool + 1
      }
    }
    ### add the current letter grouping
    if ((new_letter_bool>0) | (length(nonsigfactors)==0)){
      eval(parse(text=paste0("GROUPING_LIST$`", f, "` = c(", "GROUPING_LIST$`", f, "`, '", letter_add, "')")))
      letter_counter = letter_counter + 1
    }
  }
  ### prepare the grouping list
  GROUPING_LIST = as.matrix(lapply(GROUPING_LIST, FUN=paste, collapse=""))
  GROUPING_LIST = data.frame(LEVELS=gsub("LEVEL_", "", as.character(rownames(GROUPING_LIST))), GROUPING=as.character(GROUPING_LIST[,1]))
  # ### prepare the explanatory variable names and corresponding numbers
  # x_levels = eval(parse(text=paste0("levels(as.factor(df$`", explanatory_variable_name, "`))")))
  # x_numbers = tryCatch(as.numeric(gsub("_", "-", as.character(x_levels))),
  #                 warning=function(e){as.numeric(as.factor(x_levels))})
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
