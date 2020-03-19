#' Tukey's mean comparison and grouping using fixed effect linear modeling with optional plotting of grouping letters
#'
#' @usage mean_comparison_HSD(formula, data=NULL, explanatory_variable_name, alpha=0.05,
#'                    LOG=FALSE, BASE=10, PLOT=FALSE)
#'
#' @param formula R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
#' @param data data.frame containing the response and explanatory variables which forms the formula above [default=NULL]
#' @param explanatory_variable_name string referring to the variable name of the explanatory variable whose class means will be compared [mandatory]
#' @param alpha numeric significance level for Tukey's mean comparison [default=0.05]
#' @param LOG logical referring to whether to transform the explanatory variable into the logarithm scale [default=FALSE]
#' @param BASE numeric referring to the logarithm base to transform the explanatory variable with [default=1]
#' @param PLOT logical referring to whether plot the mean comparison grouping letters into an existing plot [default=FALSE]
#'
#' @return Tukey's honest significant difference grouping table with response variable categorical means, grouping, level names and corresponding numeric counterparts
#' @return Appends honest significant difference grouping letters into an existing plot
#'
#' @examples
#' x1 = rep(rep(rep(c(1:5), each=5), times=5), times=5)
#' x2 = rep(rep(letters[6:10], each=5*5), times=5)
#' x3 = rep(letters[11:15], each=5*5*5)
#' y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1))
#' data = data.frame(x1, x2, x3, y)
#' formula = y ~ x1 + x2 + x3 + (x2:x3)
#' DF = parse_formula(formula=formula, data=data)
#' plot_violin_1x(dat=DF, response_variable_name="y", explanatory_variable_name="x3")
#' HSD = mean_comparison_HSD(formula, data=data, explanatory_variable_name="x3", PLOT=TRUE)
#'
#' @importFrom stats aov anova sd
#' @importFrom graphics text
#'
#' @export
mean_comparison_HSD = function(formula, data=NULL, explanatory_variable_name, alpha=0.05, LOG=FALSE, BASE=10, PLOT=FALSE) {
  ### parse the formula and generate the dataframe with explicit interaction terms if expressed in the formula
  df = parse_formula(formula=formula, data=data, IMPUTE=FALSE, IMPUTE_METHOD=mean)
  response_var = df[,1]; response_var_name = colnames(df)[1]
  # explanatory_var = df[,2:ncol(df)]; explanatory_var_name = colnames(df)[2:ncol(df)]
  ### linear modelling
  mod = aov(formula, data=df)
  anova_table = as.data.frame(anova(mod))
  if (anova_table$Pr[rownames(anova_table) == explanatory_variable_name] < alpha){
    message(paste0(explanatory_variable_name, " has a significant effect on the response variable!"))
  } else {
    message(paste0(explanatory_variable_name, " has a no significant effect on the response variable!"))
  }
  ### computate the means per explanatory variable level
  means = eval(parse(text=paste0("aggregate(",  response_var_name, "~ `", explanatory_variable_name, "`, data=df, FUN=mean)")))
  colnames(means) = c("LEVELS", "MEANS")
  means = means[order(means$MEANS, decreasing=TRUE), ]
  ### compute the HSD pairwise comparison
  hsd = eval(parse(text=paste0("as.data.frame(TukeyHSD(mod, conf.level=", 1.00-alpha, ")$`", explanatory_variable_name, "`)")))
  ### add "LEVEL_" string to allow for explanatory variable that are originally numeric to be easily set as list names
  factor_labels = matrix(paste0("LEVEL_", unlist(strsplit(rownames(hsd), "-"))), ncol=2, byrow=TRUE)
  hsd$factor1 = factor_labels[,1]
  hsd$factor2 = factor_labels[,2]
  factors_all = paste0("LEVEL_", as.character(means$LEVELS))
  ### initialize the list of HSD grouping of each response variable level
  GROUPING_LIST = eval(parse(text=paste0("list('LEVEL_", paste(as.character(means$LEVELS), collapse="'=c(), 'LEVEL_"), "'=c())")))
  ### generate the vector of letters and numbers for grouping
  letters_vector = c(letters, LETTERS, 1:(nrow(hsd)^2))
  ### iterate across response variable level
  letter_counter = 1
  for (f in factors_all){
    # f = factors_all[1]
    ### subset the current factor level
    subhsd = hsd[(hsd$factor1==f) | (hsd$factor2==f), ]
    ### identify the factor levels that are not significantly from the current factor level: f
    nonsigfactors = unique(c(subhsd$factor1[subhsd$p > 0.05], subhsd$factor2[subhsd$p > 0.05]))
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
  if(PLOT){
    text(x=MERGE_GROUPING_DF$NUMBERS, y=max(response_var)+sd(response_var), lab=as.character(MERGE_GROUPING_DF$GROUPING))
  }
  return(MERGE_GROUPING_DF)
}
