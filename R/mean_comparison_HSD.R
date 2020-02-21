mean_comparison_HSD = function(formula, data=NULL, explanatory_variable_name, alpha=0.05, LOG=FALSE, BASE=10, x_levels=x_levels, x_numbers=x_numbers, PLOT=FALSE) {
  # explanatory_variable_name = "x1"; alpha = 0.05
	# mod = aov(formula, data=data)
  # anova_table = as.data.frame(anova(mod))
  # if (anova_table$Pr[rownames(anova_table) == explanatory_variable_name] < alpha){
  #   print(anova_table)
  #   print(paste0(explanatory_variable_name, " has a significant effect on the response variable!"))
  # } else {
  #   print(anova_table)
  #   print(paste0(explanatory_variable_name, " has a no significant effect on the response variable!"))
  # }
  ### parse the formula and generate the dataframe with explicit interaction terms if expressed in the formula
  df = parse_formula(formula=formula, data=data, IMPUTE=FALSE, IMPUTE_METHOD=mean)
  response_var = df[,1]; response_var_name = colnames(df)[1]
  explanatory_var = df[,2:ncol(df)]; explanatory_var_name = colnames(df)[2:ncol(df)]
  ### linear modelling
  mod = aov(formula, data=df)
  anova_table = as.data.frame(anova(mod))
  if (anova_table$Pr[rownames(anova_table) == explanatory_variable_name] < alpha){
    print(anova_table)
    print(paste0(explanatory_variable_name, " has a significant effect on the response variable!"))
  } else {
    print(anova_table)
    print(paste0(explanatory_variable_name, " has a no significant effect on the response variable!"))
  }
  ### computate the means per explanatory variable level
  means = eval(parse(text=paste0("aggregate(",  response_var_name, "~ `", explanatory_variable_name, "`, data=df, FUN=mean)")))
  means = means[order(means[,2], decreasing=TRUE), ]
  ### compute the HSD pairwise comparison
  hsd = eval(parse(text=paste0("as.data.frame(TukeyHSD(mod, conf.level=", 1.00-alpha, ")$`", explanatory_variable_name, "`)")))
  ### add "LEVEL_" string to allow for explanatory variable that are originally numeric to be easily set as list names
  factor_labels = matrix(paste0("LEVEL_", unlist(strsplit(rownames(hsd), "-"))), ncol=2, byrow=TRUE)
  hsd$factor1 = factor_labels[,1]
  hsd$factor2 = factor_labels[,2]
  factors_all = paste0("LEVEL_", as.character(means[,1]))
  ### initialize the list of HSD grouping of each response variable level
  GROUPING_LIST = eval(parse(text=paste0("list('LEVEL_", paste(as.character(means$x), collapse="'=c(), 'LEVEL_"), "'=c())")))
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
      ### test if the current factor level is the same as the non-siginificantly different factor level or if we are at the start
      # if (paste(f_letters, collapse="") != paste(g_letters, collapse="") | is.null(f_letters)){
      if ( !((sum(f_letters %in% g_letters)>0) | (sum(g_letters %in% f_letters)>0)) | is.null(f_letters) ) {
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
  if (LOG==TRUE){
    ### transform the level names into the corresponding level names we used previously (x_levels and x_numbers) because we will be merging dataframes below
    GROUPING_LIST$LEVELS = as.factor(round(log(as.numeric(as.character(GROUPING_LIST$LEVELS)), base=BASE), 2))
  }
  ### prepare the explanatory variable names and corresponding numbers
  X_LEVELS_AND_NUMBERS = data.frame(LEVELS=x_levels, NUMBERS=x_numbers)
  ### merge and append the grouping letters together with the means
  colnames(means) = c("LEVELS", "MEANS")
  MERGE_GROUPING_DF = merge(merge(GROUPING_LIST, X_LEVELS_AND_NUMBERS, by="LEVELS"), means, by="LEVELS")
  if(PLOT){
    text(x=MERGE_GROUPING_DF$NUMBERS, y=max(response_var)+sd(response_var), lab=as.character(MERGE_GROUPING_DF$GROUPING))
  }
  return(MERGE_GROUPING_DF)
}

