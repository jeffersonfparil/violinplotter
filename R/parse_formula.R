parse_formula = function(formula, data=NULL, IMPUTE=FALSE, IMPUTE_METHOD=mean){
  ### parse the input formula
  response_var = as.character(unlist(as.list(attr(terms(formula), "variables"))[-1]))[1]
  explanatory_var = as.character(unlist(as.list(attr(terms(formula), "term.labels"))))
  ### build the dataframe with explicit interaction variables (columns) if included in the formula
  non_interaction_terms = explanatory_var[!grepl(":", explanatory_var)]
  interaction_terms = explanatory_var[grepl(":", explanatory_var)]
  explanatory_list = list()
  for (i in 1:length(c(non_interaction_terms, interaction_terms))){
    # i = 1
    term = c(non_interaction_terms, interaction_terms)[i]
    explanatory_list[[i]] = eval(parse(text=paste0("paste(", paste(paste0("data$", unlist(strsplit(term, ":"))), collapse=","), ", sep=':')")))
  }
  # df =  eval(parse(text=paste0("data.frame(y=data$", response_var, ",", gsub("\"", "'", paste(paste(explanatory_list), collapse=", ")), ")")))
  df =  eval(parse(text=paste0("data.frame(y=data$", response_var, ",", gsub("-", "_", gsub("\"", "'", paste(paste(explanatory_list), collapse=", "))), ")")))
  ### impute missing response variable data?
  if (IMPUTE == TRUE) {
    idx_missing = is.na(df$y) | is.infinite(df$y)
    df$y[idx_missing] = IMPUTE_METHOD(df$y[!idx_missing])
    # eval(parse(text=paste0("data$", response_var, "[idx_missing] = IMPUTE_METHOD(df$", response_var, "[!idx_missing])")))
  }
  df = df[complete.cases(df), ]
  colnames(df) = c(response_var, non_interaction_terms, interaction_terms)
  return(df)
}

