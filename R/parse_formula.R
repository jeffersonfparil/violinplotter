# Parse an R formula to generate a dataframe where the response and explanatory variables including interaction terms are explicitly written into columns
#
# @usage parse_formula(formula, data=NULL, IMPUTE=FALSE, IMPUTE_METHOD=mean)
#
# @param formula R's compact symbolic form to represent linear models with fixed additive and interaction effects (See ?formula for more information) [mandatory]
# @param data data.frame containing the response and explanatory variables which forms the formula above [default=NULL]
# @param IMPUTE logical referring to whether impute missing and infinite datapoints [default=FALSE]
# @param IMPUTE_METHOD imputation function to use [default=mean]
#
# @return Dataframe where the response and explanatory variables including interaction terms if applicable are explicitly written into columns
# @return All explanatory variables are converted into categorical variables
#
# @examples
# x1 = rep(rep(rep(c(1:5), each=5), times=5), times=5)
# x2 = rep(rep(letters[6:10], each=5*5), times=5)
# x3 = rep(letters[11:15], each=5*5*5)
# y = rep(1:5, each=5*5*5) + rnorm(rep(1:5, each=5), length(x1))
# data = data.frame(x1, x2, x3, y)
# formula = y ~ x1 + x2 + x3 + (x2:x3)
# DF = parse_formula(formula=formula, data=data)
#
#' @importFrom stats terms complete.cases
#
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
