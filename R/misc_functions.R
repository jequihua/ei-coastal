# Coerce list of integer valued variables to factor
factorCols = function(bnbrik_df,categorical_var_vec){
  for (i in 1:length(categorical_var_vec)){
    bnbrik_df[,
              categorical_var_vec[i]]=as.factor(bnbrik_df[,
                                                          categorical_var_vec[i]])
  }
  return(bnbrik_df)
}

# Discretize list of numeric valued variables
discretizeCols = function(bnbrik_df,
                          numeric_var_vec,
                          breaks_vec=rep(5,length(numeric_var_vec)),
                          method="interval"){
  
  bnbrik_df[,numeric_var_vec] = bnlearn::discretize(bnbrik_df[,numeric_var_vec],
                                                    breaks=breaks_vec,
                                                    method=method)
  return(bnbrik_df)
}
