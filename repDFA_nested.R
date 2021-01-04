repDFA_nested <- function(xdata, testfactor, balancefactor, varnames, npercomb = NULL, nrand = 20) {
  # empty matrix for storing the results
  res <- matrix(ncol = 7, nrow = nrand)
  colnames(res) <- c("rand", "df1_best", "df1_load", "df1_pct", "df2_best", "df2_load", "df2_pct")
  
  for(i in 1:nrand) {
    res[i, "rand"] <- i
    # create balanced data set
    tempdata <- xdata %>% group_by(.dots = balancefactor) %>% sample_n(npercomb)
    # calculate DFA
    dfares <- candisc(lm(as.matrix(tempdata[, varnames]) ~ as.matrix(tempdata[, testfactor])))
    strucmat <- dfares$structure
    pcts <- round(dfares$pct/100, 3)
    # extract relevant info
    res[i, "df1_best"] <- rownames(strucmat)[which.max(abs(strucmat[, 1]))]
    res[i, "df1_load"] <- round(strucmat[, 1][which.max(abs(strucmat[, 1]))], 3)
    res[i, "df1_pct"] <- pcts[1]
    
    # do so for the second function if there is one
    if(ncol(strucmat) >= 2) {
      res[i, "df2_best"] <- rownames(strucmat)[which.max(abs(strucmat[, 2]))]
      res[i, "df2_load"] <- round(strucmat[, 2][which.max(abs(strucmat[, 2]))], 3)
      res[i, "df2_pct"] <- pcts[2]
    }
  }
  
  # transform into data frame for output
  res <- data.frame(res)
  res[, c("rand", "df1_load", "df1_pct", "df2_load", "df2_pct")] <- apply(res[, c("rand", "df1_load", "df1_pct", "df2_load", "df2_pct")], 2, as.numeric)
  return(res)
}
