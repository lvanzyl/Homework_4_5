# x is number of drivers testing positive for drug, n is total number of
#non-missing observations

# first function
perc_cis <- function(x, n) {
  
  p <- x/n
  p_perc <- round(p * 100, digits = 1)
  
  se_p <- sqrt((p * (1-p))/n)
  
  upper_ci <- round(p_perc + (1.96 * se_p) * 100, digits = 1)
  lower_ci <- round(p_perc - (1.96 * se_p) * 100, digits = 1)
  
  paste0(p_perc, "% (", lower_ci, "%, ", upper_ci, "%)")
}

# second function



