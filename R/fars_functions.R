# x is number of drivers testing positive for drug, n is total number of
#non-missing observations

library(stats)
library(tidyverse)

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

# x, a vector with the number of drivers testing positive in each year
# n, a vector with the total number of non-missing observations in each year

test_trend_ca <- function(data = clean_fars, drug){
  
  if (drug == "Nonalcohol") {
  simple_fars2 <- data %>%
    mutate(drug_type = as.character(drug_type)) %>%
    filter(drug_type != "Alcohol") %>%
    group_by(year) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  
  ca_result1 <- prop.trend.test(x = simple_fars2$positive,
                                n = simple_fars2$trials)
  
  ca_result <- as_tibble(data.frame(Z = round(sqrt(ca_result1$statistic), 1),
                                      p_value = round(ca_result1$p.value, 3)))

  }
  
  else {
  simple_fars <- data %>%
    mutate(drug_type = as.character(drug_type)) %>%
    filter(drug_type == drug) %>%
    group_by(year) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  
  ca_result2 <- prop.trend.test(x = simple_fars$positive,
                               n = simple_fars$trials)
  
  ca_result <- as_tibble(data.frame(Z = round(sqrt(ca_result2$statistic), 1),
                                    p_value = round(ca_result2$p.value, 3)))
  
  } 
  
  row.names(ca_result) <- NULL
  return(ca_result)
  
}


test_trend_ca(drug = "Alcohol")


test_trend_ca(drug = "Stimulant", data = clean_fars)
test_trend_ca(drug = "Nonalcohol")
