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
test_trend_ca <- function(drug, data = clean_fars){
  
  if (drug == "Nonalcohol") {
  simple_fars2 <- data %>%
    mutate(drug_type = as.character(drug_type)) %>%
    filter(drug_type != "Alcohol") %>%
    group_by(unique_id, year) %>%
    summarize(positive_for_drug = any(positive_for_drug)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  
  ca_result1 <- prop.trend.test(x = simple_fars2$positive,
                                n = simple_fars2$trials)
  
  ca_result <- as_tibble(data.frame(Z = round(sqrt(ca_result1$statistic), 1),
                                      p.value = round(ca_result1$p.value, 3)))

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
                                    p.value = round(ca_result2$p.value, 3)))
  
  } 
  
  row.names(ca_result) <- NULL
  return(ca_result)
  
}

#function 3

test_trend_log_reg <- function(drug, data = clean_fars){
  
  if (drug == "Nonalcohol") {
    simple_fars2 <- data %>%
      mutate(drug_type = as.character(drug_type)) %>%
      filter(drug_type != "Alcohol") %>%
      group_by(unique_id, year) %>%
      summarize(positive_for_drug = any(positive_for_drug)) %>%
      ungroup()
    
    log_reg <- glm(positive_for_drug ~ year, data = simple_fars2,
                   family = binomial(link = "logit"))
    
    trend_sum <- as_tibble(data.frame(summary(log_reg)$coefficients))
    
    trend_sum <- select(trend_sum, 3, 4) %>%
                 slice(-1) %>%
                 rename(Z = z.value, p.value = Pr...z..) %>%
                 mutate(Z = round(Z, 1), p.value = round(p.value, 3))
  }
  
  else {
    simple_fars <- data %>%
      mutate(drug_type = as.character(drug_type)) %>%
      filter(drug_type == drug)
      
      log_reg <- glm(positive_for_drug ~ year, data = simple_fars,
                    family = binomial(link = "logit"))
    
    trend_sum <- as_tibble(data.frame(summary(log_reg)$coefficients))
    
    trend_sum <- select(trend_sum, 3, 4) %>%
                 slice(-1) %>%
                 rename(Z = z.value, p.value = Pr...z..) %>%
                 mutate(Z = round(Z, 1), p.value = round(p.value, 3))
    
  } 
  
  row.names(trend_sum) <- NULL
  return(trend_sum)
}
