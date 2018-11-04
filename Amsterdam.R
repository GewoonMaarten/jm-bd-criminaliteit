library(foreach)
library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(Hmisc)

#----------------------------------------------------------------------------------------------------------------------
# Correlation function
#----------------------------------------------------------------------------------------------------------------------

calc_correlation = function(tibble, field_name, cols) {
  
  correlation_types = c("spearman", "pearson")
  
  tibbles = list()
  i = 1
  for (correlation_type in correlation_types) {
    x = tibble[cols] %>%
      na.omit() %>% 
      as.matrix() %>%
      rcorr(type = correlation_type)
    
    x = x$P %>%
      as.data.frame() %>%
      rownames_to_column(var = "corr_field") %>%
      as_tibble() %>%
      select(corr_field, field_name) %>%
      rename(!!correlation_type := field_name)
    
    tibbles[[i]] = x
    
    i = i + 1
  }
  
  tibble = inner_join(x = tibbles[[1]], y = tibbles[[2]], by = "corr_field") %>%
    na.omit() %>%
    rowwise() %>%
    mutate(
      `S>P` = spearman>pearson,
      field_name = field_name
    ) %>%
    select(field_name, corr_field, spearman, pearson, `S>P`)
  
  return(tibble)
}

#----------------------------------------------------------------------------------------------------------------------
# Cleanup
#----------------------------------------------------------------------------------------------------------------------

sheets = list(
  `2014` = 3,
  `2015` = 6,
  `2016` = 9,
  `2017` = 12
)

tibbles = list()

foreach (key = names(sheets), val = sheets) %do% {
  tibbles[[key]] = read_xlsx("data/raw/Veiligheidsgevoel en Criminaliteit Amsterdam.xlsx", sheet = val, skip = 5) %>%
    type_convert(na = c("#NULL!")) %>%
    mutate(jaar = parse_number(key)) %>%
    select(-X__1)
}

amsterdam = bind_rows(tibbles)

#----------------------------------------------------------------------------------------------------------------------
# Analysis
#----------------------------------------------------------------------------------------------------------------------

amsterdam_corr = amsterdam %>%
  group_by(jaar) %>%
  do(calc_correlation(., "ipsl_onveiligheids-gevoelens", c(6, 9:174))) %>%
  ungroup()

#----------------------------------------------------------------------------------------------------------------------
# Export
#----------------------------------------------------------------------------------------------------------------------
  
write_csv(amsterdam, "data/clean/Veiligheidsgevoel en Criminaliteit Amsterdam.csv")
write_csv(amsterdam_corr, "data/clean/Correlation Amsterdam.csv")

