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
# Cleanup Criminaliteit
#----------------------------------------------------------------------------------------------------------------------

criminaliteit <- read_csv("data/raw/Criminaliteit Provincies.csv")

criminaliteit = criminaliteit %>%
  gather(key = temp, value = waarde, 2:169) %>%
  separate(col = temp, into = c("regio", "sub_categorie", "jaar"), sep = "_") %>%
  mutate(
    `Soort Misdrijf` = str_replace_all(`Soort Misdrijf`, regex("[0-9\\.]"), ""),
    regio = str_replace_all(regio, " \\(PV\\)", ""),
    jaar = parse_number(jaar)
  ) %>%
  spread(key = `Soort Misdrijf`, value = waarde) %>%
  group_by(regio, jaar) %>%
  summarise_at(vars(4:79), funs(sum)) %>%
  ungroup() %>%
  filter(regio != "Nederland" & regio != "Niet in te delen")

#----------------------------------------------------------------------------------------------------------------------
# Cleanup Veiligheidsgevoel
#----------------------------------------------------------------------------------------------------------------------

veiligheidsgevoel <- read_csv("data/raw/Veiligheidsgevoel Provincies.csv", col_types = cols(.default = col_character()))

veiligheidsgevoel = veiligheidsgevoel %>%
  gather(key = temp, value = waarde, 3:74) %>%
  separate(col = temp, into = c("sub_categorie", "regio", "jaar"), sep = "_") %>%
  mutate(
    Onderwerp = str_replace_all(Onderwerp, regex("\\|"), ""),
    regio = str_replace_all(regio, " \\(PV\\)", ""),
    jaar = parse_number(jaar)
  ) %>%
  filter(sub_categorie == "Waarde") %>%
  select(-Perioden, -sub_categorie) %>%
  spread(key = Onderwerp, value = waarde) %>%
  type_convert(locale = locale(decimal_mark = ",")) %>%
  mutate(
    gem_veiligheidsgevoel = rowMeans(select(.,3:32))
  )

#----------------------------------------------------------------------------------------------------------------------
# Join sets
#----------------------------------------------------------------------------------------------------------------------

nederland = inner_join(x = criminaliteit, y = veiligheidsgevoel, by = c("regio", "jaar"))

#----------------------------------------------------------------------------------------------------------------------
# Analysis
#----------------------------------------------------------------------------------------------------------------------

nederland_corr = nederland %>%
  group_by(jaar) %>%
  do(calc_correlation(., "gem_veiligheidsgevoel", c(3:78, 109))) %>%
  ungroup()

#----------------------------------------------------------------------------------------------------------------------
# Export
#----------------------------------------------------------------------------------------------------------------------

write_csv(nederland, "data/clean/Veiligheidsgevoel en Criminaliteit Nederland.csv")
write_csv(nederland_corr, "data/clean/Correlation Nederland.csv")
