library(httr)
library(tidyverse)
library(jsonlite)
library(janitor)

# Source: https://opendata.cbs.nl/ODataApi/odata/83901NED
urls = c(
  "https://opendata.cbs.nl/ODataApi/odata/83901NED/TableInfos",
  "https://opendata.cbs.nl/ODataApi/odata/83901NED/UntypedDataSet",
  "https://opendata.cbs.nl/ODataApi/odata/83901NED/TypedDataSet",
  "https://opendata.cbs.nl/ODataApi/odata/83901NED/DataProperties",
  "https://opendata.cbs.nl/ODataApi/odata/83901NED/CategoryGroups",
  "https://opendata.cbs.nl/ODataApi/odata/83901NED/WijkenEnBuurten"
  )

# Haalt alle wijken buurt van elke gemeente op
get_wijken_en_buurten = function() {
  wijken_en_buurten_request = GET(url = urls[6]) %>% 
    stop_for_status()
  wijken_en_buurten_json = content(wijken_en_buurten_request, as = "text",encoding = "UTF-8") %>%
    fromJSON()
  
  return(wijken_en_buurten_json$value)
}

#Haalt de gemeente code op, op basis van naam
get_gemeente_code = function(gemeente) {
  wijken_en_buurten_tibble = get_wijken_en_buurten()
  
  gemeente_codes = wijken_en_buurten_tibble %>%
    filter(grepl(gemeente, Title) & grepl("GM", Key)) %>%
    transmute(GM_code = substring(Key, 3, 6))
  
  return(gemeente_codes[1,1])
}

#Haalt de buurt namen op basis van gemeente code
get_buurt_namen_by_gemeente_code = function(code) {
  wijken_en_buurten_tibble = get_wijken_en_buurten()
  
  buurt_namen = wijken_en_buurten_tibble %>%
    filter(grepl(code, Municipality)) %>%
    select(Key, Title)
  
  return(buurt_namen)
}

#Haalt de criminaliteits cijfers op, op basis van een code
get_criminaliteit_per_gemeente_code = function(code) {
  query_params = list(`$filter` = str_c("((substring(WijkenEnBuurten,2,4) eq '", code, "')))"))

  criminaliteit_request = GET(urls[3], query = query_params) %>% 
    stop_for_status()
  criminaliteit_json = content(criminaliteit_request, as = "text", encoding = "UTF-8") %>%
    fromJSON()
  
  return(criminaliteit_json$value)
}

amsterdam_code = get_gemeente_code("Amsterdam")

criminaliteit_amsterdam = get_criminaliteit_per_gemeente_code(amsterdam_code)
criminaliteit_buurt_namen_amsterdam = get_buurt_namen_by_gemeente_code(amsterdam_code)

criminaliteit_amsterdam = right_join(criminaliteit_amsterdam, criminaliteit_buurt_namen_amsterdam, by = c("Codering_3" = "Key"))

View(criminaliteit_amsterdam)

#namen opschonen, bijv. _12 weghalen

colnames(criminaliteit_amsterdam) <- str_replace(colnames(criminaliteit_amsterdam), regex("_[0-9]+"),"")#namen opschonen, hoofdletters en _

criminaliteit_amsterdam <- clean_names(criminaliteit_amsterdam,case= c("snake"))
criminaliteit_amsterdam <- criminaliteit_amsterdam %>%
  gather(key = "categorie", value = "waarde", 8:32)

View(criminaliteit_amsterdam)
