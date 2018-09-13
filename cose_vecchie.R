# {immunization <- immunization %>% select_if(~sum(!is.na(.)) > 0)
# death <- death %>% select_if(~sum(!is.na(.)) > 0)
# Gdp <- Gdp %>% select_if(~sum(!is.na(.)) > 0)
#   
# years_imm <- immunization[,5:ncol(immunization)] %>% 
#     filter_all(any_vars(!is.na(.))) 
# years_death <- death[,5:ncol(death)] %>% 
#     filter_all(any_vars(!is.na(.))) 
# years_Gdp <- Gdp[,5:ncol(Gdp)] %>% 
#   filter_all(any_vars(!is.na(.))) 
# 
#   
# immunization <- semi_join(immunization, years_imm, by=c("X2014", "X2015", "X2016")) %>% 
#   select(-Indicator.Name, -Indicator.Code)
# death <- semi_join(death, years_death, by=c("X2014", "X2015", "X2016")) %>% 
#   select(-Indicator.Name, -Indicator.Code)
# Gdp <- semi_join(Gdp, years_Gdp, by=c("X2014", "X2015", "X2016")) %>% 
#   select(-Indicator.Name, -Indicator.Code)
# }




###=+++++ Adding Region in Imm, Death, Gdp datasets by left_join
# subselection of regions and code
#{

#   immunization$Country.Code <- as.character(immunization$Country.Code)
#   immunization <- left_join(immunization, country_region, "Country.Code") %>%
#     select(Region, everything()) %>%
#     select(-Country.y) %>%
#     rename(Country = Country.x)
# 
#   death$Country.Code <- as.character(death$Country.Code)
#   death <- left_join(death, country_region, "Country.Code") %>%
#     select(Region, everything()) %>%
#     select(-Country.y) %>%
#     rename(Country = Country.x)
#   
#   Gdp$Country.Code <- as.character(Gdp$Country.Code)
#   Gdp <- left_join(Gdp, country_region, "Country.Code") %>%
#     select(Region, everything()) %>%
#     select(-Country.y) %>%
#     rename(Country = Country.x)
# }
###++++++

#########===== Adding Continent =====#########
{
  # countries_world <- countries_world %>% 
  #   mutate(Continent = ifelse(Region=="NORTHERN AMERICA" | Region=="LATIN AMER. & CARIB", "America", ifelse(Region=="NORTHERN AFRICA" | Region=="SUB-SAHARAN AFRICA", "Africa", ifelse(Region=="BALTICS" | Region=="WESTERN EUROPE" | Region=="EASTERN EUROPE" | Region=="C.W. OF IND. STATES", "Europe", ifelse(Region=="NEAR EAST" | Region=="ASIA (EX. NEAR EAST)", "Asia", "Oceania"))))) %>%
  #   select(Country, Continent, everything())
  
  # immunization <- immunization %>% 
  #   mutate(Continent = ifelse(Region=="NORTHERN AMERICA" | Region=="LATIN AMER. & CARIB", "America", ifelse(Region=="NORTHERN AFRICA" | Region=="SUB-SAHARAN AFRICA", "Africa", ifelse(Region=="BALTICS" | Region=="WESTERN EUROPE" | Region=="EASTERN EUROPE" | Region=="C.W. OF IND. STATES", "Europe", ifelse(Region=="NEAR EAST" | Region=="ASIA (EX. NEAR EAST)", "Asia", "Oceania"))))) %>%
  #   select(Country, Continent, everything())
  # 
  # death <- death %>% 
  #   mutate(Continent = ifelse(Region=="NORTHERN AMERICA" | Region=="LATIN AMER. & CARIB", "America", ifelse(Region=="NORTHERN AFRICA" | Region=="SUB-SAHARAN AFRICA", "Africa", ifelse(Region=="BALTICS" | Region=="WESTERN EUROPE" | Region=="EASTERN EUROPE" | Region=="C.W. OF IND. STATES", "Europe", ifelse(Region=="NEAR EAST" | Region=="ASIA (EX. NEAR EAST)", "Asia", "Oceania"))))) %>%
  #   select(Country, Continent, everything())
  # 
  # Gdp <- Gdp %>% 
  #   mutate(Continent = ifelse(Region=="NORTHERN AMERICA" | Region=="LATIN AMER. & CARIB", "America", ifelse(Region=="NORTHERN AFRICA" | Region=="SUB-SAHARAN AFRICA", "Africa", ifelse(Region=="BALTICS" | Region=="WESTERN EUROPE" | Region=="EASTERN EUROPE" | Region=="C.W. OF IND. STATES", "Europe", ifelse(Region=="NEAR EAST" | Region=="ASIA (EX. NEAR EAST)", "Asia", "Oceania"))))) %>%
  #   select(Country, Continent, everything())
  
}
#########==========#########


