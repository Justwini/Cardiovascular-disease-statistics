# Instalacja wymaganych paczek (jeśli nie są zainstalowane)
install.packages(c("ggplot2", "maps", "mapdata", "dplyr"))

# Załadowanie bibliotek
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

# Załadowanie danych
Morticd10_part5_rev <- read.csv("C:/Users/justy/Downloads/morticd10_part5/Morticd10_part5_rev")
head(Morticd10_part5_rev)
countryId<- read.csv("C:/Users/justy/Downloads/morticd10_part5/country_codes")
pop<- read.csv("C:/Users/justy/Downloads/morticd10_part5/pop")

# Unikalne kraje z danych WHO
uniqC <- unique(Morticd10_part5_rev$Country)

# Kraje WHO, które pasują do mapy
WHOcountry <- countryId %>%
  filter(country %in% uniqC) %>%
  pull(2)  # Wybieramy tylko kolumnę z nazwami krajów
WHOcountry

# Poprawa nazw krajów dla zgodności z `map_data("world")`
fix_country_names <- function(countries) {
  replace_map <- c(
    "Libyan Arab Jamahiriya" = "Libya", 
    "Antigua and Barbuda" = "Antigua", 
    "Virgin Islands (USA)" = "Virgin Islands", 
    "Saint Vincent and Grenadines" = "Saint Vincent", 
    "United States of America" = "USA", 
    "Brunei Darussalam" = "Brunei", 
    "Iran (Islamic Republic of)" = "Iran", 
    "Occupied Palestinian Territory" = "Palestine", 
    "Republic of Korea" = "South Korea", 
    "Republic of Moldova" = "Moldova", 
    "Russian Federation" = "Russia", 
    "United Kingdom" = "UK", 
    "Micronesia (Federated States of)" = "Micronesia"
  )
  
  countries <- recode(countries, !!!replace_map)  # Podmiana nazw
  unique(countries)  # Usunięcie ewentualnych duplikatów
}

# Naprawiona lista krajów
WHOcountry_fixed <- fix_country_names(WHOcountry)

# Tworzymy ramkę danych z informacją o zgłoszeniu danych do WHO
who_countries <- data.frame(region = WHOcountry_fixed, reported = 1)

# Pobranie danych mapy
mapdata <- map_data("world")


# Połączenie danych WHO z danymi mapy
mapdata <- left_join(mapdata, who_countries, by = "region")

# Zamiana wartości NA (kraje, które nie zgłosiły danych) na 0
mapdata$reported[is.na(mapdata$reported)] <- 0


# Tworzenie mapy w ggplot2
ggplot(mapdata, aes(x = long, y = lat, group = group, fill = factor(reported))) +
  geom_polygon(color = "black") +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "red"), 
                    labels = c("No Data", "Reported Data"), 
                    name = "WHO Data Submission") +
  theme_void() + 
  ggtitle("Countries Reporting Data to WHO")

####################################################################################################
#population size in 2021
Poland1=pop[pop$Country==countryId[countryId$name=="Poland",1]& pop$Year==2021 ,]
Poland1

barplot(as.numeric(Poland1[1,c(9:31)]))
Poland1[1,c(8:31)]

namesAge<-c("1","2","3","4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
            "40-34","45-49","50-54",
            "55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94",">95")
length(namesAge)


as.numeric(Poland1[1,c(9:31)])



#In column Cause code "1000" and "AAA"  refers total deaths from all causes combined.
#column Deaths1 refers to deaths from all ages.
# Deaths1 Deaths at all ages
# Deaths2 Deaths at age 0 year
# Deaths3 Deaths at age 1 year
# Deaths4 Deaths at age 2 years
# Deaths5 Deaths at age 3 years
# Deaths6 Deaths at age 4 years
# Deaths7 Deaths at age 5-9 years
# Deaths8 Deaths at age 10-14 years
# Deaths9 Deaths at age 15-19 years
# Deaths10 Deaths at age 20-24 years
# Deaths11 Deaths at age 25-29 years
# Deaths12 Deaths at age 30-34 years
# Deaths13 Deaths at age 35-39 years
# Deaths14 Deaths at age 40-44 years
# Deaths15 Deaths at age 45-49 years
# Deaths16 Deaths at age 50-54 years
# Deaths17 Deaths at age 55-59 years
# Deaths18 Deaths at age 60-64 years
# Deaths19 Deaths at age 65-69 years
# Deaths20 Deaths at age 70-74 years
# Deaths21 Deaths at age 75-79 years
# Deaths22 Deaths at age 80-84 years
# Deaths23 Deaths at age 85-89 years
# Deaths24 Deaths at age 90-94 years
# Deaths25 Deaths at age 95 years and above
# Deaths26 Deaths at age unspecified
# IM_deaths1 Infant deaths at age 0 day
# IM_deaths2 Infant deaths at age 1-6 days
# IM_deaths3 Infant deaths at age 7-27 days
# IM_deaths4 Infant deaths at age 28-364 days
#How many country send data

