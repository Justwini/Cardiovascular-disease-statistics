setwd("~/")
Morticd10_part5_rev <- read.csv("C:/Users/justy/Downloads/morticd10_part5/Morticd10_part5_rev")
head(Morticd10_part5_rev)
#sort(list(Morticd10_part5_rev$Cause)) 
poland<-Morticd10_part5_rev$Cause[Morticd10_part5_rev$Country==4230]
poland 
dane<-table(poland)
barplot(dane[0:10])

dane[0:10]
countryname<- read.csv("C:/Users/justy/Downloads/morticd10_part5/country_codes")
y<-Morticd10_part5_rev[Morticd10_part5_rev$Cause=="1000",c(1:39)]
View(y)
dim(y)
c4303<-Morticd10_part5_rev[Morticd10_part5_rev$Country==4303 & Morticd10_part5_rev$Year==2017 ,c(1:39)]
s=sum(c4303$Deaths1[c4303$Cause!="1000" & c4303$Sex==1] ) 
sy=sum(c4303$Deaths1[c4303$Cause=="1000"]) 
sy=c4303$Deaths1[c4303$Cause=="1000"& c4303$Sex==1]
s
sy
#wchich country have the most death count
#wchich country the mortality rate of children are the lareger
#wchich cause od death are most what is the most common cause of death, whole, by country, by age
length(unique(Morticd10_part5_rev$Country))  
length(unique(Morticd10_part5_rev$Country)) 
dim(countryname)
z=Morticd10_part5_rev[Morticd10_part5_rev$Cause=="1000"& Morticd10_part5_rev$Year==2017 & Morticd10_part5_rev$Sex==2,]
dim(z)
Morticd10_part5_rev[Morticd10_part5_rev$Deaths1==max(Morticd10_part5_rev$Death1),]
max(Morticd10_part5_rev$Death1)
AAA=Morticd10_part5_rev[Morticd10_part5_rev$Cause=="AAA",]
length(AAA[1,]) 
dim(AAA)
library(dplyr)
barplot(as.numeric(AAA[16,c(11:34)]),names.arg = names(AAA[1,c(11:34)]))
c=AAA[1,c(11:34)]
as.numeric(c)
c=c(1:25)
names(AAA[1,c(11:35)])

tmax(Morticd10_part5_rev$Deaths1)
Morticd10_part5_rev[Morticd10_part5_rev$Deaths1==max(Morticd10_part5_rev$Deaths1),]
t=Morticd10_part5_rev %>%
  group_by(Year,Country,Sex)%>%
  filter(Deaths1 == max(Deaths1))%>%
select(Deaths1,Cause)

ta=Morticd10_part5_rev %>%
  filter(Country == 3068)%>%
  select(Country,Deaths1,Cause,Year,Sex)

ta=Morticd10_part5_rev %>%
  filter((Country == 4308 |Country == 4310 |Country == 4320 |Country == 4330 )&Year==2017& Cause=="AAA" )%>%
  select(Country,Deaths1,Cause,Year,Sex)

ee=countryname %>%
  filter(grepl('United Kingdom', name) )
  
ee=countryname %>%
  filter(grepl('Antigua', name) )
  
length(unique(ta$Country))
ta=Morticd10_part5_rev %>%
  filter(Country == 2450 & Cause=="I713")%>%
  select(Country,Deaths1,Cause,Year,Sex)

install.packages("tidyverse")
library("tidyverse")
mapdata<-map_data("word")
1