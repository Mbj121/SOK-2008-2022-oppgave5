library(ggplot2)
library(ineq)
library(tidyverse)
library(dplyr)
library(gglorenz)
library(rjstat)
library(janitor)
library(scales)
library(httr)
library(PxWebApiData)



ssb1 <- "https://data.ssb.no/api/v0/no/table/05185/"


mm <- '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "1", "2" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "agg:Verdensdel2", "values": [ "b11", "b12", "b13", "b14", "b2", "b3", "b4", "b5", "b6", "b8", "b9" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022" ] } } ], "response": { "format": "json-stat2" } }'

mm1 <- POST(ssb, body = mm, encode = "json", verbose())

mm2 <- fromJSONstat(content(mm1, "text")) %>% 
  clean_names() %>% 
  as.data.frame()


mm2 <- mm2 %>%
  group_by(landbakgrunn, ar) %>% 
  mutate(total_innvandring = sum(value)) %>% 
  filter(kjonn == "Menn")



ggplot(mm2, aes(ar , total_innvandring, fill = landbakgrunn))+
  geom_col()+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "År",
       y= "Antall innvandre") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "Områder") +
  theme_bw()




#oppg 2

ssb2 <- 'https://data.ssb.no/api/v0/no/table/13215/'

nn <- '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "0" ] } }, { "code": "Alder", "selection": { "filter": "item", "values": [ "15-74" ] } }, { "code": "InnvandrKat", "selection": { "filter": "item", "values": [ "B" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "item", "values": [ "015a" ] } }, { "code": "NACE2007", "selection": { "filter": "agg:NACE260InnvGrupp2", "values": [ "SNI-01-03", "SNI-05-09", "SNI-10-33", "SNI-35-39", "SNI-41-43", "SNI-45-47", "SNI-49-53", "SNI-49.3", "SNI-55", "SNI-56", "SNI-58-63", "SNI-64-66", "SNI-68-75", "SNI-77-82", "SNI-78.2", "SNI-81.2", "SNI-84", "SNI-85", "SNI-86-88", "SNI-90-99", "SNI-00" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2021" ] } } ], "response": { "format": "json-stat2" } }'

nn1 <- POST(ssb2, body = nn, encode = "json", verbose())

nn2 <- fromJSONstat(content(nn1, "text")) %>% 
  clean_names() %>% 
  as.data.frame()

nn2 <- nn2 %>% 
  rename('Sektorer' = 'naering_sn2007',
         "År" = "ar")  %>% 
  subset(select= -c(statistikkvariabel))

nn2$Sektorer <- gsub("[0-9.-]","", nn2$Sektorer)


nn2 %>% 
  ggplot(aes(Sektorer , value, fill = Sektorer)) + 
  geom_col() +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Sysselsatte",
       title = "Innvandrere i arbeid fordelt på ulik sektor")

