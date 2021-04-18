#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: SDWIS Download
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 4/17/2021
# Purpose: Download SDWIS data from Envirofacts Data Service API
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#API Information: https://www.epa.gov/enviro/envirofacts-data-service-api
#SDWIS Data Model: https://www.epa.gov/enviro/envirofacts-data-service-api

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Environment --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear memory
remove(list=ls())

#Call relevant libraries
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Query Data from API-------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 download water system table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
water_system<-'https://enviro.epa.gov/enviro/efservice/Water_SYSTEM/STATE_CODE/=/PR/csv'
water_system<-GET(water_system)
water_system<- read_csv(water_system$url)

#2.2 Download violations tables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#create function to download violations for each water system
fun<-function(n){
  #Identify PWSID
  PWSID<-water_system$Water_SYSTEM.PWSID[n]
  
  #Create temp URL, query api, and download csv
  temp<-paste0('https://enviro.epa.gov/enviro/efservice/VIOLATION/PWSID/',PWSID,"/csv")
  temp<-GET(temp)
  temp<-read_csv(temp$url)
  
  #Fix rows
  temp<-temp %>% 
    mutate(across(everything(), as.character))
  
  #Export temp
  temp
}

#Apply function to indivdual PWSID events
violations<-lapply(seq(1,nrow(water_system)), fun)
violations<-bind_rows(violations)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Export and plot! ---------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export CSV files
write_csv(water_system, "data/water_system.csv")
write_csv(violations, "data/violations.csv")

#Make Plot
p1<-violations %>% 
  select(VIOLATION.COMPL_PER_BEGIN_DATE) %>% 
  mutate(VIOLATION.COMPL_PER_BEGIN_DATE = dmy(VIOLATION.COMPL_PER_BEGIN_DATE)) %>% 
  mutate(year = year(VIOLATION.COMPL_PER_BEGIN_DATE)) %>% 
  group_by(year) %>% 
  summarise(count=n()) %>% 
  arrange(year) %>% 
  filter(year<2020) %>% 
  ggplot(aes(x=year, y=count)) +
    geom_line(lty=2, lwd=1.1) +
    geom_point(pch=21, bg="grey70", col="grey30", cex=3) + 
    theme_bw() + 
    ggtitle("Municipal Water Quality Violations in Puerto Rico") +
    xlab('Year') +
    ylab('Number of New Violations') +
    theme(
      plot.title = element_text(size=16),
      axis.text = element_text(size=10),
      axis.title = element_text(size=14)
    )
p1

#Export
png("docs/puerto_rico_wq_violations.png", width=5.5, height = 3, units="in", res=150)
p1
dev.off()
    





