library(tidyverse)
library(lubridate)
library(readxl)
library(here)


 

dt  <- read.csv( here("data", "dati.csv"), sep=";")
dtMUO <- dt %>% 
  select(-X, -X.1, -X.2, -X.3) %>% 
  filter(!is.na(ddiagnosis) & !is.na(dlastvacc))  
  
# dtMUO %>% 
# mutate(ddiagnosis = ymd(ddiagnosis), 
#          dlastvacc = ymd(dlastvacc))  

dtMUO <- dtMUO[-c(64,65),]

dtMUO$ddiagnosis

ymd(dtMUO$ddiagnosis)


 
# t = as.numeric(dtMUO$ddiagnosis-dtMUO$dlastvacc)
# efftime = ifelse(t>=60, 1, 0)
# Frq = rep(1, nrow(dtMUO))
# rr = sum(efftime*(365-Frq))/sum(1-efftime)*Frq
#   

 
