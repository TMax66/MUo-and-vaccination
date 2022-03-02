library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(survminer)
library(survival)
library(ggfortify)

 

dt  <- read.csv( here("data", "dati.csv"), sep=";")

df <- dt  %>%
  filter(!is.na(ddiagnosis) & !is.na(dlastvacc)) %>%  
  select(id, diagnosis, MUO, dbirth, ddiagnosis, dlastvacc, ageatdiag) %>% 
  mutate(ddiagnosis = dmy(ddiagnosis), 
          dlastvacc = dmy(dlastvacc), 
         timetod = ddiagnosis-dlastvacc, 
         Cens = ifelse(MUO == "no", 1, 0)) %>% 
  filter(timetod >0)
         
fit<-survfit(Surv(timetod, Cens)~1,data = df )

ggsurvplot(1-fit, data = df, color = "lightblue",xlim = c(0,500))
 

autoplot(fit, fun = "event")+
  theme_bw()



 


