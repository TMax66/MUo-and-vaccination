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

g
 

autoplot(fit, fun = "event")+
  theme_bw()+
  labs(title = "Probability of MUO days after vaccination", x= "days after vaccination", y = "Probability to MUO diagnosis")



 


