library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(survminer)
library(survival)
library(ggfortify)

source("cumulative.r")

dt  <- read.csv( here("data", "dati.csv"), sep=";")

df <- dt  %>%
  filter(!is.na(ddiagnosis) & !is.na(dlastvacc)) %>%  
  select(id, diagnosis, MUO, dbirth, ddiagnosis, dlastvacc, ageatdiag) %>% 
  mutate(ddiagnosis = dmy(ddiagnosis), 
          dlastvacc = dmy(dlastvacc), 
         timetod = ddiagnosis-dlastvacc, 
         Cens = ifelse(MUO == "no", 1, 0)) %>% 
  filter(timetod >0)
         
fit<-survfit(Surv(timetod)~1,data = df )


ggsurvplot(fit,
          conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), 
          fun = "event", 
          palette = "blue"
)






autoplot(fit, fun = "event")+
  theme_bw()+
  labs(title = "Probability of MUO days after vaccination", x= "days after vaccination", y = "Probability to MUO diagnosis")



 

library(cmprsk)

ci_fit <- 
  cuminc(
    ftime = df$timetod, 
    fstatus = df$MUO
  )
