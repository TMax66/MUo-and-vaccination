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


ggsurv <- ggsurvplot(fit,
          conf.int = TRUE,
           ggtheme = theme_bw(), 
          legend = "none",
          fun = "event", 
          palette = "blue"
)+
  labs(title = "Cumulative Probability of MUO days after vaccination", x= "days after vaccination", y = "Cumulative Probability to MUO diagnosis")


ggsurv$plot+

  ggplot2::  annotate("text", x = 1000, y = 0.13, label = "Probability of MUO diagnosis in 60 days since last vaccination = 0.13 (13%)")+
  geom_segment(aes(x = 0, y = 0.13, xend = 60, yend = 0.13), color = "red", linetype= "18")+
  geom_segment(aes(x = 60, y = 0.13, xend = 60, yend = 0),color = "red", linetype= "18" )+
  annotate("text", x= 60, y = -0.01, label = "60 days", color= "red")+
  annotate("text", x= -50, y = 0.13, label = "0.13", color = "red")







#annotate("text", x = 300,  y = 0.50, label = "Median Event Time = 193 days")+
library(gt)
cumulative_tab(fit) %>% 
  gt() %>% 
  fmt_number(
    columns = 5:8,
    decimals  =2
  ) %>% 
  gtsave("tab.rtf")
  




# 
autoplot(fit, fun = "event")+
  theme_bw()+
  
  labs(title = "Probability of MUO days after vaccination", x= "days after vaccination", y = "Probability to MUO diagnosis")

# 
# 
#  
# 
library(cmprsk)

ci_fit <-
  cuminc(
    ftime = df$timetod,
    fstatus = df$MUO
  )
