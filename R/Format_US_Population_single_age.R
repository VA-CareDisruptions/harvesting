
#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

Format_US_Population_single_age <- function(){
  #these are all July 1 estimates of popsize for the year

  
   #https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/asrh/
  
  d1 <-  read.csv(file="./Data/nc-est2020-agesex-res.csv") %>%
    filter(SEX==2 & AGE !=999) %>%
    select(-POPESTIMATE2020, -SEX, )
  
  
  e1 <-  read.csv(file="./Data/nc-est2022-agesex-res.csv") %>%
    filter(SEX==2 & AGE !=999)%>%
    select( -SEX, )

  
  d2 <- d1 %>%
    left_join(e1, by='AGE')
  
  pop2 <- reshape2::melt(d2, id.vars=c('AGE')) %>%
    rename(agey=AGE, pop=value) %>%
    filter(grepl('POPESTIMATE', variable)) %>%
    mutate( year = as.numeric(gsub('POPESTIMATE','', variable)),
            month=7
            )%>%
    select(-variable)
    
  
  months <- cbind.data.frame('month'=1:12)
  
  pop2a <- bind_rows(pop2, months)
  
  pop3 <- pop2a %>%
    tidyr::complete( month, nesting(year,agey), fill=list(pop=NA) ) %>%
    mutate(date= as.Date(paste(year, month, '01', sep='-'))) %>%
    filter(date>='2013-07-01')
  
  
  filled_pop2 <- pop3 %>%
    group_by(agey )  %>%
    arrange(agey,year, month) %>%
    mutate(time= year + month/12 - 1/12 ) %>%
    mutate(pop.interpol=approx(time,pop,time)$y) %>%
    ungroup()
  
  filled_pop2 <- filled_pop2[,c('agey','date','pop.interpol')]
  
  saveRDS(filled_pop2,'./Data/pop_interpol_single_age.rds')
  
  
}
