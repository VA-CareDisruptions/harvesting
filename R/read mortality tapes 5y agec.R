#NOTE: DATA not stored locally due to size. Can find public version on NCHS website;
#local copies on pneumococcal_mortality Rproj
#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(cdlTools)

#the geographic resolution missing from the public data

#https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm

#Bridged-Race Population Estimates – Data Files and Documentation
#https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm


#file.names1<- list('VS14MORT.DUSMCPUB','VS15MORT.DUSMCPUB','VS16MORT.DUSMCPUB','VS17MORT.DUSMCPUB','Mort2018US.PubUse.txt','VS19MORT.DUSMCPUB_r20210304','VS20MORT.DUSMCPUB_r20220105')
file.names1<- list('MULT2014.USAllCnty.txt','MULT2015.USAllCnty.txt','MULT2016.USAllCnty.txt','MULT2017.USAllCnty.txt','Mort2018US.AllCnty.txt','MULT2019US.AllCnty.txt','MULT2020.USAllCnty.txt')

all.ds <- lapply(file.names1, function(x){
  d1 <- read_fwf(file=paste0("./CONFIDENTIAL/raw/" ,x),
                 fwf_positions(start=c(20,61,63,64,65,69,102,445,70,71, 77, 484, 806),
                               end=c(  20,62,63,64,66,69,105,446,  70,73,78, 486, 817),
                               col_names = c('res_status','education1989','education2003','education_flag','month','sex','year','race','age_detail_class','age_detail_number', 'agec_5y','hispanic',  'occupation' )),
                 guess_max=10000)
  return(d1)
})

df1 <- bind_rows(all.ds)
saveRDS(df1, './CONFIDENTIAL/compiled_data_age_allcause.rds')

df1 <- readRDS('./Raw Data/compiled_data_age_allcause.rds')

df1$hisp_recode <- 999
df1$hisp_recode[df1$hispanic<=199 & df1$hispanic>=100] <- 0
df1$hisp_recode[df1$hispanic >=200 & df1$hispanic <= 299] <- 1
#table(df1$hisp_recode)

#RACE: 
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island

df1$race_recode<- NA
df1$race_recode[df1$hisp_recode == 1] <- 3 #Hispanic

df1$race_recode[df1$race %in% c('01') & df1$hisp_recode != 1] <- 1 #white, non-Hispanic
df1$race_recode[df1$race %in% c('02') & df1$hisp_recode != 1 ]  <- 2 #black, non-Hispanic
df1$race_recode[ df1$race %in% c('03') & df1$hisp_recode != 1 ]  <- 4 #American Indian
df1$race_recode[ df1$race %in% c('04','05','18','28','48' ,'68','78') & df1$hisp_recode != 1]  <- 5 #Asian
df1$race_recode[ df1$race %in% c( '06','07','38','58') & df1$hisp_recode != 1]  <- 5 #Hawaain/Pac Is
df1$race_recode[is.na(df1$race_recode)] <- 999

#RACE RECODE:
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island


df1$agey <- as.numeric(df1$age_detail_number)
df1$agey[df1$age_detail_class==2] <- as.numeric(df1$age_detail_number[df1$age_detail_class==2] )/12
df1$agey[df1$age_detail_class==4] <- as.numeric(df1$age_detail_number[df1$age_detail_class==4] )/365
df1$agey[df1$age_detail_class==5] <- as.numeric(df1$age_detail_number[df1$age_detail_class==5] )/365/24
df1$agey[df1$age_detail_class==6] <- as.numeric(df1$age_detail_number[df1$age_detail_class==6] )/365/24/60


#RACE RECODE:
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island

df2 <- df1 %>%
  mutate(agey =round(agey)) %>%
  group_by(agey,agec_5y, race_recode, sex, month,year) %>%
  summarize(N_deaths=n()) %>%
  ungroup()

saveRDS(df2,'./Data/all_cause_deaths_age_race_sex_year.rds')

