# SEEF data ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)
library(SiZer)
library(gapminder)
library(magrittr)


# load seef and short Apdc ------------------------------------------------

rm(list=ls())
setwd("C:/Users/Amir/MicroCost")
load(paste(getwd(),'/Data/seef.obj',sep = ''))
load(paste(getwd(),'/Data/Apdc2.obj',sep = ''))

# 
# seefDf$Q03_sex45u <- factor(seefDf$Q03_sex45u)
# seefDf$Q01_datentoday <- as.Date(x = seefDf$Q01_datentoday,"%m/%d/%Y")
# seefDf$datentoday <- as.Date(x = seefDf$datentoday,"%m/%d/%Y")
# seefDf$Q02_dateofbirth45u <- as.Date(x = seefDf$Q02_dateofbirth45u,"%m/%d/%Y")

usefulSeef <- c("seef_project_id","Q01_datentoday","Q03_sex45u","datentoday","Q02_dateofbirth45u")
Seef <-  seefDf %>% select(which(colnames(seefDf) %in% usefulSeef))
Seef <- Seef %>% mutate(mthBirth = month(Q02_dateofbirth45u),yrBirth=year(Q02_dateofbirth45u))
str(Seef)
rm(usefulSeef,seefDf)


# Match table -------------------------------------------------------------

# testMatch <- left_join(seef,Apdc, by= c("Q03_sex45u"="sex" , "mthBirth"="mthBirth", "yrBirth"="yrBirth"),copy = FALSE)
# make a random match table
apdcKey <-  unique(Apdc$PPN)[1:nrow(Seef)]
key <- data.frame(apdcKey = apdcKey, seefKey = Seef$seef_project_id)
Seef <- left_join(Seef,key, by = c("seef_project_id"="seefKey"))
apdcJoined <-  left_join(Seef,Apdc, by = c("apdcKey"="PPN"))
rm(apdcKey,key)
# joinedAgeGender <- left_join(Seef,Apdc, by=c("Q03_sex45u"="sex", "mthBirth"="mthBirth", "yrBirth"="yrBirth"),copy = FALSE)


# Data based on date ------------------------------------------------------

dateSelect <- function (df) {
  admdate <- df$admdate
  beginDate <- unique(df$datentoday)[1]
  endDate <- beginDate + years(1) 
  costYear <- interval(beginDate, endDate)
  selected <- df %>% filter(admdate %within% costYear)
  return(selected)
}
yearCostData <-  apdcJoined %>% group_by(apdcKey) %>% do(dateSelect(.))


# Add conversion rate for inflation to data -------------------------------

load(paste(getwd(),'/Data/conversion.obj',sep = ''))
conversion <- ABS_CPI_conversion %>% select(dos,X2012)
sas.origin <- "1960-01-01"
conversion$dos <-  as.Date(conversion$dos, origin=as.Date(sas.origin))
apdcJoined <- left_join(apdcJoined,conversion, by = c("admdate"="dos"))
rm(conversion,sas.origin,ABS_CPI_conversion)


# Add NEP (Hypatheticly) --------------------------------------------------

minYear <-  min(apdcJoined$admYear)
maxYear <-  max(apdcJoined$admYear)
hypNep <- data.frame(yearRange = minYear:maxYear,NEP =seq(from=4000,to = 4090, by = 10))
apdcJoined <- left_join(apdcJoined,hypNep, by = c("admYear"="yearRange"))
rm(minYear,maxYear,hypNep)

# Calcute Cost for each guy in a year -------------------------------------

guyCost <- apdcJoined %>% group_by(seef_project_id) %>%  summarise(totCost = sum(cost_wt_avg*NEP*X2012,na.rm = TRUE), n=n())
Seef <-  left_join(Seef,guyCost, by = c("seef_project_id"="seef_project_id"))

# Draft -------------------------------------------------------------------

library(lubridate)
hist(Seef$datentoday, breaks = 'month', freq = TRUE)
hist(Seef$Q01_datentoday, breaks = 'month', freq = TRUE)
hist(unclass(round(difftime(Seef$Q01_datentoday,Seef$datentoday,units="weeks"))/52),30)
hist(Apdc$admdate, breaks = 'month')

savePath <- paste(getwd(),'/Data/',sep='')
dir.create(savePath)
save(ABS_CPI_conversion, file = paste(savePath,"conversion.obj",sep = ""))
save(Seef, file = paste(savePath,"seef.obj",sep = ""))
save(yearCostData, file = paste(savePath,"yearCostData.obj",sep = ""))

# rm(savePath)