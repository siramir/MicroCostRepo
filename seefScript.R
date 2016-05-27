# SEEF data ---------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(SiZer)
library(gapminder)
library(magrittr)

#load seef file--------
setwd("C:/Users/Amir/MicroCost")
load(paste(getwd(),'/Data/seef.obj',sep = ''))
# 
# seefDf$Q03_sex45u <- factor(seefDf$Q03_sex45u)
# seefDf$Q01_datentoday <- as.Date(x = seefDf$Q01_datentoday,"%m/%d/%Y")
# seefDf$datentoday <- as.Date(x = seefDf$datentoday,"%m/%d/%Y")
# seefDf$Q02_dateofbirth45u <- as.Date(x = seefDf$Q02_dateofbirth45u,"%m/%d/%Y")

usefulSeef <- c("seef_project_id","Q01_datentoday","Q03_sex45u","datentoday","Q02_dateofbirth45u")
Seef <-  seefDf %>% select(which(colnames(seefDf) %in% usefulSeef))
Seef <- Seef %>% mutate(mthBirth = month(Q02_dateofbirth45u),yrBirth=year(Q02_dateofbirth45u))
str(Seef)

# Match table -------------------------------------------------------------

# testMatch <- left_join(seef,Apdc, by= c("Q03_sex45u"="sex" , "mthBirth"="mthBirth", "yrBirth"="yrBirth"),copy = FALSE)
# make a random match table
apdcKey <-  unique(Apdc$PPN)[1:nrow(Seef)]
key <- data.frame(apdcKey = apdcKey, seefKey = Seef$seef_project_id)
Seef <- left_join(Seef,key, by = c("seef_project_id"="seefKey"))
apdcJoined <-  left_join(Seef,Apdc, by = c("apdcKey"="PPN"))

joinedAgeGender <- left_join(Seef,Apdc, by=c("Q03_sex45u"="sex", "mthBirth"="mthBirth", "yrBirth"="yrBirth"),copy = FALSE)
# Draft -------------------------------------------------------------------

library(lubridate)
hist(seefDf$datentoday, breaks = 'month', freq = TRUE)
hist(seefDf$Q01_datentoday, breaks = 'month', freq = TRUE)
hist(unclass(round(difftime(seefDf$Q01_datentoday,seefDf$datentoday,units="weeks"))/52),30)

seefDf$Q02_dateofbirth45u <- as.Date(x = seefDf$Q02_dateofbirth45u,"%m/%d/%Y")
hist(year(seefDf$Q02_dateofbirth45u), breaks = 60)


savePath <- paste(getwd(),'/Data/',sep='')
dir.create(savePath)
save(seefDf, file = paste(savePath,"seef.obj",sep = ""))