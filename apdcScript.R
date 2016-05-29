# APDC data ---------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(SiZer)
library(gapminder)
library(magrittr)

#load apdc file--------
setwd("C:/Users/Amir/MicroCost")
load(paste(getwd(),'/Data/apdc.obj',sep = ''))

usefulApdc <- c("PPN","admdate","admYear","mthBirth","yrBirth","sex","los","cost_wt_a","cost_wt_avg","ardrg")
Apdc <-  apdc %>% select(which(colnames(apdc) %in% usefulApdc))
rm(usefulApdc,apdc)
str(Apdc)
# some cleaning on apdc ---------------------------------------------------

# apdc$emergncy <- factor(apdc$emergncy)
# apdc$sex <- factor(apdc$sex)
# apdc$admdate <- as.Date(x = apdc$admdate,"%m/%d/%Y")
# apdc <- apdc%>% mutate(admYear = year(admdate))

# Data based on date ------------------------------------------------------

# beginDate <- apdc$admdate[1] 
# endDate <- beginDate + years(1)
# costYear <- interval(beginDate, endDate) 
# selected <- apdc %>% filter(admdate %within% costYear)

beginDate <- apdc$admdate[1] 
endDate <- beginDate + years(1)
costYear <- interval(beginDate, endDate) 
selected <- apdc %>% filter(admdate %within% costYear)

byPpn <- apdc %>% group_by(PPN)

dateSelect <- function (df) {
  admdate <-  df$admdate
  beginDate  <- unique(df$datentoday)[1]
  endDate <- beginDate + years(1) 
  costYear <- interval(beginDate, endDate)
  selected <- df %>% filter(admdate %within% costYear)
  return(selected)
}

yearCostData <-  byPpn %>% do(dateSelect(.))
# Plot cost_wt_a vs los for each drg --------------------------------------
drgCostYear <- apdc %>% select(ardrg,admdate, admYear, cost_wt_a,cost_wt_avg, los)
summArdrg <- drgCostYear %>% group_by(ardrg,admYear) %>% summarise(avg = mean(cost_wt_a,na.rm = TRUE),n =n())

bzDrg <-  summArdrg %>% filter( n >100) 
drgList <- unique(bzDrg$ardrg)

savePath <- paste(getwd(),'/plots/',sep='')
dir.create(savePath)

randomIndx <- sample(1:length(drgList), 1, replace = FALSE)
drg <-  drgList[randomIndx]
drgData <- drgCostYear %>% filter(ardrg == drg)
jpeg(paste(savePath,drg,'_losWeight_avg.jpg',sep=''),width = 4, height = 4, units = 'in', res = 300)
qplot(
  x = los, y = jitter(cost_wt_avg), data = drgData, color = factor(admYear), main = drg,na.rm = TRUE
)
dev.off()
jpeg(paste(savePath,drg,'_losWeight_a.jpg',sep=''),width = 4, height = 4, units = 'in', res = 300)
qplot(
  x = los, y = jitter(cost_wt_a), data = drgData, color = factor(admYear), main = drg,na.rm = TRUE
)
dev.off()

# Fill in the cost records with average cost for their drgs in cos --------

apdc <- mutate(apdc, cost_wt_avg = cost_wt_a)
drgCostYear <- apdc %>% select(ardrg,admdate, admYear, cost_wt_a,cost_wt_d, los)
summArdrg <- drgCostYear %>% group_by(ardrg,admYear) %>% summarise(avg = mean(cost_wt_a,na.rm = TRUE),n =n())

# Phase one DRG Year mean
naIndx <- which(is.na(drgCostYear$cost_wt_a))
naCost <- drgCostYear[naIndx,]
joined <- left_join(naCost,summArdrg, by = c('ardrg','admYear'))
apdc[naIndx,"cost_wt_avg"] <- joined$avg

# Phase two DRG mean
naIndx <- which(is.na(drgCostYear$cost_wt_a))
naCost <- drgCostYear[naIndx,]
summArdrg <- drgCostYear %>% group_by(ardrg) %>% summarise(avg = mean(cost_wt_a,na.rm = TRUE),n =n())
joined <- left_join(naCost,summArdrg, by ='ardrg')
apdc[naIndx,"cost_wt_avg"] <- joined$avg
rm(joined,naCost,naIndx,drgCostYear,summArdrg)

# Draft -------------------------------------------------------------------
# # number of records without cost = 466001 # naCost <-
# sum(is.na(apdc$cost_wt_a))

# # attach(apdc) 
# a <- ggplot(apdc,aes(admdate,color=apdc$year)) 
# a + # geom_bar(position = "fill") 
# qplot(y = admdate, color = years(admdate))


# indiv <- apdc %>% group_by(PPN)  %>% summarise(costWeight = sum(costNew,na.rm = TRUE),n = n())
byPpn <- apdc %>% group_by(PPN) asad <- byPpn %>% summarize(beginDate =  min(admdate)) 
asad <- asad %>% mutate(endDate = asad$beginDate + years(1),costYear = interval(beginDate, endDate)) 
apdc <- apdc %>% left_join(asad[,c('PPN','costYear')])


plm <- function (df) {
  x <- df %>% select(los)
  x <- as.vector(x)
  x <- unlist(x)
  x <- unname(x)
  y <- df %>% select(cost_wt_a)
  y <- as.vector(y)
  y <- unlist(y)
  y <- unname(y)
  model <- piecewise.linear(x = x,y = y,CI=FALSE)
  return(model)
}

plot(model)
print(model)
predict(model, 2001)

models <-
  apdc %>% filter(!is.na(cost_wt_a)) %>% 
  select(ardrg, admYear,los,cost_wt_a) %>% 
  group_by(ardrg,admYear) %>% do(model = plm(.))

options(scipen=999)

summary(Apdc$yrBirth)
summary(Seef$yrBirth)
elite <- Apdc %>% filter(yrBirth >= 1910 & yrBirth <= 1963)