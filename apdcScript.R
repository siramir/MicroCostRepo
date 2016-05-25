################
##    APDC   ###
################

# In this script I play with APDC data
# gettin familiar with data
library(dplyr)
library(lubridate)
library(ggplot2)

# save(up45, file = "C:/Users/Amir/Documents/R/workingdir/Microsim/microsimulation/Data/up45Df.obj")

setwd("C:/Users/Amir/Documents/R/workingdir/Microsim/microsimulation")
load(paste(getwd(),'/Data/apdcDf.obj',sep = ''))

# sample data
# pbs_sample <- pbs_sim %>% sample_n(500)
apdc_sample <- apdc %>% sample_n(10)
# seef_sample <- seef_all45up_clean_02nov2011 %>% sample_n(500)
# up45_sample <- up45 %>% sample_n(500)

#some cleaning on apdc
apdc$emergncy <- factor(apdc$emergncy)
apdc$sex <- factor(apdc$sex)
apdc$admdate <- as.Date(x = apdc$admdate,"%m/%d/%Y")
apdc <- apdc%>% mutate(admYear = year(admdate))

##--- Fill in the cost records with average cost for their drgs in costNew
apdc <- mutate(apdc, costNew = cost_wt_a)
apdc <- apdc[,c(1:41,176,42:175)]

drgCostYear <-
  apdc %>% select(ardrg,admdate, admYear, cost_wt_a,cost_wt_d, los)

summArdrg <-
  drgCostYear %>% group_by(ardrg,admYear) %>% summarise(avg = mean(cost_wt_a,na.rm = TRUE),
                                                        varA = var(cost_wt_a,na.rm = TRUE), varD = var(cost_wt_d,na.rm = TRUE), min =
                                                          min(cost_wt_d,na.rm = TRUE),max = max(cost_wt_d,na.rm = TRUE),n=n()
  )

bzDrg <-  summArdrg %>% filter( n >100) 
drgList <- unique(bzDrg$ardrg)

savePath <- paste(getwd(),'/plots/',sep='')
dir.create(savePath)

randomIndx <- sample(1:length(drgList), 1, replace = FALSE)
drg <-  drgList[randomIndx]
drgData <- drgCostYear %>% filter(ardrg == drg)

jpeg(paste(savePath,'losWeight_',drg,'.jpg',sep=''),width = 4, height = 4, units = 'in', res = 300)
qplot(
  x = los, y = jitter(cost_wt_a), data = drgData, color = factor(admYear), main = drg,na.rm = TRUE
)
dev.off()


naIndx <- which(is.na(drgCost$cost_wt_a))
naCost <- drgCost[naIndx,]
joined <- left_join(naCost,summArdrg, by = 'ardrg')
apdc[naIndx,"costNew"] <- joined$avg
rm(joined,naCost,naIndx,drgCost,summArdrg)

indiv <- apdc %>% group_by(PPN)  %>% summarise(costWeight = sum(costNew,na.rm = TRUE),n = n())
summary(indiv)

apdc <- apdc%>% mutate(year = year(admdate))


# Data extraction based on admission date
beginDate <- apdc$admdate[1]
endDate <- beginDate + years(1)
costYear <- interval(beginDate, endDate)
selected <- apdc %>% filter(admdate %within% costYear)


## Draft--------------
byPpn <- apdc %>% group_by(PPN)
asad <- byPpn %>% summarize(beginDate =  min(admdate))
asad <- asad %>% mutate(endDate = asad$beginDate + years(1),costYear = interval(beginDate, endDate))
apdc <- apdc %>% left_join(asad[,c('PPN','costYear')])
# selected <- apply(apdc, function(y) {filter(y$admdate %within% y$costYear)

# plotWeight <- function(drgCostYear){
#   p <- qplot(data=drgCostYear,
#              x=los,
#              y=cost_wt_a)
#   print(p)
#   
# }
# 
# group_by(drgCostYear, ardrg) %>%
#   do(plot = plotWeight(.))
# # colnames(apdc)
# # hist(summArdrg$avg ,breaks = 100)
# #
#

# akbar <- mutate(akbar, month = month(apdc$admdate))
# byYearMonth <- group_by(akbar, year,month)
# sum <- summarise(byYearMonth, mean(los),n())
# qplot(y = sum$`n()`,data = sum, color = factor(sum$month))


#read sas file
# read_sas(b7dat = 'C:\\Users\\Amir\\Desktop\\micro\\DATA\\Synthetic MBS and PBS data\\Synthetic MBS\\mbs_sim_v3.sas7bdat' )

# number of records without cost = 466001
# naCost <- sum(is.na(apdc$cost_wt_a))


# attach(apdc)  
# a <- ggplot(apdc,aes(admdate,color=apdc$year))
# a + geom_bar(position = "fill")
# qplot(y = admdate, color = years(admdate))

