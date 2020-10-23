options(scipen = 999)
fmtJhuDate = function(jhu.date) {
  newDate = c()
  
  for (i in seq(1, nrow(jhu.date))) {
    #print(i)
    #print(length(jhu.date))
    cur = jhu.date[i,1][[1]]
    #print(cur)
    cur.split = unlist(strsplit(cur, "[.]"))
    
    month = sub("X", "0", cur.split[1])
    #if (length(month) == 1) {
    #  month = paste0("0", month)
    #}
    
    year = paste0("20", cur.split[3])
    
    day = cur.split[2]
    if (nchar(day) == 1) {
      day = paste0("0", day)
    }
    #print('HI')
    #print(paste0(year,month,day))
    newDate = c(newDate, paste0(year,month,day))
    
  }
  t = cbind(jhu.date, newDate)
  colnames(t) = c("jhu_date", "date")
  return(t)
}

library(dplyr)
library(tidyr)

setwd("~/Desktop/covid-19/covid-19")
states = read.csv("daily.csv")
intl = read.csv("intl.csv")
capita = read.csv("capita.csv")

##intl compare
jhu.confirmed = "../data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
jhu = read.csv(jhu.confirmed)

jhu.Hubei = jhu[jhu[,1]=="Hubei",c(2,5:ncol(jhu))]
jhu.Hubei$Country.Region = "Hubei"
jhu.Italy = jhu[jhu[,2]=="Italy",c(2,5:ncol(jhu))]

jhu.states = rbind(jhu.Hubei, jhu.Italy)
jhu.states.long = pivot_longer(jhu.states, -Country.Region, names_to = "jhu_date", values_to = "positive")
jhu_date = fmtJhuDate(unique(jhu.states.long[,2]))
jhu.states.date = left_join(jhu.states.long, jhu_date)
intl = select(jhu.states.date, date, state = Country.Region, positive)
intl$date = as.integer(as.character(intl$date))

##get intl positiveIncrease##
positiveIncrease = c()
for (i in 1:nrow(intl)) {
  if (intl[i,1] == 20200122) prev = 0
  positiveIncrease = c(positiveIncrease, intl[i,]$positive - prev)
  prev = intl[i,]$positive
}
intl = cbind(intl, positiveIncrease)

##add population
states = left_join(states, capita, by="state")
intl = left_join(intl, capita, by="state")

#add index to dates
all_dates = unique(c(states$date, intl$date))
date_index = as.data.frame(cbind(sort(all_dates), seq(1, length(all_dates))))
rownames(date_index) = date_index[,1]
colnames(date_index) = c("date", "index")
states = left_join(states, date_index, by = "date")
intl = left_join(intl, date_index, by = "date")


s = states %>% select(date, state, positive, positiveIncrease, total, pop, index)
i = intl %>% mutate(total = NA) %>% select(date, state, positive, positiveIncrease, total, pop, index)
data = bind_rows(s,i)

#calculate relative data
positive.spread = select(data, date, state, positive) %>% spread(state, positive)

cross500.date = apply(positive.spread[,2:ncol(positive.spread)], 2, function(x) positive.spread[,1][min(which(!is.na(x) & x>500))])
cross500.index = date_index[as.character(cross500.date),2]
cross500.index[is.na(cross500.index)] = max(date_index$index)
cross500 = as.data.frame(unname(cbind(names(cross500.date), cross500.index)))
colnames(cross500) = c("state", "cross")
cross500$cross = as.integer(as.character(cross500$cross))
data.rel = as.data.frame(left_join(data, cross500, by="state") %>% mutate(rel = index-cross))

##get average positive increase
positiveIncrease.spread = select(data, date, state, positiveIncrease) %>% spread(state, positiveIncrease)
positiveIncrease.avgSpread = c()
for (i in 1:nrow(positiveIncrease.spread)) {
  if (i < 7) {
    cur = positiveIncrease.spread[1:i, 2:ncol(positiveIncrease.spread)]
  } else {
    cur = positiveIncrease.spread[i:(i-7), 2:ncol(positiveIncrease.spread)]
  }
  curAvg = apply(cur, 2, function(x) mean(x, na.rm=T))
  positiveIncrease.avgSpread = rbind(positiveIncrease.avgSpread, c(positiveIncrease.spread[i,1], curAvg))
}
colnames(positiveIncrease.avgSpread) = colnames(positiveIncrease.spread)
avgPosIncrease = pivot_longer(as.data.frame(positiveIncrease.avgSpread), -date, names_to = "state", values_to = "avgIncrease")
avgPosIncrease.rel500 = left_join(left_join(avgPosIncrease, date_index), cross500)  %>% mutate(rel = index-cross)

##get total test increase
totalIncrease.spread = select(states, date, state, totalTestResultsIncrease) %>% spread(state, totalTestResultsIncrease)
totalIncrease.avgSpread = c()
for (i in 1:nrow(totalIncrease.spread)) {
  if (i < 7) {
    cur = totalIncrease.spread[1:i, 2:ncol(totalIncrease.spread)]
  } else {
    cur = totalIncrease.spread[i:(i-7), 2:ncol(totalIncrease.spread)]
  }
  curAvg = apply(cur, 2, function(x) mean(x, na.rm=T))
  totalIncrease.avgSpread = rbind(totalIncrease.avgSpread, c(totalIncrease.spread[i,1], curAvg))
}
colnames(totalIncrease.avgSpread) = colnames(totalIncrease.spread)
avgTotalIncrease = pivot_longer(as.data.frame(totalIncrease.avgSpread), -date, names_to = "state", values_to = "avgTotalIncrease")
avgPosIncrease.total = left_join(inner_join(avgPosIncrease, avgTotalIncrease, by = c("date", "state")), date_index)
avgPosIncrease.total$avgIncrease = as.numeric(as.character(avgPosIncrease.total$avgIncrease))
avgPosIncrease.total$avgTotalIncrease = as.numeric(as.character(avgPosIncrease.total$avgTotalIncrease))


##get growth rate
tmp = t(positive.spread[,2:ncol(positive.spread)])
growth = rep(NA, length(tmp[,1]))
for (i in 2:ncol(tmp)) {
  growth = cbind(growth, (tmp[,i] - tmp[,(i-1)]) / tmp[,(i-1)])
  
}
growth = t(growth)
rownames(growth) = positive.spread$date
growth = growth[seq(nrow(growth),1),]
#growth = as.data.frame(growth)
growth = as.data.frame(round(growth*100, digits=1))
growth.long = bind_cols(growth, date_index[seq(nrow(date_index),1),]) %>% pivot_longer(cols = seq(1, ncol(growth)))
                                                                                       
                                                                                       
#calculate relative data percapita
norm.spread = data %>% mutate(norm = positive / (pop / 1000)) %>% select(date, state, norm) %>% spread(state, norm)
cross3.date = apply(norm.spread[,2:ncol(norm.spread)], 2, function(x) norm.spread[,1][min(which(!is.na(x) & x>0.03))])
cross3.index = date_index[as.character(cross3.date),2]
cross3.index[is.na(cross3.index)] = max(date_index$index)
cross3 = as.data.frame(unname(cbind(names(cross3.date), cross3.index)))
colnames(cross3) = c("state", "cross")
cross3$cross = as.integer(as.character(cross3$cross))
data.relCapita = as.data.frame(left_join(data, cross3, by="state") %>% mutate(rel = index-cross))
avgPosIncrease.norm = left_join(avgPosIncrease, capita) %>% mutate(norm = avgIncrease / (pop/1000))
avgPosIncrease.relCapita = left_join(left_join(avgPosIncrease.norm, date_index), cross3)  %>% mutate(rel = index-cross)



#bin states
recent = states[states$index == max(states$index),]
yesterday = states[states$index == max(states$index)-1,]
percIncrease = recent$positiveIncrease
names(percIncrease) = recent$state

recent.ordered = recent[order(recent$positive, decreasing = T),]
q = quantile(recent.ordered$positive)
cutoffs = c()
for (i in seq(2,4)) {
  cutoffs = c(cutoffs, round(q[i] / 10^floor(log10(q[i]))) * 10^floor(log10(q[i])))
}
write.table(cutoffs, file="cutoffs.txt", quote=F, row.names=F, col.names=F)

states = c()
states$bin1 = recent.ordered[recent.ordered$positive>=cutoffs[3] & !is.na(recent.ordered$positive),]$state
states$bin2 = recent.ordered[recent.ordered$positive>=cutoffs[2]  & recent.ordered$positive < cutoffs[3] & !is.na(recent.ordered$positive),]$state
states$bin3 = recent.ordered[recent.ordered$positive>=cutoffs[1]  & recent.ordered$positive < cutoffs[2] & !is.na(recent.ordered$positive),]$state
states$bin4 = recent.ordered[recent.ordered$positive<cutoffs[1] & !is.na(recent.ordered$positive),]$state
states$bin1growing = intersect(states$bin1,names(head(sort(percIncrease, decreasing = T), n=5)))
states$bin2growing = intersect(states$bin2,names(head(sort(percIncrease, decreasing = T), n=5)))
states$bin3growing = intersect(states$bin3,names(head(sort(percIncrease, decreasing = T), n=5)))
states$bin4growing = intersect(states$bin4,names(head(sort(percIncrease, decreasing = T), n=5)))

##Make data tables

dataAbs = c()
dataAbs$noNorm = data %>% mutate(plot = positive) %>% mutate(cases = plot) %>% select(date_label = date, date = index, state, plot, cases)
dataAbs$normByTotalCases = data %>%  filter(index>56) %>% mutate(plot = (positive/total) * 100)  %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state, plot, cases)
dataAbs$normByPop = data %>% mutate(plot = positive/(pop/1000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state, plot, cases)
dataAbs$growth = growth.long %>% mutate(cases = prettyNum(value, digits=3)) %>% select(date_label = date, date = index, state = name, plot = value, cases) %>% as.data.frame()


dataRel = c()
dataRel$noNorm = data.rel %>% mutate(plot = positive) %>% mutate(cases = plot) %>% select(date = rel, state, plot, cases)
dataRel$normByPop = data.relCapita %>% mutate(plot = positive/(pop/1000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date = rel, state, plot, cases)

##Government response
response_data = read.csv("response.csv")
response.rel = left_join(response_data, date_index, by="date") %>% left_join(cross500, by="state") %>% mutate(rel = index-cross) %>% select(state, response, date = rel)
response.relCapita = left_join(response_data, date_index, by="date") %>% left_join(cross3, by="state") %>% mutate(rel = index-cross) %>% select(state, response, date = rel)

response = c()
response$abs = left_join(response_data, date_index) %>% select(state, response, date = index)
response$rel = response.rel
response$relCapita = response.relCapita


dataDaily = c()
dataDaily$abs = c()
dataDaily$rel = c()

dataDaily$abs$normByPop = avgPosIncrease.relCapita  %>% mutate(plot = norm) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state, plot, cases)
dataDaily$abs$noNorm = avgPosIncrease.rel500 %>%  mutate(plot = avgIncrease) %>%mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state, plot, cases)
dataDaily$abs$normByTotalCases = avgPosIncrease.total %>% filter(index>56) %>% mutate(plot = (avgIncrease/avgTotalIncrease) * 100)  %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state, plot, cases)

dataDaily$rel$normByPop = avgPosIncrease.relCapita %>% mutate(plot = norm) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date = rel, state, plot, cases)
dataDaily$rel$noNorm = avgPosIncrease.rel500 %>%  mutate(plot = avgIncrease) %>% mutate(cases = avgIncrease) %>% select(date = rel, state, plot, cases)


#Save data
saveRDS(states, file="states.RDS")
saveRDS(dataAbs, file="dataAbs.RDS")
saveRDS(dataRel, file="dataRel.RDS")
saveRDS(response, file="response.RDS")
saveRDS(growth, file="growth.RDS")
saveRDS(dataDaily, file="daily.RDS")



###ftDataTotal.R

states = read.csv("daily.csv")
capita = read.csv("capita.csv", row.names=1)
capita = capita / 1000
#add index to dates
states = left_join(states, date_index, by = "date")
data = states

#calculate relative data
total.spread = select(data, date, state, total) %>% spread(state, total)

totalTestsPerformed = total.spread[nrow(total.spread),2:ncol(total.spread)] 
numPerformedToday = total.spread[nrow(total.spread),2:ncol(total.spread)] - total.spread[nrow(total.spread)-1,2:ncol(total.spread)]

numPerformedDaily = c()
for (i in (nrow(total.spread)-6):(nrow(total.spread)-1)) {
  t = total.spread[(i+1),2:ncol(total.spread)] - total.spread[i,2:ncol(total.spread)]
  numPerformedDaily = rbind(numPerformedDaily, t)
  
}


totalTestsPerformed.perCapita = totalTestsPerformed / capita[colnames(totalTestsPerformed),1]
numPerformedToday.perCapita = numPerformedToday / capita[colnames(numPerformedToday),1]

tests.table = t(rbind(numPerformedToday,totalTestsPerformed,round(totalTestsPerformed.perCapita, digits=2)))
colnames(tests.table) = c("Tests performed today", "Total tests performed", "Total tests per capita (1,000)")
tests.table = as.data.frame(tests.table)

saveRDS(tests.table, file="tests.RDS")