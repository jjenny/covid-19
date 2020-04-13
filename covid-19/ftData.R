library(dplyr)
library(tidyr)

setwd("~/Desktop/COVID-SHINY/covid-19")
states = read.csv("daily.csv")
intl = read.csv("intl.csv")
capita = read.csv("capita.csv")

states = left_join(states, capita, by="state")
intl = left_join(intl, capita, by="state")
#add index to dates
all_dates = unique(c(states$date, intl$date))
date_index = as.data.frame(cbind(sort(all_dates), seq(1, length(all_dates))))
rownames(date_index) = date_index[,1]
colnames(date_index) = c("date", "index")
states = left_join(states, date_index, by = "date")
intl = left_join(intl, date_index, by = "date")

s = states %>% select(date, state, positive, total, pop, index)
i = intl %>% mutate(total = NA)  %>% select(date, state, positive,total, pop, index)
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



#bin states
recent = states[states$index == max(data$index),]
recent.ordered = recent[order(recent$positive, decreasing = T),]
states = c()
states$over3000 = recent.ordered[recent.ordered$positive>=3000 & !is.na(recent.ordered$positive),]$state
states$over1000 = recent.ordered[recent.ordered$positive>=1000  & recent.ordered$positive < 3000 & !is.na(recent.ordered$positive),]$state
states$over500 = recent.ordered[recent.ordered$positive>=500  & recent.ordered$positive < 1000 & !is.na(recent.ordered$positive),]$state
states$over0 = recent.ordered[recent.ordered$positive<500 & !is.na(recent.ordered$positive),]$state


##Make data tables

dataAbs = c()
dataAbs$noNorm = data %>% mutate(plot = positive) %>% mutate(cases = plot) %>% select(date_label = date, date = index, state, plot, cases)
dataAbs$normByTotalCases = data %>% mutate(plot = positive/total)  %>% mutate(cases = prettyNum(plot, digits=2)) %>% select(date_label = date, date = index, state, plot, cases)
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



#Save data
saveRDS(states, file="states.RDS")
saveRDS(dataAbs, file="dataAbs.RDS")
saveRDS(dataRel, file="dataRel.RDS")
saveRDS(response, file="response.RDS")
saveRDS(growth, file="growth.RDS")