library(dplyr)
library(tidyr)
setwd("~/Desktop/COVID-SHINY/covid-19-eu")
###UPDATE THIS
##Input Files
#jhu.confirmed = "~/Desktop/COVID-SHINY/data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
population = "pop.csv"
response_data = read.csv("response.csv")

jhu = read.csv(jhu.confirmed)
pop = read.csv(population)

###UPDATE THIS
##pull out data for states of interest
states = c("Austria", "Belgium", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom")
jhu.states = jhu[jhu$Country.Region %in% states & (jhu$Province.State == "" | as.character(jhu$Province.State) == as.character(jhu$Country.Region)),c(2,5:ncol(jhu))]

##check you have everything
length(states)
nrow(jhu.states)

##If you want to compare to Hubei
jhu.Hubei = jhu[jhu[,1]=="Hubei",c(2,5:ncol(jhu))]
jhu.Hubei$Country.Region = "Hubei"
jhu.US = jhu[jhu[,2]=="US",c(2,5:ncol(jhu))]
jhu.India = jhu[jhu[,2]=="India",c(2,5:ncol(jhu))]
jhu.states = rbind(jhu.states, jhu.Hubei, jhu.US, jhu.India)

##reformat dates
dates = read.csv(jhu.confirmed, header=F)
dates = dates[1,5:ncol(dates)]
date_index = mutate(as.data.frame(t(dates)), index=row_number()) %>% select(date=1, index=index)
date_index$date = sapply(date_index$date, function(x) gsub("/", ".", x))

##make data table with absolute dates
jhu.tmp = pivot_longer(jhu.states, colnames(jhu.states)[2:ncol(jhu.states)]) %>% select(state=Country.Region, date = name, positive = value)
jhu.tmp$date = sapply(jhu.tmp$date, function(x) gsub("X", "", x))
jhu.abs = left_join(jhu.tmp, date_index)
jhu.abs = left_join(jhu.abs, select(pop, state = Country.Region, pop))
jhu.abs = as.data.frame(jhu.abs)

##get growth rate
tmp = jhu.states[,2:ncol(jhu.states)]
growth = rep(NA, length(tmp[,1]))
for (i in 2:ncol(tmp)) {
  growth = cbind(growth, (tmp[,i] - tmp[,(i-1)]) / tmp[,(i-1)])
  
}
growth = t(growth)
colnames(growth) = jhu.states[,1]
rownames(growth) = sapply(colnames(jhu.states)[2:ncol(jhu.states)], function(x) gsub("X", "", x))
growth = growth[seq(nrow(growth),1),]
growth = as.data.frame(round(growth*100, digits=1))


#calculate relative data
cross500 = apply(jhu.states[,2:ncol(jhu.states)], 1, function(x) min(which(!is.na(x) & x>500)))
cross500[cross500==Inf] = max(cross500[cross500<Inf])
cross500 = as.data.frame(cbind(as.character(jhu.states[,1]), cross500)) %>% select(state = V1, cross = cross500)
cross500$cross = as.numeric(as.character(cross500$cross))

jhu.rel = as.data.frame(left_join(jhu.abs, cross500, by="state") %>% mutate(rel = index-cross))

#calculate relative data percapita

jhu.withPop = left_join(pop, jhu.states, by="Country.Region")
jhu.normPop = c()
for (i in 1:nrow(jhu.withPop)) {
  jhu.normPop = rbind(jhu.normPop, jhu.withPop[i,3:ncol(jhu.withPop)] / (jhu.withPop[i,2] / 1000))
}
cross3 = apply(jhu.normPop, 1, function(x) min(which(!is.na(x) & x>0.03)))
cross3[cross3==Inf] = max(cross3[cross3<Inf])
cross3 = as.data.frame(cbind(as.character(jhu.withPop[,1]), cross3)) %>% select(state = V1, cross = cross3)
cross3$cross = as.numeric(as.character(cross3$cross))

jhu.relCapita = as.data.frame(left_join(jhu.abs, cross3, by="state") %>% mutate(rel = index-cross))

#bin states
recent = jhu.states[jhu.states[,1] != "Hubei" & jhu.states[,1] != "US"  & jhu.states[,1] != "India",ncol(jhu.states)]
names(recent) = jhu.states[jhu.states[,1] != "Hubei"  & jhu.states[,1] != "US"  & jhu.states[,1] != "India",1]
recent.ordered = recent[order(recent, decreasing = T)]

###CUSTOMIZE THIS###
states = c()
states$bin1 = names(recent.ordered[recent.ordered>=10000])
states$bin2 = names(recent.ordered[recent.ordered>=2000 & recent.ordered<10000])
states$bin3 = names(recent.ordered[recent.ordered<2000])


##Make data tables

dataAbs = c()
dataAbs$noNorm = jhu.abs %>% mutate(plot = positive) %>% mutate(cases = plot) %>% select(date_label = date, date = index, state, plot, cases)
dataAbs$normByPop = jhu.abs %>% mutate(plot = positive/(pop/1000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state, plot, cases)

dataRel = c()
dataRel$noNorm = jhu.rel %>% mutate(plot = positive) %>% mutate(cases = plot) %>% select(date = rel, state, plot, cases)
dataRel$normByPop = jhu.relCapita %>% mutate(plot = positive/(pop/1000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date = rel, state, plot, cases)


#dataRelCapita = c()
#dataRelCapita$noNorm = jhu.relCapita %>% mutate(plot = positive) %>% mutate(cases = plot) %>% select(date = rel, state, plot, cases)
#dataRelCapita$normByPop = jhu.relCapita %>% mutate(plot = positive/(pop/100000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date = rel, state, plot, cases)

##Government response
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
saveRDS(growth,file="growth.RDS")
