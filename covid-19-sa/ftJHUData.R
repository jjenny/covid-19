library(dplyr)
library(tidyr)
setwd("~/Desktop/COVID-SHINY/covid-19-sa")

##Input Files
#jhu.confirmed = "/Users/jennychen/Desktop/COVID-SHINY/data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
population = "population.country.csv"
response_data = read.csv("response.csv")

jhu = read.csv(jhu.confirmed)
pop = read.csv(population) %>% select(Country.Region, pop)


##pull out data for states of interest
states = c("Uruguay", "Argentina", "Ecuador", "Chile", "Brazil", "Peru", 
           "Bolivia", "Paraguay", "Colombia", "Venezuela")

jhu.states = jhu[jhu$Country.Region %in% states & (jhu$Province.State == "" | as.character(jhu$Province.State) == as.character(jhu$Country.Region)),c(2,5:ncol(jhu))]

##check you have everything
length(states)
nrow(jhu.states)

##add intl regions
jhu.Hubei = jhu[jhu[,1]=="Hubei",c(2,5:ncol(jhu))]
jhu.Hubei$Country.Region = "Hubei"

jhu.Italy = jhu[jhu[,2]=="Italy",c(2,5:ncol(jhu))]
jhu.Spain = jhu[jhu[,2]=="Spain",c(2,5:ncol(jhu))]
jhu.US = jhu[jhu[,2]=="US",c(2,5:ncol(jhu))]
jhu.India = jhu[jhu[,2]=="India",c(2,5:ncol(jhu))]

jhu.states = rbind(jhu.states, jhu.Hubei, jhu.Italy, jhu.Spain, jhu.US, jhu.India)
state_es = cbind(c(states, "Hubei", "Italy", "Spain", "US", "India"), c("Uruguay", "Argentina", "Ecuador", "Chile", "Brasil", "Perú", 
              "Bolivia", "Paraguay", "Colombia", "Venezuela", "Hubei", "Italia", "España", "EEUU", "India"))
colnames(state_es) = c("state", "state_es")
state_es = as.data.frame(state_es)
rownames(state_es) = state_es[,1]

##reformat dates
dates = read.csv(jhu.confirmed, header=F)
dates = dates[1,5:ncol(dates)]
date_index = mutate(as.data.frame(t(dates)), index=row_number()) %>% select(date=1, index=index)
date_index$date = sapply(date_index$date, function(x) gsub("/", ".", x))

##get growth rate
tmp = jhu.states[,2:ncol(jhu.states)]
growth = rep(NA, length(tmp[,1]))
for (i in 2:ncol(tmp)) {
  growth = cbind(growth, (tmp[,i] - tmp[,(i-1)]) / tmp[,(i-1)])
  
}
growth = t(growth)
colnames(growth) = state_es[as.character(jhu.states[,1]),2]
rownames(growth) = sapply(colnames(jhu.states)[2:ncol(jhu.states)], function(x) gsub("X", "", x))
growth = growth[seq(nrow(growth),1),]
growth = as.data.frame(round(growth*100, digits=1))

##make data table with absolute dates
jhu.tmp = pivot_longer(jhu.states, colnames(jhu.states)[2:ncol(jhu.states)]) %>% select(state=Country.Region, date = name, positive = value)
jhu.tmp$date = sapply(jhu.tmp$date, function(x) gsub("X", "", x))
jhu.abs = left_join(jhu.tmp, date_index)
jhu.abs = left_join(jhu.abs, select(pop, state = Country.Region, pop))
jhu.abs = as.data.frame(jhu.abs)

#calculate relative data
cross500 = apply(jhu.states[,2:ncol(jhu.states)], 1, function(x) min(which(!is.na(x) & x>500)))
cross500[cross500==Inf] = max(date_index$index)
cross500 = as.data.frame(cbind(as.character(jhu.states[,1]), cross500)) %>% select(state = V1, cross = cross500)
cross500$cross = as.numeric(as.character(cross500$cross))

jhu.rel = as.data.frame(left_join(jhu.abs, cross500, by="state") %>% mutate(rel = index-cross))

#calculate relative data percapita

jhu.withPop = inner_join(pop, jhu.states, by="Country.Region")
jhu.normPop = c()
for (i in 1:nrow(jhu.withPop)) {
  jhu.normPop = rbind(jhu.normPop, jhu.withPop[i,3:ncol(jhu.withPop)] / jhu.withPop[i,2])
}
cross3 = apply(jhu.normPop, 1, function(x) min(which(!is.na(x) & x>10)))
cross3[cross3==Inf] = max(cross3[cross3<Inf])
cross3 = as.data.frame(cbind(as.character(jhu.withPop[,1]), cross3)) %>% select(state = V1, cross = cross3)
cross3$cross = as.numeric(as.character(cross3$cross))

jhu.relCapita = as.data.frame(left_join(jhu.abs, cross3, by="state") %>% mutate(rel = index-cross))

#bin states
intl = c("Hubei", "Italy", "Spain", "US", "India")
recent = jhu.states[!(jhu.states[,1] %in% intl),ncol(jhu.states),]
names(recent) = jhu.states[!(jhu.states[,1] %in% intl),1]
recent.ordered = recent[order(recent, decreasing = T)]

###CUSTOMIZE THIS###


states = c()
states$bin1 = state_es[names(recent.ordered[recent.ordered>=500]),2]
states$bin2 = state_es[names(recent.ordered[recent.ordered<500]),2]

dataAbs = c()
dataAbs$noNorm = inner_join(jhu.abs, state_es) %>% mutate(plot = positive) %>% mutate(cases = plot) %>% 
  select(date_label = date, date = index, state = state_es, plot, cases)
dataAbs$normByPop = inner_join(jhu.abs, state_es) %>% mutate(plot = positive/pop) %>% 
  mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state = state_es, plot, cases)

dataRel = c()
dataRel$noNorm = inner_join(jhu.rel, state_es) %>% mutate(plot = positive) %>% 
  mutate(cases = plot) %>% select(date = rel, state = state_es, plot, cases)
dataRel$normByPop= inner_join(jhu.relCapita, state_es) %>% mutate(plot = positive/pop) %>% 
  mutate(cases = prettyNum(plot, digits=3)) %>% select(date = rel, state = state_es, plot, cases)

##add min,max dates for visualization



##Government response
response.rel = inner_join(response_data, state_es) %>% 
  left_join(date_index, by="date") %>% left_join(cross500, by="state") %>% 
  mutate(rel = index-cross) %>% select(state = state_es, response, date = rel)
response.relCapita = inner_join(response_data, state_es) %>% 
  left_join(date_index, by="date") %>% left_join(cross3, by="state") %>% 
  mutate(rel = index-cross) %>% select(state = state_es, response, date = rel)

response = c()
response$abs = inner_join(response_data, state_es) %>% 
  left_join(date_index) %>% select(state = state_es, response, date = index)
response$rel = response.rel
response$relCapita = response.relCapita



#Save data
saveRDS(states, file="states.RDS")
saveRDS(dataAbs, file="dataAbs.RDS")
saveRDS(dataRel, file="dataRel.RDS")
saveRDS(response, file="response.RDS")
saveRDS(growth,file="growth.RDS")
