library(dplyr)
library(tidyr)


states = read.csv("states-daily.csv")
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

s = states %>% select(date, state, death, total, pop, index)
i = intl %>% mutate(total = NA)  %>% select(date, state, death,total, pop, index)
data = bind_rows(s,i)

#calculate relative data
death.spread = select(data, date, state, death) %>% spread(state, death)
cross500.date = apply(death.spread[,2:ncol(death.spread)], 2, function(x) death.spread[,1][min(which(!is.na(x) & x>15))])
cross500.index = date_index[as.character(cross500.date),2]
cross500.index[is.na(cross500.index)] = max(date_index$index)
cross500 = as.data.frame(unname(cbind(names(cross500.date), cross500.index)))
colnames(cross500) = c("state", "cross")
cross500$cross = as.integer(as.character(cross500$cross))
data.rel = as.data.frame(left_join(data, cross500, by="state") %>% mutate(rel = index-cross))

#calculate relative data percapita
norm.spread = data %>% mutate(norm = death / (pop / 100000)) %>% select(date, state, norm) %>% spread(state, norm)
cross3.date = apply(norm.spread[,2:ncol(norm.spread)], 2, function(x) norm.spread[,1][min(which(!is.na(x) & x>0.05))])
cross3.index = date_index[as.character(cross3.date),2]
cross3.index[is.na(cross3.index)] = max(cross3.index, na.rm=T)
cross3 = as.data.frame(unname(cbind(names(cross3.date), cross3.index)))
colnames(cross3) = c("state", "cross")
cross3$cross = as.integer(as.character(cross3$cross))
data.relCapita = as.data.frame(left_join(data, cross3, by="state") %>% mutate(rel = index-cross))

##Make data tables
deathAbs = c()
deathAbs$noNorm = data %>% mutate(plot = death) %>% mutate(cases = plot) %>% select(date_label = date, date = index, state, plot, cases)
deathAbs$normByTotalCases = data %>% mutate(plot = death/total)  %>% mutate(cases = prettyNum(plot, digits=2)) %>% select(date_label = date, date = index, state, plot, cases)
deathAbs$normByPop = data %>% mutate(plot = death/(pop/100000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date_label = date, date = index, state, plot, cases)
deathRel = c()
deathRel$noNorm = data.rel %>% mutate(plot = death) %>% mutate(cases = plot) %>% select(date = rel, state, plot, cases)
deathRel$normByTotalCases = data.rel %>% mutate(plot = death/total)  %>% mutate(cases = prettyNum(plot, digits=2)) %>% select(date = rel, state, plot, cases)
deathRel$normByPop = data.rel %>% mutate(plot = death/(pop/100000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date = rel, state, plot, cases)
deathRelCapita = c()
deathRelCapita$noNorm = data.relCapita %>% mutate(plot = death) %>% mutate(cases = plot) %>% select(date = rel, state, plot, cases)
deathRelCapita$normByTotalCases = data.relCapita %>% mutate(plot = death/total)  %>% mutate(cases = prettyNum(plot, digits=2)) %>% select(date = rel, state, plot, cases)
deathRelCapita$normByPop = data.relCapita %>% mutate(plot = death/(pop/100000)) %>% mutate(cases = prettyNum(plot, digits=3)) %>% select(date = rel, state, plot, cases)

#Save data
saveRDS(deathAbs, file="deathAbs.RDS")
saveRDS(deathRel, file="deathRel.RDS")
saveRDS(deathRelCapita, file="deathRelCapita.RDS")
