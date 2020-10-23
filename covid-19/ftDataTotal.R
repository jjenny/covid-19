library(dplyr)
library(tidyr)
setwd("~/Desktop/covid-19/covid-19")
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