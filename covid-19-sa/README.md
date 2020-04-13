# covid-19-sa

#### ftJHUData.R

FUNCTION  
This script takes in COVID-19 tracking and relevant data and creates data structures for use by the shiny app. The purpose of this script is to minimize the amount of computation the shiny app has to do in real time.  
	
DEPENDENCIES  
dplyr  
tidyr  

INPUT FILES:   
pop.csv - csv table of country populations (millions)  
response_data.csv - csv table of government responses  
time_series_covid19_confirmed_global.csv - COVID-19 tracking data from JHU repository  

OUTPUT FILES:   
dataAbs.RDS  
dataAbs$noNorm - Unnormalized data with absolute date  
dataAbs$normByPop - Data normalized by population, with absolute date  

dataRel.RDS  
dataRel$noNorm - Unnormalized data with date relative to 500 confirmed cases (or most recent date)  
dataRel$normByPop - Data normalized by population, with date relative to 0.001% confirmed cases (or most recent date)  

states.RDS  
states$bin1 - list of countries with more than 500 cases  
states$bin2 - list of countries with less than 500 cases  

response.RDS  
	response$abs - gov't responses, with absolute date.  
	response$rel - gov't responses, with date relative to 500 confirmed cases  
	response$relCapita - gov't responses, with date relative to 0.001% confirmed cases  

---

#### app.R

FUNCTION  
This script is the shiny script that executes when someone loads the shiny app.   
	
DEPENDENCES  
shiny   
ggplot2  
dplyr   
tidyr  
	
INPUT FILES:   
dataAbs.RDS  
dataRel.RDS  
states.RDS   
responses.RDS  

