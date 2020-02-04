rm(list=ls()) # remove all existing objects in the environment
library(jsonlite)
json_file <- 'https://interface.sina.cn/news/wap/fymap2020_data.d.json'
json_data <- fromJSON(json_file)
data_flatten <- flatten(json_data$data$list)

time <- json_data$data$mtime
name <- data_flatten$name
pop <- as.numeric(data_flatten$value)
provdata <- data.frame(time = time, provname = name, provpop = as.numeric(data_flatten$value),
                       deathpop = as.numeric(data_flatten$deathNum), curepop = as.numeric(data_flatten$cureNum), 
                       stringsAsFactors = FALSE)
date <- as.Date(time)

citylist <- c()
provname <- c()
surepop <- c()
deathpop <- c()
curepop <- c()

for (i in 1:34) {
  city <- data_flatten$city[[i]]$name
  connum <- data_flatten$city[[i]]$conNum
  deathnum <- data_flatten$city[[i]]$deathNum
  curenum <- data_flatten$city[[i]]$cureNum
  prov <- rep(data_flatten$name[i], times = nrow(data_flatten$city[[i]]))
  citylist <- c(citylist, city)
  provname <- c(provname, prov)
  surepop <- c(surepop, connum)
  deathpop <- c(deathpop, deathnum)
  curepop <- c(curepop, curenum)
}
citydata <- data.frame(time = time, provname = provname, city = citylist, 
                       citypop = as.numeric(surepop), deathpop = as.numeric(deathpop), 
                       curepop = as.numeric(curepop),
                       stringsAsFactors = FALSE)

# combine direct city data to only city level
citydata2 <- citydata
directcity <- which(provname %in% c('北京', '上海', '天津', '重庆'))
citydata2 <- citydata2[-c(directcity),]

#rbind not woking well with mapping function below, change to tibble add_row
library(tibble)
citydata2 <- add_row(citydata2, time = time, provname = '北京', city = '北京', 
                     citypop = provdata[1,3], deathpop = provdata[1,4], curepop = provdata[1,5])
citydata2 <- add_row(citydata2, time = time, provname = '上海', city = '上海', 
                     citypop = provdata[15,3], deathpop = provdata[15,4], curepop = provdata[15,5])
citydata2 <- add_row(citydata2, time = time, provname = '天津', city = '天津', 
                     citypop = provdata[23,3], deathpop = provdata[23,4], curepop = provdata[23,5])
citydata2 <- add_row(citydata2, time = time, provname = '重庆', city = '重庆', 
                     citypop = provdata[7,3], deathpop = provdata[7,4], curepop = provdata[7,5])
citydata2 <- add_row(citydata2, time = time, provname = '香港', city = '香港', 
                     citypop = provdata[29,3], deathpop = provdata[29,4], curepop = provdata[29,5])
citydata2 <- add_row(citydata2, time = time, provname = '澳门', city = '澳门', 
                     citypop = provdata[28,3], deathpop = provdata[28,4], curepop = provdata[28,5])
citydata2 <- add_row(citydata2, time = time, provname = '台湾', city = '台湾', 
                     citypop = provdata[27,3], deathpop = provdata[27,4], curepop = provdata[27,5])

#write date to csv file and sqlite database
write.csv(provdata, file = paste(date,'provdata.csv'))
write.csv(citydata2, file = paste(date,'citydata.csv'))

library(RSQLite)
conn <- dbConnect(RSQLite::SQLite(), 'YiqingData.db')
dbWriteTable(conn, paste(date,'provdata'), provdata)
dbWriteTable(conn, paste(date,'citydata'), citydata2)

#create new dataset for mapping
#province level
provdatamap <- provdata
provdatamap[2,3] <- 1000 #Change confirmed population of Hubei for better map looking
#city level
citydatamap <- citydata2
citydatamap[1,4] <- 1000 #Change confirmed population of Wuhan to 1000
citydatamap[2,4] <- 1000 #Change confirmed population of Huanggang to 1000

citydata3 <- citydata2
citydata3[,4] <- log(citydata3[,4])

#leaflet method
library(leafletCN)
geojsonMap(provdatamap, "china",
           namevar = ~provname, valuevar = ~provpop,
           popup =  paste0(provdata$provname, ":", provdata$provpop),
           palette = "Reds", legendTitle = "省级确诊人数")

geojsonMap(citydata3, "city",
           namevar = ~city, valuevar = ~citypop,
           na.color = 'white', 
           popup =  paste0(citydata2$city, ":",'确诊人数:', citydata2$citypop, '死亡人数:', citydata2$deathpop, '治愈人数', citydata2$curepop),
           palette = "Reds", legendTitle = "log(确诊人数)")

#Unfinish
#ggmap method
library(ggmap)
library(Hmisc)
register_google(key = "AIzaSyDoNZIMxOMz_jY1MP0WQKpx7gJqrhGxO1U")
map <- get_map('Hubei')
ggmap(map)

#Unfinish
#map shape method
library(rgdal)
library(ggplot2)
library(plyr)

mydat <- readOGR('china-shapefiles-master/china.shp')
length(mydat)

x <- mydat@data                          
xs <- data.frame(x,id=seq(0:30)-1)          
mydat1 <- fortify(mydat)             
china_map_data <- join(mydat1, xs, type = "full")

ggplot(china_map_data,aes(x=long,y=lat,group=group), fill = ) +
  #geom_polygon(fill="white",colour="grey") +
  geom_polygon(colour="grey60") +
  coord_map("polyconic")

#history data
historydata <- as.data.frame(json_data$data$historylist, stringsAsFactors = FALSE)
historydata <- historydata[,-c(6:9)]
historydata$date <- as.Date(historydata$date, '%m.%d')
historydata$cn_conNum <- as.numeric(historydata$cn_conNum)
historydata$cn_deathNum <- as.numeric(historydata$cn_deathNum)
historydata$cn_cureNum <- as.numeric(historydata$cn_cureNum)
historydata$cn_susNum <- as.numeric(historydata$cn_susNum)
write.csv(historydata, file = 'historydata.csv')

library(ggplot2)
ggplot(historydata) + 
  geom_line(aes(x = date, y = cn_conNum, color = 'yellow')) +
  geom_line(aes(x = date, y = cn_deathNum, color = 'red')) +
  geom_line(aes(x = date, y = cn_cureNum, color = 'green')) +
  geom_line(aes(x = date, y = cn_susNum, color = 'blue'))

ggplot(historydata, aes(x = date, y = cn_cureNum, color = "Yellow")) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

library(dplyr)
historydata[2:4] %>% cor() %>% round(4) %>% view() #same with line below
cor(historydata[2:4])
cov(historydata[2:4])
library(corrplot)
corrplot(cor(historydata[2:5]), method="circle", type = 'lower')

obj1 <- prcomp(historydata[2:4])
class(obj1)
names(obj1)

obj1$rotation
obj1$x
summary(obj1)

#######
#SEIR Model
#https://rpubs.com/srijana/110753
library (deSolve) 
#Function to compute derivatives of the differential equations.
seir_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}
#Parameters
contact_rate = 10                     # number of contacts per day
transmission_probability = 0.025       # transmission probability
infectious_period = 14                 # infectious period
latent_period = 2                     # latent period
#Compute values of beta (tranmission rate) and gamma (recovery rate).
beta_value = contact_rate * transmission_probability
gamma_value = 1 / infectious_period
delta_value = 1 / latent_period
#Compute Ro - Reproductive number.
Ro = beta_value / gamma_value
#Disease dynamics parameters.
parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)
#Initial values for sub-populations.
#using 2/3 data of China
W = mean(historydata$cn_susNum[1:7])       # susceptible hosts
X = mean(historydata$cn_conNum[1:7])       # infectious hosts
Y = mean(historydata$cn_cureNum[1:7])         # recovered hosts
Z = mean(historydata$cn_deathNum[1:7])         # exposed hosts
#Compute total population.
N = W + X + Y + Z
#Initial state values for the differential equations.
initial_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N)
#Output timepoints.
timepoints = seq (0, 50, by=1)
#Simulate the SEIR epidemic.
output = lsoda (initial_values, timepoints, seir_model, parameter_list)
#Plot dynamics of Susceptibles sub-population.
plot (S ~ time, data = output, type='b', col = 'blue')       
#sub-population
plot (E ~ time, data = output, type='b', col = 'pink')  
#Plot dynamics of Infectious sub-population.
plot (I ~ time, data = output, type='b', col = 'red') 
#Plot dynamics of Recovered sub-population.
plot (R ~ time, data = output, type='b', col = 'green')  

#Plot dynamics of Susceptibles, Exposed, Infectious and Recovered sub-populations in the same plot.
# susceptible hosts over time
plot (S ~ time, data = output, type='b', ylim = c(0,1), col = 'blue', ylab = 'S, E, I, R', main = 'SEIR epidemic') 
# remain on same frame
par (new = TRUE)    
# exposed hosts over time
plot (E ~ time, data = output, type='b', ylim = c(0,1), col = 'pink', ylab = '', axes = FALSE)
# remain on same frame
par (new = TRUE) 
# infectious hosts over time
plot (I ~ time, data = output, type='b', ylim = c(0,1), col = 'red', ylab = '', axes = FALSE) 
# remain on same frame
par (new = TRUE)  
# recovered hosts over time
plot (R ~ time, data = output, type='b', ylim = c(0,1), col = 'green', ylab = '', axes = FALSE)


