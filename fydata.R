library(jsonlite)
json_file <- 'https://interface.sina.cn/news/wap/fymap2020_data.d.json'
json_data <- fromJSON(json_file)
data_flatten <- flatten(json_data$data$list)

time <- json_data$data$mtime
name <- data_flatten$name
pop <- as.numeric(data_flatten$value)
provdata <- data.frame(time = time, provname = name, provpop = pop)
date <- as.Date(time)

citylist <- c()
surepop <- c()
provname <- c()
direct_city <- function(i){
  city <- name[i]
  connum <- pop[i]
  prov <- name[i]
  citylist <- c(citylist, city)
  surepop <- c(surepop, connum)
  provname <- c(provname, prov)
  return(citylist, surepop, provname)
}
direct_city_list <- c(1,7,15,23,27,28,29) #z直辖市及港澳台地区
direct_city(direct_city_list) 

for (i in 1:34) {
  city <- data_flatten$city[[i]]$name
  connum <- data_flatten$city[[i]]$conNum
  prov <- rep(data_flatten$name[i], times = nrow(data_flatten$city[[i]]))
  citylist <- c(citylist, city)
  surepop <- c(surepop, connum)
  provname <- c(provname, prov)
}
citydata <- data.frame(time = time, provname = provname, city = citylist, citypop = as.numeric(surepop))

write.csv(provdata, file = paste(date,'provdata.csv'))
write.csv(citydata, file = paste(date,'citydata.csv'))

library(RSQLite)
conn <- dbConnect(RSQLite::SQLite(), 'YiqingData.db')
dbWriteTable(conn, paste(date,'provdata'), provdata)
dbWriteTable(conn, paste(date,'citydata'), citydata)

library(leafletCN)
geojsonMap(provdata, "china",
           namevar = ~provname, valuevar = ~provpop,
           popup =  paste0(provdata$provname, ":", provdata$provpop),
           palette = "Reds", legendTitle = "省级确诊人口")

geojsonMap(citydata, "city",
           namevar = ~city, valuevar = ~citypop,
           popup =  paste0(citydata$city, ":", citydata$citypop),
           na.color = "white", stroke = T,
           palette = "Reds", legendTitle = "确诊人口")

##daily data
library(jsonlite)
library(stringr)
day_file <- 'https://view.inews.qq.com/g2/getOnsInfo?name=wuwei_ww_cn_day_counts'
day_data <- fromJSON(day_file)
day_data2 <- day_data$data
#day_data3 <- strsplit(day_data2, '\n')
day_data3 <- str_remove(day_data2, '\n')
#day_data4 <- strsplit(day_data3, '')
day_data4 <- str_remove(day_data3, ':')

##ggmap test##
library(ggmap)
library(Hmisc)
register_google(key = "AIzaSyDoNZIMxOMz_jY1MP0WQKpx7gJqrhGxO1U")
map <- get_map('Hubei')
ggmap(map)

######
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

#####

