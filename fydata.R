rm(list=ls()) # remove all existing objects in the environment
library(jsonlite)
json_file <- 'https://interface.sina.cn/news/wap/fymap2020_data.d.json'
json_data <- fromJSON(json_file)
data_flatten <- flatten(json_data$data$list)

time <- json_data$data$mtime
name <- data_flatten$name
pop <- as.numeric(data_flatten$value)
provdata <- data.frame(time = time, provname = name, provpop = pop,
                       stringsAsFactors = FALSE)
date <- as.Date(time)

citylist <- c()
surepop <- c()
provname <- c()

for (i in 1:34) {
  city <- data_flatten$city[[i]]$name
  connum <- data_flatten$city[[i]]$conNum
  prov <- rep(data_flatten$name[i], times = nrow(data_flatten$city[[i]]))
  citylist <- c(citylist, city)
  surepop <- c(surepop, connum)
  provname <- c(provname, prov)
}
citydata <- data.frame(time = time, provname = provname, city = citylist, citypop = as.numeric(surepop),
                       stringsAsFactors = FALSE)

write.csv(provdata, file = paste(date,'provdata.csv'))
write.csv(citydata, file = paste(date,'citydata.csv'))

library(RSQLite)
conn <- dbConnect(RSQLite::SQLite(), 'YiqingData.db')
dbWriteTable(conn, paste(date,'provdata'), provdata)
dbWriteTable(conn, paste(date,'citydata'), citydata)

# combine direct city data to only city level
citydata2 <- citydata
directcity <- which(provname %in% c('北京', '上海', '天津', '重庆'))
citydata2 <- citydata2[-c(directcity),]

#rbind not woking well with mapping function below, change to tibble add_row
library(tibble)
citydata2 <- add_row(citydata2, time = time, provname = '北京', city = '北京', citypop = provdata[1,3])
citydata2 <- add_row(citydata2, time = time, provname = '上海', city = '上海', citypop = provdata[15,3])
citydata2 <- add_row(citydata2, time = time, provname = '天津', city = '天津', citypop = provdata[23,3])
citydata2 <- add_row(citydata2, time = time, provname = '重庆', city = '重庆', citypop = provdata[7,3])
citydata2 <- add_row(citydata2, time = time, provname = '香港', city = '香港', citypop = provdata[29,3])
citydata2 <- add_row(citydata2, time = time, provname = '澳门', city = '澳门', citypop = provdata[28,3])
citydata2 <- add_row(citydata2, time = time, provname = '台湾', city = '台湾', citypop = provdata[27,3])

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

library(leafletCN)
geojsonMap(provdatamap, "china",
           namevar = ~provname, valuevar = ~provpop,
           popup =  paste0(provdata$provname, ":", provdata$provpop),
           palette = "Reds", legendTitle = "省级确诊人数")

geojsonMap(citydata3, "city",
           namevar = ~city, valuevar = ~citypop,
           na.color = 'white', 
           popup =  paste0(citydata2$city, ":", citydata2$citypop),
           palette = "Reds", legendTitle = "log(确诊人数)")

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
