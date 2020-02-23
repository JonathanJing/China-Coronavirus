#新型冠状病毒疫情数据爬虫，疫情省市地图，SEIR传染病模型和基本传染数R0预测

##爬虫

数据来源 [新浪新闻](https://news.sina.cn/zt_d/yiqing0121), 每日更新。如需更多数据也可参考已有[数据仓库](https://github.com/BlankerL/DXY-COVID-19-Data)

##数据库

数据保存在Dataset文件夹

​	1：省级数据(全国34个省，直辖市包括港澳台): provdata.csv

​	2：市级数据(340个地级市含直辖市): citydata.csv

另有SQLite数据库YiqingData.db

## 疫情地图

地图包使用 [leafletCN](https://cran.r-project.org/web/packages/leafletCN/index.html), 包括全国主要省，市行政区域

## SEIR传染病模型预测



## 基本传染数R0预测

