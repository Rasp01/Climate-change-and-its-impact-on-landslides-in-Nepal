library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)
library(lubridate)
library(dplyr)
library(tidyverse)
library(spatstat)
library(spdplyr)
library(raster)
library(sf)
library(forecast)
library(tree)
library(randomForest)
library(caret)
library(rsatscan)
library(geosphere)
library(elevatr)

###############################################
#PreProcessing
###############################################
## need to to get our data 

# from the natsa global landslide catalogue
#loction_a = location accuracy 
#landslide_ = the landslide catergory 
#landslide1 = landslide trigger
#landslid_1 = estimated size 
#landslide_2 = landslide setting
landslide_catalog <- readOGR("nasa_global_landslide_catalog_point/nasa_global_landslide_catalog_point.shp")

#need to get our points in there bounding boxes

#https://data.humdata.org/dataset/cod-ab-npl
Nepal <- readOGR("npl_admbnda_nd_20201117_shp/npl_admbnda_adm0_nd_20201117.shp")

#http://projects.datameet.org/maps/states/
India_states <- readOGR("States/Admin2.shp")

# Choose the states we want from the india states
# Only want Himachal Pradesh,Uttarakhand,	Jammu & Kashmir,	Sikkim and

#32 is west bengal
states <- c(28,23,11,13)
HMA_States <- India_states[states,]

#Bind the two shapefiles together to get our study area
HMA_area <- bind(HMA_States,Nepal)


#proj4string(HMA_area) <- CRS("+proj=longlat +datum=WGS84")
#writeOGR(HMA_area,dsn = '.',layer="HMA_area",driver = "ESRI Shapefile")

# Get the landslides that are in our area

HMA_area_sf <-st_as_sf(HMA_area)
st_crs(HMA_area_sf) <- 4326

st_write(HMA_area_sf,dsn = 'HMA_area.shp')



landslide_catalog_sf <-st_as_sf(landslide_catalog)
st_crs(landslide_catalog_sf) <- 4326


#HMA_landslides <- sf::st_join(HMA_area_sf,landslide_catalog_sf,join = st_intersects)

landslides_df <- sf::st_filter(landslide_catalog_sf,HMA_area_sf)



# extracting the year and the month for each value

landslides_df <- landslides_df %>%
  mutate(month = month(landslides_df$event_date))

landslides_df <- landslides_df %>%
  mutate(year = year(landslides_df$event_date))

landslides_df$Month_Yr <- format(as.Date(landslides_df$event_date), "%Y-%m")

# creating new dataframe which removes the dates for 1990,2018,2019

landslides_df <- subset(landslides_df, year != 1990 & year != 2019 & year != 2020 )

st_write(landslides_df,dsn = 'landslides.shp')

###############################################
#Spatial Analysis
###############################################

#Visual analysis

landslides <- readOGR("landslides.shp")

min_lat <- min(landslides$latitude)

max_lat <- max(landslides$latitude)

min_long <- min(landslides$longitude)

max_long <- max(landslides$longitude)

map <- openmap(c(max_lat, min_long),c(min_lat, max_long),type= 'bing')



map2 <- openproj(map)

OpenStreetMap::autoplot.OpenStreetMap(map2)+
  geom_point(data = landslides_df, aes( x = longitude, y = latitude))+
  xlab("Longitude (°E)") + ylab("Latitude (°S)")


# CSR

# 1. store x and y coords in two vectors
lon <- landslides_df$longitude
lat <- landslides_df$latitude

# 2. create two vectors xrange and yrange with dimensions of triangle that contain all points
xrange <- range(lon, na.rm=T)
yrange <- range(lat, na.rm=T)

# 3. create ppp
lf <- ppp(lon, lat, xrange, yrange,marks = as.factor(landslides_df$landslide1))

#####################################
# quadrant test#
quadrat.test(lf, nx = 10, ny = 10)

#########################################
# kolmogorov smirnof with coordinates#
par = (mfrow=c(1,2))
ks <- cdf.test(lf, "x")
plot(ks)


########################################
# kolmogorov smirnof with density#
ds <- density(lf)
k <- cdf.test(lf, ds)
plot(k)

########################################
# K function #

ktest <- Kest(lf)
ktest

plot(ktest)

plot(Lest(lf))

##########################################
# G function #

g_test <- Gest(lf)
g_test

plot(g_test)


##########################################
# F function #

ftest <- Fest(lf)
ftest
plot(ftest)

###############################################
#Temporal Analysis 
###############################################

year <- sort(unique(landslides_df$year))

# getting subsets of the data for each year 
list_df <- split(landslides_df, landslides_df$year)


data.2007 <- tabulate(list_df$`2007`$month,12)
data.2008 <- tabulate(list_df$`2008`$month,12)
data.2009 <- tabulate(list_df$`2009`$month,12)
data.2010 <- tabulate(list_df$`2010`$month,12)
data.2011 <- tabulate(list_df$`2011`$month,12)
data.2012 <- tabulate(list_df$`2012`$month,12)
data.2013 <- tabulate(list_df$`2013`$month,12)
data.2014 <- tabulate(list_df$`2014`$month,12)
data.2015 <- tabulate(list_df$`2015`$month,12)
data.2016 <- tabulate(list_df$`2016`$month,12)
data.2017 <- tabulate(list_df$`2017`$month,12)
data.2018 <- tabulate(list_df$`2018`$month,12)


data.2007_df <- data.frame(Count = data.2007,
                           Month = seq(1,12,1),
                           Year = 2007)
data.2008_df <- data.frame(Count = data.2008,
                           Month = seq(1,12,1),
                           Year = 2008)
data.2009_df <- data.frame(Count = data.2009,
                           Month = seq(1,12,1),
                           Year = 2009)
data.2010_df <- data.frame(Count = data.2010,
                           Year = 2010,
                           Month = seq(1,12,1))
data.2011_df <- data.frame(Count = data.2011,
                           Month = seq(1,12,1),
                           Year = 2011)
data.2012_df <- data.frame(Count = data.2012,
                           Month = seq(1,12,1),
                           Year = 2012)
data.2013_df <- data.frame(Count = data.2013,
                           Month = seq(1,12,1),
                           Year = 2013)
data.2014_df <- data.frame(Count = data.2014,
                           Month = seq(1,12,1),
                           Year = 2014)
data.2015_df <- data.frame(Count = data.2015,
                           Month = seq(1,12,1),
                           Year = 2015)
data.2016_df <- data.frame(Count = data.2016,
                           Month = seq(1,12,1),
                           Year = 2016)
data.2017_df <- data.frame(Count = data.2017,
                           Month = seq(1,12,1),
                           Year = 2017)
data.2018_df <- data.frame(Count = data.2018,
                           Month = seq(1,12,1),
                           Year = 2018)

df_list <- list(data.2007_df,data.2008_df,data.2009_df,
                data.2010_df,data.2011_df,data.2012_df,
                data.2013_df,data.2014_df,data.2015_df,
                data.2016_df,data.2017_df,data.2018_df)



landslide_count<-Reduce(function(x, y) merge(x, y,all=TRUE), df_list)
landslide_count <- landslide_count[order(landslide_count$Year,landslide_count$Month),]

rownames(landslide_count) <- NULL

landslide_count $Year <- as.character(landslide_count $Year)
landslide_count $Month <- factor(landslide_count $Month, seq(1,12,1))

barchart(Count ~ Month| Year,data=landslide_count,
         groups = Year, layout = c(2,6), stack = TRUE,
         ylab = "Amount of Landslides",
         scales = list(x = list(rot = 45)))

landslide_counts <- landslide_count$Count

landslidesLagged <- data.frame(count=landslide_counts[2:(length(landslide_counts))], count_minus_1=landslide_counts[1:(length(landslide_counts)-1)])

ggplot(landslidesLagged, aes(x=landslidesLagged$count, y=landslidesLagged$count_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  annotate("text", 8.5, 10, label=paste("r =", round(cor(landslidesLagged$count,landslidesLagged$count_minus_1), 3))) # Calculate PMCC
### Outliers effecting the result????

###############################################
#STPSS Modelling
###############################################

#creating case and corrdinates file 

landslideCoordgeo <- data.frame(landslides_df[c("longitude","latitude")])
landslideCoordgeo <- within(landslideCoordgeo, rm(geometry))
landslideCoordgeo <- tibble::rowid_to_column(landslideCoordgeo, "index")

landslidescas <- landslides_df[c("event_date")]
landslidescas <- landslidescas$event_date <- format(as.Date(landslides_df$event_date), "%Y/%m/%d")
landslidecountcas<- data.frame(count=1,landslidescas)
landslidecountcas <- tibble::rowid_to_column(landslidecountcas, "index")



# changing the parameters
invisible(ss.options(reset=TRUE))
ss.options(list(CaseFile="landslides.cas", PrecisionCaseTimes=3))
ss.options(c("StartDate=2007/01/05","EndDate=2018/12/31"))
ss.options(list(CoordinatesFile="landslides.geo",CoordinatesType=0, 
                AnalysisType=3, ModelType=2, TimeAggregationUnits=2))
ss.options(list(OutputShapefiles="n",OutputTemporalGraphHTML="y"))
ss.options(list(MostLikelyClusterEachCentroidASCII="y",MostLikelyClusterCaseInfoEachCentroidASCII="y",
                CensusAreasReportedClustersASCII="y",IncludeRelativeRisksCensusAreasDBase="n",
                SaveSimLLRsASCII="y"))

td = ('C:/Users/Raffy/OneDrive - University College London/UCL/Spatial Temporal Data Analysis/Landslides')
write.cas(landslidecountcas,td,"landslides")
write.geo(landslideCoordgeo,td,"landslides")
write.ss.prm(td,"landslides")
landslides_sts <-satscan(prmlocation = td,prmfilename = "landslides", sslocation="C:/Program Files/SaTScan",
                         ssbatchfilename = "SaTScanBatch64")

#convert dates to numeric for arcgis pro for mapping in 3D

landslides.col<- landslides_sts$col
landslides.col$Start_date1 <- as.Date(landslides.col$START_DATE)
landslides.col$Start_date1 <- as.numeric(landslides.col$Start_date1)
landslides.col$End_date1 <- as.Date(landslides.col$END_DATE)
landslides.col$End_date1 <- as.numeric(landslides.col$End_date1)

write.dbf(landslides.col,"landslides.col.dbf")

###############################################
#Arima Modelling
###############################################

#Autocorrelation

acf(landslide_counts, lag.max=36, xlab="Lag", ylab="ACF", main="Autocorrelation plot of Monthly Landslides",xaxt = "n")
axis(1, at = seq(0,36,3))
# 12 needed to remove cyclic pattern 

## difference autocorrelation
par(mfrow=c(1,1))
landslide_diff <- diff(landslide_counts, lag=12, differences=1)
acf(landslide_diff, lag.max=36, xlab="Lag", ylab="ACF", main="Differenced autocorrelation plot",xaxt = "n")
axis(1, at = seq(0,36,2))
# One or more spike, rest are essentially zero = Moving average Ma, order identified by where the plot becomes zero


# Partial Autocorrelation

pacf(landslide_counts, lag.max=36, xlab="Lag", ylab="ACF", main=" Partial Autocorrelation plot of Monthly Landslides")

## difference  partial - autocorrelation
pacf(landslide_diff, lag.max=36, xlab="Lag", ylab="ACF", main="Differenced Partial  autocorrelation plot",xaxt = "n")
axis(1, at = seq(0,36,2))

###################
# AIC ARIMA MODEl

landslides_ts <- ts(landslide_counts,deltat = 1/12, start = c(2007,1), end = c(2018,12))

## Calculated arima without seasonality 

model100 <- arima(landslides_ts,order=c(1,0,0))
model200 <- arima(landslides_ts,order=c(2,0,0))
model300 <- arima(landslides_ts,order=c(3,0,0))
model400 <- arima(landslides_ts,order=c(4,0,0))
model500 <- arima(landslides_ts,order=c(5,0,0))
model600 <- arima(landslides_ts,order=c(6,0,0))
model700 <- arima(landslides_ts,order=c(7,0,0))
AIC(model100,model200,model300,model400,model500,model600,model700)

# order 2 seems to be the best

model201 <- arima(landslides_ts,order=c(2,0,1))
model202 <- arima(landslides_ts,order=c(2,0,2))
model203 <- arima(landslides_ts,order=c(2,0,3))
model204 <- arima(landslides_ts,order=c(2,0,4))
model205 <- arima(landslides_ts,order=c(2,0,5))
model206 <- arima(landslides_ts,order=c(2,0,6))
model207 <- arima(landslides_ts,order=c(2,0,7))
AIC(model201,model202,model203,model204,model205,model206,model207)

# order 2 and ma of 2 seems to be best

model202<- arima(landslides_ts,order=c(2,0,2))
model212<- arima(landslides_ts,order=c(2,1,2))
model222<- arima(landslides_ts,order=c(2,2,2))
model232<- arima(landslides_ts,order=c(2,3,2))
model242<- arima(landslides_ts,order=c(2,4,2))
AIC(model202,model212,model222,model232,model242)

# model with 2,1,2 performs the best
# AIC of 1100.564
# AR lag of 3
# diffencing of 1 
# moving average of 2 

### Calculated arima with seasonality 

model212.100<- arima(landslides_ts,order=c(2,1,2),seasonal=list(order=c(1,0,0),period=12))
model212.200<- arima(landslides_ts,order=c(2,1,2),seasonal=list(order=c(2,0,0),period=12))
model212.300<- arima(landslides_ts,order=c(2,1,2),seasonal=list(order=c(3,0,0),period=12))
AIC(model212.100,model212.200,model212.300)


# order 3 seems to be the best

model212.301<- arima(landslides_ts,order=c(2,1,2),seasonal=list(order=c(3,0,1),period=12))
model212.302<- arima(landslides_ts,order=c(2,1,2),seasonal=list(order=c(3,0,2),period=12),method="ML")
AIC(model212.301,model212.302)
# order 3 and ma of 1 seems to be best

model212.321<- arima(landslides_ts,order=c(2,1,2),seasonal=list(order=c(3,2,1),period=12))
model212.331<- arima(landslides_ts,order=c(2,1,2),seasonal=list(order=c(3,3,1),period=12))
AIC(model212.321,model212.331)

##################
# Setting up training environment

source("starima_package.R")

x <- ts(landslide_counts)
length_data <- length(x)
length_train <- round(length_data*0.83)
train_data <- ts(head(x, length_train), 
                 frequency=frequency(x), start=start(x))
validation_data <- ts(tail(x, length_data-length_train), 
                      frequency=frequency(x), end=end(x))

#training just the training data on the model 
arima_train <- arima(train_data,order=c(2,1,2),seasonal=list(order=c(3,3,1),period=12),method = "ML") 
# testing the  model on the full dataset using the model
arima_full <- Arima(x, model=arima_train)
# getting the residuals from the data from the start of the validation data
matplot(cbind(arima_full$fitted[120:144], arima_full$x[120:144]), type="l", main = "ARIMA(2,1,2)(3,3,1)12",xlab = "Month Count",ylab="Amount of Landslides")


# model 2 based on observations

arima_train_obv <- arima(train_data,order=c(2,0,3),seasonal=list(order=c(2,1,1),period=12))
arima_full_obv <- Arima(x, model=arima_train_obv)
matplot(cbind(arima_full_obv$fitted[120:144], arima_full_obv$x[120:144]), type="l", main = "ARIMA(2,0,3)(2,1,1)12",xlab = "Month Count",ylab="Amount of Landslides")

#model 3 based on auto arima 

arima_train_auto <- arima(train_data,order=c(2,0,0),seasonal=list(order=c(0,1,0),period=0))
arima_full_auto <- Arima(x, model=arima_train_auto)
matplot(cbind(arima_full_auto$fitted[120:144], arima_full_auto$x[120:144]), type="l", main = "ARIMA(2,0,0)(0,1,0)",xlab = "Month Count",ylab="Amount of Landslides")

# AIC of training model
AIC(arima_train,arima_train_obv,arima_train_auto)

# AIC from validation model 
AIC(arima_full,arima_full_obv,arima_full_auto)

#NRMSE on training models

NRMSE_fit <-c((NRMSE(res = arima_train$residuals,obs=train_data)),(NRMSE(res = arima_train_obv$residuals,obs=train_data)),(NRMSE(res = arima_train_auto$residuals,obs=train_data)))

# NRMSE on validation set
NRMSE_fit_validation <-c((NRMSE(res = arima_full$residuals,obs=train_data)),(NRMSE(res = arima_full_obv$residuals,obs=train_data)),(NRMSE(res = arima_full_auto$residuals,obs=train_data)))


par(mfrow = c(1,1))

### TSDIAG on the training models 

tsdiag(arima_train)

Box.test(arima_train$residuals,lag= 10)


tsdiag(arima_train_obv)

Box.test(arima_train_obv$residuals,lag= 10)

tsdiag(arima_train_auto)

Box.test(arima_train_auto$residuals,lag= 10)

## tsdiag on the validaiton models 


tsdiag(arima_full)

Box.test(arima_full$residuals,lag= 10)


tsdiag(arima_full_obv)

Box.test(arima_full_obv$residuals,lag= 10)

tsdiag(arima_full_auto)

Box.test(arima_full_auto$residuals,lag= 10)

###############################################
#Random Forest 
###############################################

#### Create a subset of just Nepal Landslides

landslides_sf <- st_as_sf(landslides)
Nepal_sf <- st_as_sf(Nepal)

Nepal_landslides<- sf::st_filter(landslides_sf,Nepal_sf)
st_crs(Nepal_landslides) <- 4326

Nepal_points <-cbind(Nepal_landslides$longitude,Nepal_landslides$latitude)
test_point <- Nepal_points[1,]


Nepal_df <- data.frame(Nepal_landslides)

###############################################################

# Add a new column of the elevation

Nepal_elevations <- get_elev_point(Nepal_landslides, src = "aws")

###############################################################

# add Geology type

Geology <- readOGR("STDM Machine Learning Data/Geology data/Geology.shp")

Nepal_Geology <- as(Nepal_elevations,'Spatial')

# The Geological unit for each point 
Combined<-Nepal_Geology %over% Geology

Nepal_Geology$Geology <- Combined$CLASS

Nepal_Geology$GEOL_CODE <- Combined$GEOL_CODE

################################################################

#Add soil type 

Soil <- readOGR("STDM Machine Learning Data/Geology data/Soil.shp")

Combined_soil<-Nepal_Geology %over% Soil

Nepal_Geology$SOIL_CODE <- Combined_soil$SOIL_CODE

Nepal_Geology$Soil <- Combined_soil$TYPE

#################################################################

# distance from active fault

active_faults <-readOGR("STDM Machine Learning Data/shapefile/gem_active_faults_harmonized.shp")
active_faults_sf <- st_as_sf(active_faults)

Nepal_faults <- sf::st_filter(active_faults_sf,Nepal_sf)

Nepal_faults <- as(Nepal_faults,'Spatial')


fault_distance <-dist2Line(Nepal_points, Nepal_faults, distfun=distGeo)
fault_distance <- data.frame(fault_distance)

Nepal_Geology$distance_from_faults <- fault_distance$distance



######################################################################


# add distance from built up areas

built_up_areas <-readOGR("STDM Machine Learning Data/npl_builda_25K_50K_sdn_wgs84.shp")



city_distance <-dist2Line(Nepal_points, built_up_areas, distfun=distGeo)
city_distance_df <- data.frame(city_distance)


Nepal_Geology$distance_from_built_up_areas <- city_distance_df$distance


######################################################################

##################### THIS TAKES A VERY LONG TIME###############################

# Add distance from river

rivers <-readOGR("STDM Machine Learning Data/npl-watcrsa-hydro-25k-50k-sdn-wgs84-shp/npl_watcrsa_hydro_25K_50K_sdn_wgs84.shp")


river_distance <-dist2Line(Nepal_points, rivers, distfun=distGeo)
river_distance_df <- data.frame(river_distance)

Nepal_Geology$distance_from_rivers <- river_distance_df$distance

Nepal_ML_Landslide_sf <- st_as_sf(Nepal_Geology)

st_write(Nepal_ML_Landslide_sf,dsn ='ML_Landslide_dataset.shp')

########################################

# adding rainfall information 

Regions <-readOGR("STDM Machine Learning Data/Development Regions of Nepal/data/Region.shp")

#https://gis.stackexchange.com/questions/408855/splitting-multipolygon-shapefile-into-separate-polygons-in-r

unique <- unique(Regions$REG_NAME)

#create new polygons based on the determined column
for (i in 1:length(unique)) {
  tmp <- Regions[Regions$REG_NAME == unique[i], ]
  writeOGR(tmp, dsn=getwd(), unique[i], driver="ESRI Shapefile",
           overwrite_layer=TRUE)
}
#################################

ML_Landslides <- readOGR("ML_landslide_dataset.shp")
ML_Landslides_sf <- st_as_sf(ML_Landslides)

#################################

Central_shp <- readOGR("STDM Machine Learning Data/Development Regions of Nepal/data/Central Development Region.shp")

Central <- read.csv("STDM Machine Learning Data/rainfall_for_central_Nepal.csv")

Central <- Central[!(Central$X < 2008 | Central$X > 2018),]

Central_shp@data <- Central

Central_sf <-st_as_sf(Central_shp)

Central_landslides <- sf::st_filter(ML_Landslides_sf,Central_sf)

#Central_landslides$year <- as.integer(Central_landslides$year)


as.numeric(colnames(Central_sf)<-seq(0,13,1))

years <- unique(Central_sf$`0`)

as.integer(rownames(Central_sf)<- c(years))

Central_sf <- as.data.frame(t(Central_sf))

# Extracting a value from the rainfall data
#Central_landslides$rainfall[1]<-Central_sf$`2018`$`5`

Central_landslides$rainfall<-NA

##tester for for loop
for (i in 1:nrow(Central_landslides)){
  if(Central_landslides$year[i] == 2008){
    year <- 1
  }else if (Central_landslides$year[i] == 2009){
    year <- 2
  }else if (Central_landslides$year[i] == 2010){
    year <- 3
  }else if (Central_landslides$year[i] == 2011){
    year <- 4
  }else if (Central_landslides$year[i] == 2012){
    year <- 5
  }else if (Central_landslides$year[i] == 2013){
    year <- 6
  }else if (Central_landslides$year[i] == 2014){
    year <- 7
  }else if (Central_landslides$year[i] == 2015){
    year <- 8
  }else if (Central_landslides$year[i] == 2016){
    year <- 9
  }else if (Central_landslides$year[i] == 2017){
    year <- 10
  }else if (Central_landslides$year[i] == 2018){
    year <- 11
  }
  if (Central_landslides$month[i] == 1){
    month <- 2
  }else if(Central_landslides$month[i] == 2){
    month <- 3
  }else if(Central_landslides$month[i] == 3){
    month <- 4
  }else if(Central_landslides$month[i] == 4){
    month <- 5
  }else if(Central_landslides$month[i] == 5){
    month <- 6
  }else if(Central_landslides$month[i] == 6){
    month <- 7
  }else if(Central_landslides$month[i] == 7){
    month <- 8
  }else if(Central_landslides$month[i] == 8){
    month <- 9
  }else if(Central_landslides$month[i] == 9){
    month <- 10
  }else if(Central_landslides$month[i] == 10){
    month <- 11
  }else if(Central_landslides$month[i] == 11){
    month <- 12
  }else if(Central_landslides$month[i] == 12){
    month <- 13
  }
  Central_landslides$rainfall[i] <- Central_sf[month,year]
}

Central_landslides_shp <- as(Central_landslides,"Spatial")




####################################

Eastern_shp <- readOGR("STDM Machine Learning Data/Development Regions of Nepal/data/Eastern Development Region.shp")

Eastern <- read.csv("STDM Machine Learning Data/rainfall_for_eastern_Nepal.csv")

Eastern <- Eastern[!(Eastern$X < 2008 | Eastern$X > 2018),]

Eastern_shp@data <- Eastern

Eastern_sf <-st_as_sf(Eastern_shp)

Eastern_landslides <- sf::st_filter(ML_Landslides_sf,Eastern_sf)

#Eastern_landslides$year <- as.integer(Eastern_landslides$year)


as.numeric(colnames(Eastern_sf)<-seq(0,13,1))

years <- unique(Eastern_sf$`0`)

as.integer(rownames(Eastern_sf)<- c(years))

Eastern_sf <- as.data.frame(t(Eastern_sf))

# Extracting a value from the rainfall data
#Eastern_landslides$rainfall[1]<-Eastern_sf$`2018`$`5`

Eastern_landslides$rainfall<-NA

##tester for for loop
for (i in 1:nrow(Eastern_landslides)){
  if(Eastern_landslides$year[i] == 2008){
    year <- 1
  }else if (Eastern_landslides$year[i] == 2009){
    year <- 2
  }else if (Eastern_landslides$year[i] == 2010){
    year <- 3
  }else if (Eastern_landslides$year[i] == 2011){
    year <- 4
  }else if (Eastern_landslides$year[i] == 2012){
    year <- 5
  }else if (Eastern_landslides$year[i] == 2013){
    year <- 6
  }else if (Eastern_landslides$year[i] == 2014){
    year <- 7
  }else if (Eastern_landslides$year[i] == 2015){
    year <- 8
  }else if (Eastern_landslides$year[i] == 2016){
    year <- 9
  }else if (Eastern_landslides$year[i] == 2017){
    year <- 10
  }else if (Eastern_landslides$year[i] == 2018){
    year <- 11
  }
  if (Eastern_landslides$month[i] == 1){
    month <- 2
  }else if(Eastern_landslides$month[i] == 2){
    month <- 3
  }else if(Eastern_landslides$month[i] == 3){
    month <- 4
  }else if(Eastern_landslides$month[i] == 4){
    month <- 5
  }else if(Eastern_landslides$month[i] == 5){
    month <- 6
  }else if(Eastern_landslides$month[i] == 6){
    month <- 7
  }else if(Eastern_landslides$month[i] == 7){
    month <- 8
  }else if(Eastern_landslides$month[i] == 8){
    month <- 9
  }else if(Eastern_landslides$month[i] == 9){
    month <- 10
  }else if(Eastern_landslides$month[i] == 10){
    month <- 11
  }else if(Eastern_landslides$month[i] == 11){
    month <- 12
  }else if(Eastern_landslides$month[i] == 12){
    month <- 13
  }
  Eastern_landslides$rainfall[i] <- Eastern_sf[month,year]
}

Eastern_landslides_shp <- as(Eastern_landslides,"Spatial")

#######################################

FarWestern_shp <- readOGR("STDM Machine Learning Data/Development Regions of Nepal/data/Far-Western Development Region.shp")

FarWestern <- read.csv("STDM Machine Learning Data/rainfall_for_farwestern_Nepal.csv")

FarWestern <- FarWestern[!(FarWestern$X < 2008 | FarWestern$X > 2018),]

FarWestern_shp@data <- FarWestern

FarWestern_sf <-st_as_sf(FarWestern_shp)

FarWestern_landslides <- sf::st_filter(ML_Landslides_sf,FarWestern_sf)

#FarWestern_landslides$year <- as.integer(FarWestern_landslides$year)


as.numeric(colnames(FarWestern_sf)<-seq(0,13,1))

years <- unique(FarWestern_sf$`0`)

as.integer(rownames(FarWestern_sf)<- c(years))

FarWestern_sf <- as.data.frame(t(FarWestern_sf))

# Extracting a value from the rainfall data
#FarWestern_landslides$rainfall[1]<-FarWestern_sf$`2018`$`5`

FarWestern_landslides$rainfall<-NA

##tester for for loop
for (i in 1:nrow(FarWestern_landslides)){
  if(FarWestern_landslides$year[i] == 2008){
    year <- 1
  }else if (FarWestern_landslides$year[i] == 2009){
    year <- 2
  }else if (FarWestern_landslides$year[i] == 2010){
    year <- 3
  }else if (FarWestern_landslides$year[i] == 2011){
    year <- 4
  }else if (FarWestern_landslides$year[i] == 2012){
    year <- 5
  }else if (FarWestern_landslides$year[i] == 2013){
    year <- 6
  }else if (FarWestern_landslides$year[i] == 2014){
    year <- 7
  }else if (FarWestern_landslides$year[i] == 2015){
    year <- 8
  }else if (FarWestern_landslides$year[i] == 2016){
    year <- 9
  }else if (FarWestern_landslides$year[i] == 2017){
    year <- 10
  }else if (FarWestern_landslides$year[i] == 2018){
    year <- 11
  }
  if (FarWestern_landslides$month[i] == 1){
    month <- 2
  }else if(FarWestern_landslides$month[i] == 2){
    month <- 3
  }else if(FarWestern_landslides$month[i] == 3){
    month <- 4
  }else if(FarWestern_landslides$month[i] == 4){
    month <- 5
  }else if(FarWestern_landslides$month[i] == 5){
    month <- 6
  }else if(FarWestern_landslides$month[i] == 6){
    month <- 7
  }else if(FarWestern_landslides$month[i] == 7){
    month <- 8
  }else if(FarWestern_landslides$month[i] == 8){
    month <- 9
  }else if(FarWestern_landslides$month[i] == 9){
    month <- 10
  }else if(FarWestern_landslides$month[i] == 10){
    month <- 11
  }else if(FarWestern_landslides$month[i] == 11){
    month <- 12
  }else if(FarWestern_landslides$month[i] == 12){
    month <- 13
  }
  FarWestern_landslides$rainfall[i] <- FarWestern_sf[month,year]
}

FarWestern_landslides_shp <- as(FarWestern_landslides,"Spatial")


###########################################

MidWestern_shp <- readOGR("STDM Machine Learning Data/Development Regions of Nepal/data/Mid-Western Development Region.shp")

MidWestern <- read.csv("STDM Machine Learning Data/rainfall_for_midwestern_Nepal.csv")

MidWestern <- MidWestern[!(MidWestern$X < 2008 | MidWestern$X > 2018),]

MidWestern_shp@data <- MidWestern

MidWestern_sf <-st_as_sf(MidWestern_shp)

MidWestern_landslides <- sf::st_filter(ML_Landslides_sf,MidWestern_sf)

#MidWestern_landslides$year <- as.integer(MidWestern_landslides$year)


as.numeric(colnames(MidWestern_sf)<-seq(0,13,1))

years <- unique(MidWestern_sf$`0`)

as.integer(rownames(MidWestern_sf)<- c(years))

MidWestern_sf <- as.data.frame(t(MidWestern_sf))

# Extracting a value from the rainfall data
#MidWestern_landslides$rainfall[1]<-MidWestern_sf$`2018`$`5`

MidWestern_landslides$rainfall<-NA

##tester for for loop
for (i in 1:nrow(MidWestern_landslides)){
  if(MidWestern_landslides$year[i] == 2008){
    year <- 1
  }else if (MidWestern_landslides$year[i] == 2009){
    year <- 2
  }else if (MidWestern_landslides$year[i] == 2010){
    year <- 3
  }else if (MidWestern_landslides$year[i] == 2011){
    year <- 4
  }else if (MidWestern_landslides$year[i] == 2012){
    year <- 5
  }else if (MidWestern_landslides$year[i] == 2013){
    year <- 6
  }else if (MidWestern_landslides$year[i] == 2014){
    year <- 7
  }else if (MidWestern_landslides$year[i] == 2015){
    year <- 8
  }else if (MidWestern_landslides$year[i] == 2016){
    year <- 9
  }else if (MidWestern_landslides$year[i] == 2017){
    year <- 10
  }else if (MidWestern_landslides$year[i] == 2018){
    year <- 11
  }
  if (MidWestern_landslides$month[i] == 1){
    month <- 2
  }else if(MidWestern_landslides$month[i] == 2){
    month <- 3
  }else if(MidWestern_landslides$month[i] == 3){
    month <- 4
  }else if(MidWestern_landslides$month[i] == 4){
    month <- 5
  }else if(MidWestern_landslides$month[i] == 5){
    month <- 6
  }else if(MidWestern_landslides$month[i] == 6){
    month <- 7
  }else if(MidWestern_landslides$month[i] == 7){
    month <- 8
  }else if(MidWestern_landslides$month[i] == 8){
    month <- 9
  }else if(MidWestern_landslides$month[i] == 9){
    month <- 10
  }else if(MidWestern_landslides$month[i] == 10){
    month <- 11
  }else if(MidWestern_landslides$month[i] == 11){
    month <- 12
  }else if(MidWestern_landslides$month[i] == 12){
    month <- 13
  }
  MidWestern_landslides$rainfall[i] <- MidWestern_sf[month,year]
}

MidWestern_landslides_shp <- as(MidWestern_landslides,"Spatial")

###########################################

Western_shp <- readOGR("STDM Machine Learning Data/Development Regions of Nepal/data/Western Development Region.shp")

Western <- read.csv("STDM Machine Learning Data/rainfall_for_western_Nepal.csv")

Western <- Western[!(Western$X < 2008 | Western$X > 2018),]

Western <- Western[!(Western$X < 2008 | Western$X > 2018),]

Western_shp@data <- Western

Western_sf <-st_as_sf(Western_shp)

Western_landslides <- sf::st_filter(ML_Landslides_sf,Western_sf)

#Western_landslides$year <- as.integer(Western_landslides$year)


as.numeric(colnames(Western_sf)<-seq(0,13,1))

years <- unique(Western_sf$`0`)

as.integer(rownames(Western_sf)<- c(years))

Western_sf <- as.data.frame(t(Western_sf))

# Extracting a value from the rainfall data
#Western_landslides$rainfall[1]<-Western_sf$`2018`$`5`

Western_landslides$rainfall<-NA

##tester for for loop
for (i in 1:nrow(Western_landslides)){
  if(Western_landslides$year[i] == 2008){
    year <- 1
  }else if (Western_landslides$year[i] == 2009){
    year <- 2
  }else if (Western_landslides$year[i] == 2010){
    year <- 3
  }else if (Western_landslides$year[i] == 2011){
    year <- 4
  }else if (Western_landslides$year[i] == 2012){
    year <- 5
  }else if (Western_landslides$year[i] == 2013){
    year <- 6
  }else if (Western_landslides$year[i] == 2014){
    year <- 7
  }else if (Western_landslides$year[i] == 2015){
    year <- 8
  }else if (Western_landslides$year[i] == 2016){
    year <- 9
  }else if (Western_landslides$year[i] == 2017){
    year <- 10
  }else if (Western_landslides$year[i] == 2018){
    year <- 11
  }
  if (Western_landslides$month[i] == 1){
    month <- 2
  }else if(Western_landslides$month[i] == 2){
    month <- 3
  }else if(Western_landslides$month[i] == 3){
    month <- 4
  }else if(Western_landslides$month[i] == 4){
    month <- 5
  }else if(Western_landslides$month[i] == 5){
    month <- 6
  }else if(Western_landslides$month[i] == 6){
    month <- 7
  }else if(Western_landslides$month[i] == 7){
    month <- 8
  }else if(Western_landslides$month[i] == 8){
    month <- 9
  }else if(Western_landslides$month[i] == 9){
    month <- 10
  }else if(Western_landslides$month[i] == 10){
    month <- 11
  }else if(Western_landslides$month[i] == 11){
    month <- 12
  }else if(Western_landslides$month[i] == 12){
    month <- 13
  }
  Western_landslides$rainfall[i] <- Western_sf[month,year]
}

Western_landslides_shp <- as(Western_landslides,"Spatial")


#########################
ML_rainfall <- bind(Central_landslides_shp,Eastern_landslides_shp,Western_landslides_shp,MidWestern_landslides_shp,FarWestern_landslides_shp)

ML_rainfall$rainfall <- as.numeric(unlist(ML_rainfall$rainfall))

########################

ML_dataset <- data.frame(ML_rainfall)

# APPLYING TO Random Forest
ML_dataset$GEOL_CO <- as.numeric(ML_dataset$GEOL_CO)

ML_dataset$SOIL_CO<- as.numeric(ML_dataset$SOIL_CO)

#uknown doesn't help us, construction only 1 example, mining only 6 and only 1 for other 
ML_dataset<- ML_dataset[!(ML_dataset$lndsld1 == "unknown"|ML_dataset$lndsld1 == "construction"|ML_dataset$lndsld1 == "other"),]

library(randomForest)
#Random Forest

X <- as.matrix(ML_dataset[,-c((1:32),34,35,38,43,44,45)])
y <- as.matrix(ML_dataset[,"lndsld1"])
y[which(y==0)] <- -1
set.seed(1)
n <- nrow(X)
trainInd <- sort(sample(1:n, n*.8))
XTrain <- X[trainInd,]
XTest <- X[-trainInd,]

yTrain <- as.factor(y[trainInd])
yTest <- as.factor(y[-trainInd])



lsModel <- randomForest(x=XTrain, y=yTrain, ntree=500, mtry=3)
lsPred <- predict(lsModel, XTest)
lsErr <- as.numeric(yTest)-as.numeric(lsPred)
length(which((lsErr)>0))/length(lsErr)


varImpPlot(lsModel)


#https://code-examples.net/en/q/17a70ac

pred <- as.factor(lsPred)

actual<-as.factor(yTest)

my_data1 <- data.frame(data = pred , type = "prediction")
my_data2 <- data.frame(data = actual, type = "real")
my_data3 <- rbind(my_data1,my_data2)

# Check if the levels are identical
identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))

confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1],  dnn = c("Prediction", "Reference"), mode="everything")



