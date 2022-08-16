![Density  of Landslides based on causes](https://user-images.githubusercontent.com/92431548/184884320-53da008e-36e6-49d8-99b8-a1fc951fa4c5.png)


# Climate-change-and-its-impact-on-landslides-in-Nepal
Repository for R-code and report on understanding the impact of climate change on landslides in High Mountain Asia 

To run the code for that creates the data shown in the report run the file called "STDM Final Script"

This code was created in R Studio using R version 4.1.1

Make sure you have all the packages required to run the code:
maptools, lattice,spdep,sprgdal,tmap,ggplot2,gridExtra,gstat,OpenStreetMap,spacetime,lubridate,dplyr,tidyverse,spatstat,
spdplyr,raster,sf,forecast,tree,randomForest,caret,rsatscan,geosphere,elevatr

To run the rsatscan package you must have SatScan on your device. It can be downloaded at :
https://www.satscan.org/


IMPORTANT NOTES
- Make sure to set your Working Directory to "STDM Submission Files" 
Measuring river distance:
- The file is too large to be uploaded but can be downloaded from:
- Make sure to download the Polygon shapefile which is much smaller 
https://data.humdata.org/dataset/nepal-watercourses-rivers

- The process to measure the distance from every point to it's closet river takes approximately  30 hours. HOWEVER I have provided a shapefile called "ML_Landslide_dataset" that has all the preprocessing of the factors for the random forest apart from the rainfall data. 

