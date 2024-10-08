
library(sf)
library(tidyverse)

county.col <- function(d)
{
    latlong_sf<-data.frame("Longitude"=d$NextLongitude,"Latitude"=d$NextLatitude)
    latlong_sf<-latlong_sf%>%
        filter(!is.na(Latitude), !is.na(Longitude)) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs=st_crs(counties))
    
    intersected <- st_intersects(latlong_sf, counties)
    df_final <- latlong_sf %>%
        mutate(intersection = as.integer(intersected),
               fips = if_else(is.na(intersection), "",
                              counties$GEOID[intersection]))
    m=as.matrix(table(as.numeric(df_final$fips)))
    return(m);
}

counties<-st_read("./cb_2018_us_county_500k.shp", quiet=T)


# Predictions made using PhyREX


# YEAR 2000
# RRW
d=read.table("./2000/rrw/clean_predict",header=T);
m <- county.col(d); 
write.table(file="./2000/rrw/wnv_prediction_2000.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
write.table(file="./2000/rrw/wnv_log_prediction_2000.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# IBM
d=read.table("./2000/ibm/clean_predict",header=T);
m <- county.col(d); 
write.table(file="./2000/ibm/wnv_prediction_2000.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
write.table(file="./2000/ibm/wnv_log_prediction_2000.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")



# YEAR 2001
# RRW
d=read.table("./2001/rrw/clean_predict",header=T);
m <- county.col(d); 
write.table(file="./2001/rrw/wnv_prediction_2001.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
write.table(file="./2001/rrw/wnv_log_prediction_2001.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# IBM
d=read.table("./2001/ibm/clean_predict",header=T);
m <- county.col(d); 
write.table(file="./2001/ibm/wnv_prediction_2001.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
write.table(file="./2001/ibm/wnv_log_prediction_2001.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")


# YEAR 2002
# RRW
d=read.table("./2002/rrw/clean_predict",header=T);
m <- county.col(d); 
write.table(file="./2002/rrw/wnv_prediction_2002.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
write.table(file="./2002/rrw/wnv_log_prediction_2002.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# IBM
d=read.table("./2002/ibm/clean_predict",header=T);
m <- county.col(d); 
write.table(file="./2002/ibm/wnv_prediction_2002.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
write.table(file="./2002/ibm/wnv_log_prediction_2002.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")


# # YEAR 2003
# # RRW
# d=read.table("./2003/rrw/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2003/rrw/wnv_prediction_2003.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2003/rrw/wnv_log_prediction_2003.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# # IBM
# d=read.table("./2003/ibm/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2003/ibm/wnv_prediction_2003.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2003/ibm/wnv_log_prediction_2003.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")


# # YEAR 2004
# # RRW
# d=read.table("./2004/rrw/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2004/rrw/wnv_prediction_2004.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2004/rrw/wnv_log_prediction_2004.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# # IBM
# d=read.table("./2004/ibm/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2004/ibm/wnv_prediction_2004.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2004/ibm/wnv_log_prediction_2004.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")


# # YEAR 2005
# # RRW
# d=read.table("./2005/rrw/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2005/rrw/wnv_prediction_2005.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2005/rrw/wnv_log_prediction_2005.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# # IBM
# d=read.table("./2005/ibm/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2005/ibm/wnv_prediction_2005.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2005/ibm/wnv_log_prediction_2005.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")


# # YEAR 2006
# # RRW
# d=read.table("./2006/rrw/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2006/rrw/wnv_prediction_2006.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2006/rrw/wnv_log_prediction_2006.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# # IBM
# d=read.table("./2006/ibm/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2006/ibm/wnv_prediction_2006.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2006/ibm/wnv_log_prediction_2006.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")



# # YEAR 2007
# # RRW
# d=read.table("./2007/rrw/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2007/rrw/wnv_prediction_2007.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2007/rrw/wnv_log_prediction_2007.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")

# # IBM
# d=read.table("./2007/ibm/clean_predict",header=T);
# m <- county.col(d); 
# write.table(file="./2007/ibm/wnv_prediction_2007.csv",data.frame(County=rownames(m),Incidence=m[,1]),row.names=FALSE,quote=F,sep=",")
# write.table(file="./2007/ibm/wnv_log_prediction_2007.csv",data.frame(County=rownames(m),Incidence=log(m[,1],base=2)),row.names=FALSE,quote=F,sep=",")



# Incidence data collected from https://www.cdc.gov/westnile/statsmaps/historic-data.html

d=read.csv("./2000/wnv_incidence_2000.csv",header=T);
write.table(file="./2000/wnv_log_incidence_2000.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")

d=read.csv("./2001/wnv_incidence_2001.csv",header=T);
write.table(file="./2001/wnv_log_incidence_2001.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")

d=read.csv("./2002/wnv_incidence_2002.csv",header=T);
write.table(file="./2002/wnv_log_incidence_2002.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")

d=read.csv("./2003/wnv_incidence_2003.csv",header=T);
write.table(file="./2003/wnv_log_incidence_2003.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")

d=read.csv("./2004/wnv_incidence_2004.csv",header=T);
write.table(file="./2004/wnv_log_incidence_2004.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")

d=read.csv("./2005/wnv_incidence_2005.csv",header=T);
write.table(file="./2005/wnv_log_incidence_2005.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")

d=read.csv("./2006/wnv_incidence_2006.csv",header=T);
write.table(file="./2006/wnv_log_incidence_2006.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")

d=read.csv("./2007/wnv_incidence_2007.csv",header=T);
write.table(file="./2007/wnv_log_incidence_2007.csv",data.frame(County=d$County,Incidence=log(d$Incidence,base=2)),row.names=FALSE,quote=F,sep=",")









