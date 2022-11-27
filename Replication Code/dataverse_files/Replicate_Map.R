### Replication file for
### FDI, Unmet Expectations, and the Prospects of Political Leaders

### Plot the locations of FDI, aid, and surveyed areas on the map
rm(list=ls())

### Use "Alt+O" to collapse all sections and "Shift+Alt+O" to expand all

### Set your own working directory

# Load required packages --------------------------------------------------
library(sf)          # classes and functions for vector data
library(spData)        # load geographic data
library(lwgeom)        # for calculating distance
library(tidyverse)

# Data of FDI projects ----------------------------------------------------

### Read FDI data into R
fdi<-read_csv("FDIProjGeocode.csv")


aid_11 <- c("Botswana", "Ghana", "Kenya", "Madagascar", "Mozambique", "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda", "Zimbabwe")
nonaid_10 <- c("Algeria", "Cameroon", "Cote d'Ivoire", "Egypt", "Malawi", "Mauritius", "Morocco", "Namibia", "Tunisia", "Zambia")

north_africa <- c("Algeria","Egypt","Morocco","Tunisia")

fdi <- subset(fdi, Destination_country %in% north_africa)

nrow(fdi)

### Projects that are precisely coded, from mainland China, and new
fdi_precise<-fdi %>% filter(Precision_final %in% c(1,2,9),
                            Source_country=="China",
                            Project_type!="Expansion",
                            Project_type!="Co-location")

### Year of announcement (if missing, use the year in the dataset)
fdi_precise<-fdi_precise %>% 
  mutate(Ancmtyear=as.numeric(Ancmtyear),
         Ancmtyear=ifelse(is.na(Ancmtyear),Year,Ancmtyear))

### Year of operation (if missing ,code it as 9999)
fdi_precise<-fdi_precise %>% 
  mutate(Opyear_final=as.numeric(Opyear_final),
         Opyear_final=ifelse(is.na(Opyear_final),9999,Opyear_final))

### Sectors: manufacturing, resources, service
fdi_precise<-fdi_precise %>% 
  mutate(Sector_type=ifelse(Sector %in% c("Coal, oil & gas","Metals",
                                          "Minerals","Renewable energy"),
                            "Resource",
                            ifelse(Sector %in% c("Communications","Aerospace",
                                                 "Real estate","Healthcare",
                                                 "Financial services",
                                                 "Business services",
                                                 "Warehousing","Transportation",
                                                 "Software & IT services"),
                                   "Service","Manufacture")))

### Ownership: private vs state-owned
fdi_precise<-fdi_precise %>% 
  mutate(Ownership_final=trimws(Ownership_final),
         Ownership_final=ifelse(Ownership_final!="POE","State-owned","Private"))

### Countries from FDI data
country_fdi<-fdi_precise %>% 
  pull(Destination_country) %>% 
  unique()

### Convert the data frame to "sf" type for geocomputation
fdi_precise_sf<-st_as_sf(fdi_precise,
                         coords=c("xcoord","ycoord"),# longitudes and latitudes
                         crs=4326 # specify the geocoordination system
                         )

# Data of aid projects -----------------------------------------------------

### Read the aid data into R
aid<-read_csv("ChinaAid.csv")

### Rename the unique aid project id
aid<-aid %>% rename(.,"AIDProj"="aiddata_tuff_id")

### Subset AID project in ODA type, with precise location and year of start
aid_ODA<-aid %>% filter(flow_class=="ODA-like",
                        precision_code %in% c(1,2),
                        start_actual!="")

### Year of start and announcement
aid_ODA<-aid_ODA %>% 
  mutate(year_start=start_actual %>% 
           stri_sub(.,-2,-1) %>% 
           paste(20,.,sep="") %>% 
           as.numeric(),
         year_ancmt=year_)

### Subset the data with variables needed
aid_ODA<-aid_ODA %>% 
  subset(select=c(AIDProj,ADM0_NAME,year_start,year_ancmt,latitude,longitude))

### Country names in the aid data
aid_ODA<-aid_ODA %>% 
  mutate(country=ifelse(ADM0_NAME=="United Republic of Tanzania",
                        "Tanzania",ifelse(ADM0_NAME=="CÃ´te dâ€™ Ivoire",
                                          "Cote d'Ivoire",ADM0_NAME)))

### Countries from aid data
country_aid<-aid_ODA %>% 
  pull(country) %>% 
  unique()

### Convert the data frame to sf type
aid_ODA_sf<-st_as_sf(aid_ODA,
                     coords=c("longitude","latitude"),# longitude and latitude
                     crs=4326 # specify the coordination reference system
                     )

# Data of Afrobarometer ---------------------------------------------------

### Read the geocoded Afrobarometer data (round 1 to 7) into R
load("afrob_full.RData")

### Combine the respondent ID with round number
afrob_full<-afrob_full %>% 
  mutate(respno_r=paste(respno,round,sep="_"))

### Year of interview: convert the string to numeric
afrob_full<-afrob_full %>% 
  mutate(yearintr=as.numeric(yearintr))

### Country names in Afrobarometer
afrob_full<-afrob_full %>% 
  mutate(country=ifelse(country %in% 
                          c("Cote d'Ivoire","Côte d'Ivoire","Cote d’Ivoire"),
                        "Cote d'Ivoire",
                        ifelse(country=="eSwatini","Swaziland",
                               ifelse(country=="Cabo Verde",
                                      "Cape Verde",country))))

### Collapse the data into survey_cluster (a smaller data to work with)
afrob_cluster<-afrob_full %>% 
  subset(select=c(country,xlat,ylong,yearintr,round)) %>% 
  unique() %>% 
  filter(!is.na(xlat)) %>% # remove the missing coordinates (only from round 7)
  mutate(survey_cluster=paste("(",xlat,",",ylong,")",sep=""))

### How many survey cluster are repeatedly visited
afrob_cluster<-afrob_cluster %>% 
  arrange(survey_cluster,yearintr) %>% 
  group_by(survey_cluster) %>% 
  mutate(times=length(survey_cluster),
         repeated=1:unique(times))

### Countries in the Afrobarometer
country_afb<-afrob_cluster %>% 
  pull(country) %>% 
  unique()

### Convert the data frame to a sf object
afrob_cluster_sf<-st_as_sf(afrob_cluster,
                          coords=c("ylong","xlat"),# longitudes and latitudes
                          crs=4326 # specify the coordination reference system
                          )


# Plot the map (Figure A.1) -----------------------------------------------
windows(width=600,height = 500)
par(mar=c(1,1,1,1)) # set the graphic margin

### plot the African map
world_africa<-world[world$continent=="Africa",]
plot(world_africa[0],lwd=2,reset=F)

### plot the surveyed Afrobarometer areas on the map
plot(afrob_cluster_sf[0],add=T,pch=16,cex=0.5,col="grey80",alpha=0.5)

### plot the aid projects on the map
# plot(aid_ODA_sf[0],add=T,pch=1,cex=1.2,col="black",lwd=2)

### plot the FDI Projects on the map
plot(fdi_precise_sf[0],add=T,pch=2,cex=1.2,col="black",lwd=2)

### add legend
legend(-25,-5,legend=c("Aid projects","FDI projects","Surveyed areas"),
       col=c("black","black","grey80"),
       pch=c(1,2,16))

