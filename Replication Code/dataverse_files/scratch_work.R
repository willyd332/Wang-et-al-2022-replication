## Getting Descriptive Statistics

library(sf)          # classes and functions for vector data
library(spData)        # load geographic data
library(lwgeom)        # for calculating distance
library(tidyverse)
library(xtable)

# Data of FDI projects ----------------------------------------------------


fdi_all <-read_csv("FDIProjGeocode.csv")

all_countries <- c("Botswana", "Ghana", "Kenya", "Madagascar", "Mozambique", 
                   "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda", 
                   "Zimbabwe", "Algeria", "Cameroon", "Cote d'Ivoire", "Egypt", 
                   "Malawi", "Mauritius", "Morocco", "Namibia", "Tunisia", "Zambia")

all_used_countries <- c("Algeria", "Botswana", "Cameroon", "Cote d'Ivoire", "Egypt", 
                        "Ghana", "Kenya", "Madagascar", "Mauritius", "Morocco",
                        "Mozambique", "Nigeria", "Senegal", "South Africa", "Tanzania", 
                        "Tunisia", "Uganda", "Zambia", "Zimbabwe")

unuseable_countries <- c("Algeria", "Botswana", "Mozambique","Tanzania", "Zimbabwe", "Cameroon", 
                         "Malawi", "Mauritius", "Namibia", "Tunisia", "Cote d'Ivoire")

useable_countries <- c("Ghana", "Kenya", "Madagascar", "Nigeria", "Senegal", 
                       "South Africa", "Uganda", "Egypt", "Morocco", "Zambia")

aid_countries <- c("Botswana", "Ghana", "Kenya", "Madagascar", "Mozambique", 
                   "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda", "Zimbabwe")

nonaid_countries <- c("Algeria", "Cameroon", "Cote d'Ivoire", "Egypt", "Malawi", 
                      "Mauritius", "Morocco", "Namibia", "Tunisia", "Zambia")

unuseable_fdi <- subset(fdi_all, Destination_country %in% unuseable_countries)
useable_fdi <- subset(fdi_all, Destination_country %in% useable_countries)

unuseable_table <- table(unuseable_fdi$Destination_country)
useable_table <- table(useable_fdi$Destination_country)


aid_fdi <- subset(fdi_all, Destination_country %in% aid_countries)
nonaid_fdi <- subset(fdi_all, Destination_country %in% nonaid_countries)

aid_table <- table(aid_fdi$Destination_country)
nonaid_table <- table(nonaid_fdi$Destination_country)

all_table <- table(fdi_all$Destination_country)


print(xtable(all_table), file="fdi_all_countries.tex")
print(xtable(aid_table), file="fdi_aid_countries.tex")
print(xtable(nonaid_table), file="fdi_nonaid_countries.tex")


print(xtable(useable_table), file="fdi_useable_countries.tex")
print(xtable(unuseable_table), file="fdi_unuseable_countries.tex")



load("data_respno_FDI.RData")

data_close_fdi<-list()
for (j in 1:length(data_respno_FDI)){
  
  data_close_fdi[[j]]<-data_respno_FDI[[j]] %>% filter(D_close_fdi==1)
  
  # drop the countries that have no variation on announced, active, and eventual
  tab<-data_close_fdi[[j]] %>% 
    subset(select=c(country,resp_status)) %>% 
    table()
  drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
  country_drop<-rownames(tab)[drop]
  
  # filter the dataset: drop the countries that have no variation
  data_close_fdi[[j]]<-data_close_fdi[[j]] %>% 
    filter(!country %in% country_drop)
  
}


table(data_close_fdi[[1]]$country)

#### Replicate the map

fdi <- useable_fdi

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
legend(-25,-5,legend=c("Surveyed areas","FDI projects"),
       col=c("black","grey80"),
       pch=c(1,2,16))





########## Get Tables

### Replication file for
### FDI, Unmet Expectations, and the Prospects of Political Leaders

### Part 2: Tables and figures in the manuscript
rm(list=ls())

### Use "Alt+O" to collapse all sections and "Shift+Alt+O" to expand all

### Set your own working directory

# Load required packages --------------------------------------------------
library(tidyverse)
library(miceadds)
library(car)
library(Hmisc)
library(xtable)
library(logr) # optional, only to print the log files

### Suppress scientific numeric
options(scipen=999)

# Plot themes --------------------------------------------------------------

### theme 1
my_theme_1<-theme_bw()+
  theme(legend.position = "bottom",legend.text = element_text(size=18),
        panel.grid = element_blank(),legend.key.height = unit(1,"cm"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 5, l = 0),
                                   size=14,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.caption = element_text(size=18),
        strip.background =element_rect(color="gray60"),
        strip.text = element_text(colour = 'black',size=20),
        panel.border = element_rect(colour = "black"))

### theme 2
my_theme_2<-theme_bw()+
  theme(panel.grid = element_blank(),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.position = "bottom",legend.title = element_blank(),
        legend.text = element_text(size=12),legend.key.height = unit(1,"cm"),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 5, l = 5),
                                   size=12,color="black"),
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 5, l = 0),
                                   size=12,color="black",vjust = -0.5),
        axis.ticks = element_line(color="black",size=1),
        axis.ticks.length = unit(0.1, "cm"),
        axis.title.x = element_text(size=12,
                                    margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.title.y = element_text(size=12,
                                    margin = margin(t = 0, r = 0, b = 5, l = 0)),
        strip.text = element_text(colour = 'black',size=11))

# Load data_respno_FDI and data_respno_aid --------------------------------
load("data_respno_FDI.RData")
load("data_respno_aid.RData")


all_countries <- c("Botswana", "Ghana", "Kenya", "Madagascar", "Mozambique", 
                   "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda", 
                   "Zimbabwe", "Algeria", "Cameroon", "Cote d'Ivoire", "Egypt", 
                   "Malawi", "Mauritius", "Morocco", "Namibia", "Tunisia", "Zambia")

aid_countries <- c("Botswana", "Ghana", "Kenya", "Madagascar", "Mozambique", 
                   "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda", "Zimbabwe")

nonaid_countries <- c("Algeria", "Cameroon", "Cote d'Ivoire", "Egypt", "Malawi", 
                      "Mauritius", "Morocco", "Namibia", "Tunisia", "Zambia")


data_respno_FDI[[1]] <- subset(data_respno_FDI[[1]], country %in% nonaid_countries)
data_respno_FDI[[2]] <- subset(data_respno_FDI[[2]], country %in% nonaid_countries)

# FDI Dataset: drop those not close to any projects -----------------------
data_close_fdi<-list()
for (j in 1:length(data_respno_FDI)){
  
  data_close_fdi[[j]]<-data_respno_FDI[[j]] %>% filter(D_close_fdi==1)
  
  # drop the countries that have no variation on announced, active, and eventual
  tab<-data_close_fdi[[j]] %>% 
    subset(select=c(country,resp_status)) %>% 
    table()
  drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
  country_drop<-rownames(tab)[drop]
  
  # filter the dataset: drop the countries that have no variation
  data_close_fdi[[j]]<-data_close_fdi[[j]] %>% 
    filter(!country %in% country_drop)
  
}

# Aid dataset: drop those not close to any projects -----------------------
data_close_aid<-list()
for (j in 1:length(data_respno_aid)){
  
  data_close_aid[[j]]<-data_respno_aid[[j]] %>% filter(D_close_aid==1)
  
  # drop the countries that have no variation on announced, active, and eventual
  tab<-data_close_aid[[j]] %>% 
    subset(select=c(country,resp_status)) %>% 
    table()
  drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
  country_drop<-rownames(tab)[drop]
  
  data_close_aid[[j]]<-data_close_aid[[j]] %>% 
    filter(!country %in% country_drop)
  
}



# FDI and aid dataset: countries with both FDI and aid --------------------
data_close_fdi_sub<-data_close_aid_sub<-list()
for (j in 1:length(data_close_fdi)){
  
  # countries in both FDI and aid dataset
  country_both<-intersect(data_close_aid[[j]]$country,
                          data_close_fdi[[j]]$country)
  # subset the fdi data
  data_close_fdi_sub[[j]]<-data_close_fdi[[j]] %>% 
    filter(country %in% country_both)
  # subset the aid data
  data_close_aid_sub[[j]]<-data_close_aid[[j]] %>% 
    filter(country %in% country_both)
  
}




# Create announced and active within 1, 2, and 3 years --------------------

### Create variables of announced and active by year for FDI full data
data_close_fdi_time<-data_close_fdi[[1]] %>%
  filter(!(D_close_ancmt_fdi==1 & Duration_ancmt_min>5),
         !(D_close_active_fdi==1 & Duration_active_min>5)) %>% 
  mutate(Ancmt_1=ifelse((D_close_ancmt_fdi==1 & Duration_ancmt_min==1),1,0),
         Ancmt_2=ifelse((D_close_ancmt_fdi==1 & Duration_ancmt_min==2),1,0),
         Ancmt_3=ifelse((D_close_ancmt_fdi==1 & Duration_ancmt_min>=3),1,0),
         Active_1=ifelse((D_close_active_fdi==1 & Duration_active_min==1),1,0),
         Active_2=ifelse((D_close_active_fdi==1 & Duration_active_min==2),1,0),
         Active_3=ifelse((D_close_active_fdi==1 & Duration_active_min>=3),1,0),
         Eventual=ifelse(D_close_eventual_fdi==1,1,0)
  )

### Create variables of announced and active by year for FDI subcountry data
data_close_fdi_sub_time<-data_close_fdi_sub[[1]] %>% 
  filter(!(D_close_ancmt_fdi==1 & Duration_ancmt_min>5),
         !(D_close_active_fdi==1 & Duration_active_min>5)) %>% 
  mutate(Ancmt_1=ifelse((D_close_ancmt_fdi==1 & Duration_ancmt_min==1),1,0),
         Ancmt_2=ifelse((D_close_ancmt_fdi==1 & Duration_ancmt_min==2),1,0),
         Ancmt_3=ifelse((D_close_ancmt_fdi==1 & Duration_ancmt_min>=3),1,0),
         Active_1=ifelse((D_close_active_fdi==1 & Duration_active_min==1),1,0),
         Active_2=ifelse((D_close_active_fdi==1 & Duration_active_min==2),1,0),
         Active_3=ifelse((D_close_active_fdi==1 & Duration_active_min>=3),1,0),
         Eventual=ifelse(D_close_eventual_fdi==1,1,0)
  )

### Create variables of announced and active by year for aid subcountry data
data_close_aid_sub_time<-data_close_aid_sub[[1]] %>% 
  filter(!(D_close_ancmt_aid==1 & Duration_ancmt_min>5),
         !(D_close_active_aid==1 & Duration_active_min>5)) %>% 
  mutate(Ancmt_1=ifelse((D_close_ancmt_aid==1 & Duration_ancmt_min==1),1,0),
         Ancmt_2=ifelse((D_close_ancmt_aid==1 & Duration_ancmt_min==2),1,0),
         Ancmt_3=ifelse((D_close_ancmt_aid==1 & Duration_ancmt_min>=3),1,0),
         Active_1=ifelse((D_close_active_aid==1 & Duration_active_min==1),1,0),
         Active_2=ifelse((D_close_active_aid==1 & Duration_active_min==2),1,0),
         Active_3=ifelse((D_close_active_aid==1 & Duration_active_min>=3),1,0),
         Eventual=ifelse(D_close_eventual_aid==1,1,0)
  )


# Function: model with country and survey FE ------------------------------
func_model_FE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and status to projects
                d[,dv]~D_close_active_fdi+D_close_ancmt_fdi
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # country fixed effects
                +as.factor(country)
                # survey fixed effects
                +as.factor(round)
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:3])
  
  # t values
  tvalue<-unname(summary(m)[2:3,3])
  
  # active-announced
  df_unmet<-coefs[1]-coefs[2]
  test_unmet<-linearHypothesis(m,"D_close_active_fdi=D_close_ancmt_fdi",
                               singular.ok=T)
  F_unmet<-test_unmet$Chisq[2]
  p_unmet<-test_unmet$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% 
    subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                    "D_close_eventual_fdi","Urban","age","gender","Edu")) %>% 
    na.omit() %>% 
    nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,p_unmet,
         Mean_dv,NA,NA,NA,N_obs,N_ctry,N_twnvill,NA,
         summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}


# Function: create main table ---------------------------------------------
table_main<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(12:14),]<-format(round(out_table[-c(12:14),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[9:11,]<-"Yes"
  out_table[c(2,4),]<-paste("(",out_table[c(2,4),],")",sep="")
  out_table[15,]<-"1-7"
  names_iv<-c("Announced","","Active","","Active-Announced",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Country fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of townships and villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

# Table 1: Chinese FDI and perceptions of economic conditions -------------

### Dependent variables: economic perceptions
dv_econ<-c("PSP_econ_D","PSP_econ_O","PSP_econin1yr_D","PSP_econin1yr_O")

### Estimate effects and output tables
out_econ<-list()
for (k in 1:length(dv_econ)){
  out_econ[[k]]<-func_model_FE(data_close_fdi[[1]],dv_econ[k])
}

### Print Table 1
Table_1<-table_main(out_econ)
colnames(Table_1)<-c("","(1)","(2)","(3)","(4)")
Table_1

# ### Print log file for Table 1
# log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table 1.log")
# log_print(Table_1)
# log_close()

print(xtable(Table_1),file=paste("./NonAid_Countries_Table_1.tex"))

# Table 2: Chinese FDI and perceptions of political competence ------------

### Dependent variables: economic perceptions
dv_poli<-c("SG_econ_D","SG_econ_O","SG_job_D","SG_job_O",
           "SG_prfpres_D","SG_prfpres_O")

### Estimate effects and output tables
out_poli<-list()
for (k in 1:length(dv_poli)){
  out_poli[[k]]<-func_model_FE(data_close_fdi[[1]],dv_poli[k])
}

### Print Table 2
Table_2<-table_main(out_poli)
colnames(Table_2)<-c("","(1)","(2)","(3)","(4)","(5)","(6)")
Table_2

# ### Print log file for Table 2
# log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table 2.log")
# log_print(Table_2)
# log_close()


print(xtable(Table_2),file=paste("./NonAid_Countries_Table_2.tex"))
