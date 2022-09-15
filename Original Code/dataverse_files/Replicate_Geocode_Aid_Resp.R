### Replication file for
### FDI, Unmet Expectations, and the Prospects of Political Leaders

### Part 1: Connect the geocoded Chinese aid data with Afrobarometer
### unit of analysis: respondent
rm(list=ls())

### Use "Alt+O" to collapse all sections and "Shift+Alt+O" to expand all

### Set your own working directory

# Load required packages --------------------------------------------------
library(sf)          # classes and functions for vector data
library(spData)        # load geographic data
library(lwgeom)        # for calculating distance
library(tidyverse)

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


# Data of president in African countries ----------------------------------

### Read the list of presidents into R
africa_president<-read_csv("List of Presidents.csv")

### Number (ID) of presidents by country
africa_president<-africa_president %>% 
  group_by(country) %>% 
  mutate(ID=1:length(country))

### Extract the month
africa_president<-africa_president %>% 
  mutate(Start_month=gsub("[^a-zA-Z]", "", Start_date),
         End_month=gsub("[^a-zA-Z]", "", End_date),
         Start_month=match(Start_month,month.abb),
         End_month=match(End_month,month.abb))

### Length of tenure
africa_president<-africa_president %>% 
  mutate(Start_Year_1=Start_Year+1,Length=End_Year-Start_Year_1+1)

### Expand the data points for each president by the length of tenure
africa_president<-africa_president %>% 
  filter(Length>0) %>% # drop two presidents that serve less than one year
  group_by(country,ID) %>% 
  uncount(Length)

### Create a variable of Year by country
africa_president<-africa_president %>% 
  group_by(country,ID) %>% 
  mutate(Tenure=0:(length(President)-1),
         Year=Start_Year_1+Tenure)

### Make sure there is no duplicated year within a country
africa_president %>% 
  group_by(country,Year) %>% 
  filter(n()>1) %>% 
  nrow()

# Distance matrix: aid and Afrobarometer respondents ----------------------

### Countries in both aid and Afb
country_afb_aid<-intersect(country_afb,country_aid)

### Distance buffers
dis_buffer<-c(50,200)

### Create an empty list to store the wide form distance matrix
distance_matrix_aid<-list()

### Create an empty list to store the long form merged data with distance
distance_long_aid<-list()

### A loop over country to create distance matrix
for (i in 1:length(country_afb_aid)){
  
  # Information of FDI projects in country i
  aid_country<-aid_ODA %>% 
    filter(country==country_afb_aid[i]) %>% 
    subset(select=c(AIDProj,year_ancmt,year_start)) 
  
  # Pull the project id of country i
  proj_id<-aid_country %>% pull(AIDProj) %>% unique()
  
  # Survey cluster in country i
  cluster<-afrob_cluster %>% 
    filter(country==country_afb_aid[i]) %>% 
    subset(select=c(country,survey_cluster,round,yearintr,times,repeated))
  
  # Calculate the distance (unit km), this returns a "unit" type object
  distance_matrix_aid[[i]]<-
    st_distance(afrob_cluster_sf %>% 
                  filter(country %in% country_afb_aid[i]),
                aid_ODA_sf %>% 
                  filter(country %in% country_afb_aid[i]))/1000
  
  # Convert the "unit" type object to a data frame
  distance_matrix_aid[[i]]<-as.data.frame(distance_matrix_aid[[i]])
  
  # Convert each column of the data frame from "unit" to "numeric"
  distance_matrix_aid[[i]]<-apply(distance_matrix_aid[[i]],2,as.numeric)
  
  # Change the column names to the id of projects
  colnames(distance_matrix_aid[[i]])<-proj_id
  
  # Add the respondent id (for the following data merge)
  distance_matrix_aid[[i]]<-cbind.data.frame(cluster,distance_matrix_aid[[i]],
                                             stringsAsFactors=F)
  
  # Convert the wide form to long form
  distance_long_aid[[i]]<-distance_matrix_aid[[i]] %>% 
    pivot_longer(.,!c("country","survey_cluster","round",
                   "yearintr","times","repeated"),
                 values_to = "distance",names_to = "AIDProj")
  
  # Merge with aid projects information
  distance_long_aid[[i]]<-merge(distance_long_aid[[i]],aid_country,
                                by=c("AIDProj"),all.x=T)
 
  flush.console()
  print(i)
}

# Code active, announced, eventual, and not close for aid projects --------

### A loop over distance buffers
cluster_project_aid<-rep(list(rep(list(0),length(country_afb_aid))),
                         length(dis_buffer))  
for (k in 1:length(dis_buffer)){
  
  for (i in 1:length(country_afb_aid)){
    
    cluster_project_aid[[k]][[i]]<-distance_long_aid[[i]] %>% 
      mutate(D_status=ifelse(distance>dis_buffer[k],"Not close",
                             ifelse(yearintr>year_start,"Active",
                                    ifelse((yearintr<=year_start & 
                                              yearintr>year_ancmt),
                                           "Announcement","Eventual"))))
    
  }
  
  flush.console()
  print(k)
}

### Combine the data over country for each distance cut-off
for (k in 1:length(dis_buffer)){
  cluster_project_aid[[k]]<-bind_rows(cluster_project_aid[[k]])
}

# Merge the data with the presidential terms for aid projects -------------

### Merge with presidents in the same year of interview
africa_president_intr<-africa_president %>% 
  mutate(president_intr=President,yearintr=Year) %>% 
  subset(select=c(country,president_intr,yearintr))
for (j in 1:length(cluster_project_aid)){
  cluster_project_aid[[j]]<-merge(cluster_project_aid[[j]],
                                  africa_president_intr,
                                  by=c("country","yearintr"),all.x=T)
}

### Merge with presidents in the same year of announcement
africa_president_ancmt<-africa_president %>% 
  mutate(president_ancmt=President,year_ancmt=Year) %>% 
  subset(select=c(country,president_ancmt,year_ancmt))
for (j in 1:length(cluster_project_aid)){
  cluster_project_aid[[j]]<-merge(cluster_project_aid[[j]],
                                  africa_president_ancmt,
                                  by=c("country","year_ancmt"),all.x=T)
}

### Merge with presidents: presidents in the same year of active
africa_president_active<-africa_president %>% 
  mutate(president_active=President,year_start=Year) %>% 
  subset(select=c(country,president_active,year_start))
for (j in 1:length(cluster_project_aid)){
  cluster_project_aid[[j]]<-merge(cluster_project_aid[[j]],
                                  africa_president_active,
                                  by=c("country","year_start"),all.x=T)
}

# Collapse the data by survey cluster (aid projects) ---------------------

### A loop over distance buffers (it takes some time)
data_cluster_aid<-list() 
for (k in 1:length(dis_buffer)){
  
  data_cluster_aid[[k]]<-cluster_project_aid[[k]] %>%
    # sort by cluster and year of interview
    arrange(survey_cluster,yearintr) %>%
    # group by cluster and year of interview
    group_by(survey_cluster,yearintr) %>% 
    
          # whether the cluster is close to this project
    mutate(D_close=ifelse(D_status!="Not close",1,0),
           # number of projects (active and eventual) the cluster close to
           Num_proj=sum(D_close),
           # number of active projects the cluster close to
           Num_proj_active=sum(D_status=="Active"),
           # number of announcement projects the cluster close to
           Num_proj_ancmt=sum(D_status=="Announcement"),
           # number of eventual projects the cluster close to
           Num_proj_eventual=sum(D_status=="Eventual"),
           
           # distance to the closest active project
           Distance_active_min=min(distance[D_status=="Active"]),
           # distance to the closest announced project
           Distance_ancmt_min=min(distance[D_status=="Announcement"]),
           # distance to the closest eventual project
           Distance_eventual_min=min(distance[D_status=="Eventual"]),
           # distance of not close
           Distance_no_min=min(distance[D_status=="Not close"]),
           
           # maximum duration of the active project
           Duration_active_max=
             max((yearintr-year_start)[D_status=="Active"]),
           # minimum duration of the active project
           Duration_active_min=
             min((yearintr-year_start)[D_status=="Active"]),
           
           # maximum duration of the announced project
           Duration_ancmt_max=
             max((yearintr-year_ancmt)[D_status=="Announcement"]),
           # minimum duration of the announced project
           Duration_ancmt_min=
             min((yearintr-year_ancmt)[D_status=="Announcement"]),
           
           # minimum time to be announced
           Time_eventual_min=min((year_ancmt-yearintr)[D_status=="Eventual"]),
           
           # dummy: close to at least one project (active or inactive)
           D_close_aid=ifelse(Num_proj>0,1,0),
           # dummy: close to at least one active project
           D_close_active_aid=ifelse(Num_proj_active>0,1,0),
           # dummy: only close to announced projects but not active ones
           D_close_ancmt_aid=ifelse((Num_proj_ancmt>0&Num_proj_active==0),1,0),
           # dummy: only close to eventual projects but not active or announced
           D_close_eventual_aid=ifelse((Num_proj_eventual>0 & 
                                          Num_proj_active==0 &
                                          Num_proj_ancmt==0),1,0),
           
           # same president or not relative to interview time
           president_same_ancmt=ifelse((president_intr==president_ancmt & 
                                          D_close_ancmt_aid==1),1,0),
           president_same_active=ifelse((D_close_active_aid==1 &
                                           president_intr==president_active),
                                        1,0),
           president_same_ancmt_active=
             ifelse((D_close_active_aid==1 & 
                       president_intr==president_active & 
                       president_intr==president_ancmt),1,0),
           
           D_president_same_ancmt=max(president_same_ancmt,na.rm=T),
           D_president_same_active=max(president_same_active,na.rm=T),
           D_president_same_ancmt_active=max(president_same_ancmt_active,
                                             na.rm=T)
           ) %>% 
    
    # subset the dataset
    subset(select=c(country,survey_cluster,round,yearintr,times,repeated,
                    Num_proj,Num_proj_active,Num_proj_ancmt,Num_proj_eventual,
                    D_close_aid,D_close_active_aid,D_close_ancmt_aid,
                    D_close_eventual_aid,
                    Distance_active_min,Distance_ancmt_min,
                    Distance_eventual_min,
                    Distance_no_min,Duration_active_max,Duration_active_min,
                    Duration_ancmt_max,Duration_ancmt_min,Time_eventual_min,
                    president_intr,
                    D_president_same_ancmt,D_president_same_active,
                    D_president_same_ancmt_active)) %>% 
    unique() %>% 
    as.data.frame()
  
  flush.console()
  print(k)
}

# Merge with Afrobarometer (unit of analysis is respondent) --------
afrob_full<-afrob_full %>% 
  filter(!is.na(xlat)) %>% 
  mutate(survey_cluster=paste("(",xlat,",",ylong,")",sep=""))
data_respno_aid<-list()
for (i in 1:length(dis_buffer)){
  data_respno_aid[[i]]<-merge(data_cluster_aid[[i]],afrob_full,
                              by=c("survey_cluster","yearintr",
                                   "round","country"),all.x=T)
}



# Recode variables --------------------------------------------------------

### Status to FDI projects
for (j in 1:length(data_respno_aid)){
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(resp_status=ifelse(D_close_active_aid==1,"Active",
                              ifelse(D_close_ancmt_aid==1,"Announced",
                                     ifelse(D_close_eventual_aid==1,
                                            "Eventual","Not close")))
    )
}

### Survey round index
for (j in 1:length(data_respno_aid)){
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(round_index=recode(round,"r1"=1,"r2"=2,"r3"=3,"r4"=4,
                              "r5"=5,"r6"=6,"r7"=7))
}

### Township and villages
for (j in 1:length(data_respno_aid)){
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(twnvill=ifelse((is.na(twnvill)|twnvill==""),place_name,twnvill),
           # to lower case
           twnvill=tolower(twnvill),
           # only keep the Alphanumeric
           twnvill=gsub("[^A-Za-z0-9 ]","",twnvill),
           # delete the space
           twnvill=gsub("[[:space:]]", "", twnvill))
}

### Subnational regions
for (j in 1:length(data_respno_aid)){
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    # to lower case
    mutate(region_name=tolower(region_name),
           # only keep the Alphanumeric
           region_name=gsub("[^A-Za-z0-9 ]","",region_name),
           # delete the space
           region_name=gsub("[[:space:]]", "", region_name))
}

### Satisfaction with economic performance
for (j in 1:length(data_respno_aid)){
  
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(SG_econ_D=ifelse(SG_econ %in% c("Fairly Well","Very Well"),1,
                            ifelse(SG_econ %in%  c("Fairly Badly","Very Badly"),
                                   0,NA)),
           SG_econ_O=ifelse(SG_econ=="Very Badly",0,
                            ifelse(SG_econ=="Fairly Badly",1,
                                   ifelse(SG_econ=="Fairly Well",2,
                                          ifelse(SG_econ=="Very Well",3,NA)))))
}

### Satisfaction with how government handles jobs
for (j in 1:length(data_respno_aid)){
  
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(SG_job_D=ifelse(SG_job %in% c("Fairly well","Fairly Well",
                                         "Very Well","Very well"),1,
                           ifelse(SG_job %in%  c("Fairly Badly","Very Badly",
                                                 "Quite badly/Not very well",
                                                 "Very badly/Not at all well"),
                                  0,NA)),
           SG_job_O=ifelse(SG_job %in% c("Very Badly",
                                         "Very badly/Not at all well"),0,
                           ifelse(SG_job %in% c("Fairly Badly",
                                                "Quite badly/Not very well"),1,
                                  ifelse(SG_job %in% c("Fairly Well",
                                                       "Fairly well"),2,
                                         ifelse(SG_job %in% c("Very Well",
                                                              "Very well"),3,
                                                NA)))))
}

### Satisfaction with president performance
for (j in 1:length(data_respno_aid)){
  
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(SG_prfpres_D=ifelse(SG_prfpres %in% 
                                 c("Very satisfied/Strongly approve",
                                   "Strongly Approve",
                                   "Somewhat satisfied/Approve","Approve"),1,
                               ifelse(SG_prfpres %in% 
                                        c("Disapprove","Strongly Disapprove",
                                          "Somewhat unsatisfied/Disapprove",
                                          "Very unsatisfied/Strongly disapprove"),
                                      0,NA)),
           SG_prfpres_O=ifelse(SG_prfpres %in% 
                                 c("Very unsatisfied/Strongly disapprove",
                                   "Strongly Disapprove"),0,
                               ifelse(SG_prfpres %in% 
                                        c("Somewhat unsatisfied/Disapprove",
                                          "Disapprove"),1,
                                      ifelse(SG_prfpres %in% 
                                               c("Approve",
                                                 "Somewhat satisfied/Approve"),
                                             2,ifelse(SG_prfpres %in% 
                                                        c("Strongly Approve",
                                                          "Very satisfied/Strongly approve"),
                                                      3,NA)))))
  
}

### Perceptions of present economic situation
for (j in 1:length(data_respno_aid)){
  
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(PSP_econ_D=ifelse(PSP_econ %in% c("Fairly good","Fairly Good",
                                             "Very good"),1,
                             ifelse(PSP_econ %in% c("Fairly bad","Fairly Bad",
                                                    "Very bad","Very Bad",
                                                    "Neither good nor bad"),0,
                                    NA)),
           PSP_econ_O=ifelse(PSP_econ %in% c("Very bad","Very Bad"),0,
                             ifelse(PSP_econ %in% c("Fairly bad","Fairly Bad"),1,
                                    ifelse(PSP_econ %in% c("Neither good nor bad"),2,
                                           ifelse(PSP_econ %in% 
                                                    c("Fairly good",
                                                      "Fairly Good"),3,
                                                  ifelse(PSP_econ %in% 
                                                           c("Very good"),4,
                                                         NA))))))
  
}

### Perceptions of economic situations in 1 year
for (j in 1:length(data_respno_aid)){
  
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(PSP_econin1yr_D=
             ifelse(PSP_econin1yr %in% c("Much better","Much Better","Better",
                                         "Much more satified/Much better",
                                         "slightly more satisfied/Better"),1,
                    ifelse(PSP_econin1yr %in% c("about the same","Same",
                                                "Much worse","Much Worse",
                                                "Worse",
                                                "slightly less satisfied/Worse",
                                                "much less satisfied/much worse"),
                           0,NA)),
           PSP_econin1yr_O=
             ifelse(PSP_econin1yr %in% c("Much worse","Much Worse",
                                         "much less satisfied/much worse"),0,
                    ifelse(PSP_econin1yr %in% 
                             c("Worse",
                               "slightly less satisfied/Worse"),1,
                           ifelse(PSP_econin1yr %in% 
                                    c("about the same",
                                      "Same"),2,
                                  ifelse(PSP_econin1yr %in% 
                                           c("slightly more satisfied/Better",
                                             "Better"),3,
                                         ifelse(PSP_econin1yr %in% 
                                                  c("Much better",
                                                    "Much Better",
                                                    "Much more satified/Much better"),
                                                4,NA))))))
  
}

### Individual control variables
edu_levels<-c("No formal school","Primary school","Secondary school",
              "Post-secondary school","Post-graduate")

for (j in 1:length(data_respno_aid)){
  
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    mutate(Urban=ifelse(DM_urban %in% c("Semi-Urban","Peri-Urban"),"Semi-Urban",
                        ifelse(DM_urban=="Rural","Rural",
                               ifelse(DM_urban=="Urban","Urban",NA))),
           age=as.numeric(DM_age),
           age=ifelse(is.na(age),age,
                      ifelse(age<18,18,
                             ifelse(age>110,110,age))),
           gender=ifelse(DM_fem %in% c("Female","Male"),DM_fem,NA),
           Edu=ifelse(DM_edu %in% 
                        c("No formal schooling","Informal schooling only"),
                      "No formal school",
                      ifelse(DM_edu %in% c("Some primary schooling",
                                           "Primary school completed",
                                           "Primary only"),"Primary school",
                             ifelse(DM_edu %in% 
                                      c("Secondary school completed/high school",
                                        "Secondary school / high school completed",
                                        "Some secondary school/high school",
                                        "Some secondary school / high school",
                                        "Secondary"),"Secondary school",
                                    ifelse(DM_edu %in% 
                                             c("University completed",
                                               "Post-secondary qualifications, not university",
                                               "Some university",
                                               "Some university, college",
                                               "University, college completed",
                                               "Post-secondary",
                                               "Post-secondary qualifications, other than university"),
                                           "Post-secondary school",
                                           ifelse(DM_edu %in% c("Post-graduate"),
                                                  "Post-graduate",
                                                  NA))))),
           Edu=factor(Edu,levels=edu_levels)
    )
  
}

### Subset the data with variables that will be used
for (j in 1:length(data_respno_aid)){
  
  data_respno_aid[[j]]<-data_respno_aid[[j]] %>% 
    subset(select=c(survey_cluster:D_president_same_ancmt_active,
                    respno_r:Edu,
                    region_name,
                    twnvill))
  
}

### Name the list
names(data_respno_aid)<-c("50km","200km")

# Save the data -----------------------------------------------------------
save(data_respno_aid,file="data_respno_aid.RData")

