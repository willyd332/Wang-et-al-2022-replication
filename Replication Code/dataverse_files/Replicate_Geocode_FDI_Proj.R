### Replication file for
### FDI, Unmet Expectations, and the Prospects of Political Leaders

### Part 1: Connect the geocoded Chinese FDI with Afrobarometer
### Unit of analysis: project
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
fdi_all <-read_csv("FDIProjGeocode.csv")

all_countries <- c("Botswana", "Ghana", "Kenya", "Madagascar", "Mozambique", "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda", "Zimbabwe", "Algeria", "Cameroon", "Cote d'Ivoire", "Egypt", "Malawi", "Mauritius", "Morocco", "Namibia", "Tunisia", "Zambia")

for (country in all_countries) {
  fdi <- subset(fdi_all, Destination_country == country)
  
  
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
  
  # Distance matrix: FDI and Afrobarometer respondents ----------------------
  
  ### Countries in both FDI and Afb
  country_afb_fdi<-intersect(country_afb,country_fdi)
  
  ### Distance buffers
  dis_buffer<-c(50,200)
  
  ### Create an empty list to store the wide form distance matrix
  distance_matrix_fdi<-list()
  
  ### Create an empty list to store the long form merged data with distance
  distance_long_fdi<-list()
  
  print(country)
  print("TESTING 1 TESTING 1")
  ### A loop over country to create distance matrix
  for (i in 1:length(country_afb_fdi)){
    
    # Information of FDI projects in country i
    fdi_country<-fdi_precise %>% 
      filter(Destination_country==country_afb_fdi[i]) %>% 
      subset(select=c(FDIProj,Sector_type,Ownership_final,Opyear_final,Ancmtyear)) 
    
    # Pull the project id of country i
    proj_id<-fdi_country %>% pull(FDIProj) %>% unique()
    
    # Survey cluster in country i
    cluster<-afrob_cluster %>% 
      filter(country==country_afb_fdi[i]) %>% 
      subset(select=c(country,survey_cluster,round,yearintr,times,repeated))
    
    # Calculate the distance (unit km), this returns a "unit" type object
    distance_matrix_fdi[[i]]<-
      st_distance(afrob_cluster_sf %>% 
                    filter(country %in% country_afb_fdi[i]),
                  fdi_precise_sf %>% 
                    filter(Destination_country %in% country_afb_fdi[i]))/1000
    
    # Convert the "unit" type object to a data frame
    distance_matrix_fdi[[i]]<-as.data.frame(distance_matrix_fdi[[i]])
    
    # Convert each column of the data frame from "unit" to "numeric"
    distance_matrix_fdi[[i]]<-apply(distance_matrix_fdi[[i]],2,as.numeric)
    
    # Change the column names to the id of projects
    colnames(distance_matrix_fdi[[i]])<-proj_id
    
    # Add the respondent id (for the following data merge)
    distance_matrix_fdi[[i]]<-cbind.data.frame(cluster,distance_matrix_fdi[[i]],
                                               stringsAsFactors=F)
    
    # Convert the wide form to long form
    distance_long_fdi[[i]]<-distance_matrix_fdi[[i]] %>% 
      pivot_longer(.,!c("country","survey_cluster","round",
                     "yearintr","times","repeated"),
                   values_to = "distance",names_to = "FDIProj")
    
    # Merge with FDI projects information
    distance_long_fdi[[i]]<-merge(distance_long_fdi[[i]],fdi_country,
                                  by=c("FDIProj"),all.x=T)
   
    flush.console()
    print(i)
  }
  
  print("TESTING 2 TESTING 2")
  
  # Code active, announced, eventual, and not close for FDI projects --------
  
  ### A loop over distance buffers
  cluster_project_fdi<-rep(list(rep(list(0),length(country_afb_fdi))),
                           length(dis_buffer))  
  for (k in 1:length(dis_buffer)){
    
    for (i in 1:length(country_afb_fdi)){
      
      cluster_project_fdi[[k]][[i]]<-distance_long_fdi[[i]] %>% 
        mutate(D_status=ifelse(distance>dis_buffer[k],"Not close",
                               ifelse(yearintr>Opyear_final,"Active",
                                      ifelse((yearintr<=Opyear_final & 
                                                yearintr>Ancmtyear),
                                             "Announcement","Eventual"))))
      
    }
    
    flush.console()
    print(k)
  }
  
  
  ### Combine the data over country for each distance cut-off
  for (k in 1:length(dis_buffer)){
    cluster_project_fdi[[k]]<-bind_rows(cluster_project_fdi[[k]])
  }
  
  # Merge the data with presidential terms for FDI projects -------------
  
  ### Merge with presidents in the same year of interview
  africa_president_intr<-africa_president %>% 
    mutate(president_intr=President,yearintr=Year) %>% 
    subset(select=c(country,president_intr,yearintr))
  for (j in 1:length(cluster_project_fdi)){
    cluster_project_fdi[[j]]<-merge(cluster_project_fdi[[j]],
                                    africa_president_intr,
                                    by=c("country","yearintr"),all.x=T)
  }
  
  ### Merge with presidents in the same year of announcement
  africa_president_ancmt<-africa_president %>% 
    mutate(president_ancmt=President,Ancmtyear=Year) %>% 
    subset(select=c(country,president_ancmt,Ancmtyear))
  for (j in 1:length(cluster_project_fdi)){
    cluster_project_fdi[[j]]<-merge(cluster_project_fdi[[j]],
                                    africa_president_ancmt,
                                    by=c("country","Ancmtyear"),all.x=T)
  }
  
  ### Merge with presidents: presidents in the same year of active
  africa_president_active<-africa_president %>% 
    mutate(president_active=President,Opyear_final=Year) %>% 
    subset(select=c(country,president_active,Opyear_final))
  for (j in 1:length(cluster_project_fdi)){
    cluster_project_fdi[[j]]<-merge(cluster_project_fdi[[j]],
                                    africa_president_active,
                                    by=c("country","Opyear_final"),all.x=T)
  }
  
  print("TESTING 3 TESTING 3")
  
  # Collapse the data by FDI project ----------------------------------------
  
  ### Only keep those close to FDI (drop not close)
  cluster_project_fdi_close<-list()
  for (j in 1:length(cluster_project_fdi)){
    cluster_project_fdi_close[[j]]<-cluster_project_fdi[[j]] %>% 
      filter(D_status!="Not close") 
  }
  
  ### Keep FDI projects that have variations on active, announced, and eventual
  project_FDI<-list()
  for (j in 1:length(cluster_project_fdi_close)){
    
   
    project_FDI[[j]]<-cluster_project_fdi_close[[j]] %>% 
      group_by(FDIProj) %>% 
      mutate(status=unique(D_status) %>% sort() %>% paste(.,collapse=","),
             select=ifelse((status==("Active,Eventual")|
                              status==("Announcement,Eventual")|
                              status==("Active,Announcement,Eventual")),1,0)) %>% 
      filter(select==1) %>% 
      mutate(Time_active=yearintr-Opyear_final,
             Time_ancmt=yearintr-Ancmtyear,
             Time_eventual=Ancmtyear-yearintr,
             D_president_same_ancmt=ifelse(president_intr==president_ancmt,1,0),
             D_president_same_active=ifelse(president_intr==president_active,1,0),
             D_president_same_ancmt_active=
               ifelse((president_intr==president_ancmt & 
                         president_intr==president_active),1,0)) %>% 
      as.data.frame()
    
  }
  
  
  print("TESTING 4 TESTING 4")
  
  # Merge with Afrobarometer (unit of analysis is project) ---------------
  afrob_full<-afrob_full %>% 
    filter(!is.na(xlat)) %>% 
    mutate(survey_cluster=paste("(",xlat,",",ylong,")",sep=""))
  
  data_project_FDI<-list()
  for (i in 1:length(dis_buffer)){
    data_project_FDI[[i]]<-merge(project_FDI[[i]],afrob_full,
                                 by=c("survey_cluster","yearintr",
                                      "round","country"),all.x=T)
  }
  
  
  print("TESTING 5 TESTING 5")
  
  # Drop duplicated respondents ---------------------------------------------
  
  ### For each respondent, active override announced, announce override eventual
  for (j in 1:length(data_project_FDI)){
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
      group_by(respno_r) %>% 
      mutate(resp_status=unique(D_status) %>% sort() %>% 
               paste(.,collapse=","),
             drop=ifelse((D_status=="Announcement" & 
                            (resp_status=="Active,Announcement,Eventual"|
                               resp_status=="Active,Announcement")),1,
                         ifelse((D_status=="Eventual" & 
                                   (resp_status=="Active,Announcement,Eventual"|
                                      resp_status=="Announcement,Eventual"|
                                      resp_status=="Active,Eventual")),1,0))
      ) %>% 
      filter(drop==0)
    
  }
  
  print("TESTING 6 TESTING 6")
  
  
  ### Keep the observation that is most recent in time and closet in distance
  for (j in 1:length(data_project_FDI)){
    
    # announced
    data_ancmt<-data_project_FDI[[j]] %>% 
      filter(D_status=="Announcement") %>% 
      group_by(respno_r) %>% 
      arrange(Time_ancmt,distance)
    
    # index of duplicated announced respondents
    dup_ancmt<-data_ancmt %>% pull(respno_r) %>% duplicated()
    
    # drop the duplicated ones of announced
    data_ancmt<-data_ancmt[!dup_ancmt,]
    
    # active
    data_active<-data_project_FDI[[j]] %>% 
      filter(D_status=="Active") %>% 
      group_by(respno_r) %>% 
      arrange(Time_active,distance)
    
    # index of duplicated active respondents
    dup_active<-data_active %>% pull(respno_r) %>% duplicated()
    
    # drop the duplicated annoucned ones
    data_active<-data_active[!dup_active,]
    
    # eventual
    data_eventual<-data_project_FDI[[j]] %>% 
      filter(D_status=="Eventual") %>% 
      group_by(respno_r) %>% 
      arrange(Time_eventual,distance)
    
    # index of duplicated active respondents
    dup_eventual<-data_eventual %>% pull(respno_r) %>% duplicated()
    
    # drop the duplicated annoucned ones
    data_eventual<-data_eventual[!dup_eventual,]
    
    # row bind announced, active, and eventual
    data_project_FDI[[j]]<-rbind(data_ancmt,data_active,data_eventual)
    
    # drop projects that have no variation on active, announcement, and eventual
    tab<-data_project_FDI[[j]] %>% subset(select=c(FDIProj,D_status)) %>% table()
    
    if (length(colnames(tab)) < 3){
      if ("Eventual" %in% colnames(tab)) { 
        drop <-(tab[,1]==0 | tab[,"Eventual"]==0)
      } else {
        drop<-TRUE
      }
    } else {
      drop<-((tab[,1]==0 & tab[,2]==0) | tab[,3]==0)
    }

    drop_FDI<-rownames(tab)[drop]
    
    print("TESTING 6.5 TESTING 6.5")
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
      filter(!FDIProj %in% drop_FDI) %>%
      as.data.frame()
    
  }


  
  # Recode variables --------------------------------------------------------
  
  ### Create respondents' status to Chinese FDI
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
      mutate(D_close_active_fdi=ifelse(D_status=="Active",1,0),
             D_close_ancmt_fdi=ifelse(D_status=="Announcement",1,0),
             D_close_eventual_fdi=ifelse(D_status=="Eventual",1,0))
    
  }
  
  ### Survey round index
  for (j in 1:length(data_project_FDI)){
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
      mutate(round_index=recode(round,"r1"=1,"r2"=2,"r3"=3,"r4"=4,
                                "r5"=5,"r6"=6,"r7"=7))
  }
  
  ### Township and villages
  for (j in 1:length(data_project_FDI)){
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
      mutate(twnvill=ifelse((is.na(twnvill)|twnvill==""),place_name,twnvill),
             # to lower case
             twnvill=tolower(twnvill),
             # only keep the Alphanumeric
             twnvill=gsub("[^A-Za-z0-9 ]","",twnvill),
             # delete the space
             twnvill=gsub("[[:space:]]", "", twnvill))
  }
  
  ### Satisfaction with economic performance
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
      mutate(SG_econ_D=ifelse(SG_econ %in% c("Fairly Well","Very Well"),1,
                              ifelse(SG_econ %in%  c("Fairly Badly","Very Badly"),
                                     0,NA)),
             SG_econ_O=ifelse(SG_econ=="Very Badly",0,
                              ifelse(SG_econ=="Fairly Badly",1,
                                     ifelse(SG_econ=="Fairly Well",2,
                                            ifelse(SG_econ=="Very Well",3,NA)))))
  }
  
  ### Satisfaction with how government handles jobs
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
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
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
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
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
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
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
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
  
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
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
  for (j in 1:length(data_project_FDI)){
    
    data_project_FDI[[j]]<-data_project_FDI[[j]] %>% 
      subset(select=c(survey_cluster:D_president_same_ancmt_active,
                      respno_r:Edu,
                      region_name,
                      twnvill))
    
  }
  
  print("TESTING 7 TESTING 7")
  
  ### Name the list
  names(data_project_FDI)<-c("50km","200km")
  
  # Save the data -----------------------------------------------------------
  save(data_project_FDI,file=paste(country,"_data_project_FDI.RData"))
 
}
