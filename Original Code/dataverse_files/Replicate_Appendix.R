### Replication file for
### FDI, Unmet Expectations, and the Prospects of Political Leaders

### Part 3: Tables and figures in the appendix
rm(list=ls())

### Use "Alt+O" to collapse all sections and "Shift+Alt+O" to expand all

### Set your own working directory

# Load required packages --------------------------------------------------
library(tidyverse)
library(miceadds)
library(car)
library(Hmisc)
library(cem)
library(xtable)
library(logr) # optional, only to print the log files

### Suppress scientific numeric
options(scipen=999)


# Load required dataset ---------------------------------------------------

### FDI projects
fdi<-read_csv("FDIProjGeocode.csv")

### Aid projects
aid<-read_csv("ChinaAid.csv")

### Merged FDI and Afrobarometer data (unit of analysis is respondent)
load("data_respno_FDI.RData")

### Merged FDI and Afrobarometer data (unit of analysis is project)
load("data_project_FDI.RData")

### Merged aid and Afrobarometer data (unit of analysis is respondent)
load("data_respno_aid.RData")

# Dataset: drop not close (FDI and country FE) ----------------------------

### Drop not close
data_close_fdi<-data_respno_FDI[[1]] %>% filter(D_close_fdi==1)

### Drop countries that have no variation on announced, active, and eventual
tab<-data_close_fdi %>% 
  subset(select=c(country,resp_status)) %>% 
  table()
drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
country_drop<-rownames(tab)[drop]

data_close_fdi<-data_close_fdi %>% filter(!country %in% country_drop)


# Dataset: countries with both active and announced (FDI) -----------------

### Drop countries with no observation on active, announced, or eventual
tab<-data_close_fdi %>% 
  subset(select=c(country,resp_status)) %>% table()
drop<-rowSums(tab==0)>0
country_drop<-rownames(tab)[drop]

data_close_fdi_subctry<-data_close_fdi %>% filter(!country %in% country_drop)

# Dataset: eventual within three or five years (FDI) ----------------------

### Set timeframes
time_eventual<-c(3,5)

### Drop eventual outside the timeframes
data_close_fdi_narrow<-list()

for (k in 1:length(time_eventual)){
  
  # drop eventual above three or five years
  data_close_fdi_narrow[[k]]<-data_close_fdi %>% 
    filter(!(D_close_eventual_fdi==1 & Time_eventual_min>time_eventual[k]))
  
  # drop countries that have no variation on announced, active, and eventual
  tab<-data_close_fdi_narrow[[k]] %>% 
    subset(select=c(country,resp_status)) %>% table()
  drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
  country_drop<-rownames(tab)[drop]
  
  data_close_fdi_narrow[[k]]<-data_close_fdi_narrow[[k]] %>% 
    filter(!country %in% country_drop)
  
}

# Dataset: subnational regions with variations (FDI) ----------------------

### Drop subnational regions with no variation on active, announced, and eventual
tab<-data_close_fdi %>% 
  subset(select=c(region_name,resp_status)) %>% table()
drop<-(tab[,1]==0 & tab[,2]==0)|tab[,3]==0
region_drop<-rownames(tab)[drop]

data_close_fdi_subnational<-data_close_fdi %>% 
  filter(!region_name %in% region_drop)

# Dataset: close to manufacturing or resources projects (FDI) -------------

### Close to manufacturing or resources projects
data_close_manuf_resource<-data_close_fdi %>% 
  filter((D_close_active_fdi==1 & Num_proj_manufacture_active>0)|
           (D_close_ancmt_fdi==1 & Num_proj_manufacture_ancmt>0)|
           (D_close_eventual_fdi==1 & Num_proj_manufacture_eventual>0)|
           (D_close_active_fdi==1 & Num_proj_resource_active>0)|
           (D_close_ancmt_fdi==1 & Num_proj_resource_ancmt>0)|
           (D_close_eventual_fdi==1 & Num_proj_resource_eventual>0)
  )

### Drop countries that have no variation on announced, active, and eventual
tab<-data_close_manuf_resource %>% 
  subset(select=c(country,resp_status)) %>% table()
drop<-(tab[,1]==0 & tab[,2]==0)|tab[,3]==0
country_drop<-rownames(tab)[drop]

data_close_manuf_resource<-data_close_manuf_resource %>% 
  filter(!country %in% country_drop)

# Dataset: close to service only (FDI) ------------------------------------

### Close to service projects only
respno_drop<-data_close_manuf_resource %>% pull(respno_r)
data_close_service<-data_close_fdi %>% filter(!respno_r %in% respno_drop)

### Drop countries that have no variation on announced, active, and eventual
tab<-data_close_service %>% 
  subset(select=c(country,resp_status)) %>% table()
drop<-(tab[,1]==0 & tab[,2]==0)|tab[,3]==0
country_drop<-rownames(tab)[drop]

data_close_service<-data_close_service %>% filter(!country %in% country_drop)



# Dataset: younger and older age groups -----------------------------------

### Cut the age into four groups by quantile
age_group<-data_close_fdi %>% 
  filter(!is.na(age)) %>% 
  pull(age) %>% 
  cut2(.,g=4) %>% 
  unique() %>% 
  sort()

### Create dataset with younger and older age groups
data_close_fdi_age<-list()
for (k in 1:2){
  
  data_close_fdi_age[[k]]<-data_close_fdi %>% 
    filter(!is.na(age)) %>% 
    mutate(age_cut=cut2(age,g=4)) %>% 
    filter(age_cut==age_group[k+2])
  
}

# Dataset: FDI and aid comparison -----------------------------------------

### Drop not close in the aid dataset
data_close_aid<-data_respno_aid[[1]] %>% filter(D_close_aid==1)

### Drop countries that have no variation on announced, active, and eventual
tab<-data_close_aid %>% 
  subset(select=c(country,resp_status)) %>% 
  table()
drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
country_drop<-rownames(tab)[drop]

data_close_aid<-data_close_aid %>% filter(!country %in% country_drop)

### Countries with both Chinese FDI and aid projects
country_afb_fdi<-data_close_fdi %>% pull(country) %>% unique()
country_afb_aid<-data_close_aid %>% pull(country) %>% unique()
country_both<-intersect(country_afb_fdi,country_afb_aid)

### Create FDI and aid dataset for comparison
data_close_fdi_compare<-data_close_fdi %>% filter(country %in% country_both)
data_close_aid_compare<-data_close_aid %>% filter(country %in% country_both)


# Dataset: FDI dataset for presidential FE --------------------------------

### Only keep those experiencing the same president
data_close_fdi_president<-data_close_fdi %>% 
  filter(!(D_close_ancmt_fdi==1 & D_president_same_ancmt==0),
         !(D_close_active_fdi==1 & D_president_same_active==0)) %>% 
  filter(D_close_fdi==1)

### Drop country-president with no variation on active, announced, and eventual
data_close_fdi_president<-data_close_fdi_president %>% 
  mutate(country_president=paste(country,":"," ",president_intr,sep=""))

tab<-data_close_fdi_president %>% 
  subset(select=c(country_president,resp_status)) %>% table()
drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
president_drop<-rownames(tab)[drop]

data_close_fdi_president<-data_close_fdi_president %>% 
  filter(!country_president %in% president_drop)

# Create active and announced within 1, 2, and 3 years --------------------

### A list of FDI dataset (unit of analysis is respondent)
data_fdi_time<-list(data_close_fdi,
                    data_close_fdi_subctry,
                    data_close_fdi_narrow[[1]],data_close_fdi_narrow[[2]],
                    data_close_fdi_subnational,
                    data_close_manuf_resource,data_close_service,
                    data_close_fdi_age[[1]],data_close_fdi_age[[2]],
                    data_close_fdi_president,
                    data_close_fdi_compare)
names(data_fdi_time)<-c("Main","Subctry","Narrow_3","Narrow_5",
                        "Subregion","ManufResource","Service",
                        "Age_1","Age_2","Presidential","Compare")

### Create Ancmt 1-3 and Acitve 1-3 for each dataset
for (j in 1:length(data_fdi_time)){
  
  data_fdi_time[[j]]<-data_fdi_time[[j]] %>% 
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
  
}

### Create Ancmt 1-3 and Active 1-3 for FDI data using project as unit
data_project_FDI_time<-data_project_FDI[[1]] %>% 
    filter(!(D_close_ancmt_fdi==1 & Time_ancmt>5),
           !(D_close_active_fdi==1 & Time_active>5)) %>% 
    mutate(Ancmt_1=ifelse((D_close_ancmt_fdi==1 & Time_ancmt==1),1,0),
           Ancmt_2=ifelse((D_close_ancmt_fdi==1 & Time_ancmt==2),1,0),
           Ancmt_3=ifelse((D_close_ancmt_fdi==1 & Time_ancmt>=3),1,0),
           Active_1=ifelse((D_close_active_fdi==1 & Time_active==1),1,0),
           Active_2=ifelse((D_close_active_fdi==1 & Time_active==2),1,0),
           Active_3=ifelse((D_close_active_fdi==1 & Time_active>=3),1,0),
           Eventual=ifelse(D_close_eventual_fdi==1,1,0)
    )

### Create Ancmt 1-3 and Active 1-3 for aid dataset
data_close_aid_compare_time<-data_close_aid_compare %>% 
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

# Model: country and survey FE (for FDI) ----------------------------------

### Country and survey fixed effects model
func_model_countryFE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
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
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% 
    nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,
         p_unmet,Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

### Effects by time: country and survey fixed effects model
func_model_time_countryFE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Ancmt_3+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # country fixed effects
                +as.factor(country)
                # survey fixed effectsd
                +as.factor(round)
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:7])
  
  # t values
  tvalue<-unname(summary(m)[2:7,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[4]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[5]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # active-announced: year>=3
  df_unmet_3<-coefs[6]-coefs[3]
  test_unmet_3<-linearHypothesis(m,"Active_3=Ancmt_3",singular.ok=T)
  F_unmet_3<-test_unmet_3$Chisq[2]
  p_unmet_3<-test_unmet_3$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% 
    nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],coefs[6],tvalue[6],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         df_unmet_3,F_unmet_3,p_unmet_3,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

### Effects by time: country and survey fixed effects model (for service)
func_model_time_service_countryFE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # country fixed effects
                +as.factor(country)
                # survey fixed effectsd
                +as.factor(round)
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:6])
  
  # t values
  tvalue<-unname(summary(m)[2:6,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[3]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[4]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% 
    nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

# Model: subnational and survey FE (for FDI) ------------------------------

### Subnational and year fixed effects model
func_model_subnationalFE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~D_close_active_fdi+D_close_ancmt_fdi
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # subnational region fixed effects
                +as.factor(region_name)
                # year fixed effects
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
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of subnational regions
  N_region<-d %>% pull(region_name) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,
         p_unmet,Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_region,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
} 

### Effects by time: subnational and survey fixed effects model
func_model_time_subnationalFE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Ancmt_3+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # subnational region fixed effects
                +as.factor(region_name)
                # survey fixed effectsd
                +as.factor(round)
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:7])
  
  # t values
  tvalue<-unname(summary(m)[2:7,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[4]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[5]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # active-announced: year>=3
  df_unmet_3<-coefs[6]-coefs[3]
  test_unmet_3<-linearHypothesis(m,"Active_3=Ancmt_3",singular.ok=T)
  F_unmet_3<-test_unmet_3$Chisq[2]
  p_unmet_3<-test_unmet_3$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of subnational regions
  N_region<-d %>% pull(region_name) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],coefs[6],tvalue[6],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         df_unmet_3,F_unmet_3,p_unmet_3,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_region,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

# Model: country and survey FE with linear trends (for FDI) ---------------

### Country and survey fixed effects model with country specific linear trends
func_model_trend<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~D_close_active_fdi+D_close_ancmt_fdi
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # country fixed effects
                +as.factor(country)
                # survey fixed effects
                +as.factor(round)
                # country specific trends
                +as.factor(country)*round_index
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
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% 
    nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,p_unmet,Mean_dv,
         NA,NA,NA,NA,N_obs,N_ctry,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
} 

### Effects by time: FE with linear time trends
func_model_time_trend<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Ancmt_3+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # country fixed effects
                +as.factor(country)
                # survey fixed effects
                +as.factor(round)
                # country specific linear time trends
                +as.factor(country)*round_index
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:7])
  
  # t values
  tvalue<-unname(summary(m)[2:7,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[4]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[5]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # active-announced: year>=3
  df_unmet_3<-coefs[6]-coefs[3]
  test_unmet_3<-linearHypothesis(m,"Active_3=Ancmt_3",singular.ok=T)
  F_unmet_3<-test_unmet_3$Chisq[2]
  p_unmet_3<-test_unmet_3$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% 
    nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],coefs[6],tvalue[6],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         df_unmet_3,F_unmet_3,p_unmet_3,
         Mean_dv,NA,NA,NA,NA,
         N_obs,N_ctry,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

# Model: presidential and survey FE (for FDI) -----------------------------

### Presidential and survey fixed effects model
func_model_presidentFE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~D_close_active_fdi+D_close_ancmt_fdi
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # presidential fixed effects
                +as.factor(country_president)
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
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of presidential terms
  N_president<-d %>% pull(country_president) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,p_unmet,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_president,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

### Effects by time: presidential and survey fixed effects model
func_model_time_presidentFE<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Ancmt_3+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # presidential fixed effects
                +as.factor(country_president)
                # survey fixed effectsd
                +as.factor(round)
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:7])
  
  # t values
  tvalue<-unname(summary(m)[2:7,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[4]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[5]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # active-announced: year>=3
  df_unmet_3<-coefs[6]-coefs[3]
  test_unmet_3<-linearHypothesis(m,"Active_3=Ancmt_3",singular.ok=T)
  F_unmet_3<-test_unmet_3$Chisq[2]
  p_unmet_3<-test_unmet_3$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of presidential terms
  N_president<-d %>% pull(country_president) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],coefs[6],tvalue[6],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         df_unmet_3,F_unmet_3,p_unmet_3,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_president,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

# Model: project and survey FE (for FDI) ----------------------------------

### project and survey fixed effects model (clustered SE on village)
func_model_projectFE_cl1<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~D_close_active_fdi+D_close_ancmt_fdi
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # project fixed effects
                +as.factor(FDIProj)
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
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of projects
  N_proj<-d %>% pull(FDIProj) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,p_unmet,Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_proj,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  
  print(out)
  
} 

### project and survey fixed effects model (clustered SE on project)
func_model_projectFE_cl2<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$FDIProj,
                # dv and key iv
                d[,dv]~D_close_active_fdi+D_close_ancmt_fdi
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # project fixed effects
                +as.factor(FDIProj)
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
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of projects
  N_proj<-d %>% pull(FDIProj) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,p_unmet,Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_proj,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

### effects by time: project and survey FE (clustered SE on village)
func_model_time_projectFE_cl1<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Ancmt_3+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # project fixed effects
                +as.factor(FDIProj)
                # survey fixed effects
                +as.factor(round)
                
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:7])
  
  # t values
  tvalue<-unname(summary(m)[2:7,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[4]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[5]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # active-announced: year>=3
  df_unmet_3<-coefs[6]-coefs[3]
  test_unmet_3<-linearHypothesis(m,"Active_3=Ancmt_3",singular.ok=T)
  F_unmet_3<-test_unmet_3$Chisq[2]
  p_unmet_3<-test_unmet_3$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of projects
  N_proj<-d %>% pull(FDIProj) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],coefs[6],tvalue[6],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         df_unmet_3,F_unmet_3,p_unmet_3,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_proj,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

### effects by time: project and survey FE (clustered SE on project)
func_model_time_projectFE_cl2<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$FDIProj,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Ancmt_3+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # project fixed effects
                +as.factor(FDIProj)
                # survey fixed effects
                +as.factor(round)
                
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:7])
  
  # t values
  tvalue<-unname(summary(m)[2:7,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[4]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[5]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # active-announced: year>=3
  df_unmet_3<-coefs[6]-coefs[3]
  test_unmet_3<-linearHypothesis(m,"Active_3=Ancmt_3",singular.ok=T)
  F_unmet_3<-test_unmet_3$Chisq[2]
  p_unmet_3<-test_unmet_3$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_fdi","D_close_ancmt_fdi",
                               "D_close_eventual_fdi",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of projects
  N_proj<-d %>% pull(FDIProj) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],coefs[6],tvalue[6],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         df_unmet_3,F_unmet_3,p_unmet_3,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_proj,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

# Model: country and survey FE (for aid) ----------------------------------

### country and survey fixed effects model
func_model_countryFE_aid<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~D_close_active_aid+D_close_ancmt_aid
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
  test_unmet<-linearHypothesis(m,"D_close_active_aid=D_close_ancmt_aid",
                               singular.ok=T)
  F_unmet<-test_unmet$Chisq[2]
  p_unmet<-test_unmet$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_aid","D_close_ancmt_aid",
                               "D_close_eventual_aid",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[2],tvalue[2],coefs[1],tvalue[1],df_unmet,F_unmet,
         p_unmet,Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}

### Effects by time: country and survey fixed effects model
func_model_time_countryFE_aid<-function(d,dv){
  
  m<-lm.cluster(data=d,
                #cluster the standard error in the survey cluster
                cluster=d$twnvill,
                # dv and key iv
                d[,dv]~Ancmt_1+Ancmt_2+Ancmt_3+Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # country fixed effects
                +as.factor(country)
                # survey fixed effectsd
                +as.factor(round)
  )
  
  # coefficients
  coefs<-unname(coef(m)[2:7])
  
  # t values
  tvalue<-unname(summary(m)[2:7,3])
  
  # active-announced: year=1
  df_unmet_1<-coefs[4]-coefs[1]
  test_unmet_1<-linearHypothesis(m,"Active_1=Ancmt_1",singular.ok=T)
  F_unmet_1<-test_unmet_1$Chisq[2]
  p_unmet_1<-test_unmet_1$`Pr(>Chisq)`[2]
  
  # active-announced: year=2
  df_unmet_2<-coefs[5]-coefs[2]
  test_unmet_2<-linearHypothesis(m,"Active_2=Ancmt_2",singular.ok=T)
  F_unmet_2<-test_unmet_2$Chisq[2]
  p_unmet_2<-test_unmet_2$`Pr(>Chisq)`[2]
  
  # active-announced: year>=3
  df_unmet_3<-coefs[6]-coefs[3]
  test_unmet_3<-linearHypothesis(m,"Active_3=Ancmt_3",singular.ok=T)
  F_unmet_3<-test_unmet_3$Chisq[2]
  p_unmet_3<-test_unmet_3$`Pr(>Chisq)`[2]
  
  # mean dv
  Mean_dv<-d %>% pull(dv) %>% mean(.,na.rm=T)
  
  # number of observations
  N_obs<-d %>% subset(select=c(dv,"D_close_active_aid","D_close_ancmt_aid",
                               "D_close_eventual_aid",
                               "Urban","age","gender","Edu")) %>% 
    na.omit() %>% 
    nrow()
  
  # number of countries
  N_ctry<-d %>% pull(country) %>% unique() %>% length()
  
  # number of township and village
  N_twnvill<-d %>% pull(twnvill) %>% unique() %>% length()
  
  # combine the results
  out<-c(coefs[1],tvalue[1],coefs[2],tvalue[2],coefs[3],tvalue[3],
         coefs[4],tvalue[4],coefs[5],tvalue[5],coefs[6],tvalue[6],
         df_unmet_1,F_unmet_1,p_unmet_1,df_unmet_2,F_unmet_2,p_unmet_2,
         df_unmet_3,F_unmet_3,p_unmet_3,
         Mean_dv,NA,NA,NA,
         N_obs,N_ctry,N_twnvill,NA,summary(m$lm_res)$adj.r.squared)
  
  print(out)
  
}


# Function to make tables: main results -----------------------------------

### Make tables for country FE
table_main_countryFE<-function(out_list){
  
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
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

### Make tables for subnational FE
table_main_subnationalFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(12:15),]<-format(round(out_table[-c(12:15),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[9:11,]<-"Yes"
  out_table[c(2,4),]<-paste("(",out_table[c(2,4),],")",sep="")
  out_table[16,]<-"1-7"
  names_iv<-c("Announced","","Active","","Active-Announced","F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Subnational region fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of subnational regions",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

### Make tables for linear trend
table_main_trend_countryFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(13:15),]<-format(round(out_table[-c(13:15),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[9:12,]<-"Yes"
  out_table[c(2,4),]<-paste("(",out_table[c(2,4),],")",sep="")
  out_table[16,]<-"1-7"
  names_iv<-c("Announced","","Active","","Active-Announced",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Country fixed effects","Survey round fixed effects",
              "Country specific linear time trends",
              "Number of observations","Number of countries",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

### Make tables for presidential FE
table_main_presidentialFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(12:15),]<-format(round(out_table[-c(12:15),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[9:11,]<-"Yes"
  out_table[c(2,4),]<-paste("(",out_table[c(2,4),],")",sep="")
  out_table[16,]<-"1-7"
  names_iv<-c("Announced","","Active","","Active-Announced",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Presidential fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of presidential terms",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

### Make tables for project FE
table_main_projectFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(12:15),]<-format(round(out_table[-c(12:15),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[9:11,]<-"Yes"
  out_table[c(2,4),]<-paste("(",out_table[c(2,4),],")",sep="")
  out_table[16,]<-"1-7"
  names_iv<-c("Announced","","Active","","Active-Announced","F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Project fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries","Number of project",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

# Function to make tables: effects by time  --------
table_time_countryFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(26:28),]<-format(round(out_table[-c(26:28),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[23:25,]<-"Yes"
  out_table[c(2,4,6,8,10,12),]<-paste("(",out_table[c(2,4,6,8,10,12),],")",sep="")
  out_table[29,]<-"1-7"
  names_iv<-c("Announced: first year","", "Announced: second year","",
              "Announced: third year and above","",
              "Active: first year","", "Active: second year","",
              "Active: third year and above","",
              "Active-Announced: first year","F test: Active=Announced",
              "p value","Active-Announced: second year",
              "F test: Active=Announced","p value",
              "Active-Announced: third year and above",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Country fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

table_time_subnationalFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(26:29),]<-format(round(out_table[-c(26:29),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[23:25,]<-"Yes"
  out_table[c(2,4,6,8,10,12),]<-paste("(",out_table[c(2,4,6,8,10,12),],")",
                                      sep="")
  out_table[30,]<-"1-7"
  names_iv<-c("Announced: first year","", "Announced: second year","",
              "Announced: third year and above","",
              "Active: first year","", "Active: second year","",
              "Active: third year and above","",
              "Active-Announced: first year","F test: Active=Announced",
              "p value",
              "Active-Announced: second year","F test: Active=Announced",
              "p value",
              "Active-Announced: third year and above",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Subnational region fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of subnational regions",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

table_time_trend_countryFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(27:29),]<-format(round(out_table[-c(27:29),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[23:26,]<-"Yes"
  out_table[c(2,4,6,8,10,12),]<-paste("(",out_table[c(2,4,6,8,10,12),],")",sep="")
  out_table[30,]<-"1-7"
  names_iv<-c("Announced: first year","", "Announced: second year","",
              "Announced: third year and above","",
              "Active: first year","", "Active: second year","",
              "Active: third year and above","",
              "Active-Announced: first year","F test: Active=Announced","p value",
              "Active-Announced: second year","F test: Active=Announced","p value",
              "Active-Announced: third year and above","F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Country fixed effects","Survey round fixed effects",
              "Country specific linear time trends",
              "Number of observations","Number of countries",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

table_time_presidentialFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(26:29),]<-format(round(out_table[-c(26:29),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[23:25,]<-"Yes"
  out_table[c(2,4,6,8,10,12),]<-paste("(",out_table[c(2,4,6,8,10,12),],")",sep="")
  out_table[30,]<-"1-7"
  names_iv<-c("Announced: first year","", "Announced: second year","",
              "Announced: third year and above","",
              "Active: first year","", "Active: second year","",
              "Active: third year and above","",
              "Active-Announced: first year","F test: Active=Announced",
              "p value",
              "Active-Announced: second year","F test: Active=Announced",
              "p value",
              "Active-Announced: third year and above",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Presidential fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of presidential terms",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

table_time_projectFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(26:29),]<-format(round(out_table[-c(26:29),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[23:25,]<-"Yes"
  out_table[c(2,4,6,8,10,12),]<-paste("(",out_table[c(2,4,6,8,10,12),],")",sep="")
  out_table[30,]<-"1-7"
  names_iv<-c("Announced: first year","", "Announced: second year","",
              "Announced: third year and above","",
              "Active: first year","", "Active: second year","",
              "Active: third year and above","",
              "Active-Announced: first year","F test: Active=Announced",
              "p value",
              "Active-Announced: second year","F test: Active=Announced",
              "p value",
              "Active-Announced: third year and above",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Project fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of projects",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

table_time_service_countryFE<-function(out_list){
  
  out_table<-do.call(cbind,out_list)
  
  out_table[-c(21:23),]<-format(round(out_table[-c(21:23),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[18:20,]<-"Yes"
  out_table[c(2,4,6,8,10),]<-paste("(",out_table[c(2,4,6,8,10),],")",sep="")
  out_table[24,]<-"1-7"
  names_iv<-c("Announced: first year","", "Announced: second year","",
              "Active: first year","", "Active: second year","",
              "Active: third year and above","",
              "Active-Announced: first year","F test: Active=Announced",
              "p value","Active-Announced: second year",
              "F test: Active=Announced","p value",
              "Mean of dependent variable","Individual controls",
              "Country fixed effects","Survey round fixed effects",
              "Number of observations","Number of countries",
              "Number of villages",
              "Survey rounds","Adjusted R squared")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

# Table A.1: sectors of ODA-type Chinese aid projects in Africa -----------

### Print Table A.1
Table_A1<-aid %>% 
  filter(flow_class=="ODA-like") %>% 
  count(crs_sector_name) %>% 
  as.data.frame()
Table_A1

### Print the log file of Table A.1
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A1.log")
log_print(Table_A1)
log_close()

# Descriptive: precise and imprecise FDI projects (Table A.2-A.3) ---------

### Print Table A.2: precisely and imprecisely geocoded FDI projects by country
country_afb_fdi<-data_respno_FDI[[1]] %>% pull(country) %>% unique()
Table_A2<-fdi %>% 
  filter(Source_country=="China",Project_type=="New",
         Destination_country %in% country_afb_fdi) %>% 
  mutate(D_precise=ifelse(Precision_final %in% c(1,2,9),
                          "Precise location","Imprecise location"),
         D_precise=factor(D_precise,levels=c("Precise location",
                                             "Imprecise location"))) %>% 
  subset(select=c(Destination_country,D_precise)) %>% 
  table()
Table_A2

### Print the log file of Table A.2
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A2.log")
log_print(Table_A2)
log_close()

### Print Table A.3: Characteristics of precise and imprecise projects

# sector
precise_sector<-fdi %>% 
  mutate(Sector_type=ifelse(Sector %in% c("Coal, oil & gas","Metals",
                                          "Minerals","Renewable energy"),
                            "Resource",
                            ifelse(Sector %in% c("Communications","Aerospace",
                                                 "Real estate","Healthcare",
                                                 "Financial services",
                                                 "Business services",
                                                 "Warehousing","Transportation",
                                                 "Software & IT services"),
                                   "Service","Manufacture")),
         D_precise=ifelse(Precision_final %in% c(1,2,9),
                          "Precise location","Imprecise location"),
         D_precise=factor(D_precise,levels=c("Precise location",
                                             "Imprecise location"))) %>% 
  filter(Source_country=="China",Project_type=="New",
         Destination_country %in% country_afb_fdi) %>% 
  subset(select=c(Sector_type,D_precise)) %>% 
  table() %>% 
  prop.table(.,2)

# ownership
precise_ownership<-fdi %>% 
  mutate(Ownership_final=trimws(Ownership_final),
         Ownership_final=ifelse(Ownership_final!="POE","State-owned firms",
                                "Private firms"),
         D_precise=ifelse(Precision_final %in% c(1,2,9),
                          "Precise location","Imprecise location"),
         D_precise=factor(D_precise,levels=c("Precise location",
                                             "Imprecise location"))) %>% 
  filter(Source_country=="China",Project_type=="New",
         Destination_country %in% country_afb_fdi) %>% 
  subset(select=c(Ownership_final,D_precise)) %>% 
  table() %>% 
  prop.table(.,2)

# investment amount
precise_amount<-fdi %>% 
  mutate(Amount_cut=cut2(Amount,g=4),
         D_precise=ifelse(Precision_final %in% c(1,2,9),
                          "Precise location","Imprecise location"),
         D_precise=factor(D_precise,levels=c("Precise location",
                                             "Imprecise location"))) %>% 
  filter(Source_country=="China",Project_type=="New",
         Destination_country %in% country_afb_fdi) %>% 
  subset(select=c(Amount_cut,D_precise)) %>% 
  table() %>% 
  prop.table(.,2)

# year that appears in the fDi market data
precise_year<-fdi %>% 
  mutate(Year_cut=ifelse((Year>=2003 & Year<=2006),"2003-2006",
                         ifelse((Year>=2007 & Year<=2010),"2007-2010",
                                ifelse((Year>=2011 & Year<=2014),
                                       "2011-2014","2015-2018"))),
         D_precise=ifelse(Precision_final %in% c(1,2,9),
                          "Precise location","Imprecise location"),
         D_precise=factor(D_precise,levels=c("Precise location",
                                             "Imprecise location"))) %>% 
  filter(Source_country=="China",Project_type=="New",
         Destination_country %in% country_afb_fdi) %>% 
  subset(select=c(Year_cut,D_precise)) %>% 
  table() %>% 
  prop.table(.,2)

# print Table A.3
Table_A3<-rbind(precise_sector,precise_ownership,precise_amount,precise_year)
Table_A3<-100*Table_A3
Table_A3<-round(Table_A3,2)
Table_A3

### Print the log file of Table A.3
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A3.log")
log_print(Table_A3)
log_close()

# Descriptive: status to Chinese FDI by country (Table A.6) ---------------

### Print Table A.6: active, announced, and eventual by country
Table_A6<-data_respno_FDI[[1]] %>% 
  filter(D_close_fdi==1) %>% 
  subset(select=c(country,resp_status)) %>% 
  table()
Table_A6

### Print the log file of Table A.6
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A6.log")
log_print(Table_A6)
log_close()

# Descriptive: balance of observables in main analysis (Table A.7) --------

### Countries used in the main analysis
country_main<-data_close_fdi %>% pull(country) %>% unique() 

### Urban balance
urban_bal_main<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_main) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(Urban,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

### Education balance
edu_bal_main<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_main) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(Edu,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

### Age balance
age_bal_main<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_main) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  group_by(resp_status) %>% 
  summarise(mean=mean(age,na.rm=T)) %>% 
  t()

### Gender balance
gender_bal_main<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_main) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(gender,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

### Combine the balance test
bal_main<-rbind(urban_bal_main[3,],edu_bal_main,
                age_bal_main[2,],gender_bal_main[1,])
bal_main<-apply(bal_main,2,as.numeric)
bal_main[-7,]<-bal_main[-7,]*100
bal_main<-cbind(bal_main,bal_main[,1]-bal_main[,3],bal_main[,2]-bal_main[,3])
colnames(bal_main)[c(5,6)]<-c("Announced-Eventual","Active-Eventual")
bal_main<-round(bal_main,2)
row_labels<-c("Living in urban areas (%)","No formal school (%)", 
              "Primary school (%)","Secondary school (%)",
              "Post-secondary school (%)",
              "Post-graduate (%)","Age","Female (%)")
bal_main<-cbind.data.frame(row_labels,bal_main)

### Print Table A.7
Table_A7<-bal_main
Table_A7

### Print the log file of Table A.7
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A7.log")
log_print(Table_A7)
log_close()

# Descriptive: summary statistics for main variables (Table A.8) ----------

### Variables to make the table of summary statistics
fdi_summary<-data_close_fdi %>% 
  subset(select=c(D_close_ancmt_fdi,D_close_active_fdi,D_close_eventual_fdi,
                  PSP_econ_D,PSP_econ_O,PSP_econin1yr_D,PSP_econin1yr_O,
                  SG_econ_D,SG_econ_O,SG_job_D,SG_job_O,SG_prfpres_D,
                  SG_prfpres_O,
                  Urban,age,gender,Edu)) %>% 
  mutate(Urban=factor(Urban,levels=c("Rural","Semi-Urban","Urban")),
         Urban=as.numeric(Urban)-1,
         gender=factor(gender,levels=c("Male","Female")),
         gender=as.numeric(gender)-1,
         Edu=as.numeric(Edu)-1)

### Manually calculate the summary statistics
N<-apply(fdi_summary,2,function(x) sum(!is.na(x)))
Mean<-apply(fdi_summary,2,function(x) mean(x,na.rm=T))
StDev<-apply(fdi_summary,2,function(x) sd(x,na.rm=T))
Min<-apply(fdi_summary,2,function(x) min(x,na.rm=T))
Pctl_25<-apply(fdi_summary,2,function(x) quantile(x,0.25,na.rm=T))
Pctl_75<-apply(fdi_summary,2,function(x) quantile(x,0.75,na.rm=T))
Max<-apply(fdi_summary,2,function(x) max(x,na.rm=T))

Table_summary_1<-cbind(N,Mean,StDev,Min,Pctl_25,Pctl_75,Max)

### Summary statistics of distance and year to announced, active, and eventual
fdi_summary_ancmt<-data_close_fdi %>% 
  filter(D_close_ancmt_fdi==1) %>% 
  subset(select=c(Distance_ancmt_min,Duration_ancmt_min))

fdi_summary_active<-data_close_fdi %>% 
  filter(D_close_active_fdi==1) %>% 
  subset(select=c(Distance_active_min,Duration_active_min))

fdi_summary_eventual<-data_close_fdi %>% 
  filter(D_close_eventual_fdi==1) %>% 
  subset(select=c(Distance_eventual_min,Time_eventual_min))

summary_list<-list(fdi_summary_ancmt,fdi_summary_active,fdi_summary_eventual)
N<-Mean<-StDev<-Min<-Pctl_25<-Pctl_75<-Max<-list()
for (i in 1:length(summary_list)){
  
  N[[i]]<-apply(summary_list[[i]],2,function(x) sum(!is.na(x)))
  Mean[[i]]<-apply(summary_list[[i]],2,function(x) mean(x,na.rm=T))
  StDev[[i]]<-apply(summary_list[[i]],2,function(x) sd(x,na.rm=T))
  Min[[i]]<-apply(summary_list[[i]],2,function(x) min(x,na.rm=T))
  Pctl_25[[i]]<-apply(summary_list[[i]],2,function(x) quantile(x,0.25,na.rm=T))
  Pctl_75[[i]]<-apply(summary_list[[i]],2,function(x) quantile(x,0.75,na.rm=T))
  Max[[i]]<-apply(summary_list[[i]],2,function(x) max(x,na.rm=T))
  
  
}
N<-unlist(N)
N<-N[c(1,3,5,2,4,6)]

Mean<-unlist(Mean)
Mean<-Mean[c(1,3,5,2,4,6)]

StDev<-unlist(StDev)
StDev<-StDev[c(1,3,5,2,4,6)]

Min<-unlist(Min)
Min<-Min[c(1,3,5,2,4,6)]

Pctl_25<-unlist(Pctl_25)
Pctl_25<-Pctl_25[c(1,3,5,2,4,6)]

Pctl_75<-unlist(Pctl_75)
Pctl_75<-Pctl_75[c(1,3,5,2,4,6)]

Max<-unlist(Max)
Max<-Max[c(1,3,5,2,4,6)]

Table_summary_2<-cbind(N,Mean,StDev,Min,Pctl_25,Pctl_75,Max)

### Add the two parts and make Table A.8
Table_A8<-rbind(Table_summary_1[1:3,],Table_summary_2,Table_summary_1[4:17,])
Table_A8[,-1]<-round(Table_A8[,-1],3)
row_labels<-c("Active 50km","Announced 50km","Eventual 50km",
              "Distance to announced projects",
              "Distance to active projects",
              "Distance to eventual projects",
              "Years after announced",
              "Years after active",
              "Years to eventual",
              "Current economic conditions: dummy",
              "Current economic conditions: ordinal",
              "Economic conditions in one year: dummy",
              "Economic conditions in one year: ordinal",
              "How government manages economy:dummy",
              "How government manages economy:ordinal",
              "How government creates jobs: dummy",
              "How government creates jobs: ordinal",
              "Presidential approval: dummy","Presidential approval: ordinal",
              "Urban (rural=0, semi-urban=1, urban=2)","Age","Gender (female=1)",
              "Education (no formal school=0, post-graduate=4)")

Table_A8<-cbind(row_labels,Table_A8)
rownames(Table_A8)<-NULL
Table_A8

### Print the log file of Table A.8
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A8.log")
log_print(Table_A8)
log_close()

# Effects by time: Table A.9 ----------------------------------------------

### Dependent variables
dv<-c("PSP_econ_D","PSP_econ_O","PSP_econin1yr_D","PSP_econin1yr_O",
      "SG_econ_D","SG_econ_O","SG_job_D","SG_job_O",
      "SG_prfpres_D","SG_prfpres_O")

### Estimate effects and output tables
out_fdi_time<-list()
for (k in 1:length(dv)){
  
  out_fdi_time[[k]]<-func_model_time_countryFE(data_fdi_time$Main,dv[k])
  
}

### Print Table A.9
Table_A9<-table_time_countryFE(out_fdi_time)
colnames(Table_A9)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                      "(7)","(8)","(9)","(10)")
Table_A9

### Print the log file of Table A.9
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A9.log")
log_print(Table_A9)
log_close()

# Results by sector: Table A.11-14 ----------------------------------------

### Manufacturing and resources: main results
out_manuf_resource_main<-list()
for (k in 1:length(dv)){
  
  out_manuf_resource_main[[k]]<-func_model_countryFE(data_close_manuf_resource,
                                                     dv[k])
  
}

### Print Table A.11
Table_A11<-table_main_countryFE(out_manuf_resource_main)
colnames(Table_A11)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                      "(7)","(8)","(9)","(10)")
Table_A11

### Print the log file of Table A.11
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A11.log")
log_print(Table_A11)
log_close()

### Manufacturing and resources: effects by time
out_manuf_resource_time<-list()
for (k in 1:length(dv)){
  
  out_manuf_resource_time[[k]]<-
    func_model_time_countryFE(data_fdi_time$ManufResource,dv[k])
  
}

### Print Table A.12
Table_A12<-table_time_countryFE(out_manuf_resource_time)
colnames(Table_A12)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                       "(7)","(8)","(9)","(10)")
Table_A12

### Print the log file of Table A.12
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A12.log")
log_print(Table_A12)
log_close()

### Service: main results
out_service_main<-list()
for (k in 1:length(dv)){
  
  out_service_main[[k]]<-func_model_countryFE(data_close_service,
                                                     dv[k])
  
}

### Print Table A.13
Table_A13<-table_main_countryFE(out_service_main)
colnames(Table_A13)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                       "(7)","(8)","(9)","(10)")
Table_A13

### Print the log file of Table A.13
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A13.log")
log_print(Table_A13)
log_close()

### Service: effects by time
out_service_time<-list()
for (k in 1:length(dv)){
  
  out_service_time[[k]]<-
    func_model_time_service_countryFE(data_fdi_time$Service,dv[k])
  
}

### Print Table A.14
Table_A14<-table_time_service_countryFE(out_service_time)
colnames(Table_A14)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                       "(7)","(8)","(9)","(10)")
Table_A14

### Print the log file of Table A.14
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A14.log")
log_print(Table_A14)
log_close()

# Results by age: Table A.15-18 -------------------------------------------

### Main results by age group
out_age_main<-rep(list(rep(list(0),length(dv))),length(data_close_fdi_age))
for (j in 1:length(data_close_fdi_age)){
  
  for (k in 1:length(dv)){
    
    out_age_main[[j]][[k]]<-func_model_countryFE(data_close_fdi_age[[j]],
                                                 dv[k])
    
  }
  
}

### Effects by time by age group
out_age_time<-rep(list(rep(list(0),length(dv))),length(data_close_fdi_age))
for (j in 1:length(data_close_fdi_age)){
  
  for (k in 1:length(dv)){
    
    out_age_time[[j]][[k]]<-func_model_time_countryFE(data_fdi_time[[j+7]],
                                                      dv[k])
    
  }
  
}

### Print Table A.15
Table_A15<-table_main_countryFE(c(out_age_main[[1]][1:4],
                                out_age_main[[2]][1:4]))
colnames(Table_A15)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                       "(7)","(8)")
Table_A15

### Print the log file of Table A.15
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A15.log")
log_print(Table_A15)
log_close()

### Print Table A.16
Table_A16<-table_time_countryFE(c(out_age_time[[1]][1:4],
                                  out_age_time[[2]][1:4]))
colnames(Table_A16)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                       "(7)","(8)")
Table_A16

### Print the log file of Table A.16
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A16.log")
log_print(Table_A16)
log_close()

### Print Table A.17
Table_A17<-table_main_countryFE(c(out_age_main[[1]][5:10],
                                  out_age_main[[2]][5:10]))
colnames(Table_A17)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                       "(7)","(8)","(9)","(10)","(11)","(12)")
Table_A17

### Print the log file of Table A.17
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A17.log")
log_print(Table_A17)
log_close()

### Print Table A.18
Table_A18<-table_time_countryFE(c(out_age_time[[1]][5:10],
                                  out_age_time[[2]][5:10]))
colnames(Table_A18)<-c("","(1)","(2)","(3)","(4)","(5)","(6)",
                       "(7)","(8)","(9)","(10)","(11)","(12)")
Table_A18

### Print the log file of Table A.18
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A18.log")
log_print(Table_A18)
log_close()
# Compare FDI with aid: Table A.19-24 -------------------------------------

### Number of active, announced, and eventual in FDI and aid dataset
resp_fdi_compare<-data_close_fdi_compare %>% 
  subset(select=c(country,resp_status)) %>% 
  table()

resp_aid_compare<-data_close_aid_compare %>% 
  subset(select=c(country,resp_status)) %>% 
  table()

### Print Table A.19
Table_A19<-cbind(resp_fdi_compare,resp_aid_compare)
Table_A19

### Print the log file of Table A.19
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A19.log")
log_print(Table_A19)
log_close()

### Balance of observables in FDI and aid dataset

# urban balance FDI
urban_bal_fdi<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(Urban,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

# education balance FDI
edu_bal_fdi<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(Edu,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

# age balance FDI
age_bal_fdi<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  group_by(resp_status) %>% 
  summarise(mean=mean(age,na.rm=T)) %>% 
  t()

# gender balance FDI
gender_bal_fdi<-data_respno_FDI[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(gender,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

# urban balance aid
urban_bal_aid<-data_respno_aid[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(Urban,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

# education balance aid
edu_bal_aid<-data_respno_aid[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(Edu,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

# age balance aid
age_bal_aid<-data_respno_aid[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  group_by(resp_status) %>% 
  summarise(mean=mean(age,na.rm=T)) %>% 
  t()

# gender balance aid
gender_bal_aid<-data_respno_aid[[1]] %>% 
  filter(country %in% country_both) %>% 
  mutate(resp_status=factor(resp_status,levels=c("Announced","Active",
                                                 "Eventual","Not close"))) %>%
  subset(select=c(gender,resp_status)) %>% 
  table() %>% 
  prop.table(.,2)

### Print Table A.20
bal_fdi_compare<-rbind(urban_bal_fdi[3,],edu_bal_fdi,
                       age_bal_fdi[2,],gender_bal_fdi[1,])
bal_aid_compare<-rbind(urban_bal_aid[3,],edu_bal_aid,
                       age_bal_aid[2,],gender_bal_aid[1,])

Table_A20<-cbind(bal_fdi_compare,bal_aid_compare)
Table_A20<-apply(Table_A20,2,as.numeric)
Table_A20[-7,]<-100*Table_A20[-7,]
Table_A20<-round(Table_A20,2)

row_labels<-c("Living in urban areas (%)","No formal school (%)", 
              "Primary school (%)","Secondary school (%)",
              "Post-secondary school (%)",
              "Post-graduate (%)","Age","Female (%)")
Table_A20<-cbind.data.frame(row_labels,Table_A20)

### Print the log file of Table A.20
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A20.log")
log_print(Table_A20)
log_close()

### Main results: compare FDI and aid

# main results FDI
out_fdi_compare<-list()
for (k in 1:length(dv)){
  
  out_fdi_compare[[k]]<-func_model_countryFE(data_close_fdi_compare,dv[k])
  
}

# main results Aid
out_aid_compare<-list()
for (k in 1:length(dv)){
  
  out_aid_compare[[k]]<-func_model_countryFE_aid(data_close_aid_compare,dv[k])
  
}

### Effects by time: compare FDI and aid

# effects by time FDI
out_fdi_time_compare<-list()
for (k in 1:length(dv)){
  
  out_fdi_time_compare[[k]]<-func_model_time_countryFE(data_fdi_time$Compare,
                                                       dv[k])
  
}

# effects by time aid
out_aid_time_compare<-list()
for (k in 1:length(dv)){
  
  out_aid_time_compare[[k]]<-
    func_model_time_countryFE_aid(data_close_aid_compare_time,dv[k])
  
}

### Print Table A.21
Table_A21<-table_main_countryFE(c(out_fdi_compare[1:4],out_aid_compare[1:4]))
colnames(Table_A21)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)")
Table_A21

### Print the log file of Table A_21
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A21.log")
log_print(Table_A21)
log_close()

### Print Table A.22
Table_A22<-table_time_countryFE(c(out_fdi_time_compare[1:4],
                                  out_aid_time_compare[1:4]))
colnames(Table_A22)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)")
Table_A22

### Print the log file of Table A_22
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A22.log")
log_print(Table_A22)
log_close()

### Print Table A.23
Table_A23<-table_main_countryFE(c(out_fdi_compare[5:10],out_aid_compare[5:10]))
colnames(Table_A23)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)","(11)","(12)")
Table_A23

### Print the log file of Table A_23
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A23.log")
log_print(Table_A23)
log_close()

### Print Table A.24
Table_A24<-table_time_countryFE(c(out_fdi_time_compare[5:10],
                                  out_aid_time_compare[5:10]))
colnames(Table_A24)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)","(11)","(12)")
Table_A24

### Print the log file of Table A_24
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A24.log")
log_print(Table_A24)
log_close()

# Robust: countries with both active and announced (Table A.25-26) --------

### Main results in countries with both active and announced
out_subctry_main<-list()
for (k in 1:length(dv)){
  
  out_subctry_main[[k]]<-func_model_countryFE(data_close_fdi_subctry,
                                              dv[k])
  
}

### Print Table A.25
Table_A25<-table_main_countryFE(out_subctry_main)
colnames(Table_A25)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")

Table_A25

### Print the log file of Table A.25
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A25.log")
log_print(Table_A25)
log_close()

### Effects by time in countries with both active and announced
out_subctry_time<-list()
for (k in 1:length(dv)){
  
  out_subctry_time[[k]]<-func_model_time_countryFE(data_fdi_time$Subctry,
                                              dv[k])
  
}

### Print Table A.26
Table_A26<-table_time_countryFE(out_subctry_time)
colnames(Table_A26)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A26

### Print the log file of Table A.26
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A26.log")
log_print(Table_A26)
log_close()

# Robust: subnational fixed effects (Table A.27-28) -----------------------

### Main results: subnational region FE
out_subregion_main<-list()
for (k in 1:length(dv)){
  
  out_subregion_main[[k]]<-func_model_subnationalFE(data_close_fdi_subnational,
                                                    dv[k])
  
}

### Print Table A.27
Table_A27<-table_main_subnationalFE(out_subregion_main)
colnames(Table_A27)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A27

### Print the log file of Table A.27
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A27.log")
log_print(Table_A27)
log_close()

### Effects by time: subnational FE
out_subregion_time<-list()
for (k in 1:length(dv)){
  
  out_subregion_time[[k]]<-
    func_model_time_subnationalFE(data_fdi_time$Subregion,
                                  dv[k])
  
}

### Print Table A.28
Table_A28<-table_time_subnationalFE(out_subregion_time)
colnames(Table_A28)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A28

### Print the log file of Table A.28
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A28.log")
log_print(Table_A28)
log_close()

# Robust: narrower time windows (Table A.29-32) ---------------------------

### Main results: narrower time windows
out_narrower_main<-rep(list(rep(list(0),length(dv))),
                       length(data_close_fdi_narrow))
for (j in 1:length(data_close_fdi_narrow)){
  
  for (k in 1:length(dv)){
    
    out_narrower_main[[j]][[k]]<-
      func_model_countryFE(data_close_fdi_narrow[[j]],dv[k])
    
  }
  
}

### Effects by time: narrower time windows
out_narrower_time<-rep(list(rep(list(0),length(dv))),
                       length(data_close_fdi_narrow))
for (j in 1:length(data_close_fdi_narrow)){
  
  for (k in 1:length(dv)){
    
    out_narrower_time[[j]][[k]]<-
      func_model_time_countryFE(data_fdi_time[[j+2]],dv[k])
    
  }
  
}

### Print Table A.29
Table_A29<-table_main_countryFE(c(out_narrower_main[[2]][1:2],
                                  out_narrower_main[[1]][1:2],
                                  out_narrower_main[[2]][3:4],
                                  out_narrower_main[[1]][3:4]))
colnames(Table_A29)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)")
Table_A29

### Print the log file of Table A.29
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A29.log")
log_print(Table_A29)
log_close()

### Print Table A.30
Table_A30<-table_time_countryFE(c(out_narrower_time[[2]][1:2],
                                  out_narrower_time[[1]][1:2],
                                  out_narrower_time[[2]][3:4],
                                  out_narrower_time[[1]][3:4]))
colnames(Table_A30)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)")
Table_A30

### Print the log file of Table A.30
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A30.log")
log_print(Table_A30)
log_close()

### Print Table A.31
Table_A31<-table_main_countryFE(c(out_narrower_main[[2]][5:6],
                                  out_narrower_main[[1]][5:6],
                                  out_narrower_main[[2]][7:8],
                                  out_narrower_main[[1]][7:8],
                                  out_narrower_main[[2]][9:10],
                                  out_narrower_main[[1]][9:10]))
colnames(Table_A31)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)","(11)","(12)")
Table_A31

### Print the log file of Table A.31
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A31.log")
log_print(Table_A31)
log_close()

### Print Table A.32
Table_A32<-table_time_countryFE(c(out_narrower_time[[2]][5:6],
                                  out_narrower_time[[1]][5:6],
                                  out_narrower_time[[2]][7:8],
                                  out_narrower_time[[1]][7:8],
                                  out_narrower_time[[2]][9:10],
                                  out_narrower_time[[1]][9:10]))
colnames(Table_A32)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)","(11)","(12)")
Table_A32

### Print the log file of Table A.32
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A32.log")
log_print(Table_A32)
log_close()

# Robust: fixed effects with linear time trends (Table A.33-34) -----------

### Main results: linear time trends
out_trend_main<-list()
for (k in 1:length(dv)){
  
  out_trend_main[[k]]<-func_model_trend(data_close_fdi,dv[k])
  
}

### Print Table A.33
Table_A33<-table_main_trend_countryFE(out_trend_main)
colnames(Table_A33)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A33

### Print the log file of Table A.33
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A33.log")
log_print(Table_A33)
log_close()

### Effects by time: linear time trends
out_trend_time<-list()
for (k in 1:length(dv)){
  
  out_trend_time[[k]]<-func_model_time_trend(data_fdi_time$Main,dv[k])
  
}

### Print Table A.34
Table_A34<-table_time_trend_countryFE(out_trend_time)
colnames(Table_A34)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A34

### Print the log file of Table A.34
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A34.log")
log_print(Table_A34)
log_close()
# Robust: presidential fixed effects (Table A.35-37) ----------------------

### Active, announced, and eventual by presidential terms (Table A.35)
Table_A35<-data_close_fdi_president %>% 
  subset(select=c(country_president,resp_status)) %>% 
  table()
Table_A35

### Print the log file of Table A.35
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A35.log")
log_print(Table_A35)
log_close()

### Main results: presidential FE
out_president_main<-list()
for (k in 1:length(dv)){
  
  out_president_main[[k]]<-func_model_presidentFE(data_close_fdi_president,
                                                  dv[k])
  
}

### Print Table A.36
Table_A36<-table_main_presidentialFE(out_president_main)
colnames(Table_A36)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A36

### Print the log file of Table A.36
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A36.log")
log_print(Table_A36)
log_close()

### Effects by time: presidential FE
out_president_time<-list()
for (k in 1:length(dv)){
  
  out_president_time[[k]]<-
    func_model_time_presidentFE(data_fdi_time$Presidential,dv[k])
  
}

### Print Table A.37
Table_A37<-table_time_presidentialFE(out_president_time)
colnames(Table_A37)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A37

### Print the log file of Table A.37
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A37.log")
log_print(Table_A37)
log_close()

# Robust: project fixed effects (Table A.38-41) ---------------------------

### Main results: project FE with SE at the village level
out_project_main_cl1<-list()
for (k in 1:length(dv)){
  
  out_project_main_cl1[[k]]<-func_model_projectFE_cl1(data_project_FDI[[1]],
                                                      dv[k])
  
}

### Print Table A.38
Table_A38<-table_main_projectFE(out_project_main_cl1)
colnames(Table_A38)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A38

### Print the log file of Table A.38
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A38.log")
log_print(Table_A38)
log_close()

### Effects by time: project FE with SE at the village level
out_project_time_cl1<-list()
for (k in 1:length(dv)){
  
  out_project_time_cl1[[k]]<-
    func_model_time_projectFE_cl1(data_project_FDI_time,dv[k])
  
}

### Print Table A.39
Table_A39<-table_time_projectFE(out_project_time_cl1)
colnames(Table_A39)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A39

### Print the log file of Table A.39
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A39.log")
log_print(Table_A39)
log_close()

### Main results: project FE with SE at the project level
out_project_main_cl2<-list()
for (k in 1:length(dv)){
  
  out_project_main_cl2[[k]]<-func_model_projectFE_cl2(data_project_FDI[[1]],
                                                      dv[k])
  
}

### Print Table A.40
Table_A40<-table_main_projectFE(out_project_main_cl2)
colnames(Table_A40)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A40

### Print the log file of Table A.40
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A40.log")
log_print(Table_A40)
log_close()

### Effects by time: project FE with SE at the village level
out_project_time_cl2<-list()
for (k in 1:length(dv)){
  
  out_project_time_cl2[[k]]<-
    func_model_time_projectFE_cl2(data_project_FDI_time,dv[k])
  
}

### Print Table A.41
Table_A41<-table_time_projectFE(out_project_time_cl2)
colnames(Table_A41)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A41

### Print the log file of Table A.41
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A41.log")
log_print(Table_A41)
log_close()

# Robust: Matching (Table A.42) -------------------------------------------

### The dataset used for matching
data_cem<-data_fdi_time$Presidential %>% 
  filter(!(D_close_eventual_fdi==1 & Time_eventual_min>3))
tab<-table(data_cem$country_president,data_cem$resp_status)
drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)
president_drop<-rownames(tab)[drop]
data_cem<-data_cem %>% 
  filter(!country_president %in% president_drop)

data_cem<-data_cem %>%   
  mutate(Age_cut=cut2(age,g=4),
         Status_time=ifelse(Ancmt_1==1,"Announced_1",
                            ifelse(Ancmt_2==1,"Announced_2",
                                   ifelse(Ancmt_3==1,"Announced_3",
                                          ifelse(Active_1==1,"Active_1",
                                                 ifelse(Active_2==1,
                                                        "Active_2",
                                                        ifelse(Active_3==1,
                                                               "Active_3",
                                                               "Eventual")))))))
### The list of type of respondents to match
status_to_match<-c("Announced_1","Announced_2","Announced_3",
                   "Active_1","Active_2","Active_3")

### Match each type of respondents from eventual
data_respno_mat<-rep(list(0),length(status_to_match))
for (k in 1:length(status_to_match)){
  
  # subset the dataset for matching (choosing active, announced or inactive)
  d<-data_cem %>% 
    filter((Status_time==status_to_match[k]|Status_time=="Eventual")) %>% 
    subset(select=c(respno_r,Status_time,country_president,Urban,
                    Age_cut,gender,Edu)) %>% 
    na.omit() %>% 
    mutate_at(vars(country_president:Edu),funs(as.factor(.)))
  
  # coarsend exact matching
  mat<-cem(treatment = "Status_time",data=d,drop="respno_r",
           keep.all = T,k2k = F)
  
  # the cases that are matched
  respno_mat<-d$respno_r[mat$matched]
  
  # the weight
  w<-mat$w[mat$matched]
  
  # create the matched dataset
  data_respno_mat[[k]]<-data_cem %>% 
    filter(respno_r %in% respno_mat) %>% 
    mutate(mat_weight=w,
           treat=ifelse(Status_time==status_to_match[k],1,0)) 
  
}

### Calculate average treatment effects
out_mat_time<-rep(list(rep(list(0),length(data_respno_mat))),length(dv))
for (k in 1:length(dv)){
  for (j in 1:length(data_respno_mat)){
    
    m<-lm.cluster(data=data_respno_mat[[j]],
                  # cluster the standard error in the survey cluster
                  cluster=data_respno_mat[[j]]$twnvill,
                  weights=data_respno_mat[[j]]$mat_weight,
                  # dv and key iv
                  data_respno_mat[[j]][,dv[k]]~treat
    )
    
    out_mat_time[[k]][[j]]<-cbind(coef(m)[2],summary(m)[2,3])
    
  }
}

### Number of matched respondents
N_tr<-N_co<-list()
for (j in 1:length(data_respno_mat)){
  
  N_tr[[j]]<-table(data_respno_mat[[j]]$Status_time)[1]
  N_co[[j]]<-table(data_respno_mat[[j]]$Status_time)[2]
}

### Output the table
out_table_mat<-rep(list(rep(list(0),length(data_respno_mat))),
                   length(out_mat_time))
for (k in 1:length(out_mat_time)){
  for (j in 1:length(data_respno_mat)){
    
    out_table_mat[[k]][[j]]<-
      c(out_mat_time[[k]][[j]][1],out_mat_time[[k]][[j]][2],
        N_tr[[j]],N_co[[j]]) 
    
  }
}

### Organize the table
for (k in 1:length(out_mat_time)){
  out_table_mat[[k]]<-unlist(out_table_mat[[k]])
}
out_table_mat<-do.call(cbind,out_table_mat)
out_table_mat

### a function to output the table
cem_time<-function(out_table){
  
  out_table[-c(3,4,7,8,11,12,15,16,19,20,23,24),]<-
    format(round(out_table[-c(3,4,7,8,11,12,15,16,19,20,23,24),],3),nsmall=3)
  out_table<-trimws(out_table)
  out_table[c(2,6,10,14,18,22),]<-paste("(",out_table[c(2,6,10,14,18,22),],")",sep="")
  names_iv<-
    c("Announced: first year","", 
      "N of Announced matched","N of Eventual matched", 
      "Announced: second year","",
      "N of Announced matched","N of Eventual matched",
      "Announced: >= third year","","N of Announced matched",
      "N of Eventual matched",
      "Active: first year","", "N of Active matched","N of Eventual matched", 
      "Active: second year","","N of Active matched","N of Eventual matched",
      "Active: >= third year","","N of Active matched","N of Eventual matched")
  
  out_table<-cbind(names_iv,out_table)
  
  print(out_table)
  
}

### Print Table A.42
Table_A42<-cem_time(out_table_mat)
rownames(Table_A42)<-NULL
colnames(Table_A42)<-c("","(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)",
                       "(9)","(10)")
Table_A42

### Print the log file of Table A.42
log_open("C:/Users/xwang21/Dropbox/Research/China FDI Africa/JOP R and R/JOP Replication Files/Table_A42.log")
log_print(Table_A42)
log_close()

