### Replication file for
### FDI, Unmet Expectations, and the Prospects of Political Leaders

### Part 2: Tables and figures in the manuscript
rm(list=ls())

### Use "Alt+O" to collapse all sections and "Shift+Alt+O" to expand all

### Set your own working directory

# Load required packages --------------------------------------------------
library(tidyverse)
library(miceadds)
library(grDevices)
library(car)
library(Hmisc)
library(xtable)
library(logr) # optional, only to print the log files
library(grDevices)

### Suppress scientific numeric
options(scipen=999)


all_countries <- c("Botswana", "Ghana", "Kenya", "Madagascar", "Mozambique", "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda", "Zimbabwe", "Algeria", "Cameroon", "Cote d'Ivoire", "Egypt", "Malawi", "Mauritius", "Morocco", "Namibia", "Tunisia", "Zambia")

# for (country in all_countries) {

country <- "Botswana"

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
load("data_respno_aid.RData")
load(paste(country,"_data_respno_FDI.RData", sep = ""))

# FDI Dataset: drop those not close to any projects -----------------------
data_close_fdi<-list()
for (j in 1:length(data_respno_FDI)){
  
  data_close_fdi[[j]]<-data_respno_FDI[[j]] %>% filter(D_close_fdi==1)
  
  # drop the countries that have no variation on announced, active, and eventual
  tab<-data_close_fdi[[j]] %>% 
    subset(select=c(country,resp_status)) %>% 
    table()
  
  if (ncol(tab) < 3){
    drop <- TRUE
  } else {
    drop<-((tab[,1]==0 & tab[,2]==0)|tab[,3]==0)    
  }

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

### Print log file for Table 1
log_open(paste("./",country,"_Table 1.log"))
log_print(Table_1)
log_close()

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

### Print log file for Table 2
log_open(paste("./",country,"_Table 2.log"))
log_print(Table_2)
log_close()


# Estimate effects by distance --------------------------------------------

### Create a variable Distance for active, announced, and eventual
data_close_fdi[[2]]<-data_close_fdi[[2]] %>% 
  mutate(Distance=ifelse(D_close_active_fdi==1,Distance_active_min,
                         ifelse(D_close_ancmt_fdi==1,Distance_ancmt_min,
                                Distance_eventual_min)),
         Distance_bin=cut2(Distance,g=10,levels.mean=F,onlycuts = F)
  )

### Format distance bins for plot
bin<-data_close_fdi[[2]] %>% pull(Distance_bin) %>% unique() %>% sort()

bin_num<-str_extract(bin, "\\d+")
bin_num<-c(bin_num,200)

bin_label<-NA
for (i in 1:length(bin)){
  bin_label[i]<-paste("[",bin_num[i],",",bin_num[i+1],")",sep="")
}
bin_label<-factor(bin_label,levels=bin_label)

### Dependent variables for plotting the effects by distance
dv_distance<-c("PSP_econ_O","PSP_econin1yr_O",
               "SG_econ_O","SG_job_O","SG_prfpres_O")

### Create separate dataset for each bin
data_bin<-list()
for (s in 1:length(bin)){
  data_bin[[s]]<-data_close_fdi[[2]] %>% 
    filter(Distance_bin==bin[s])
}

### Estimate effects for each bin
out_ancmt_bin<-out_active_bin<-
  rep(list(rep(list(0),length(dv_distance))),length(bin))
for (s in 1:length(bin)){
  
  for (k in 1:length(dv_distance)){
    
    m<-lm.cluster(data=data_bin[[s]],
                  #cluster the standard error in the survey cluster
                  cluster=data_bin[[s]]$twnvill,
                  # dv and status to projects
                  data_bin[[s]][,dv_distance[k]]~
                    D_close_active_fdi+D_close_ancmt_fdi
                  # control variables
                  +as.factor(Urban)+age+I(age^2)+
                    as.factor(gender)+as.factor(Edu)
                  # country fixed effects
                  +as.factor(country)
                  # survey round fixed effects
                  +as.factor(round)
    )
    
    
    out_ancmt_bin[[s]][[k]]<-c(coef(m)[3],summary(m)[3,2])
    out_active_bin[[s]][[k]]<-c(coef(m)[2],summary(m)[2,2])
    
  }
  
}

### Organize the output for plot
out_ancmt_bin<-bind_rows(out_ancmt_bin)
colnames(out_ancmt_bin)<-c("Estimate","SE")

out_active_bin<-bind_rows(out_active_bin)
colnames(out_active_bin)<-c("Estimate","SE")

# announced
out_ancmt_bin<-out_ancmt_bin %>% 
  mutate(DV=rep(c("Current economic conditions","Future economic conditions",
                  "Managing economy","Creating jobs","Presidential approval"),
                length(bin)),
         Distance_bin=sort(rep(bin_label,5)),
         Stage="Announced")

# active
out_active_bin<-out_active_bin %>% 
  mutate(DV=rep(c("Current economic conditions","Future economic conditions",
                  "Managing economy","Creating jobs","Presidential approval"),
                length(bin)),
         Distance_bin=sort(rep(bin_label,5)),
         Stage="Active")

# combine
out_effects_distance<-bind_rows(out_ancmt_bin,out_active_bin)
out_effects_distance<-out_effects_distance %>% 
  mutate(DV=factor(DV,levels=c("Current economic conditions",
                               "Future economic conditions",
                               "Managing economy","Creating jobs",
                               "Presidential approval")),
         Stage=factor(Stage,levels=c("Announced","Active")))

# Create plots: effects by distance ---------------------------------------

### Name of the dependent variables
dv_name<-out_effects_distance %>% pull(DV) %>% unique()

### For x axis breaks
skipper <-  function(n) {
  function(x) {
    x[c(TRUE, rep(FALSE, n - 1))]
  }
}

### The code to produce plots
p_distance<-list()
for (i in 1:length(dv_name)){
  
  p_distance[[i]]<-out_effects_distance %>% 
    filter(DV %in% dv_name[i]) %>% 
    ggplot()+
    geom_hline(yintercept = 0, color = "black", size = 1, alpha = 1, lty = 3) +
    geom_smooth(aes(x = as.numeric(Distance_bin),y = Estimate,color = Stage),
                method = "loess", se = F) +
    geom_pointrange(aes(x = Distance_bin, 
                        y = Estimate, 
                        ymin = Estimate - 1.96 * SE, 
                        ymax = Estimate + 1.96 * SE,
                        color = Stage,
                        pch=Stage),size = 1, fatten = 4) +
    scale_shape_manual(values = c(16, 16), 
                       guide = guide_legend(title = NULL,
                                            override.aes = 
                                              list(color = c("gray60","black"),
                                                   linetype = c(1, 1)))) +
    scale_color_manual(values = c("gray60","black"), guide = F) +
    scale_linetype_manual(values = c(1,1), guide = F) +
    scale_x_discrete(breaks = skipper(3)) +
    my_theme_1+
    labs(x = "Distance to Chinese FDI (km)",
         y= dv_name[i],
         caption = "95% confidence intervals around point estimates")+
    facet_wrap(~Stage,ncol=2)
}

# Plot Figure 1 -----------------------------------------------------------

### panel (a) of Figure 1
windows(width=600,height = 350)
p_distance[[1]]
ggsave(paste("./charts/",country,"_Figure_1_a.tiff",p_distance[[1]]))

### panel (b) of Figure 1
windows(width=600,height = 350)
p_distance[[2]]
ggsave(paste("./charts/",country,"_Figure_1_b.tiff",p_distance[[2]]))

# Plot Figure 2 -----------------------------------------------------------

### panel (a) of Figure 2
windows(width=600,height = 350)
p_distance[[3]]
ggsave(paste("./charts/",country,"_Figure_2_a.tiff",p_distance[[3]]))

### panel (b) of Figure 2
windows(width=600,height = 350)
p_distance[[4]]
ggsave(paste("./charts/",country,"_Figure_2_b.tiff",p_distance[[4]]))

### panel (c) of Figure 2
windows(width=600,height = 350)
p_distance[[5]]
ggsave(paste("./charts/",country,"_Figure_2_c.tiff",p_distance[[5]]))

# Estimate effects by time ------------------------------------------------

### Dependent variables for plotting the effects by time
dv_time<-c("PSP_econ_O","PSP_econin1yr_O",
           "SG_econ_O","SG_job_O","SG_prfpres_O")

### Estimate the effects by time
out_ancmt_time<-out_active_time<-list()
for (k in 1:length(dv_time)){
  
  m<-lm.cluster(data=data_close_fdi_time,
                #cluster the standard error in the survey cluster
                cluster=data_close_fdi_time$twnvill,
                # dv and status to projects
                data_close_fdi_time[,dv_time[k]]~Ancmt_1+Ancmt_2+Ancmt_3+
                  Active_1+Active_2+Active_3
                # control variables
                +as.factor(Urban)+age+I(age^2)+as.factor(gender)+as.factor(Edu)
                # country fixed effects
                +as.factor(country)
                # year fixed effects
                +as.factor(round)
  )
  
  out_ancmt_time[[k]]<-cbind(coef(m)[2:4],summary(m)[2:4,2])
  out_active_time[[k]]<-cbind(coef(m)[5:7],summary(m)[5:7,2])
  
}

### Organize the output for plot
out_ancmt_time<-do.call(rbind,out_ancmt_time)
colnames(out_ancmt_time)<-c("Estimate","SE")

out_active_time<-do.call(rbind,out_active_time)
colnames(out_active_time)<-c("Estimate","SE")

# announced
out_ancmt_time<-out_ancmt_time %>% 
  as_tibble() %>% 
  mutate(DV=c(rep("Current economic conditions",3),
              rep("Future economic conditions",3),
              rep("Managing economy",3),
              rep("Creating jobs",3),
              rep("Presidential approval",3)),
         Timeframe=rep(c("First year","Second year","Third year and above"),5),
         Stage="Announced")

# active
out_active_time<-out_active_time %>% 
  as_tibble() %>% 
  mutate(DV=c(rep("Current economic conditions",3),
              rep("Future economic conditions",3),
              rep("Managing economy",3),
              rep("Creating jobs",3),
              rep("Presidential approval",3)),
         Timeframe=rep(c("First year","Second year","Third year and above"),5),
         Stage="Active")

# combine
out_effects_time<-bind_rows(out_ancmt_time,out_active_time)
out_effects_time<-out_effects_time %>% 
  mutate(DV=factor(DV,levels=c("Current economic conditions",
                               "Future economic conditions",
                               "Managing economy","Creating jobs",
                               "Presidential approval"))
  )

# Plot Figure 3 -----------------------------------------------------------
windows(width=1000,height = 350)
out_effects_time %>% 
  ggplot()+
  geom_pointrange(aes(x=Timeframe,
                      y=Estimate,
                      ymin=Estimate-1.96*SE,
                      ymax=Estimate+1.96*SE,
                      color=Stage),size=1)+
  scale_color_manual(values=c("black","gray60"))+
  labs(x="Number of years after the project being announced or active",y="")+
  geom_hline(yintercept=0,lty=2)+
  scale_x_discrete(labels=c("1 yr.","2 yr.","3-5 yr."))+
  my_theme_2+
  facet_wrap(~DV,nrow=1)
ggsave(paste("./charts/",country,"_Figure_3.tiff", sep = ""))

# Compare FDI and aid: effects by time ------------------------------------

### FDI and aid dataset
data_time<-list(data_close_fdi_sub_time,data_close_aid_sub_time)

### Estimate the effects by time
out_ancmt_compare<-out_active_compare<-
  rep(list(rep(list(0),length(dv_time))),length(data_time))

for (j in 1:length(data_time)){
  
  for (k in 1:length(dv_time)){
    
    m<-lm.cluster(data=data_time[[j]],
                  #cluster the standard error in the survey cluster
                  cluster=data_time[[j]]$twnvill,
                  # dv and status to projects
                  data_time[[j]][,dv_time[k]]~Ancmt_1+Ancmt_2+Ancmt_3+
                    Active_1+Active_2+Active_3
                  # control variables
                  +as.factor(Urban)+age+I(age^2)+
                    as.factor(gender)+as.factor(Edu)
                  # country fixed effects
                  +as.factor(country)
                  # year fixed effectsd
                  +as.factor(round)
    )
    
    out_ancmt_compare[[j]][[k]]<-cbind(coef(m)[2:4],summary(m)[2:4,2])
    out_active_compare[[j]][[k]]<-cbind(coef(m)[5:7],summary(m)[5:7,2])
    
  }
  
}

### Organize the output for plot

# announced
for (j in 1:length(out_ancmt_compare)){
  
  out_ancmt_compare[[j]]<-do.call(rbind,out_ancmt_compare[[j]])
  colnames(out_ancmt_compare[[j]])<-c("Estimate","SE")
  out_ancmt_compare[[j]]<-out_ancmt_compare[[j]] %>% 
    as_tibble() %>% 
    mutate(DV=c(rep("Current economic conditions",3),
                rep("Future economic conditions",3),
                rep("Managing economy",3),
                rep("Creating jobs",3),
                rep("Presidential approval",3)),
           Timeframe=rep(c("First year","Second year",
                           "Third year and above"),5),
           Stage="Announced")
}
out_ancmt_compare<-bind_rows(out_ancmt_compare) %>% 
  mutate(Dataset=c(rep("Close to FDI",15),rep("Close to Aid", 15))
  )

# active
for (j in 1:length(out_active_compare)){
  
  out_active_compare[[j]]<-do.call(rbind,out_active_compare[[j]])
  colnames(out_active_compare[[j]])<-c("Estimate","SE")
  out_active_compare[[j]]<-out_active_compare[[j]] %>% 
    as_tibble() %>% 
    mutate(DV=c(rep("Current economic conditions",3),
                rep("Future economic conditions",3),
                rep("Managing economy",3),
                rep("Creating jobs",3),
                rep("Presidential approval",3)),
           Timeframe=rep(c("First year","Second year",
                           "Third year and above"),5),
           Stage="Active")
}
out_active_compare<-bind_rows(out_active_compare) %>% 
  mutate(Dataset=c(rep("Close to FDI",15),rep("Close to Aid", 15))
  )

# combine announced with active
out_effects_compare<-bind_rows(out_ancmt_compare,out_active_compare)
out_effects_compare<-out_effects_compare %>% 
  mutate(Timeframe=factor(Timeframe,
                          levels=c("First year","Second year",
                                   "Third year and above")),
         DV=factor(DV,levels=c("Current economic conditions",
                               "Future economic conditions",
                               "Managing economy","Creating jobs",
                               "Presidential approval")),
         Dataset=factor(Dataset,levels=c("Close to FDI","Close to Aid"))
  )


# Plot Figure 4 -----------------------------------------------------------
windows(width=1000,height = 500)
out_effects_compare %>% 
  ggplot()+
  geom_pointrange(aes(x=Timeframe,
                      y=Estimate,
                      ymin=Estimate-1.96*SE,
                      ymax=Estimate+1.96*SE,
                      color=Stage),size=1)+
  scale_color_manual(values=c("black","gray60"))+
  labs(x="Number of years after the project being announced or active",y="")+
  geom_hline(yintercept=0,lty=2)+
  scale_x_discrete(labels=c("1 yr.","2 yr.","3-5 yr."))+
  my_theme_2+
  facet_grid(Dataset~DV)

ggsave(paste("./charts/",country,"_Figure_4.tiff", sep = ""))



# }
