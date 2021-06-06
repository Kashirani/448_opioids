##here is the code for our systematic review regarding opioid addiction treatment 
##in Iran and the United states

##SET UP________________________________________________________________________
##Library in packages
library("dplyr")
library("tidyverse")
##Basic Data frame assignment
opioid_df<-read.csv("448_values.csv")
##Get rid of "n" and change to NA
opioid_df[opioid_df=="n"]<- NA
##Data frame for only Iran
iran_df<- opioid_df %>% 
  filter(Location=="Iran")
##Dataframe for only USA
usa_df<-opioid_df %>% 
  filter(Location=="USA")
##BASIC QUESTIONS_______________________________________________________________

##YEAR##
##year range
year_range<-range(opioid_df$Year)

##SAMPLE SIZE ##
sample_range<-range(opioid_df$Sample.Size, na.rm = TRUE)
##largest sample location
big_samp_place<- opioid_df %>% 
  filter(Sample.Size=="736") %>% 
  pull(Location)

##TREATMENT##
##Types of treatment
treatments<- opioid_df %>% 
  pull(Treatment) %>% 
  unique()
##Most common treatment type

##Most common drug used
methadone<- opioid_df %>% 
  filter(grepl('methadone',If.pharma)) %>% 
  nrow()
buprenorphin<- opioid_df %>% 
  filter(grepl('Buprenorphin',If.pharma)) %>% 
  nrow()
Naloxone<-opioid_df %>% 
  filter(grepl('Naloxone',If.pharma)) %>% 
  nrow()
Naltrexon<-opioid_df %>% 
  filter(grepl('Naltrexon',If.pharma)) %>% 
  nrow()
Clonidine<-opioid_df %>% 
  filter(grepl('Clonidine',If.pharma)) %>% 
  nrow()

drug_frequency_table<-data.frame("Drug"=("Methadone","Buprenorphin","Naloxone","Naltrexon","Clonidine"),
                                 "Frequency"= (methadone, buprenorphin, Naloxone, Naltrexon, Clonidine))
##Number of different drugs used (done manually because I couldn't figure it out)
num_drugs<- 5
 
##Number of studies that used only pharmacological treatments
##Number of studies that used combination of pharmacological and other treatments

##LOCATION##
##Location that used only pharmacological treatments the most A:USA
usa_pharma_num<- usa_df %>% 
  filter(Treatment=="Pharma") %>% 
  nrow()
iran_pharma_num<-iran_df %>% 
  filter(Treatment=="Pharma") %>% 
  nrow()


 





