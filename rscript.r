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
##Rename 'Citation' column to 'Name' for clarity
opioid_df <- opioid_df %>% 
  rename(Name = Citation)
##Remove commas and change to numeric for sample size
opioid_df$Sample.Size <- as.numeric(gsub(",","",opioid_df$Sample.Size))
##Data frame for only Iran
iran_df<- opioid_df %>% 
  filter(Location=="Iran")
iran_df$Sample.Size <- as.numeric(gsub(",","",iran_df$Sample.Size))
##Dataframe for only USA
usa_df<-opioid_df %>% 
  filter(Location=="USA")
usa_df$Sample.Size <- as.numeric(gsub(",","",usa_df$Sample.Size))
##BASIC QUESTIONS_______________________________________________________________

##YEAR##
##year range
year_range<-range(opioid_df$Year)
##SAMPLE SIZE ##
samp_range<- range(opioid_df$Sample.Size, na.rm = TRUE)
##largest sample location
big_samp_place<- opioid_df %>% 
  filter(Sample.Size=="736") %>% 
  pull(Location)

##AGE##
age_lowest<- opioid_df %>% 
  pull(Age.low)
youngest<-min(age_lowest, na.rm = TRUE)

age_highest<- opioid_df %>% 
  pull(Age.high)
oldest<-max(age_highest, na.rm = TRUE)
##TREATMENT##
##Types of treatment
treatments<- opioid_df %>% 
  pull(Treatment) %>% 
  unique()
##Most common treatment type A: Pharmacological, with 11 studies. Combo with 5 and 
##mindfull with 2
combo<- opioid_df %>% 
  filter(grepl('Combo',Treatment)) %>% 
  nrow()
pharmalogical<-opioid_df %>% 
  filter(grepl('Pharma',Treatment)) %>% 
  nrow()+1 ##+1 because UROD is pharmacological but also a new treatment style 
          ##so it should be included as both its own thing and as a pharma treatment
mind<-opioid_df %>% 
  filter(grepl('mindfull',Treatment)) %>% 
  nrow()

##Most common drug used A: Methadone, used in 12 studies. Buprenorphin used in 8, 
##Nalaxon in 2, Naltrexon in 2 and clonidine in 1. Adds up to more than 18 because 
##some studies used more than one drug
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

drug_freq_table<- tibble("Drug" = c("Methadone","Buprenorphin","Naloxone","Naltrexon","Clonidine"),
                             "Freq"=c(methadone,buprenorphin,Naloxone,Naltrexon,Clonidine))

##Number of different drugs used (done manually because I couldn't figure it out)
num_drugs<- 5
##Number of studies that used only pharmacological treatments
print(pharmalogical)
##Number of studies that used combination of pharmacological and other treatments
print(combo)

##most common drug in USA and then in Iran
methadone_usa<- usa_df %>% 
  filter(grepl('methadone',If.pharma)) %>% 
  nrow()
buprenorphin_usa<- usa_df %>% 
  filter(grepl('Buprenorphin',If.pharma)) %>% 
  nrow()
Naloxone_usa<-usa_df %>% 
  filter(grepl('Naloxone',If.pharma)) %>% 
  nrow()
Naltrexon_usa<-usa_df %>% 
  filter(grepl('Naltrexon',If.pharma)) %>% 
  nrow()
Clonidine_usa<-usa_df %>% 
  filter(grepl('Clonidine',If.pharma)) %>% 
  nrow()
drug_freq_table_usa<- tibble("Drug" = c("Methadone","Buprenorphin","Naloxone","Naltrexon","Clonidine"),
                         "Freq"=c(methadone_usa,buprenorphin_usa,Naloxone_usa,Naltrexon_usa,
                                  Clonidine_usa))
print(drug_freq_table_usa)

methadone_iran<- iran_df %>% 
  filter(grepl('methadone',If.pharma)) %>% 
  nrow()
buprenorphin_iran<- iran_df %>% 
  filter(grepl('Buprenorphin',If.pharma)) %>% 
  nrow()
Naloxone_iran<-iran_df %>% 
  filter(grepl('Naloxone',If.pharma)) %>% 
  nrow()
Naltrexon_iran<-iran_df %>% 
  filter(grepl('Naltrexon',If.pharma)) %>% 
  nrow()
Clonidine_iran<-iran_df %>% 
  filter(grepl('Clonidine',If.pharma)) %>% 
  nrow()
drug_freq_table_iran<- tibble("Drug" = c("Methadone","Buprenorphin","Naloxone","Naltrexon","Clonidine"),
                             "Freq"=c(methadone_iran,buprenorphin_iran,Naloxone_iran,Naltrexon_iran,
                                      Clonidine_iran))
print(drug_freq_table_iran)
##Most common treatment in USA and then in Iran
combo_usa<- usa_df %>% 
  filter(grepl('Combo',Treatment)) %>% 
  nrow()
pharmalogical_usa<-usa_df %>% 
  filter(grepl('Pharma',Treatment)) %>% 
  nrow() 
mind_usa<-usa_df %>% 
  filter(grepl('mindfull',Treatment)) %>% 
  nrow()

treatment_freq_table_usa<- tibble("Treatment" = c("Combonation","Pharmacological","Mindfullness"),
                              "Freq"=c(combo_usa,pharmalogical_usa,mind_usa))
print(treatment_freq_table_usa)

combo_iran<- iran_df %>% 
  filter(grepl('Combo',Treatment)) %>% 
  nrow()
pharmalogical_iran<-iran_df %>% 
  filter(grepl('Pharma',Treatment)) %>% 
  nrow() 
mind_iran<-iran_df %>% 
  filter(grepl('mindfull',Treatment)) %>% 
  nrow()
treatment_freq_table_iran<- tibble("Treatment" = c("Combonation","Pharmacological","Mindfullness"),
                                  "Freq"=c(combo_iran,pharmalogical_iran,mind_iran))
print(treatment_freq_table_iran)
##LOCATION##
##Location that used only pharmacological treatments the most A:USA
usa_pharma_num<- usa_df %>% 
  filter(Treatment=="Pharma") %>% 
  nrow()
iran_pharma_num<-iran_df %>% 
  filter(Treatment=="Pharma") %>% 
  nrow()
##EFFICACY##
##efficacy of pharma____
##Create table with new yes, no or inconclusive results for effectivness
eff_df<-opioid_df
eff_df$effective<- c("inconclusive","inconclusive","yes","yes","yes","yes","yes","yes","inconclusive",
                     "yes","inconclusive","yes","yes","yes","no","inconclusive","yes","yes")
##Number of total effective treatments
num_effective<- eff_df %>% 
  filter(effective=="yes") %>% 
  nrow()
##Number of effective for each treatment type
pharma_table<-eff_df %>% 
  filter(Treatment=="Pharma") %>% 
  filter(effective=="yes")
combo_table<-eff_df %>% 
  filter(Treatment=="Combonation") %>% 
  filter(effective=="yes")
mindfull_table<-eff_df %>% 
  filter(Treatment=="mindfull") %>% 
  filter(effective=="yes")
##Number of effective for each drug
meth_eff<-pharma_table %>% 
  filter(grepl('methadone',If.pharma)) %>% 
  nrow()
bup_eff<-pharma_table %>% 
  filter(grepl("Buprenorphin",If.pharma)) %>% 
  nrow()
naloxone_eff<-pharma_table %>% 
  filter(grepl('Naloxone',If.pharma)) %>% 
  nrow()
##Number that were inconclusive
inconclusive_df<-eff_df %>% 
  filter(grepl('inconclusive',effective))
##number that used pharma and were incon
pharma_incon<- inconclusive_df %>% 
  filter(grepl('Pharma',Treatment)) %>% 
  nrow()
combo_incon<- inconclusive_df %>% 
  filter(grepl('Combonation',Treatment)) %>% 
  nrow()
View(inconclusive_df)
##Number that were not effective
not_eff<-eff_df %>% 
  filter(grepl('no',effective))

##Number effective in Iran
iran_eff<-eff_df %>% 
  filter(Location=="Iran") %>% 
  filter(grepl('yes',effective)) %>% 
  nrow()
iran_eff_df<- eff_df %>% 
  filter(Location=="Iran")
iran_yes<-iran_eff_df %>% 
  filter(grepl('yes',effective)) #%>% 
  pull(Treatment)
##Number effective in usa
  usa_eff<-eff_df %>% 
    filter(Location=="USA") %>% 
    filter(grepl('yes',effective))# %>% 
    nrow()
  view(usa_eff)
##IRAN SPECIFIC QUESTIONS##_____________________________________________________
##Number of participants from iran
participants_iran<-iran_df %>% 
  summarise(sum(Sample.Size))
##Ages from iran
View(iran_df)

##USA SPECIFIC QUESTIONS##______________________________________________________
##Participants from usa
participants_usa<-usa_df %>% 
  summarise(sum(Sample.Size, na.rm = TRUE))
print(participants_usa)

#ages usa
View(usa_df)
