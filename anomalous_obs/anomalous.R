library(rnpn)
library(dplyr)
library(readr)
library(ggplot2)
rm(list=ls())

Dir <- "~/Documents/My Files/USA-NPN/Data/Analysis/R_default/Groundhog"
setwd(Dir)

#This code identifies anomalous observations in the current year compared to the long term record (2009 to 2022)
#Individual phenometrics for the current year is downloaded, and then prior data are downloaded for the species observed this year
#This year's observations are then compared to the distribution of the prior record.
#Early (lower than 5th percentile) and outliers (lower than 1.5x the interquartile range below the 25th percentile) are flagged and plotted

#Developed by Theresa Crimmins and Alyssa Rosemartin

#species <- npn_species()
#Phenophase and phenoclass lookup info
#http://www.usanpn.org/npn_portal/phenophases/getPhenophases.xml
#371 breaking leaf buds*
#483 leaves
#500 flowers/flower buds
#501 open flowers*
#482 initial growth (forb)*

#Pheno classes
# 1- initial shoot leaf growth (Blb)*
# 3 - leaves
# 6 - flowers/flower buds
# 7 - open flowers*

#*indicates the 2023 dataset as of Feb 13th has data for these phenophases/pheno classes - 
#if leaf or flower data shows up, would need to add plots

#download all ind pmetrics so far for the current year, for leafing and flowering (pheno classes 1 and 7)
df_current <- npn_download_individual_phenometrics(
  request_source = 'Alyssa', 
  years = c(2023), 
  #phenophase_ids = c(371,373,482,492,201,205,210)
  pheno_class_ids = c(1,3,6,7),
  additional_fields = c("Observed_Status_Conflict_Flag", "species_functional_type")
)

write_csv(df_current, '2023_flowering_leafing.csv')
df_current <- (read.csv('2023_flowering_leafing.csv'))

#limit to deciduous trees and forbs
df_current1 <- subset(df_current, species_functional_type == "Forb" | species_functional_type == "Deciduous broadleaf" ) 
  
spp_list <- paste0(unique(df_current1$species_id, collapse=",")) 

#download the prior 11 years of data, for the species represented in the current year dataset
df_prior <- npn_download_individual_phenometrics(
  request_source = 'Alyssa', 
  years = c(2009:2022), 
  species_ids = c(spp_list),
  pheno_class_ids = c(1,7),
  additional_fields = c("Observed_Status_Conflict_Flag", "species_functional_type")
)

write_csv(df_prior, '2012-2022_year_flowering_leafing.csv')
df_prior <- (read.csv('2012-2022_year_flowering_leafing.csv'))

#combine current and prior years of data
df_all <- rbind(df_current1, df_prior)

#require a prior no, no conflict, and at least 10 years of data (prior to 2023)
df_all1 <- df_all %>%
  subset(numdays_since_prior_no != -9999 & observed_status_conflict_flag == -9999) %>%
  group_by(state, phenophase_id, species_id) %>% 
  filter(n_distinct(first_yes_year) > 10) %>% 
  ungroup() 
   
#identify quantiles in the prior record, by state and phenophase
quantiles <- as.data.frame(df_all1 %>%
                             subset(first_yes_year != 2023) %>%
                             group_by(state, phenophase_id, species_id) %>%
                             summarize(QFive = quantile(first_yes_doy, .05),
                                       QTwentyFive = quantile(first_yes_doy, .25), 
                                       QFifty = quantile(first_yes_doy, .50),
                                       QSeventyFive = quantile(first_yes_doy, .75),
                                       QNinetyFive = quantile(first_yes_doy, .95),
                                       IQR = IQR(first_yes_doy)))

#join quantiles onto dataset
df_all1 <- inner_join(df_all1, quantiles, by=c('state'='state', 'phenophase_id'='phenophase_id', 'species_id' = 'species_id'))

#add state_species, and outlier23 and early23 binary fields for use in data viz
df_all1$state_species <- paste0(df_all1$state, sep = '_', df_all1$common_name)
df_all1$outlier_2023 <- ifelse(df_all1$first_yes_year == 2023 & df_all1$first_yes_doy< (df_all1$QTwentyFive - 1.5*df_all1$IQR), 1, 0)
df_all1$early_2023 <- ifelse(df_all1$first_yes_year == 2023 & df_all1$first_yes_doy < df_all1$QFive, 1, 0)
df_all1$difference <-  df_all1$first_yes_doy - df_all1$QFifty

#create reference tables for finding the appropriate boxplot data for the prior record
leaf_state_spp <- subset(df_all1,early_2023==1 & pheno_class_id == 1, select=unique("state_species"))
bloom_state_spp <- subset(df_all1,early_2023==1 & pheno_class_id == 7, select=unique("state_species"))

#write csv with data filtered to records where there is a 2023 outlier or early record
write_csv(subset(df_all1, state_species %in% leaf_state_spp$state_species | state_species %in% bloom_state_spp$state_species), 
          '2023_vs_2009-2022_flowering_leafing_early&outlier.csv')

#box plot with points for current year - leaf
p = ggplot() +
  geom_boxplot(data = subset(df_all1, state_species %in% leaf_state_spp$state_species & pheno_class_id == 1 & first_yes_year != 2023), aes(x=state_species, y=first_yes_doy)) +
  geom_point(data = subset(df_all1, early_2023 == 1 & pheno_class_id == 1 ),  #earlier than 95% of records
             aes(x=state_species, y=first_yes_doy, size = 1), color="pink") +
  geom_point(data = subset(df_all1, outlier_2023 == 1 & pheno_class_id == 1 ),  #Tukey outliers
             aes(x=state_species, y=first_yes_doy, size = 1), color="red") +
  ggtitle("Early (pink) and outlier (red) records for Breaking Leaf Buds/Initial Growth, 2023", 
          subtitle = "Boxplot for 2009-2022 Distribution") + 
  theme(axis.text.x = element_text(angle = 90))
plot(p)  

#box plot with points for current year - bloom
p = ggplot() +
  geom_boxplot(data = subset(df_all1, state_species %in% bloom_state_spp$state_species & pheno_class_id == 7 & first_yes_year != 2023), 
              aes(x=state_species, y=first_yes_doy)) +
  geom_point(data = subset(df_all1, early_2023 == 1 & pheno_class_id == 7 ),  #earlier than 95% of records
              aes(x=state_species, y=first_yes_doy, size = 1), color="pink") +
  geom_point(data = subset(df_all1, outlier_2023 == 1 & pheno_class_id == 7),  #Tukey outliers
              aes(x=state_species, y=first_yes_doy, size = 1), color="red") +
  ggtitle("Early (pink) and outlier (red) records for Open Flowers, 2023", 
          subtitle = "Boxplot for 2009-2022 Distribution") + 
  theme(axis.text.x = element_text(angle = 90))
plot(p)  



