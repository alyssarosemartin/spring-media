library(rnpn)
library(dplyr)
library(readr)
library(ggplot2)
rm(list=ls())

Dir <- "~/Documents/My Files/USA-NPN/Data/Analysis/R_default/Groundhog"
setwd(Dir)

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

#remove records with no prior no, with status conflict and limit to deciduous trees and forbs
df_current1 <- df_current  %>%
  subset(numdays_since_prior_no != -9999)  %>%
  subset(observed_status_conflict_flag == -9999)  %>%
  subset(species_functional_type == "Forb" | species_functional_type == "Deciduous broadleaf" ) 

#get a list of spp ids for those spp observed so far this year  
species <- paste0(unique(df_current1$species_id, collapse=","))

#download the prior 11 years of data, for the species represented in the current year dataset
df_prior <- npn_download_individual_phenometrics(
  request_source = 'Alyssa', 
  years = c(2012:2022), 
  species_ids = c(paste0(species)),
  pheno_class_ids = c(1,7),
  additional_fields = c("Observed_Status_Conflict_Flag", "species_functional_type")
)

write_csv(df_prior, '2012-2022_year_flowering_leafing.csv')
df_prior <- (read.csv('2012-2022_year_flowering_leafing.csv'))

#require a prior no, no conflict, and choose only records where there is a 10y record for phenoclass/state/spp
df_prior <- subset(df_prior, numdays_since_prior_no != -9999 & observed_status_conflict_flag == -9999)
df_prior_10Y <- df_prior %>%
  group_by(df_prior$state, df_prior$phenophase_id, df_prior$species_id) %>% 
  filter(n_distinct(first_yes_year) > 9) %>% 
  ungroup()

#identify quantiles in the 10 year record, by state and leafing v flowering
quantiles <- as.data.frame(df_prior_10Y %>%
                             group_by(state, phenophase_id, species_id) %>%
                             summarize(QFive = quantile(first_yes_doy, .05),
                                       QTwentyFive = quantile(first_yes_doy, .25), 
                                       QSeventyFive = quantile(first_yes_doy, .75),
                                       QNinetyFive = quantile(first_yes_doy, .95),
                                       IQR = IQR(first_yes_doy)))

#join quantiles onto current year dataset
df_current1_Q <- inner_join(df_current1, quantiles, by=c('state'='state', 'phenophase_id'='phenophase_id', 'species_id' = 'species_id'))

#Code learning note - a left join keeps all rows, but we can't use most, bc no matching quartile, so we want the inner in this case
#df_current1_Qleft <- left_join(df_current1, quantiles, by=c('state'='state', 'pheno_class_id'='pheno_class_id', 'species_id' = 'species_id'))

df_current1_Q$state_species <- 
  paste0(df_current1_Q$state, sep = '_', df_current1_Q$common_name)

#limit to a dataframe where all records are either Tukey outliers OR earlier than 95% of records
df <- df_current1_Q %>%
  subset(first_yes_doy < (QTwentyFive - 1.5*IQR) | first_yes_doy < QFive)

#plot blb, and blb lilacs and initial growth - phenoclass 1
p = ggplot() +
    geom_point(data = subset(df, first_yes_doy < QFive & pheno_class_id == 1),  #earlier than 95% of records
                aes(x=state_species, y=first_yes_doy, size = 1), color="pink") +
    geom_point(data = subset(df, first_yes_doy < (QTwentyFive - 1.5*IQR) & pheno_class_id == 1),  #Tukey outliers
                aes(x=state_species, y=first_yes_doy, size = 1), color="red") +
    geom_point(data = subset(df, pheno_class_id == 1),  #lower quartile
                aes(x=state_species, y=QTwentyFive, size = 1), color="blue") +
    geom_point(data = subset(df, pheno_class_id == 1),  #upper quartile
               aes(x=state_species, y=QSeventyFive, size = 1), color="blue") +
    ggtitle("Early (pink) and outlier (red) records for Initital Shoot/Leaf Growth (BLB), 2023")
plot(p) 

#plot open flowers - phenoclass 7
p = ggplot() +
  geom_point(data = subset(df, first_yes_doy < QFive & pheno_class_id == 7),  #earlier than 95% of records
             aes(x=state_species, y=first_yes_doy, size = 1), color="pink") +
  geom_point(data = subset(df, first_yes_doy < (QTwentyFive - 1.5*IQR) & pheno_class_id == 7),  #Tukey outliers
             aes(x=state_species, y=first_yes_doy, size = 1), color="red") +
  geom_point(data = subset(df, pheno_class_id == 7),  #lower quartile
             aes(x=state_species, y=QTwentyFive, size = 1), color="blue") +
  geom_point(data = subset(df, pheno_class_id == 7),  #upper quartile
             aes(x=state_species, y=QSeventyFive, size = 1), color="blue") +
  ggtitle("Early (pink) and outlier (red) records for Open Flowers, 2023")
plot(p) 

