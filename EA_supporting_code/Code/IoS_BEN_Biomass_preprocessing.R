###########################################################################
##
## Title: Biomass
##
###########################################################################
##
## Author: Owen Exeter
##
## Date: Tue Mar 12 11:47:50 2024
##
###########################################################################

library(tidyverse)
library(terra)

#### read in length data
Lengths_species <- read.csv('~/IOS_benchmark_BRUVS_2025/EA_supporting_code/Data/IoS_BEN_Lengths.csv')

## calculate a mean length per species per sample
Mean_length_species <- Lengths_species %>%
  group_by(GRTS_ID,Species) %>%
  summarise(Mean_length = mean(Length))

#### read in Obs data
MaxN_biomass<-read.csv('~/IOS_benchmark_BRUVS_2025/EA_supporting_code/Data/IoS_BEN_Obs_Biomass.csv')

## join mean lengths and observations datasets
merged_df <- Mean_length_species %>%
  right_join(MaxN_biomass, by=c('GRTS_ID', 'Species'))
merged_df$source <- 'Measured'

## remove observations which couldn't be ID to a minimum of family level
merged_df <- merged_df %>% 
  drop_na(Family)

## calculate a habitat species mean for missing values
Mean_length_habitat <- Lengths_species %>%
  group_by(Broadscale_habitat,Species) %>%
  summarise(Mean_length = mean(Length))

## subset NA values
merged_df_no_NA <- na.omit(merged_df)
merged_df_NA <- merged_df[!complete.cases(merged_df), ]
merged_df_NA <- merged_df_NA[,c('deployment_ID','Species','Family','Genus','Code','Species_latin','MaxN','GRTS_ID','Broadscale_habitat')]
  
## join mean lengths to NA dataset
merged_df_2 <- Mean_length_habitat %>%
  right_join(merged_df_NA, by=c('Broadscale_habitat', 'Species'))

## subset any further NA values
merged_df_2_no_NA <- na.omit(merged_df_2)
merged_df_2_no_NA$source <- 'Proxy_habitat'

## join habitat level proxy lengths to the true lengths
Species_with_lengths <- rbind(merged_df_no_NA,merged_df_2_no_NA)

## calculate a total study area species mean for remaining missing values
merged_df_2_NA <- merged_df_2[!complete.cases(merged_df_2), ] 
merged_df_2_NA <- merged_df_2_NA[,c('deployment_ID','Species','Family','Genus','Code','Species_latin','MaxN','GRTS_ID','Broadscale_habitat')]

## fix typo error in sandeel family name
merged_df_2_NA[merged_df_2_NA == 'Hyperoplus lanceolatus'] <- 'Ammodytidae'

## calculate a total study area species mean for remaining missing values
Mean_length_total <- Lengths_species %>%
  group_by(Species) %>%
  summarise(Mean_length = mean(Length))

## join mean lengths to remaining NA dataset
merged_df_3 <- Mean_length_total %>%
  right_join(merged_df_2_NA, by=c('Species'))
merged_df_3_no_NA <- na.omit(merged_df_3)
merged_df_3_no_NA$source <- 'Proxy_study'

## join total study area proxy lengths to the habitat and true lengths
Species_with_lengths <- rbind(Species_with_lengths,merged_df_3_no_NA)

## calculate a total study area family mean for remaining missing values  - REMOVED THIS STEP
Mean_length_family <- Lengths_species %>%
  group_by(Family) %>%
  summarise(Mean_length = mean(Length))
merged_df_3_NA <- merged_df_3[!complete.cases(merged_df_3), ] 
merged_df_3_NA <- merged_df_3_NA[,c('deployment_ID','Species','Family','Genus','Code','Species_latin','MaxN','GRTS_ID','Broadscale_habitat')]

## join mean lengths to remaining NA dataset
merged_df_4 <- Mean_length_family %>%
  right_join(merged_df_3_NA, by=c('Family'))
merged_df_4_no_NA <- na.omit(merged_df_4)

#merged_df_4_no_NA <- merged_df_4 %>% drop_na(Mean_length)
merged_df_4_no_NA$source <- 'Proxy_study_family'

## join family proxy lengths to the study area, habitat and true lengths - REMOVED THIS STEP
Species_with_lengths <- rbind(Species_with_lengths,merged_df_4_no_NA)

merged_df_4_NA <- merged_df_4[!complete.cases(merged_df_4$Mean_length), ] 

## subset the remaining species of interest (i.e no marine mammals etc)
remaining <- as.list(c('Gobiidae','Pholidae','Callionymidae','Blenniidae'))
proxy_lengths <- subset(merged_df_4_NA,merged_df_4_NA$Family %in% remaining)

print(unique(proxy_lengths$Species))

## insert the proxy lengths from FishBase
proxy_lengths$Mean_length <- ifelse(proxy_lengths$Family ==  'Blenniidae', 175,
                     ifelse(proxy_lengths$Family == 'Callionymidae', 150,
                            ifelse(proxy_lengths$Family ==  'Pholidae', 233, 
                                   ifelse(proxy_lengths$Family == 'Gobiidae'& proxy_lengths$Species == 'Two_spotted_goby', 37.5 , 73.5))))

proxy_lengths$source <- 'Proxy_fishbase'

## merge fishbase proxy lengths with all other lengths
Species_with_lengths2 <- rbind(Species_with_lengths,proxy_lengths)

## observations without a latin name needed to join to FishBase ratios use common
Species_with_lengths2 <- Species_with_lengths2 %>% mutate(Species_latin = ifelse(is.na(Species_latin), 0, Species_latin))
Species_with_lengths2$Species_latin <- ifelse(Species_with_lengths2$Species_latin == 0 ,Species_with_lengths2$Species, Species_with_lengths2$Species_latin)

##---------------------------------------------------------------------------------##

#### read in the species length-weight relationships from fishbase
LW_ratios <- read.csv('~/IOS_benchmark_BRUVS_2025/EA_supporting_code/Data/Fishbase_lengthweight_ratios.csv')

## merge fishbase proxy lengths with all other lengths
Species_LW <- LW_ratios %>%
  right_join(Species_with_lengths2, by=c('Species_latin'))

#### calculate species biomass by sample
## convert mm to cm lengths
Species_LW$Length_cm <- Species_LW$Mean_length/10
## use Fishbase LW ratios
Species_LW$Biomass <- Species_LW$a*(Species_LW$Length_cm^Species_LW$b)
## multiply by MaxN of individuals
Species_LW$Biomass_MaxN <- Species_LW$MaxN*Species_LW$Biomass

## calculate total biomass per sample
Biomass <- Species_LW %>%
  group_by(GRTS_ID) %>%
  summarise(Total_biomass = sum(Biomass_MaxN))
Biomass$Total_biomass_kg <- Biomass$Total_biomass/1000

## export processed biomass values
outfile <- '~/IOS_benchmark_BRUVS_2025/EA_supporting_code/Data/IOS_BEN_biomass.csv'
write.csv(Biomass,outfile)

##---------------------------------------------------------------------------------##
