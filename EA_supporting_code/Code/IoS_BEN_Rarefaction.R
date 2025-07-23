###########################################################################
##
## Title: Species Rarefaction plots - Figure 7
##
###########################################################################
##
## Author: Owen Exeter
##
## Date: Tue Mar 05 10:10:20 2024
##
###########################################################################

library(tidyverse)
library(iNEXT)
library(ggplot2)
library(grid)
library(gridExtra)

# Rarefaction -------------------------------------------------------------

## read in BRUVS metadata
meta <- read.csv('C:/Users/oe247/OneDrive - University of Exeter/PhD/IoS/analysis/5_BEN/BRUVS_analysis/IOS_BEN_BRUVS_analysis/processed_data/IoS_BEN_metadata.csv')

## remove unnecessary columns 
meta2 <- meta[,c('GRTS_ID','Broadscale_habitat')]
meta2 <- unique(meta2[])
## reate a character ID for each deployment
meta2$ID <-paste("IOS_",meta2$GRTS_ID, sep="")

## subset lists of site IDs by habitat type
sediment_list <- subset(meta2,meta2$Broadscale_habitat == 'Subtidal sediments')
sediment_list <- as.list(sediment_list$ID)
seagrass_list <- subset(meta2,meta2$Broadscale_habitat == 'Subtidal seagrass')
seagrass_list <- as.list(seagrass_list$ID)
infra_list <- subset(meta2,meta2$Broadscale_habitat == 'Infralittoral rock')
infra_list <- as.list(infra_list$ID)
circa_list <- subset(meta2,meta2$Broadscale_habitat == 'Circalittoral rock')
circa_list <- as.list(circa_list$ID)

## read in primer style matrix of occurrence data by site
pres_ab <- read.csv('C:/Users/oe247/OneDrive - University of Exeter/PhD/IoS/analysis/5_BEN/BRUVS_analysis/IOS_BEN_BRUVS_analysis/processed_data/iNEXT_data_format.csv')
## turn species into presence/absence
pres_abs <- pres_ab
pres_abs[pres_abs>=1]<-1

## subset the presence abs. by habitat deployment ID list
sediment_format <- subset(pres_abs, select = names(pres_abs) %in% sediment_list)
sediment_format <- sediment_format[rowSums(sediment_format[])>0,]
seagrass_format <- subset(pres_abs, select = names(pres_abs) %in% seagrass_list)
seagrass_format <- seagrass_format[rowSums(seagrass_format[])>0,]
infra_format <- subset(pres_abs, select = names(pres_abs) %in% infra_list)
infra_format <- infra_format[rowSums(infra_format[])>0,]
circa_format <- subset(pres_abs, select = names(pres_abs) %in% circa_list)
circa_format <- circa_format[rowSums(circa_format[])>0,]

## format each matrix in iNEXT package
format <- as.incfreq(pres_abs)
format2 <- as.incfreq(seagrass_format)
format3 <- as.incfreq(infra_format)
format4 <- as.incfreq(circa_format)
format1 <- as.incfreq(sediment_format)

## merge all formatted data
data <- list(format1,format2,format3,format4)

## rename elements
names(data) <- c('Subtidal sediments','Subtidal seagrass','Infralittoral rock','Circalittoral rock')

#### plot
## convert data for iNEXT
str(data)
t <- seq(1, 1500, by=10)
out.inc <- iNEXT(data, q=0, datatype="incidence_freq", size=t)
##summary stata
out.inc$AsyEst
options(max.print=5000)
out.inc$iNextEst$size_based

guide = guide_legend(reverse = TRUE)

## ggplot
p1 <- ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  scale_x_continuous(expand = c(0, 0), limits = c(0,301)) +
  scale_y_continuous(expand = c(0, 0))+
  scale_shape_manual(values=c(19,19,19,19), guide = guide_legend(reverse = TRUE))+
  scale_fill_manual(values=c('#003C67FF','#7AA6DCFF','#CD534CFF','#EFC000FF'), guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values=c('#003C67FF','#7AA6DCFF','#CD534CFF','#EFC000FF')) +
  labs(x = "")+
  theme_bw(base_size = 18) +
  theme(legend.box = "vertical",axis.text = element_text(size = 14),axis.title.y = element_blank(),legend.text = element_text(size = 16))
p1 

#### repeat using higher habitat classifications
## assign a new broader habitat category
meta3 <- meta2
meta3$Broadscale_habitat <- as.factor(ifelse(meta3$Broadscale_habitat == 'Circalittoral rock' | meta3$Broadscale_habitat ==  'Infralittoral rock','Circalittoral','Sediment'))

## subset lists of site IDs by habitat type
sediments_list <- subset(meta3,meta3$Broadscale_habitat == 'Sediment')
sediments_list <- as.list(sediments_list$ID)
reef_list <- subset(meta3,meta3$Broadscale_habitat == 'Circalittoral')
reef_list <- as.list(reef_list$ID)

## susbet the presence abscne by habitat deployment ID list
sediments_format <- subset(pres_abs, select = names(pres_abs) %in% sediments_list)
sediments_format <- sediments_format[rowSums(sediments_format[])>0,]
reef_format <- subset(pres_abs, select = names(pres_abs) %in% reef_list)
reef_format <- reef_format[rowSums(reef_format[])>0,]

## format each matrix for sure in iNEXT package
format5 <- as.incfreq(sediments_format)
format6 <- as.incfreq(reef_format)

## merge all formatted data
data2 <- list(format5,format6)

## rename elements
names(data2) <- c('Sediments','Reefs')

#### plot
## convert data for iNEXT
str(data2)
t <- seq(1, 3000, by=10)
out.inc <- iNEXT(data2, q=0, datatype="incidence_freq", size=t)
##summary stats
out.inc$AsyEst
options(max.print=7000)
out.inc$iNextEst$size_based
guide = guide_legend(reverse = TRUE)

## ggplot
p2 <- ggiNEXT(out.inc, type=1, color.var="Assemblage") +
  scale_x_continuous(expand = c(0, 0), limits = c(0,301)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,90))+
  scale_shape_manual(values=c(19,19,19,19), guide = guide_legend(reverse = TRUE))+
  scale_fill_manual(values=c("#077DAA","orange"), guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values=c("#077DAA","orange")) +
  labs(x = "")+
  theme_bw(base_size = 18) +
  theme(legend.box = "vertical",axis.text = element_text(size = 14),axis.title.y = element_blank(),legend.text = element_text(size = 18))
p2 


#### repeat for all data aggregated
## convert data for iNEXT
str(format)
t2 <- seq(1, 1500, by=10)
out.inc2 <- iNEXT(format, q=c(0), datatype="incidence_freq", size=t2, conf=0.95)

## stats
out.inc2$AsyEst
options(max.print=2500)
out.inc2$iNextEst$size_based

## ggplot
p3 <- ggiNEXT(out.inc2, type=1,se=TRUE) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,502)) +
  scale_y_continuous(expand = c(0, 0))+
  scale_shape_manual(values=c(19))+
  scale_fill_manual(values=c('black')) +
  scale_color_manual(values=c('black')) +
  labs(y = "Species richness (n.)", x = "Sampling units (n.)")+
  theme_bw(base_size = 18) +
  theme(legend.position ="none",axis.title = element_text(size = 18),axis.text = element_text(size = 14))
p3

## merge plots
p4 <- grid.arrange(p3, p2, ncol=2)
p4

##save
ggsave("C:/Users/oe247/OneDrive - University of Exeter/PhD/IoS/analysis/5_BEN/BRUVS_analysis/IOS_BEN_BRUVS_analysis/plots/Rarefaction_cures.tiff", plot = p4, dpi=600, unit = "mm", height = 150, width = 350)






