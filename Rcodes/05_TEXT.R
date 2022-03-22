## This script gives the values presented in the material and method section

rm(list = ls())


# Functions ---------
library(dplyr)

# Load data ---------
df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")
df <- df[,-c(1:2)]

## Method section ------

## Study design
summary(df$SemiNatural)

## field characteristics
summary(df$VineVariety)

## vegetation management
df %>% group_by(Organic) %>% summarize(mean(TillageFreq, na.rm = TRUE), sd(TillageFreq, na.rm = TRUE))
summary(df$TillageFreq)
table(df$Organic, df$TillageFreq)
table(df$Organic, df$InterRowManagement)

df %>% group_by(Organic) %>% summarize(mean(MowingFreq, na.rm = TRUE))

df %>% group_by(Organic) %>% summarize(mean(SprayingFreq_Herbicide, na.rm = TRUE))
summary(df$SprayingFreq_Herbicide)

## chemical inputs
df %>% group_by(Organic) %>% summarize(mean(SprayingFreq, na.rm = TRUE))
df %>% group_by(Organic) %>% summarize(mean(SprayingFreq_Fungicide, na.rm = TRUE))
df %>% group_by(Organic) %>% summarize(mean(SprayingFreq_Insecticide, na.rm = TRUE))
table(df$Organic, df$SprayingFreq_Insecticide)

## fungicides
df %>% group_by(Organic) %>% summarize(mean(NoAI, na.rm = TRUE))
df %>% group_by(Organic) %>% summarize(mean(NoAI_Herbicide, na.rm = TRUE))
df %>% group_by(Organic) %>% summarize(mean(NoAI_Insecticide, na.rm = TRUE))
df %>% group_by(Organic) %>% summarize(mean(NoAI_Fungicide, na.rm = TRUE))

# landscape
summary(df$Forests)
summary(df$Grasslands)
summary(df$HedgeRows)
summary(df$Vineyards)

cor.test(df$SemiNatural,df$Vineyards)
cor.test(df$Forests, df$Grasslands)
cor.test(df$Forests, df$HedgeRows)
cor.test(df$Grasslands, df$HedgeRows)

df %>% group_by(LandscapeID) %>% summarize(mean(Crops)) #other crops

# config
summary(df$Edge)
df %>% group_by(Organic) %>% summarize(mean(Edge, na.rm = TRUE))

summary(df$MPS_Vineyards)
summary(df$MPS_SemiNatural)
df %>% group_by(Organic) %>% summarize(mean(MPS_Vineyards, na.rm = TRUE))
df %>% group_by(Organic) %>% summarize(mean(MPS_SemiNatural, na.rm = TRUE))

summary(df$Dist.snh)
df %>% group_by(Organic) %>% summarize(mean(Dist.snh, na.rm = TRUE))

# correlations
cor.test(df$SemiNatural, df$Dist.snh)

## Multidiversity indices )-----------------

cor.test(df$MB_MeanScMax, df$MB_MeanZscore)
cor.test(df$MB_MeanScMax, df$MB_Thresh70)
cor.test(df$MA_MeanScMax, df$MA_MeanZscore)
cor.test(df$MA_MeanScMax, df$MA_Thresh70)

## Discussion: magnitude of the effects of OF --------------

magres <- read.csv("Output/Models_1OrgLandscape/ModelResults_MagnitudePredictEffects.csv")
magres

# bird, pollinators and butterflies richness
range(magres %>% filter(Response %in% c("BirdRichness", "ButtRichness", "BeeRichness", "SyrphidRichness")) %>% select(PercentChange))

magres[order(magres$PercentChange), ]

# earthworms and ground beetles
magres[grepl("Spider", magres$Response),]

# earthworms and ground beetles
magres[grepl("Eworm|Carab", magres$Response),]
magres[grepl("Cover", magres$Response),]

# predation
magres[grepl("PredRate", magres$Response),]

# order by percent change - RICHNESS
magresri <- magres[grepl("Richness", magres$Response),]
magresri[order(magresri$PercentChange),]

# order by percent change - ABUNDANCE
magresab <- magres[grepl("Abundance|Cover", magres$Response),]
magresab[order(magresab$PercentChange),]

# order by percent change - FUNS
magresfu <- magres[!grepl("Richness|Cover|Abundance|MA|MB", magres$Response),]
magresfu[order(magresfu$PercentChange),]


