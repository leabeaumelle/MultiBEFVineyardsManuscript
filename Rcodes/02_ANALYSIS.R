## This script runs the analyses

## Investigates the interactive effects of landscape complexity and organic management on multidiversity and its individual components, and on multifunctionnality and its individual components.

## In part II. script runs models to investigate synergies and tradeoffs between wine production, biodiversity and ecosystem services.

## Part III. contains supplementary analyses associated with the synergies/tradeoffs analysis

rm(list =ls())

# Functions -----------
library(dplyr)
library(lme4)
library(DHARMa)
library(car)
library(sjPlot)
library(vegan)
library(nlme)
library(ggplot2)
library(patchwork)
library(ggeffects)

# Load Data -----------
df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")
df <- df[,-c(1:2)]

# Shannon
df$Shannon <- diversity(df %>% select(Crops, Grasslands, Forests, Vineyards, GrassStrips, HedgeRows, SoftPaths, Artificial, Water, Others), index = "shannon")

# vector of all the response variables
WhichResponseVar <- c(
  df %>% select(starts_with("MB"), -MB_N) %>% colnames(),  # multidiv indices
  df %>% select(ends_with("Richness")) %>% colnames(),  # 14 taxa groups
  df %>% select(starts_with("MA"), -MA_N) %>% colnames(),  # multiabundance indices
  df %>% select(ends_with("Abundance"), PlantCover) %>% colnames(),  # 14 taxa groups
  
  df %>% select(starts_with("EMF"), -EMF_N) %>% colnames(),  # multifun indices
  df %>% select(YieldRealised_hlperha, Ntester,
                SoilC, SoilN, DecompokRate, DecompoSfactor, SoilPhosphatase, SoilGlucosidase, SoilUrease,
                PredRateEggs, PredRatePupae, PredRatePlasticine,
                PathogenDamage, PestDamage)%>% colnames()  # individual functions
)


# I. Organic farming x landscape effects -----------------

# model structure, example
modtest <- lmer(EMF_Regulation~ Organic*SemiNatural + Organic*Dist.snh + (1|LandscapeID), data = df)

# 38 observations vs. 8 df
logLik(modtest); nrow(df)



## 1/ Fit the models for all response variables -----------
for(i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  dfa <- dfa[!is.na(dfa$respvar),]
  
  # fit the model 
  mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa)
  
  # create custom file path name to store the model output
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape","NormalDistrib",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  
  # store model output
  saveRDS(mod, file = mypath)
  
}


## 2/ Residual diagnostic plots --------------------------


# Are there patterns in the residuals? Need to use other distribution, covariate or nonlinear effect?

# for each response variable, 
# function to create residual diagnostic plots with DHARMA

for(i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  dfa <- dfa[!is.na(dfa$respvar),]
  
  # management variables NA for plotID 16B: for plots of residuals, assume management variables are average across organic study plots
  dfa$TillageFreq[dfa$PlotID=="16B"] <- mean(dfa$TillageFreq[dfa$Organic=="Organic"], na.rm = TRUE)
  dfa$MowingFreq[dfa$PlotID=="16B"] <- mean(dfa$MowingFreq[dfa$Organic=="Organic"], na.rm = TRUE)
  dfa$SprayingFreq[dfa$PlotID=="16B"] <- mean(dfa$SprayingFreq[dfa$Organic=="Organic"], na.rm = TRUE)
  
  # read the model and simulate residuals with DHARMA
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape","NormalDistrib",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  mod2 <- readRDS(mypath)
  res <- simulateResiduals(mod2, plot = FALSE)
  
  # create custom file path name for storing resid plots
  mypath2 <- file.path(getwd(), "Output", "Models_1OrgLandscape","NormalDistrib",paste("ResidCheck_", WhichResponseVar[i], ".jpg", sep = ""))
  
  # plot residuals (general diagnostic plot)
  jpeg(file=mypath2)
  mytitle = paste(WhichResponseVar[i])
  plot(res)
  dev.off()
  
  # plot residuals vs landscape variables
  mypath3 <- file.path(getwd(), "Output", "Models_1OrgLandscape","NormalDistrib",paste("ResidLandscape_", WhichResponseVar[i], ".jpg", sep = ""))
  
  jpeg(file=mypath3)
  mytitle = paste(WhichResponseVar[i])
  par(mfrow=c(2,2), mar=c(4,2,2,2))
  plotResiduals(res, dfa$SemiNatural, xlab = "SemiNatural")
  plotResiduals(res, dfa$Dist.snh, xlab = "Distance to SNH")
  plotResiduals(res, dfa$Edge, xlab = "Edge density")
  plotResiduals(res, dfa$HedgeRows, xlab = "HedgeRows")
  dev.off()
  
  # plot residuals vs local management variables
  mypath4 <- file.path(getwd(), "Output", "Models_1OrgLandscape","NormalDistrib",paste("ResidLocalMan_", WhichResponseVar[i], ".jpg", sep = ""))
  
  jpeg(file=mypath4)
  mytitle = paste(WhichResponseVar[i])
  par(mfrow=c(2,2), mar=c(4,2,2,2))
  try(plotResiduals(res, dfa$Organic, xlab = "Organic"))
  try(plotResiduals(res, dfa$TillageFreq, xlab = "Tillage freq "))
  try(plotResiduals(res, dfa$MowingFreq, xlab = "Mowing freq"))
  try(plotResiduals(res, dfa$SprayingFreq, xlab = "Spraying freq"))
  dev.off()
}


## 3/ Refit models with log-transformation to improve model fit ---------------

# variables identified from residual diagnostic plot inspection
WhichLogTransformed <- df %>% select(BirdRichness, CarabRichness, CollemboRichness, SpiderFoliageRichness, ColeopteraFoliageRichness, HeteropteraFoliageRichness, EwormAbundance, SyrphidAbundance, BeeAbundance, SpiderAbundance, HemipteraFoliageAbundance, HeteropteraFoliageAbundance, EMF_Production, EMF_Regulation, EMF_MeanScMax, EMF_MeanZscore, EMF_Thresh70, YieldRealised_hlperha, SoilC, SoilPhosphatase, PredRatePupae, PathogenDamage, PestDamage) %>% colnames()


for(i in 1:length(WhichLogTransformed)){
  dfa <- df
  dfa$respvar <- df[,WhichLogTransformed[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  dfa <- dfa[!is.na(dfa$respvar),]
  
  # determine constant to add to variable before log-transformation (log(Y+a) so that min(Y+a)=1 for richness and abundance, and min(Y+a)=0.001 for functions)
  
  a <- 1-min(dfa$respvar, na.rm = TRUE)
  
  # log transformation
  mod2 <- lmer(log(respvar+a) ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa)
  
  # store models
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape", "LogTransformed", paste("ModelResults_Log", WhichLogTransformed[i], ".rds", sep = ""))
  saveRDS(mod2, file = mypath)
  
  # create resid plots
  res2 <- simulateResiduals(mod2, plot = FALSE)
  mypath2 <- file.path(getwd(), "Output", "Models_1OrgLandscape", "LogTransformed", paste("ResidCheck_Log", WhichLogTransformed[i], ".jpg", sep = ""))
  jpeg(file=mypath2)
  mytitle = paste(WhichLogTransformed[i])
  plot(res2)
  dev.off()
  
}

### Inspect further for variables that show equivalent log/normal----------------

WhichVars <- c("BirdRichness", "CollembolaRichness", "HeteropteraRichness", "EMF_Regulation", "PestDamage")

## Birds: variancee homogeneity in organic and conv plots is better with log transformation
mod <- readRDS("Output/Models_1OrgLandscape/LogTransformed/ModelResults_LogBirdRichness.rds")

res <- simulateResiduals(mod)
plot(res)

hist(res)

plotResiduals(res, df$Organic)
plotResiduals(res, df$SemiNatural)
plotResiduals(res, df$Dist.snh)
plotResiduals(res, df$HedgeRows)
plotResiduals(res, df$TillageFreq)

## Collembola: log transfo leads to asymetric distribution + patterns with edge d are worst with log-transformation
mod <- readRDS("Output/Models_1OrgLandscape/LogTransformed/ModelResults_LogCollemboRichness.rds")

res <- simulateResiduals(mod)
plot(res)

hist(res)

plotResiduals(res, df$Organic[!is.na(df$CollemboRichness)])
plotResiduals(res, df$SemiNatural[!is.na(df$CollemboRichness)])
plotResiduals(res, df$Dist.snh[!is.na(df$CollemboRichness)])
plotResiduals(res, df$Edge[!is.na(df$CollemboRichness)])
plotResiduals(res, df$HedgeRows[!is.na(df$CollemboRichness)])

# 
1-min(df$CollemboRichness, na.rm= T)
hist(df$CollemboRichness)
hist(log(df$CollemboRichness+(1-min(df$CollemboRichness, na.rm = T))))

## Heteroptera: difficult, but normal model has singularity issue
mod <- readRDS("Output/Models_1OrgLandscape/LogTransformed/ModelResults_LogHeteropteraFoliageRichness.rds")

res <- simulateResiduals(mod)
plot(res)
hist(res)

plotResiduals(res, df$Organic)
plotResiduals(res, df$SemiNatural)
plotResiduals(res, df$Dist.snh)
plotResiduals(res, df$HedgeRows)
plotResiduals(res, df$TillageFreq)

# 
1-min(df$HeteropteraFoliageRichness, na.rm= T)
hist(df$HeteropteraFoliageRichness)
hist(log(df$HeteropteraFoliageRichness+(1-min(df$HeteropteraFoliageRichness, na.rm = T))))
plot(df$HeteropteraFoliageRichness~df$Organic)
plot(log(df$HeteropteraFoliageRichness+(1-min(df$HeteropteraFoliageRichness, na.rm = T)))~df$Organic)

# 
mod2 <- readRDS("Output/Models_1OrgLandscape/ModelResults_HeteropteraFoliageRichness.rds")
res2 <- simulateResiduals(mod2)
plotResiduals(res2, df$Organic)
plotResiduals(res, df$Organic)

# inspect model results
summary(mod2)
isSingular(mod2)


## EMF regulation: log transformation does not improve anything
mod <- readRDS("Output/Models_1OrgLandscape/LogTransformed/ModelResults_LogEMF_Regulation.rds")

res <- simulateResiduals(mod)
plot(res)

hist(res)

plotResiduals(res, df$Organic)
plotResiduals(res, df$SemiNatural)
plotResiduals(res, df$Dist.snh)
plotResiduals(res, df$HedgeRows)
plotResiduals(res, df$Edge)

# 
1-min(df$EMF_Regulation, na.rm= T)
hist(df$EMF_Regulation)
hist(log(df$EMF_Regulation+(1-min(df$EMF_Regulation, na.rm = T))))
hist(log(df$EMF_Regulation))
plot(df$EMF_Regulation~df$Organic)
plot(log(df$EMF_Regulation+(1-min(df$EMF_Regulation, na.rm = T)))~df$Organic)
plot(log(df$EMF_Regulation)~df$Organic)

# changing the constatnt for log transformaation does not ameliorate
mod3 <- lmer((EMF_Regulation) ~ Organic * SemiNatural + Organic * Dist.snh +      (1 | LandscapeID), 
             data = df)
res3 <- simulateResiduals(mod3)
plot(res3)


## pathogen damage: log transfo: check constant

# pathogen damage should be log transformed without constant
summary(df$PathogenDamage)
1-min(df$PathogenDamage, na.rm= T)
hist(df$PathogenDamage)
hist(log(df$PathogenDamage+(1-min(df$PathogenDamage, na.rm = T))))
hist(log(df$PathogenDamage+0.001))
summary(log(df$PathogenDamage+(1-0.008)))
plot(df$PathogenDamage~df$Organic)
plot(log(df$PathogenDamage+(1-min(df$PathogenDamage, na.rm = T)))~df$Organic)
plot(log(df$PathogenDamage+0.001)~df$Organic)

mod3 <- lmer(log(df$PathogenDamage+0.001)~Organic*SemiNatural + Organic*Dist.snh +(1|LandscapeID), data = df)
res3 <- simulateResiduals(mod3)
plot(res3)



# pest damage, change the constant
summary(df$PestDamage)
1-min(df$PestDamage, na.rm= T)
hist(df$PestDamage)
hist(log(df$PestDamage+(1-min(df$PestDamage, na.rm = T))))
hist(log(df$PestDamage+0.001))
hist(log(df$PestDamage+0.000001))
plot(df$PestDamage~df$Organic)
plot(log(df$PestDamage+(1-min(df$PestDamage, na.rm = T)))~df$Organic)
plot(log(df$PestDamage)~df$Organic)
plot(log(df$PestDamage+0.001)~df$Organic)

# adding zero until distribution is less bad
mod3 <- lmer(log(df$PestDamage+0.000001)~Organic*SemiNatural + Organic*Dist.snh +(1|LandscapeID), data = df)
res3 <- simulateResiduals(mod3)
plot(res3)


## 4/ Final models -------------------------------------------------------------

WhichLogTransformedFinal <- df %>% select(BirdRichness, CarabRichness, ColeopteraFoliageRichness, HeteropteraFoliageRichness, 
                                          EwormAbundance, SyrphidAbundance, BeeAbundance, SpiderAbundance, HemipteraFoliageAbundance, HeteropteraFoliageAbundance,
                                          EMF_Production, EMF_MeanScMax, EMF_MeanZscore, SoilC, SoilPhosphatase, PredRatePupae, PathogenDamage, PestDamage) %>% colnames()


for (i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  
  # fit models with log transformed Y
  if(WhichResponseVar[i] %in% WhichLogTransformedFinal){
    
    # determine constant to add to variable before log-transformation (log(Y+a) so that min(Y+a)=1, 
    # except for pest and pathogen damage, where small value added (because lots of zeros)
    ifelse(WhichResponseVar[i] =="PestDamage", a <-  0.000001, 
           ifelse(WhichResponseVar[i] == "PathogenDamage", a <-  0.001,
                  a <-  1-min(dfa$respvar, na.rm = TRUE)))
    
    mod <- lmer(log(respvar+a)~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa)
  }
  else{
    mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa)
  }
  
  # save final models
  mypath4 <- file.path(getwd(), "Output", "Models_1OrgLandscape", paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  
  saveRDS(mod, file=mypath4)
  
}


## 5/ Inspect residuals of final models ----------------------------

# for each response variable, 
# function to create residual diagnostic plots with DHARMA

for(i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  dfa <- dfa[!is.na(dfa$respvar),]
  
  # management variables NA for plotID 16B: for plots of residuals, assume management variables are average across organic study plots
  dfa$TillageFreq[dfa$PlotID=="16B"] <- mean(dfa$TillageFreq[dfa$Organic=="Organic"], na.rm = TRUE)
  dfa$MowingFreq[dfa$PlotID=="16B"] <- mean(dfa$MowingFreq[dfa$Organic=="Organic"], na.rm = TRUE)
  dfa$SprayingFreq[dfa$PlotID=="16B"] <- mean(dfa$SprayingFreq[dfa$Organic=="Organic"], na.rm = TRUE)
  
  # read the model and simulate residuals with DHARMA
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  mod2 <- readRDS(mypath)
  res <- simulateResiduals(mod2, plot = FALSE)
  
  # create custom file path name for storing resid plots
  mypath2 <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ResidCheck_", WhichResponseVar[i], ".jpg", sep = ""))
  
  # plot residuals (general diagnostic plot)
  jpeg(file=mypath2)
  mytitle = paste(WhichResponseVar[i])
  plot(res)
  dev.off()
  
  # plot residuals vs landscape variables
  mypath3 <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ResidLandscape_", WhichResponseVar[i], ".jpg", sep = ""))
  
  jpeg(file=mypath3)
  mytitle = paste(WhichResponseVar[i])
  par(mfrow=c(2,2), mar=c(4,2,2,2))
  plotResiduals(res, dfa$SemiNatural, xlab = "SemiNatural")
  plotResiduals(res, dfa$Dist.snh, xlab = "Distance to SNH")
  plotResiduals(res, dfa$Edge, xlab = "Edge density")
  plotResiduals(res, dfa$HedgeRows, xlab = "HedgeRows")
  dev.off()
  
  # plot residuals vs local management variables
  mypath4 <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ResidLocalMan_", WhichResponseVar[i], ".jpg", sep = ""))
  
  jpeg(file=mypath4)
  mytitle = paste(WhichResponseVar[i])
  par(mfrow=c(2,2), mar=c(4,2,2,2))
  try(plotResiduals(res, dfa$Organic, xlab = "Organic"))
  try(plotResiduals(res, dfa$TillageFreq, xlab = "Tillage freq "))
  try(plotResiduals(res, dfa$MowingFreq, xlab = "Mowing freq"))
  try(plotResiduals(res, dfa$SprayingFreq, xlab = "Spraying freq"))
  dev.off()
}



### Inspect singularity issues--------------------------------------------------
singularityIssues <- c()

for(i in 1:length(WhichResponseVar)){
  mypath1 <- file.path(getwd(), "Output", "Models_1OrgLandscape", paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  mod1 <- readRDS(mypath1)
  singularityIssues <-  c(singularityIssues, isSingular(mod1))
}

cbind(singularityIssues, WhichResponseVar)[singularityIssues==TRUE,]


## 6/ Are there significant main and interactive effects? Do they differ for landscape composition and configuration? -----

# Creates summary table to compare the effects of organic, landscape and interaction for landscape composition vs configuration

results <- data.frame()
for(i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # load the model
  mypath5 <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  
  mod <- readRDS(mypath5)
  
  # extract Anova results
  results <- rbind(results, 
                   data.frame(t(data.frame(Anova(mod)[3])))
  )
  rownames(results)[i] <- WhichResponseVar[i]
}

# Store
write.csv(results, "Output/Models_1OrgLandscape/ResultsAnova.csv")


## 7/ Plot standardized coefficients of organic management effects across taxa and functions -----

# load model
mod <- readRDS("Output/Models_1OrgLandscape/ModelResults_MB_MeanScMax.rds")

# get standardized coefficients (i.e. variables are scaled before model fit)
get_model_data(mod, type = "std")

# get the management effect size (coefficients of Organic = how much bigger is Y in organic compared to conventional?) and CI
get_model_data(mod, type = "std")[1, c("estimate", "conf.low", "conf.high")]

## get model data does not work with log(x+1) transformed data
esdata <- data.frame()

for (i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  
  # refit models with log transformed Y with scalings and get standardized coefficients
  if(WhichResponseVar[i] %in% WhichLogTransformedFinal){
    
    # determine constant to add to variable before log-transformation (log(Y+a) so that min(Y+a)=1, 
    # except for pest and pathogen damage, where small value added (because lots of zeros)
    ifelse(WhichResponseVar[i] =="PestDamage", a <-  0.000001, 
           ifelse(WhichResponseVar[i] == "PathogenDamage", a <-  0.001,
                  a <-  1-min(dfa$respvar, na.rm = TRUE)))
    
    mod <- lmer(scale(log(respvar+a))~ Organic*scale(landcompo) + Organic*scale(landconfig) + (1|LandscapeID), data = dfa)
  }
  else{
    mod <- lmer(scale(respvar) ~ Organic*scale(landcompo) + Organic*scale(landconfig) + (1|LandscapeID), data = dfa)
  }
  
  coeffs <- get_model_data(mod, type = "est")[1, c("estimate", "conf.low", "conf.high")]
  rownames(coeffs) <- paste(WhichResponseVar[i])
  
  # store into a df
  esdata <- rbind(esdata, coeffs)
  
}



# store data into the output
write.csv(esdata, "Output/Models_1OrgLandscape/ModelResults_EffectSizeOrganicAcross.csv")


## 8/ Calculate magnitude of the effects predicted -----

LRR <- c()
PercentChange <- c()


for (i in 1:length(WhichResponseVar)){
  ## Data
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  # dfa$respvar <- df$MB_MeanScMax
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # when variable has been log-transformed, define constant a
  
  # determine constant to add to variable before log-transformation (log(Y+a) so that min(Y+a)=1, 
  # except for pest and pathogen damage, where small value added (because lots of zeros)
  ifelse(WhichResponseVar[i] =="PestDamage", a <-  0.000001, 
         ifelse(WhichResponseVar[i] == "PathogenDamage", a <-  0.001,
                a <-  1-min(dfa$respvar, na.rm = TRUE)))
  
  # Load model results
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  mod <- readRDS(mypath)
  
  # get the predictions with ggeffects
  me <- ggemmeans(mod, "Organic")
  
  # # take the predicted data from ggeffects and compute means and sds
  pred <- as.data.frame(me)
  
  # effect sizes (LRR):
  LRR <- c(LRR, log(pred$predicted[pred$x=="Organic"]/pred$predicted[pred$x=="Conventional"]))
  
  # in percent change:
  PercentChange <- c(PercentChange,
                     100 * (exp(log(pred$predicted[pred$x=="Organic"]/pred$predicted[pred$x=="Conventional"])) - 1))
  
  # clean 
  rm(a)
  
  
}

magres <- data.frame(Response = WhichResponseVar,
                     LRR, PercentChange)

# remove variables for which this approach is not relevant (variables close to zero and with negative values: MeanZscore)
magres <- magres %>% filter(Response != "MB_MeanZscore", 
                            Response != "MA_MeanZscore", 
                            Response != "EMF_MeanZscore")


write.csv(magres, "Output/Models_1OrgLandscape/ModelResults_MagnitudePredictEffects.csv")


### Calculate the magnitude of landscape composition effects for each farming system
PercentChange_Org <- c()
PercentChange_Conv <- c()


for (i in 1:length(WhichResponseVar)){
  ## Data
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  # dfa$respvar <- df$MB_MeanScMax
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # when variable has been log-transformed, define constant a
  
  # determine constant to add to variable before log-transformation (log(Y+a) so that min(Y+a)=1, 
  # except for pest and pathogen damage, where small value added (because lots of zeros)
  ifelse(WhichResponseVar[i] =="PestDamage", a <-  0.000001, 
         ifelse(WhichResponseVar[i] == "PathogenDamage", a <-  0.001,
                a <-  1-min(dfa$respvar, na.rm = TRUE)))
  
  # Load model results
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  mod <- readRDS(mypath)
  
  # get the predictions with ggeffects
  me <- ggemmeans(mod, terms = c("landcompo [0,60]", "Organic"))
  
  # # take the predicted data from ggeffects and compute means and sds
  pred <- as.data.frame(me)
  
  # percent change from lowest to highest landscape complexity in organic vineyards
  PercentChange_Org <- c(PercentChange_Org,
                         100 * (pred$predicted[pred$x=="60"&pred$group=="Organic"]/pred$predicted[pred$x=="0"&pred$group=="Organic"] - 1))
  
  # percent change conventional
  PercentChange_Conv <- c(PercentChange_Conv,
                          100 * (pred$predicted[pred$x=="60"&pred$group=="Conventional"]/pred$predicted[pred$x=="0"&pred$group=="Conventional"] - 1))
  
  # clean 
  rm(a)
  
  
}

magres <- data.frame(Response = WhichResponseVar,
                     PercentChange_Org, PercentChange_Conv)

# remove variables for which this approach is not relevant (variables close to zero and with negative values: MeanZscore)
magres <- magres %>% filter(Response != "MB_MeanZscore", 
                            Response != "MA_MeanZscore", 
                            Response != "EMF_MeanZscore")


write.csv(magres, "Output/Models_1OrgLandscape/ModelResults_MagnitudePredictEffects_Landscape.csv")


## 9/ Sensitivity analysis ---------

### 1/ Weighted multidiv ----------------------------------

source("Rcodes/FUN_MultiDivFunction_Allan2014.R")  # multidiv/multifun function

MultidivWeighted <- data.frame(
  PlotID = df$PlotID,
  MB_Below = multidiv(df %>% select(EwormRichness, MicrobeRichness, CollemboRichness))[,1],
  MB_Ground = multidiv(df %>% select(CarabRichness, PlantRichness, SpiderRichness))[,1],
  MB_Foliage = multidiv(df %>% select(ColeopteraFoliageRichness, HeteropteraFoliageRichness, HemipteraFoliageRichness, SpiderFoliageRichness))[,1],
  MB_Aerial = multidiv(df %>% select(BirdRichness, SyrphidRichness, BeeRichness, ButtRichness))[,1])

MultidivWeighted <- MultidivWeighted %>% 
  mutate(MB_Weighted = multidiv(MultidivWeighted %>% select(MB_Below, MB_Ground, MB_Foliage, MB_Aerial))[,1])

# compare weighted  and unweighted
plot(MultidivWeighted$MB_Weighted ~ df$MB_MeanScMax)
cor.test(MultidivWeighted$MB_Weighted ,df$MB_MeanScMax)  # correlated r=0.99

MultidivWeighted %>% mutate(Organic = factor(ifelse(grepl("B", PlotID), "Organic", "Conv"))) %>% 
  ggplot(aes(y=MB_Weighted, x =Organic))+geom_boxplot()


# model 
dfa <- left_join(df, MultidivWeighted, by = "PlotID")
dfa$respvar <- dfa$MB_Weighted
dfa$landcompo <- dfa$SemiNatural
dfa$landconfig <- dfa$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)

mod2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa)

# save the model
mypath4 <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("SensitivityAnalysis_", "WeightedMultidiv", ".rds", sep = ""))

saveRDS(mod2, file=mypath4)

# results
res <- simulateResiduals(mod2, plot = T)
Anova(mod2)
tab_model(mod2)

# estimate effect of organic 
get_model_data(mod2, type = "std")[1, c(1:5)]

# compare with the other ES for richness and multidiv
esdata <- read.csv(file = "Output/Models_1OrgLandscape/ModelResults_EffectSizeOrganicAcross.csv")

get_model_data(mod2, type = "std")[1, c(1:5)];esdata[1:17,]

# std estimate is a bit lower for weighted multidiv but similar order of magnitude and results obtained.


### 2/ Test for spatial correlations ----------------------------------

library(gstat)
library(sp)


WhichVar <- c("MB_MeanScMax", "MA_MeanScMax", "EMF_Regulation", "EMF_Soil", "EMF_Production", "EMF_MeanScMax")

for (i in 1:length(WhichVar)){
  ## Data
  dfa <- df
  dfa$respvar <- df[,WhichVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # Load model results
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ModelResults_", WhichVar[i], ".rds", sep = ""))
  
  mod <- readRDS(mypath)
  
  # get residuals
  resEud<-residuals(mod, "pearson", scaled = TRUE)
  # get coordiantes
  coordinates(dfa)<-c("X.x","Y")
  dfa$resEud <- resEud
  
  # make bubble plot
  B1<-bubble(dfa,"resEud",col=c("black","grey"),
             main="Residuals",xlab="X-coordinates",
             ylab="Y-coordinates")
  
  mypath2 <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ResidCheck_BubblePlot_", WhichVar[i], ".jpg", sep = ""))
  
  jpeg(file=mypath2)
  print(B1)
  dev.off()
  
  # make variograms
  Vario1 <- variogram(resEud ~ 1, dfa)
  
  mypath3 <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ResidCheck_Variogram_", WhichVar[i], ".jpg", sep = ""))
  
  jpeg(file=mypath3)
  print(plot(Vario1))
  dev.off()
  
}



# II. Tradeoffs between production and biodiversity and biodiversity-mediated services ---------------------

## Exploratory plots
plot(df$MA_MeanScMax, df$EMF_Production, col = df$Organic)
plot(df$EMF_Regulation, df$EMF_Production, col = df$Organic)
plot(df$EMF_Soil, df$EMF_Production, col = df$Organic)


## Tradeoff Biodiversity - production ------------------------------------------

# data description
summary(df$MB_MeanScMax)
summary(df$EMF_Production)

hist(df$MB_MeanScMax)
hist(df$EMF_Production)

ggplot(df, aes(x=EMF_Production, y = MB_MeanScMax, col = Organic))+
  geom_point()+geom_smooth(method = "lm")



# fit models
dfa <- df
dfa$respvar <- df$MB_MeanScMax
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*EMF_Production+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

# adding organic-production interaction does not improve model fit
mod0 <- readRDS("Output/Models_1OrgLandscape/ModelResults_MB_MeanScMax.rds")

tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)


# LRT tests : all variables except farming system can be dropped 
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landcompo)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:landconfig)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - Organic:EMF_Production)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landcompo)
drop1(modsimpler4, test = "Chisq")

modsimpler5 <- update(modsimpler4, .~. - EMF_Production)
drop1(modsimpler5, test = "Chisq")

modsimpler6 <- update(modsimpler5, .~. - landconfig)
drop1(modsimpler6, test = "Chisq")

Anova(mod)

# save the model
saveRDS(mod, "Output/Models_2ProductionTradeoffs/ModelResults_MB_MeanScMax.rds")

# compare results with yield as explaining variable instead of index of production
ggplot(df, aes(x=YieldRealised_hlperha, y = MB_MeanScMax, col = Organic))+
  geom_point()+geom_smooth(method = "lm")

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*YieldRealised_hlperha+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

# adding organic-yield effect does not improve model fit
tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

Anova(mod)

# LRT tests: indicate significant interaction organic-yield when dropping other variables
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landcompo)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:landconfig)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - landcompo)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landconfig)
drop1(modsimpler4, test = "Chisq")


# inspect residuals
res4 <- simulateResiduals(modsimpler4)
plot(res4)

plotResiduals(res4, dfa$landcompo)
plotResiduals(res4, dfa$landconfig)
plotResiduals(res4, dfa$Organic)

# inpsect results
Anova(modsimpler4)
tab_model(modsimpler4)

# plot results
plot_model(modsimpler4, type = "pred", terms = "Organic [all]")
plot_model(modsimpler4, type = "pred", terms = c("YieldRealised_hlperha", "Organic"))

## role of outliers in this relationship
mod2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*YieldRealised_hlperha+ (1|LandscapeID), data = dfa[dfa$YieldRealised_hlperha>20,])


# inspect residuals
res <- simulateResiduals(mod2)
plot(res)

# adding organic-yield effect does not improve model fit
mod0.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[dfa$YieldRealised_hlperha>20,])
tab_model(mod0.2, mod2, show.aicc = TRUE)

mod0.reml <- update(mod0.2, REML = FALSE)
mod.reml <- update(mod2, REML = FALSE)
anova(mod0.reml, mod.reml)

Anova(mod)

# LRT tests: indicate that organic-yield relationship is no longer significant
drop1(mod2, test = "Chisq")

modsimpler1 <- update(mod2, .~. - Organic:landcompo)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:landconfig)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - Organic:YieldRealised_hlperha)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landcompo)
drop1(modsimpler4, test = "Chisq")

modsimpler5 <- update(modsimpler4, .~. - landconfig)
drop1(modsimpler5, test = "Chisq")

modsimpler6 <- update(modsimpler5, .~. - YieldRealised_hlperha)
drop1(modsimpler6, test = "Chisq")


### Compare results for realised vs. target yield 

# prep data
dfa <- df
dfa$respvar <- df$MB_MeanScMax
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

# Realised vs target yield: data description
ggplot(df, aes(y=YieldRealised_hlperha, x = YieldTarget_hlperha, col = Organic))+
  geom_point()+geom_smooth(method = "lm")+geom_abline(slope = 1, intercept = 0)

# ratio realised target
dfa$PercentYieldTarget <- 100*df$YieldRealised_hlperha/df$YieldTarget_hlperha

hist(dfa$PercentYieldTarget)  # one outlier with much lower yield than target

summary(dfa$PercentYieldTarget)  # half vineyards achieved 94% of their target


# relationship with multidiversity
ggplot(dfa, aes(x=PercentYieldTarget, y = MB_MeanScMax, col = Organic))+
  geom_point()+geom_smooth(method = "lm")


# a model
mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$landconfig[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$Organic[!is.na(dfa$YieldTarget_hlperha)])

# adding organic-yield effect does not improve model fit
mod0 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

# remove the outlier
mod.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

# inspect residuals
res2 <- simulateResiduals(mod.2)
plot(res2)

# adding organic-yield effect does not improve model fit
mod0.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

mod0.reml2 <- update(mod0.2, REML = FALSE)
mod.reml2 <- update(mod.2, REML = FALSE)
anova(mod0.reml2, mod.reml2)

# relationship still n.s. does not improve model fit

plot_model(mod, type = "pred", terms = c("PercentYieldTarget", "Organic"))
plot_model(mod.2, type = "pred", terms = c("PercentYieldTarget", "Organic"))

# exploration: target yield is related to pest control index
plot(dfa$YieldTarget_hlperha,dfa$EMF_Regulation)
cor.test(dfa$YieldTarget_hlperha, dfa$EMF_Regulation)

# even when removing the outlier with a low target yield
plot(dfa$YieldTarget_hlperha[dfa$YieldTarget_hlperha > 30],dfa$EMF_Regulation[dfa$YieldTarget_hlperha > 30])
cor.test(dfa$YieldTarget_hlperha[dfa$YieldTarget_hlperha > 30], dfa$EMF_Regulation[dfa$YieldTarget_hlperha > 30])

# Sauvignon blanc have higher yields than Merlot
boxplot(dfa$YieldTarget_hlperha~dfa$VineVariety)
boxplot(dfa$YieldRealised_hlperha~dfa$VineVariety)
# but EMF regulation and biodiv index are not different 
boxplot(dfa$EMF_Regulation~dfa$VineVariety)
boxplot(dfa$MB_MeanScMax~dfa$VineVariety)



## Tradeoff Pest regulation - production ---------------------------------------

# data description
summary(df$EMF_Regulation)
summary(df$EMF_Production)

hist(df$EMF_Regulation)
hist(df$EMF_Production)

ggplot(df, aes(x=EMF_Production, y = EMF_Regulation, col = Organic))+
  geom_point()+geom_smooth(method = "lm")



# fit models
dfa <- df
dfa$respvar <- df$EMF_Regulation
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*EMF_Production+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

# test if relationship with production is significant
mod0 <- readRDS("Output/Models_1OrgLandscape/ModelResults_EMF_Regulation.rds")

tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)


# LRT tests, same answer
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landconfig)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:EMF_Production)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - Organic:landcompo)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landcompo)
drop1(modsimpler4, test = "Chisq")

modsimpler5 <- update(modsimpler4, .~. - landconfig)
drop1(modsimpler5, test = "Chisq")

modsimpler6 <- update(modsimpler5, .~. - Organic)
drop1(modsimpler6, test = "Chisq")

Anova(mod)

# save the model
saveRDS(mod, "Output/Models_2ProductionTradeoffs/ModelResults_EMF_Regulation.rds")

# compare results with yield as explaining variable instead of index of production
ggplot(df, aes(x=YieldRealised_hlperha, y = EMF_Regulation, col = Organic))+
  geom_point()+geom_smooth(method = "lm")

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*YieldRealised_hlperha+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

# test if relationship with production is significant
tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

Anova(mod)

# LRT tests
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landconfig)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:YieldRealised_hlperha)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - landconfig)
drop1(modsimpler3, test = "Chisq")

# inspect residuals
res4 <- simulateResiduals(modsimpler3)
plot(res4)

plotResiduals(res4, dfa$landcompo)
plotResiduals(res4, dfa$landconfig)
plotResiduals(res4, dfa$Organic)

# inpsect results
Anova(modsimpler3)
tab_model(modsimpler3)

# plot results
plot_model(modsimpler4, type = "pred", terms = "Organic [all]")
plot_model(modsimpler4, type = "pred", terms = c("YieldRealised_hlperha", "Organic"))


## Compare the results with the ratio of achived vs target yield
# prep data
dfa <- df
dfa$respvar <- df$EMF_Regulation
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

# ratio realised target
dfa$PercentYieldTarget <- 100*df$YieldRealised_hlperha/df$YieldTarget_hlperha

# relationship with pest control index
ggplot(dfa, aes(x=PercentYieldTarget, y = EMF_Regulation, col = Organic))+
  geom_point()+geom_smooth(method = "lm")

# a model
mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$landconfig[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$Organic[!is.na(dfa$YieldTarget_hlperha)])

# adding organic-yield effect does not improve model fit
mod0 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

# remove the outlier
mod.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

# inspect residuals
res2 <- simulateResiduals(mod.2)
plot(res2)

# adding organic-yield effect does not improve model fit
mod0.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

mod0.reml2 <- update(mod0.2, REML = FALSE)
mod.reml2 <- update(mod.2, REML = FALSE)
anova(mod0.reml2, mod.reml2)
# relationship is no longer significant when removing that outlier


Anova(mod)  # overall relationship, and the interaction percentyieldtarget and organic are significant
plot_model(mod, type = "pred", terms = c("PercentYieldTarget", "Organic"))
tab_model(mod)
# predicts a negative relationship (tradeoff) especially in organic


Anova(mod.2)  # but those relationships don't hold when removing a low value outlier, although still a negative trend in organic vineyards
plot_model(mod.2, type = "pred", terms = c("PercentYieldTarget", "Organic"))
tab_model(mod.2, digits = 3)

# relationship with pest control index: the data
ggplot(dfa[dfa$PercentYieldTarget > 30,], aes(x=PercentYieldTarget, y = EMF_Regulation, col = Organic))+
  geom_point()+geom_smooth(method = "lm")+ggtitle("without outlier")+
  ggplot(dfa, aes(x=PercentYieldTarget, y = EMF_Regulation, col = Organic))+
  geom_point()+geom_smooth(method = "lm")





## Tradeoff Soil quality/fertility - production --------------------------------
# data description
summary(df$EMF_Soil)
summary(df$EMF_Production)

hist(df$EMF_Soil)
hist(df$EMF_Production)

ggplot(df, aes(x=EMF_Production, y = EMF_Soil, col = Organic))+
  geom_point()+geom_smooth(method = "lm")



# fit models
dfa <- df
dfa$respvar <- df$EMF_Soil
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*EMF_Production+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)  # heterogeneity

bartlett.test(df$EMF_Soil, df$Organic)  # bartlett indicates similar variances
bartlett.test(resid(mod), df$Organic)  # no significant deviation

# test if relationship with production is significant
mod0 <- readRDS("Output/Models_1OrgLandscape/ModelResults_EMF_Soil.rds")

tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)


# LRT tests, same answer
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landconfig)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:landcompo)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - Organic:EMF_Production)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landconfig)
drop1(modsimpler4, test = "Chisq")

modsimpler5 <- update(modsimpler4, .~. - Organic)
drop1(modsimpler5, test = "Chisq")

modsimpler6 <- update(modsimpler5, .~. - landcompo)
drop1(modsimpler6, test = "Chisq")

modsimpler7 <- update(modsimpler6, .~. - EMF_Production)
drop1(modsimpler7, test = "Chisq")


# save the model
saveRDS(mod, "Output/Models_2ProductionTradeoffs/ModelResults_EMF_Soil.rds")

# compare results with yield as explaining variable instead of index of production
ggplot(df, aes(x=YieldRealised_hlperha, y = EMF_Soil, col = Organic))+
  geom_point()+geom_smooth(method = "lm")

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*YieldRealised_hlperha+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

# test if relationship with production is significant
tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

Anova(mod)

# LRT tests
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landconfig)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:landcompo)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - Organic:YieldRealised_hlperha)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landconfig)
drop1(modsimpler4, test = "Chisq")

modsimpler5 <- update(modsimpler4, .~. - landcompo)
drop1(modsimpler5, test = "Chisq")

modsimpler6 <- update(modsimpler5, .~. - Organic)
drop1(modsimpler6, test = "Chisq")

modsimpler7 <- update(modsimpler6, .~. - YieldRealised_hlperha)
drop1(modsimpler7, test = "Chisq")



# plot results
plot_model(mod, type = "pred", terms = "Organic [all]")
plot_model(mod, type = "pred", terms = c("YieldRealised_hlperha", "Organic"))

Anova(mod)

# simpler
mod0 <- lmer(respvar ~ 1+ (1|LandscapeID), data = dfa)
mod1 <- lmer(respvar ~ Organic*YieldRealised_hlperha+ (1|LandscapeID), data = dfa)

anova(mod0, mod1)




### Compare results for realised vs. target yield 

# prep data
dfa <- df
dfa$respvar <- df$EMF_Soil
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

# Realised vs target yield: data description
dfa$PercentYieldTarget <- 100*df$YieldRealised_hlperha/df$YieldTarget_hlperha

# relationship with multidiversity
ggplot(dfa, aes(x=PercentYieldTarget, y = EMF_Soil, col = Organic))+
  geom_point()+geom_smooth(method = "lm")


# a model
mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$landconfig[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$Organic[!is.na(dfa$YieldTarget_hlperha)])

# adding organic-yield effect does not improve model fit
mod0 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

# remove the outlier
mod.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

# inspect residuals
res2 <- simulateResiduals(mod.2)
plot(res2)

# adding organic-yield effect does not improve model fit
mod0.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

mod0.reml2 <- update(mod0.2, REML = FALSE)
mod.reml2 <- update(mod.2, REML = FALSE)
anova(mod0.reml2, mod.reml2)

# relationship not significant with and without outlier
plot_model(mod, type = "pred", terms = c("PercentYieldTarget", "Organic"))
plot_model(mod.2, type = "pred", terms = c("PercentYieldTarget", "Organic"))



## Tradeoff Multiabundance - production --------------------------------
# data description
summary(df$MA_MeanScMax)
summary(df$EMF_Production)

hist(df$MA_MeanScMax)
hist(df$EMF_Production)

ggplot(df, aes(x=EMF_Production, y = MA_MeanScMax, col = Organic))+
  geom_point()+geom_smooth(method = "lm")

# fit models
dfa <- df
dfa$respvar <- df$MA_MeanScMax
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*EMF_Production+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)  # heterogeneity

bartlett.test(df$MA_MeanScMax, df$Organic)  # bartlett indicates similar variances
bartlett.test(resid(mod), df$Organic)  # but on residuals: problem


# test if relationship with production is significant
mod0 <- readRDS("Output/Models_1OrgLandscape/ModelResults_MA_MeanScMax.rds")

tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

Anova(mod)

# LRT tests
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landconfig)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:landcompo)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - landcompo)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landconfig)
drop1(modsimpler4, test = "Chisq")

modsimpler5 <- update(modsimpler4, .~. - Organic:EMF_Production)
drop1(modsimpler5, test = "Chisq")

modsimpler6 <- update(modsimpler5, .~. - EMF_Production)
drop1(modsimpler6, test = "Chisq")

modsimpler7 <- update(modsimpler6, .~. - Organic)
drop1(modsimpler7, test = "Chisq")


# save the model
saveRDS(mod, "Output/Models_2ProductionTradeoffs/ModelResults_MA_MeanScMax.rds")


# compare results with yield as explaining variable instead of index of production
ggplot(df, aes(x=YieldRealised_hlperha, y = MA_MeanScMax, col = Organic))+
  geom_point()+geom_smooth(method = "lm")

mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*YieldRealised_hlperha+ (1|LandscapeID), data = dfa)


# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

# test if relationship with production is significant: yes
tab_model(mod0, mod, show.aicc = TRUE)

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

Anova(mod)

# LRT tests
drop1(mod, test = "Chisq")

modsimpler1 <- update(mod, .~. - Organic:landconfig)
drop1(modsimpler1, test = "Chisq")

modsimpler2 <- update(modsimpler1, .~. - Organic:landcompo)
drop1(modsimpler2, test = "Chisq")

modsimpler3 <- update(modsimpler2, .~. - landcompo)
drop1(modsimpler3, test = "Chisq")

modsimpler4 <- update(modsimpler3, .~. - landconfig)
drop1(modsimpler4, test = "Chisq")


# plot results
plot_model(modsimpler4, type = "pred", terms = "Organic [all]")
plot_model(modsimpler4, type = "pred", terms = c("YieldRealised_hlperha", "Organic"))

Anova(modsimpler4)


# inspect residuals
res <- simulateResiduals(modsimpler4)
plot(res)

plotResiduals(res, dfa$landcompo)  # outlier
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

plot(modsimpler4)

# plot results
plot_model(modsimpler4, type = "pred", terms = "Organic [all]")
plot_model(modsimpler4, type = "pred", terms = c("YieldRealised_hlperha", "Organic"))



## Refit without random effect 

modup <- lm(respvar ~ Organic*landcompo + Organic*landconfig + Organic*EMF_Production, data = dfa)

# inspect residuals
res <- simulateResiduals(modup)
plot(res)

plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

# variance heterogeneity: significant
bartlett.test(resid(modup ), dfa$Organic)


# refit gls with variance structure
modup2 <- gls(respvar ~ Organic*landcompo+ Organic*landconfig+ Organic*EMF_Production, data = dfa, weights = varIdent(form =~1|Organic))

res <- simulateResiduals(modup2)
plot(res)
plotResiduals(res, dfa$landcompo)
plotResiduals(res, dfa$landconfig)
plotResiduals(res, dfa$Organic)

anova(modup2, modup)  # log likelihood ratio test indicates that the model with different variances per farming system is not better

# save the model without random effect 
mypath2 <- file.path(getwd(), "Output", "Models_2ProductionTradeoffs",paste("ModelResults_", "MA_MeanScMax_WithoutRE", ".rds", sep = ""))


saveRDS(modup, mypath2)




### Compare results for realised vs. target yield 

# prep data
dfa <- df
dfa$respvar <- df$MA_MeanScMax
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa <- dfa[!is.na(dfa$respvar),]

# Realised vs target yield: data description
dfa$PercentYieldTarget <- 100*df$YieldRealised_hlperha/df$YieldTarget_hlperha

# relationship with multidiversity
ggplot(dfa, aes(x=PercentYieldTarget, y = MA_MeanScMax, col = Organic))+
  geom_point()+geom_smooth(method = "lm")


# a model
mod <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

# inspect residuals
res <- simulateResiduals(mod)
plot(res)

plotResiduals(res, dfa$landcompo[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$landconfig[!is.na(dfa$YieldTarget_hlperha)])
plotResiduals(res, dfa$Organic[!is.na(dfa$YieldTarget_hlperha)])
# residuals indicate variance heterogeneity between conv and org


# adding organic-yield effect does not improve model fit
mod0 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha),])

mod0.reml <- update(mod0, REML = FALSE)
mod.reml <- update(mod, REML = FALSE)
anova(mod0.reml, mod.reml)

# remove the outlier
mod.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + Organic*PercentYieldTarget+ (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

# inspect residuals
res2 <- simulateResiduals(mod.2)
plot(res2)

# adding organic-yield effect does not improve model fit
mod0.2 <- lmer(respvar ~ Organic*landcompo + Organic*landconfig + (1|LandscapeID), data = dfa[!is.na(dfa$YieldTarget_hlperha)&dfa$PercentYieldTarget > 30,])

mod0.reml2 <- update(mod0.2, REML = FALSE)
mod.reml2 <- update(mod.2, REML = FALSE)
anova(mod0.reml2, mod.reml2)

# relationship not significant with and without outlier
plot_model(mod, type = "pred", terms = c("PercentYieldTarget", "Organic"))
plot_model(mod.2, type = "pred", terms = c("PercentYieldTarget", "Organic"))


# III. Supplement analysis: what drives synergies biodiversity-wine production?-----

# create an index taking value of 1 when biodiversity index and wine production index are above the mean, 0 otherwise
# other types of indices explored in script 03_ANALYSIS_DataExploration, were less relevant

index <- ifelse(df$MB_MeanScMax > mean(df$MB_MeanScMax) & df$EMF_Production > mean(df$EMF_Production), 1, 0)

df$synergyBP <- index

table(df$synergyBP)  # 10 vineyards have this "synergy"
table(df$synergyBP, df$Organic)  # 5 are organic, 5 are conventional

plot(df$synergyBP~df$SemiNatural)
plot(df$synergyBP~df$Dist.snh)
plot(df$synergyBP~df$TillageFreq)
plot(df$synergyBP~df$MowingFreq)
plot(df$synergyBP~df$SprayingFreq)
plot(df$synergyBP~df$QuantityFungicide)
plot(df$synergyBP~df$QuantityInsecticide)
plot(df$synergyBP~df$NoAI)
plot(df$synergyBP~df$Organic)

# correlations between our main local and landscape variables (cf. PCA supplement)
pairs(df %>% select(synergyBP, SemiNatural, Shannon, SprayingFreq, MowingFreq, QuantityHerbicide, NoAI, QuantityInsecticide, Edge, Vineyards, Dist.snh, QuantityFungicide, TillageFreq) %>% filter(!is.na(df$TillageFreq)), diag.panel = function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}, lower.panel = panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
})


# a NA for management practices for plot 16B: it is a 0 (no synergy)
table(df$synergyBP[!is.na(df$TillageFreq)])

# variables selected based on PCA and parsimony: SNH, tillage, mowing and spraying freqs, fungicides and insecticides amounts

# a mixed model : singular because landscape already in the fixed effects
modnull <- glm(synergyBP ~ 1, data= df[!is.na(df$TillageFreq),], family = binomial)

mod0 <- glm(synergyBP ~ SemiNatural+TillageFreq+SprayingFreq+MowingFreq+QuantityFungicide+QuantityInsecticide, data= df[!is.na(df$TillageFreq),], family = binomial)

# check vif
vif(mod0)

# check residuals
res0 <- simulateResiduals(mod0)
plot(res0)

plotResiduals(res0, df$SemiNatural[!is.na(df$TillageFreq)])
plotResiduals(res0, df$TillageFreq[!is.na(df$TillageFreq)])
plotResiduals(res0, df$SprayingFreq[!is.na(df$TillageFreq)])
plotResiduals(res0, df$MowingFreq[!is.na(df$TillageFreq)])
plotResiduals(res0, df$QuantityFungicide[!is.na(df$TillageFreq)])
plotResiduals(res0, df$QuantityInsecticide[!is.na(df$TillageFreq)])

# other covariates
plotResiduals(res0, df$Dist.snh[!is.na(df$TillageFreq)])
plotResiduals(res0, df$Shannon[!is.na(df$TillageFreq)])
plotResiduals(res0, df$Edge[!is.na(df$TillageFreq)])
plotResiduals(res0, df$HedgeRows[!is.na(df$TillageFreq)])
plotResiduals(res0, df$NoAI[!is.na(df$TillageFreq)])
plotResiduals(res0, df$Organic[!is.na(df$TillageFreq)])  #   pattern 
plotResiduals(res0, df$InterRowManagement[!is.na(df$TillageFreq)])  
plotResiduals(res0, df$VineVariety[!is.na(df$TillageFreq)])  
plotResiduals(res0, df$NoAI_Insecticide[!is.na(df$TillageFreq)])  
plotResiduals(res0, df$NoAI_Fungicide[!is.na(df$TillageFreq)])

# model results
summary(mod0)
Anova(mod0) 

anova(mod0, modnull, test = "Chisq") # model not better than a null one

# possible that missing covariates: organic-conv 
mod1 <- glm(synergyBP ~ SemiNatural+Organic+TillageFreq+SprayingFreq+MowingFreq+QuantityFungicide+QuantityInsecticide, data= df[!is.na(df$TillageFreq),], family = binomial)

# check vif
vif(mod1)

# check residuals: adding organic improves residual diagnostic plots
res1 <- simulateResiduals(mod1)
plot(res1)

plotResiduals(res1, df$SemiNatural[!is.na(df$TillageFreq)])
plotResiduals(res1, df$TillageFreq[!is.na(df$TillageFreq)])
plotResiduals(res1, df$SprayingFreq[!is.na(df$TillageFreq)])
plotResiduals(res1, df$MowingFreq[!is.na(df$TillageFreq)])
plotResiduals(res1, df$QuantityFungicide[!is.na(df$TillageFreq)])
plotResiduals(res1, df$QuantityInsecticide[!is.na(df$TillageFreq)])

# other covariates
plotResiduals(res1, df$Dist.snh[!is.na(df$TillageFreq)])
plotResiduals(res1, df$Shannon[!is.na(df$TillageFreq)])
plotResiduals(res1, df$Edge[!is.na(df$TillageFreq)])
plotResiduals(res1, df$HedgeRows[!is.na(df$TillageFreq)])
plotResiduals(res1, df$NoAI[!is.na(df$TillageFreq)])
plotResiduals(res1, df$Organic[!is.na(df$TillageFreq)])
plotResiduals(res1, df$InterRowManagement[!is.na(df$TillageFreq)])  
plotResiduals(res1, df$VineVariety[!is.na(df$TillageFreq)])  
plotResiduals(res1, df$NoAI_Insecticide[!is.na(df$TillageFreq)])  
plotResiduals(res1, df$NoAI_Fungicide[!is.na(df$TillageFreq)])

# model results
summary(mod1)
Anova(mod1)

anova(modnull, mod1, test = "Chisq") # but the model not better than a null one

tab_model(mod1)
plot_model(mod1, type = "pred", terms = "QuantityInsecticide [all]")
plot_model(mod1, type = "pred", terms = "SemiNatural [all]")

# distribution of quantity insecitice: is weird
hist(df$QuantityInsecticide)
summary(df$QuantityInsecticide)
plot(df$synergyBP~df$QuantityInsecticide)
plot(df$synergyBP~log(df$QuantityInsecticide))

hist(df$SprayingFreq_Insecticide)
plot(df$SprayingFreq_Insecticide,df$QuantityInsecticide, col = df$Organic)
boxplot(df$SprayingFreq_Insecticide~df$Organic)
boxplot(df$QuantityInsecticide~df$Organic)

plot(df$SprayingFreq_Insecticide,df$synergyBP)

summary(df$QuantityInsecticide)


# Including the random effect does not change the results much
mod1r <- glmer(synergyBP ~ SemiNatural+TillageFreq+SprayingFreq+MowingFreq+QuantityFungicide+QuantityInsecticide + (1|LandscapeID), data= df[!is.na(df$TillageFreq),], family = binomial)

Anova(mod1r)
drop1(mod1r, test = "Chisq")

res2 <- simulateResiduals(mod1r)
plot(res2)

plotResiduals(res2, df$SemiNatural[!is.na(df$TillageFreq)])
plotResiduals(res2, df$TillageFreq[!is.na(df$TillageFreq)])
plotResiduals(res2, df$SprayingFreq[!is.na(df$TillageFreq)])
plotResiduals(res2, df$MowingFreq[!is.na(df$TillageFreq)])
plotResiduals(res2, df$QuantityFungicide[!is.na(df$TillageFreq)])
plotResiduals(res2, df$QuantityInsecticide[!is.na(df$TillageFreq)])

# other covariates
plotResiduals(res2, df$Dist.snh[!is.na(df$TillageFreq)])
plotResiduals(res2, df$Shannon[!is.na(df$TillageFreq)]) # pattern
plotResiduals(res2, df$Edge[!is.na(df$TillageFreq)])
plotResiduals(res2, df$HedgeRows[!is.na(df$TillageFreq)])
plotResiduals(res2, df$NoAI[!is.na(df$TillageFreq)])
plotResiduals(res2, df$Organic[!is.na(df$TillageFreq)]) # no pattern
plotResiduals(res2, df$InterRowManagement[!is.na(df$TillageFreq)])  
plotResiduals(res2, df$VineVariety[!is.na(df$TillageFreq)])  
plotResiduals(res2, df$NoAI_Insecticide[!is.na(df$TillageFreq)])  
plotResiduals(res2, df$NoAI_Fungicide[!is.na(df$TillageFreq)])



