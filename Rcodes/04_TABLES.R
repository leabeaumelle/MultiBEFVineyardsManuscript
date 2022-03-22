## This script prepares the tables with model results 

# Functions
rm(list =ls())

# Functions -----------
# library(ggbiplot)
library(dplyr)
library(lme4)
library(DHARMa)
library(car)
library(sjPlot)
library(vegan)
library(nlme)
library(ggplot2)
library(patchwork)


## Load Data -----------
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

## Raw data tables -----------------------------------------

# make table showing mean and SD values of local and landscape variables
WhichVars <- c("TillageFreq", "MowingFreq", "SprayingFreq", "QuantityFungicide", "QuantityHerbicide", "QuantityInsecticide")

# means and sd conventional
df_conv <- df[df$Organic=="Conventional",c(WhichVars)]

tab_conv <- cbind(
  apply(df_conv[!is.na(df_conv$TillageFreq),], 2, mean),
  apply(df_conv[!is.na(df_conv$TillageFreq),], 2, sd), 
  apply(df_conv[!is.na(df_conv$TillageFreq),], 2, min),
  apply(df_conv[!is.na(df_conv$TillageFreq),], 2, max))

# means and sd conventional
df_org <- df[df$Organic=="Organic",c(WhichVars)]

tab_org <- cbind(
  apply(df_org[!is.na(df_org$TillageFreq),], 2, mean),
  apply(df_org[!is.na(df_org$TillageFreq),], 2, sd),
  apply(df_org[!is.na(df_org$TillageFreq),], 2, min),
  apply(df_org[!is.na(df_org$TillageFreq),], 2, max))

# final table
TableSupp1 <- cbind(tab_conv, tab_org)

write.csv(TableSupp1, "Tables/RawDataLocalManag.csv")


# landscape variables
WhichVars2 <- c("SemiNatural", "Dist.snh", "Edge", 
                "Vineyards", "Forests", "Grasslands", "HedgeRows")

# min and max values to reflect our gradient
df_conv2 <- df[df$Organic=="Conventional",c(WhichVars2)]

tab_conv2<- cbind(
  apply(df_conv2, 2, mean),
  apply(df_conv2, 2, sd),
  apply(df_conv2, 2, min),
  apply(df_conv2, 2, max))

# means and sd conventional
df_org2 <- df[df$Organic=="Organic",c(WhichVars2)]

tab_org2 <- cbind(
  apply(df_org2, 2, mean),
  apply(df_org2, 2, sd),
  apply(df_org2, 2, min),
  apply(df_org2, 2, max))

# final table
TableSupp2 <- cbind(tab_conv2, tab_org2)

write.csv(TableSupp2, "Tables/RawDataLandscape.csv")


## Models local-landscape effects --------------------------------


### 1) ANOVA -----------------------------------------------


#### Table showing Chi sqaure and P

# Makes a list of dataframes with Chi and P for each model

anovares <- data.frame()

for (i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # get the model
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  
  mod <- readRDS(mypath)
  
  # organize Chi and P per explan var
  org <- cbind(as.data.frame(Anova(mod)[1,-2]), sig=ifelse(Anova(mod)[1,3]< 0.05, "*", ""))
  lcp <- cbind(as.data.frame(Anova(mod)[2,-2]), sig=ifelse(Anova(mod)[2,3]< 0.05, "*", ""))
  lcf <- cbind(as.data.frame(Anova(mod)[3,-2]), sig=ifelse(Anova(mod)[3,3]< 0.05, "*", ""))
  o.lcp <- cbind(as.data.frame(Anova(mod)[4,-2]), sig=ifelse(Anova(mod)[4,3]< 0.05, "*", ""))
  o.lcf <- cbind(as.data.frame(Anova(mod)[5,-2]), sig=ifelse(Anova(mod)[5,3]< 0.05, "*", ""))
  
  anovares <-  rbind(anovares, 
                     data.frame(org, lcp, lcf, o.lcp, o.lcf))
}

rownames(anovares) <- WhichResponseVar
colnames(anovares) <- c("Organic, Chi","Organic, P", "sig1",
                        "Landscape compo., Chi","Landscape compo., P", "sig2",
                        "Landscape config., Chi","Landscape config., P", "sig3",
                        "Org:Landscape compo., Chi","Org:Landscape compo., P", "sig4",
                        "Org:Landscape config., Chi","Org:Landscape config., P", "sig5"
)
anovares
write.csv(anovares, "Tables/Model1_OrgLand_ANOVAres.csv")



## add adjusted P-values

# only relevant variables
WhichResponseVarPadj <- WhichResponseVar[!WhichResponseVar%in%c("MB_MeanZSscore", "MB_Thresh70", "MA_MeanZSscore", "MA_Thresh70", "EMF_MeanZscore", "EMF_Thresh70")]

restab <- anovares[rownames(anovares) %in% WhichResponseVarPadj,]

# a function to adjust p-value for multiple comparisons
myfunp <- function(x) p.adjust(p = x, method = "fdr", n = 50)


adjP <- restab %>% mutate_at(vars(contains(", P")), 
                             myfunp) %>% 
  select(contains(", P"))


table <- data.frame(Response = rownames(restab), 
                    restab[,1:3], adjP[,1], sigadjP=ifelse(adjP[,1] > 0.2, "", "*"),
                    restab[4:6], adjP[,2],  sigadjP1=ifelse(adjP[,2] > 0.2, "", "*"),
                    restab[7:9], adjP[,3],  sigadjP2=ifelse(adjP[,3] > 0.2, "", "*"),
                    restab[10:12], adjP[,4], sigadjP3=ifelse(adjP[,4] > 0.2, "", "*"),
                    restab[13:15], adjP[,5], sigadjP4=ifelse(adjP[,5] > 0.2, "", "*"))

colnames(table) <- c("Response", "Org, Chi", "P", "", "P adj", "",
                     "L. compo, Chi", "P", "","P adj", "",
                     "L. config, Chi", "P", "","P adj", "",
                     "Org: compo, Chi", "P", "","P adj", "",
                     "Org: config, Chi", "P", "","P adj", "")

write.csv(table, "Tables/Model1_OrgLand_ANOVAresWithAdjP.csv")




### 2) Models -----------------------------------------------
library(performance) # for r2 function used in sjplot

estres <- data.frame()

for (i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # get the model
  mypath <- file.path(getwd(), "Output", "Models_1OrgLandscape",paste("ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  
  mod <- readRDS(mypath)
  
  # organize coeff and SE per explan var
  org <- cbind(get_model_data(mod, type = "est")[1,2], get_model_data(mod, type = "est")[1,3])
  lcp <- cbind(get_model_data(mod, type = "est")[2,2], get_model_data(mod, type = "est")[2,3])
  lcf <- cbind(get_model_data(mod, type = "est")[3,2], get_model_data(mod, type = "est")[3,3])
  o.lcp <- cbind(get_model_data(mod, type = "est")[4,2], get_model_data(mod, type = "est")[4,3])
  o.lcf <- cbind(get_model_data(mod, type = "est")[5,2], get_model_data(mod, type = "est")[5,3])
  sigma <- summary(mod)$sigma
  no.obs <- summary(mod)$devcomp$dims[1]
  R2 <- cbind(as.numeric(r2(mod)$R2_marginal), as.numeric(r2(mod)$R2_conditional))
  
  rm(list = "mod")  
  estres <-  rbind(estres, 
                   data.frame(org, lcp, lcf, o.lcp, o.lcf, 
                              sigma, no.obs, R2))
}

rownames(estres) <- WhichResponseVar
colnames(estres) <- c("Organic, Estimate","SE", 
                      "Landscape compo., Estimate","SE",
                      "Landscape config., Estimate","SE",
                      "Org:Landscape compo., Estimate","SE", 
                      "Org:Landscape config., Estimate","SE", 
                      "sigma", "N", "Marginal R2", "Conditional R2"
)
estres
write.csv(estres, "Tables/Model1_OrgLand_ESTIMATESres.csv")


### 3) Magnitude of effects in percent changes-------------

# make a big table showing percent changes and p-values together for organic effects and landscape effects

esdataorg <- read.csv("Output/Models_1OrgLandscape/ModelResults_MagnitudePredictEffects.csv")
esdataland <- read.csv("Output/Models_1OrgLandscape/ModelResults_MagnitudePredictEffects_Landscape.csv")

esdata <- left_join(esdataorg[,-c(1,3)], esdataland[,-1], by = "Response")

# add if effect was stat signif based on p-values
# anova data with pvalue and adjusted p
res <- read.csv("Tables/Model1_OrgLand_ANOVAresWithAdjP.csv")
res$X.8 <- rep("", length(res$X.8)) # replace NA with blanks (no asterisk in this column)

# take the asterisk associated with P and adjusted P
pval <- res %>% select(Response, X.1, X.2, X.7, X.8) %>% dplyr::rename(Pval_Organic = X.1, AdjPval_Organic = X.2, Pval_Interaction = X.7, AdjPval_Interaction=X.8)

# join pval and percent change data
table <- left_join(esdata, pval, by = "Response")

# remove rows not relevant (threshold emf, mean z scores)
table <- table %>% filter(!Response %in% c("MB_Thresh70", "MA_Thresh70", "EMF_Thresh70"))

# reorder columns
table <- table %>% select(Response, PercentChange, Pval_Organic, AdjPval_Organic, PercentChange_Org, PercentChange_Conv, Pval_Interaction, AdjPval_Interaction)

write.csv(table, "Tables/Model1_OrgLand_MAGNITUDE.csv")
table <- read.csv("Tables/Model1_OrgLand_MAGNITUDE.csv")

## Models of tradeoff production - biodiversity and services ------------------------

WhichResponseVar3 <- c("MB_MeanScMax", "MA_MeanScMax",
                       "EMF_Regulation", "EMF_Soil")

### 1) ANOVA ----------

#### Table showing Chi sqaure and P

# Makes a list of dataframes with Chi and P for each model

anovares <- data.frame()

for (i in 1:length(WhichResponseVar3)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar3[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # get the model
  mypath <- file.path(getwd(), "Output", "Models_2ProductionTradeoffs",paste("ModelResults_", WhichResponseVar3[i], ".rds", sep = ""))
  
  mod <- readRDS(mypath)
  
  # organize Chi and P per explan var
  org <- cbind(as.data.frame(Anova(mod)[1,-2]), sig=ifelse(Anova(mod)[1,3]< 0.05, "*", ""))
  lcp <- cbind(as.data.frame(Anova(mod)[2,-2]), sig=ifelse(Anova(mod)[2,3]< 0.05, "*", ""))
  lcf <- cbind(as.data.frame(Anova(mod)[3,-2]), sig=ifelse(Anova(mod)[3,3]< 0.05, "*", ""))
  prod <- cbind(as.data.frame(Anova(mod)[4,-2]), sig=ifelse(Anova(mod)[4,3]< 0.05, "*", ""))
  o.lcp <- cbind(as.data.frame(Anova(mod)[5,-2]), sig=ifelse(Anova(mod)[5,3]< 0.05, "*", ""))
  o.lcf <- cbind(as.data.frame(Anova(mod)[6,-2]), sig=ifelse(Anova(mod)[6,3]< 0.05, "*", ""))
  o.prod <- cbind(as.data.frame(Anova(mod)[7,-2]), sig=ifelse(Anova(mod)[7,3]< 0.05, "*", ""))
  
  anovares <-  rbind(anovares, 
                     data.frame(org, lcp, lcf, prod, o.lcp, o.lcf, o.prod))
}

rownames(anovares) <- WhichResponseVar3
colnames(anovares) <- c("Organic, Chi","Organic, P", "",
                        "Landscape compo., Chi","P", "",
                        "Landscape config., Chi","P", "",
                        "Production, Chi","P", "",
                        "Org:Landscape compo., Chi","P", "",
                        "Org:Landscape config., Chi","P", "",
                        "Org:Prod., Chi","P", ""
)
anovares
write.csv(anovares, "Tables/Model2_Tradeoffs_ANOVAres.csv")




### 2) Models ---------

estres <- data.frame()

for (i in 1:length(WhichResponseVar3)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar3[i]]
  dfa$landcompo <- df$SemiNatural
  dfa$landconfig <- df$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # get the model
  mypath <- file.path(getwd(), "Output", "Models_2ProductionTradeoffs",paste("ModelResults_", WhichResponseVar3[i], ".rds", sep = ""))
  
  mod <- readRDS(mypath)
  
  # organize coeff and SE per explan var
  org <- cbind(get_model_data(mod, type = "est")[1,2], get_model_data(mod, type = "est")[1,3])
  lcp <- cbind(get_model_data(mod, type = "est")[2,2], get_model_data(mod, type = "est")[2,3])
  lcf <- cbind(get_model_data(mod, type = "est")[3,2], get_model_data(mod, type = "est")[3,3])
  prod <- cbind(get_model_data(mod, type = "est")[4,2], get_model_data(mod, type = "est")[4,3])
  o.lcp <- cbind(get_model_data(mod, type = "est")[5,2], get_model_data(mod, type = "est")[5,3])
  o.lcf <- cbind(get_model_data(mod, type = "est")[6,2], get_model_data(mod, type = "est")[6,3])
  o.prod <- cbind(get_model_data(mod, type = "est")[7,2], get_model_data(mod, type = "est")[7,3])
  sigma <- summary(mod)$sigma
  no.obs <- summary(mod)$devcomp$dims[1]
  R2 <- cbind(as.numeric(r2(mod)$R2_marginal), as.numeric(r2(mod)$R2_conditional))
  
  rm(list = "mod")  
  estres <-  rbind(estres, 
                   data.frame(org, lcp, lcf, prod,
                              o.lcp, o.lcf, o.prod,
                              sigma, no.obs, R2))
}

rownames(estres) <- WhichResponseVar3
colnames(estres) <- c("Organic, Estimate","SE", 
                      "Landscape compo., Estimate","SE",
                      "Landscape config., Estimate","SE",
                      "Production, Estimate","SE",
                      "Org:Landscape compo., Estimate","SE", 
                      "Org:Landscape config., Estimate","SE", 
                      "Org:Production, Estimate","SE", 
                      "sigma", "N", "Marginal R2", "Conditional R2"
)
estres
write.csv(estres, "Tables/Model2_Tradeoffs_ESTIMATESres.csv")


### 3) multiabundance models ------------

mypath <- file.path(getwd(), "Output", "Models_2ProductionTradeoffs",paste("ModelResults_MA_MeanScMax",".rds", sep = ""))
modRE <- readRDS(mypath)

mypath2 <- file.path(getwd(), "Output", "Models_2ProductionTradeoffs",paste("ModelResults_MA_MeanScMax_WithoutRE",".rds", sep = ""))
modNoRE <- readRDS(mypath2)

# compare the results: they are identical
Anova(modRE);Anova(modNoRE)

# compare the estimates: they are identical
modRE; modNoRE

# manuscript will only show results from model with RE for philosophical reasons (keep RE structure to reflect the study design)

# # save Anova table
# write.csv(as.data.frame(Anova(modRE)), "Tables/Model2_Tradeoffs_ANOVAres_MultiAb_updated.csv")
# 
# # save multiabundance model results in a html table for supplementary information, but for gls, no R2
# tab_model(mod, file = "Tables/Model2_Tradeoffs_ESTIMATESres_MultiAb_updated.html")

