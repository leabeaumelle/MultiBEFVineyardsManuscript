## This script provides additional analyses requested by reviewers


# Functions ------
library(rio)
library(dplyr)
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

# 1. Landscape at 1000 m effect on multidiversity-----------------

# Load SNH data at 1000 m
ldat <- import("Data/LandscapeSNH1000m.xlsx")
ldat$PlotID <- as.factor(ldat$PlotID)

# Load biodiv data
df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")
df <- df[,-c(1:2)]

# add 1000m landscape data
df <- left_join(df, ldat %>% select(SNH, PlotID), by = "PlotID")

## 0. Correlation SNH at 500 and 1000 m
cor.test(df$SNH, df$SemiNatural)


# vector of all the response variables (biodiv only)
WhichResponseVar <- c(
  df %>% select(starts_with("MB"), -MB_N) %>% colnames(),  # multidiv indices
  df %>% select(ends_with("Richness")) %>% colnames()  # 14 taxa groups
)

## I. Model Organic farming x landscape effects -----------------

### 1/ Fit the models for all response variables -----------
for(i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SNH
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  dfa <- dfa[!is.na(dfa$respvar),]
  
  # fit the model 
  mod <- lmer(respvar ~ Organic*landcompo + (1|LandscapeID), data = dfa)
  
  # create custom file path name to store the model output
  mypath <- file.path(getwd(), "Output", "Revisions", paste("Land1000ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  
  # store model output
  saveRDS(mod, file = mypath)
  
}

## II. Check model results
anovares <- data.frame()

for (i in 1:length(WhichResponseVar)){
  dfa <- df
  dfa$respvar <- df[,WhichResponseVar[i]]
  dfa$landcompo <- df$SNH
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  
  # get the model
  mypath <- file.path(getwd(), "Output", "Revisions", paste("Land1000ModelResults_", WhichResponseVar[i], ".rds", sep = ""))
  
  mod <- readRDS(mypath)
  
  # organize Chi and P per explan var
  org <- cbind(as.data.frame(Anova(mod)[1,-2]), sig=ifelse(Anova(mod)[1,3]< 0.05, "*", ""))
  lcp <- cbind(as.data.frame(Anova(mod)[2,-2]), sig=ifelse(Anova(mod)[2,3]< 0.05, "*", ""))
  o.lcp <- cbind(as.data.frame(Anova(mod)[3,-2]), sig=ifelse(Anova(mod)[3,3]< 0.05, "*", ""))
  
  anovares <-  rbind(anovares, 
                     data.frame(org, lcp, o.lcp))
}

rownames(anovares) <- WhichResponseVar
colnames(anovares) <- c("Organic, Chi","Organic, P", "sig1",
                        "Landscape compo., Chi","Landscape compo., P", "sig2",
                        "Org:Landscape compo., Chi","Org:Landscape compo., P", "sig4")
anovares
write.csv(anovares, "Output/Revisions/Land1000TableModelResults.csv")

# Results show significant effect of SNH proportion at 1000 m on butterflies and syrphids only
# also a significant interaction OF-SNH on foliage spider richness.

# compare with 500 m radius results
anovares2 <- read.csv("Tables/Model1_OrgLand_ANOVAres.csv")
anovares2[1:17,]

# similar effects on syrphids and butterflies
# similar effect on spider richness
# but there was an effect on coleoptera richness at 500m that is not there at 1000m



# 2. EMF thresholds----------------------------
## Functions ---------
library(dplyr)
source("Rcodes/FUN_MultiDivFunction_Allan2014.R")  # multidiv/multifun function

## I. Quantify EMF with different thresholds : 50, 60, 70, 80% of the maximum------
# 1. Load the output data site, management, landscape, diversity and ecosystem functions------
LocalManagementData <- read.csv("Data/LocalManagement_clean.csv")
LandData <- read.csv("Data/LandscapeMetrics_clean.csv")

# diversity (richness and abundance data). 
# The taxonomic resolution  is species, genus or family depending on the taxa group (see the detailed methodology in Supplementary information associated with the manuscript and in the metadata provided in Data/METADATA.xlsx)
BirdData <- read.csv("Data/BirdData.csv")
BirdData <- BirdData[,-1]
ButterfliesData <- read.csv("Data/ButterfliesData.csv")
ButterfliesData <- ButterfliesData[,-1]
EwormData <- read.csv("Data/EwormData.csv")
EwormData <- EwormData[,-1]
MicrobeData <- read.csv("Data/MicrobesData.csv")
MicrobeData <- MicrobeData[,-1]
PlantData <- read.csv("Data/PlantData.csv")
PlantData <- PlantData[,-1]
PollinatorData <- read.csv("Data/Pollinators.csv")
PollinatorData <- PollinatorData[,-1]
SpiderData <- read.csv("Data/SpiderData.csv")
SpiderData <- SpiderData[,-1]
CarabData <- read.csv("Data/CarabidaeData.csv")
CarabData <- CarabData[,-1]
CollemboData <- read.csv("Data/CollembolaData.csv")
CollemboData <- CollemboData[,-1]
SpiderFoliageData <- read.csv("Data/SpiderFoliageData.csv")
SpiderFoliageData <- SpiderFoliageData[,-1]
ColeopteraFoliageData <- read.csv("Data/ColeopteraFoliageData.csv")
ColeopteraFoliageData <- ColeopteraFoliageData[,-1]
HemipteraFoliageData <- read.csv("Data/HemipteraFoliageData.csv")  # Hemiptera = Auchenorrhyncha (leafhoppers, aphrophoridae,..), but calling them hemiptera for simplifying the code
HemipteraFoliageData <- HemipteraFoliageData[,-1]
HeteropteraFoliageData <- read.csv("Data/HeteropteraFoliageData.csv")
HeteropteraFoliageData <- HeteropteraFoliageData[,-1]


# functions and proxies of ecosystem services
YieldData <- read.csv("Data/YieldData_20210224.csv")
YieldData <- YieldData[,-1]
# missing yield for plot 16B: imputation based on yields of other plots under organic
val <- mean(
  c(YieldData[grepl("B", YieldData$PlotID),"YieldRealised_hlperha"], 
    YieldData[grepl("B", YieldData$PlotID),"YieldMean5years_hlperha"]), 
  na.rm = TRUE)

YieldData[is.na(YieldData$YieldRealised_hlperha),"YieldRealised_hlperha"] <- val

ChloroData <- read.csv("Data/ChlorophyllData_20210326.csv")
ChloroData <- ChloroData[,-1]
DecompoData <- read.csv("Data/DecompositionData_clean.csv")
DecompoData <- DecompoData[,-1]
SoilCNData <- read.csv("Data/SoilCNData_clean.csv")
SoilCNData <- SoilCNData[,-1]
SoilEnzData <- read.csv("Data/SoilEnzymes.csv")
SoilEnzData <- SoilEnzData[,-1]
SoilEnzData <- SoilEnzData %>% 
  dplyr::rename(SoilPhosphatase = Phosphatase, SoilGlucosidase=Glucosidase, SoilUrease=Urease)
PredLobesiaData <- read.csv("Data/PredLobesia_clean.csv")
PredLobesiaData <- PredLobesiaData[,-1]
PredPlastiData <- read.csv("Data/PredPlasticine_clean.csv")
PredPlastiData <- PredPlastiData[,-1]
PathogenData <- read.csv("Data/PathogenDamageData_clean.csv")
PathogenData <- PathogenData[,-1]
PestData <- read.csv("Data/PestDamageData_clean.csv")
PestData <- PestData[,-1]

## 2. Calculate multidiversity -----------------------------
# join multiple df with dplyr::left join and base R Reduce
BiodivData <- list(BirdData, ButterfliesData, EwormData, 
                   MicrobeData, PlantData, PollinatorData, SpiderData, CarabData, CollemboData, 
                   SpiderFoliageData, ColeopteraFoliageData, HemipteraFoliageData, HeteropteraFoliageData) %>% 
  Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = "PlotID"), .)

# dataset with species and pollinators genus richness 
Multidiv <- BiodivData[,grepl("Richness", names(BiodivData))]

MultidivData <- data.frame(
  PlotID = BiodivData$PlotID,
  # Multidiversity indices
  MB_Thresh70 = multidiv(Multidiv, threshold = 0.7)[,1] , # multidiversity: threshold 70%
  MB_Thresh50 = multidiv(Multidiv, threshold = 0.5)[,1] , # multidiversity
  MB_Thresh60 = multidiv(Multidiv, threshold = 0.6)[,1] , # multidiversity
  MB_Thresh80 = multidiv(Multidiv, threshold = 0.8)[,1] , # multidiversity
  MB_N = multidiv(Multidiv)[,2])

plot(MultidivData)
cor(MultidivData[,-1]) # multidiversity indices are strongly correlated with MB 70%: 0.7, 0.9, 0.7 for 50, 60 and 80% respectively


# dataset with species and pollinators genus richness 
Multiab <- BiodivData[,grepl("Abundance", names(BiodivData))|
                        grepl("Cover", names(BiodivData))]

MultiabData <- data.frame(
  PlotID = BiodivData$PlotID,
  # Multidiversity indices
  MA_Thresh70 = multidiv(Multiab, threshold = 0.7)[,1] , # multidiversity: threshold 70%
  MA_Thresh50 = multidiv(Multiab, threshold = 0.5)[,1] , # multidiversity: threshold 50%
  MA_Thresh60 = multidiv(Multiab, threshold = 0.6)[,1] , # multidiversity: threshold 60%
  MA_Thresh80 = multidiv(Multiab, threshold = 0.8)[,1] , # multidiversity: threshold 80%
  MA_N = multidiv(Multiab)[,2])

plot(MultiabData)
cor(MultiabData[,-1]) # multiab indices strongly correlated with MA 70%: 0.7, 0.8, 0.9 for 50,60,80% thresholds respectively


## 3. Calculate multifunctionality -----------------------
# join multiple df with dplyr::left join and base R Reduce
FunctionData <- list(YieldData, ChloroData, 
                     SoilCNData, DecompoData, SoilEnzData,
                     PredLobesiaData, PredPlastiData, PathogenData, PestData) %>% 
  Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = "PlotID"), .)


# subset topic functions
Multifun <- FunctionData %>% 
  select(
    YieldRealised_hlperha, Ntester,
    SoilC, SoilN, DecompokRate, DecompoSfactor, SoilPhosphatase, SoilGlucosidase, SoilUrease,
    PredRateEggs, PredRatePupae, PredRatePlasticine,
    PathogenLackOf, PestLackOf)

# calculate multifun indices
MultifunData <- data.frame(
  PlotID = FunctionData$PlotID,
  
  # one EMF per ecosystem service
  EMF_Production = multidiv(Multifun %>% select(YieldRealised_hlperha, Ntester))[,1],
  EMF_Soil = multidiv(Multifun %>% select(contains("Decompo"), contains("Soil")))[,1],
  EMF_Regulation = multidiv(Multifun %>% select(PathogenLackOf, PestLackOf, contains("Pred")))[,1],
  
  EMF_N = multidiv(Multifun)[,2])

# Overall EMF giving equal weight to each of three ecosystem services considered
MultifunData <- MultifunData %>% 
  mutate(
    # weighted EMF
    EMF_Thresh70 = multidiv(MultifunData %>% select(EMF_Production, EMF_Soil, EMF_Regulation), threshold = 0.7)[,1],  # threshold 70%)
    EMF_Thresh50 = multidiv(MultifunData %>% select(EMF_Production, EMF_Soil, EMF_Regulation), threshold = 0.5)[,1],  # threshold 50%)
    EMF_Thresh60 = multidiv(MultifunData %>% select(EMF_Production, EMF_Soil, EMF_Regulation), threshold = 0.6)[,1],  # threshold 60%)
    EMF_Thresh80 = multidiv(MultifunData %>% select(EMF_Production, EMF_Soil, EMF_Regulation), threshold = 0.8)[,1],  # threshold 80%)
    
  )


## 4. Store a big dataset with all data, ready for analysis ---------------------

MultiDiversity <- list(LocalManagementData, LandData, BiodivData, MultidivData, MultiabData, FunctionData, MultifunData) %>% 
  Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = "PlotID"), .)

write.csv(MultiDiversity, "Output/Revisions/MultidiversityForAnalysisWithDifferentThresholds.csv")

## II. Make supplementary figures with the EMF thresholds------------------------

library(rlang)  # needed to call unquoted col in ggplot in custom function
library(ggplot2)
library(viridis)
library(patchwork)

df <- read.csv("Output/Revisions/MultidiversityForAnalysisWithDifferentThresholds.csv")
df <- df[,-c(1:2)]

# ggplot options
mycols <- viridis_pal(option = "A")(6)[c(2, 5)]
sizelegend <- 7
sizetitle <- 8

# my custom function
myfunboxplot <- function(data, Yvar, Xvar, mycols, mytitle, myaxistitle ){
  ggplot(data, aes(y = {{Yvar}}, x = {{Xvar}}, col = Xvar))+
    geom_boxplot(col = mycols)+
    ylab(myaxistitle)+
    ggtitle(mytitle)+
    theme_bw()+
    theme(legend.position = "none",
          plot.title = element_text(face = "bold",size = sizetitle),
          axis.text.y=element_text(size = sizelegend),
          axis.text.x=element_text(size = sizelegend),
          axis.title.y = element_text(size= sizetitle),
          axis.title.x = element_blank())
}


## Multidiversity, abundance and function indices-----------------------

BigPlotEMF <- 
  
  myfunboxplot(data = df, Yvar = MB_Thresh50, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic diversity - 50%", myaxistitle ="index (threshold)")+
  myfunboxplot(data = df, Yvar = MB_Thresh60, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic diversity - 60%", myaxistitle ="index (threshold)")+
  myfunboxplot(data = df, Yvar = MB_Thresh70, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic diversity - 70%", myaxistitle ="index (threshold)")+
  myfunboxplot(data = df, Yvar = MB_Thresh80, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic diversity - 80%", myaxistitle ="index (threshold)")+
  
  
  myfunboxplot(data = df, Yvar = MA_Thresh50, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic abundance- 50%", myaxistitle = "index (threshold)")+
  myfunboxplot(data = df, Yvar = MA_Thresh60, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic abundance- 60%", myaxistitle = "index (threshold)")+
  myfunboxplot(data = df, Yvar = MA_Thresh70, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic abundance- 70%", myaxistitle = "index (threshold)")+
  myfunboxplot(data = df, Yvar = MA_Thresh80, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic abundance - 80%", myaxistitle = "index (threshold)")+
  
  
  
  myfunboxplot(data = df, Yvar = EMF_Thresh50, Xvar = Organic, mycols = mycols, mytitle = "Multifunctionality-50%", myaxistitle = "index (threshold)")+
  myfunboxplot(data = df, Yvar = EMF_Thresh60, Xvar = Organic, mycols = mycols, mytitle = "Multifunctionality-60%", myaxistitle = "index (threshold)")+
  myfunboxplot(data = df, Yvar = EMF_Thresh70, Xvar = Organic, mycols = mycols, mytitle = "Multifunctionality-70%", myaxistitle = "index (threshold)")+
  myfunboxplot(data = df, Yvar = EMF_Thresh80, Xvar = Organic, mycols = mycols, mytitle = "Multifunctionality-80%", myaxistitle = "index (threshold)")+
  plot_layout(ncol = 3, nrow = 4)



## Supplement figure for raw biodiv data
ppi <- 300

png("Figures/FigSupp_EMFThresholds.png",
    width=20,
    height=15,
    units = "cm",
    res=ppi)
BigPlotEMF+plot_layout(ncol = 4)
dev.off()




# 3. Pest control index------
## 1. Pest control index without pathogen damage----------
PestPathoData <- list(
  PredLobesiaData, PredPlastiData, PathogenData, PestData) %>% 
  Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = "PlotID"), .)


# subset topic functions
Multipestpatho <- PestPathoData %>% 
  select(
    PredRateEggs, PredRatePupae, PredRatePlasticine,
    PathogenLackOf, PestLackOf)

# calculate multifun indices
MultiPestPathoData <- data.frame(
  PlotID = PestPathoData$PlotID,
  
  # one EMF per ecosystem service
  EMF_Regulation = multidiv(Multipestpatho %>% select(PathogenLackOf, PestLackOf, contains("Pred")))[,1],
  EMF_PestRegulation = multidiv(Multipestpatho %>% select(PestLackOf, contains("Pred")))[,1],
  
  EMF_N = multidiv(Multipestpatho)[,2])

# compare both indices
plot(MultiPestPathoData$EMF_PestRegulation, MultiPestPathoData$EMF_Regulation, 
     xlab = "Pest control index - only pest", ylab = "Pest control index - with pathogen"); abline(0,1)

# correlation 0.99, p < 2.2e-16
cor.test(MultiPestPathoData$EMF_PestRegulation, MultiPestPathoData$EMF_Regulation)

# data for analysis

PestIndex <- list(LocalManagementData, LandData, MultiPestPathoData) %>% 
  Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = "PlotID"), .)

write.csv(PestIndex, "Output/Revisions/PestIndexWithoutPathogen.csv")


## 2.refit models of of tradeoff with e-----------
# load data
dfa <- read.csv("Output/Revisions/PestIndexWithoutPathogen.csv")
df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")

# add EMF production
dfa$EMF_Production <- df$EMF_Production

## Tradeoff Pest regulation - production ---------------------------------------

P1 <- ggplot(dfa, aes(x=EMF_Production, y = EMF_Regulation, group = Organic))+
  geom_point()+geom_smooth(method = "lm")

P2<- ggplot(dfa, aes(x=EMF_Production, y = EMF_PestRegulation, group = Organic))+
  geom_point()+geom_smooth(method = "lm")

P1+P2


# fit models
mod0 <- lmer(EMF_Regulation ~ Organic*SemiNatural + Organic*Dist.snh + Organic*EMF_Production+ (1|LandscapeID), data = dfa)
mod1 <- lmer(EMF_PestRegulation ~ Organic*SemiNatural + Organic*Dist.snh + Organic*EMF_Production+ (1|LandscapeID), data = dfa)

# save the model
saveRDS(mod1, "Output/Revisions/PestIndexModelWithoutPathogen.rds")

# Check the anova table
Anova(mod1)

# make anova table to compare both models
anovares <- cbind(as.data.frame(Anova(mod0)), as.data.frame(Anova(mod1)))
anovares

write.csv(Anova(mod1), 'Output/Revisions/PestIndexWithoutPathoTableModelResults.csv')

# make a figure to show similar results-------------------
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
library(sjPlot)
library(ggeffects)
library(ggrepel)
library(vegan)
library(scales)

# ggplot colors and size ------------
# set sizes of text in plots
sizetext <- 10
sizelegend <- 8

ppi = 300
w = 20

show_col(viridis_pal(option="A")(6))
mycols <- viridis_pal(option = "A")(6)[c(2, 5)]


## EMF Regulation ------------

### Models and data with pathogen damage
mod0 <- readRDS("Output/Models_2ProductionTradeoffs/ModelResults_EMF_Regulation.rds")
df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")

# Data associated with the model ---
dfa <- df
dfa$respvar <- df$EMF_Regulation
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa$group <- dfa$Organic
dfa <- dfa[!is.na(dfa$respvar), ]

# model predictions (at mean landscape compo and config)
me <- ggpredict(mod0, c("EMF_Production", "Organic"))
me <- me[-1,]  # plot only over the range of observed values for conventional 

### Panel A --------

FSA <- ggplot(me, aes(x = x, y = predicted, colour = group), col = mycols) +
  geom_line(size = 1, lty = "solid")+
  geom_ribbon(inherit.aes = FALSE, 
              mapping = aes(x = x, y = predicted, group = group, fill = group,
                            ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.15)+
  geom_point(data = dfa, mapping = aes(x = EMF_Production, y = respvar, fill = group, shape = group), 
             size = 2.2, colour = "black")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Production")+
  ylab("Pest control")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = sizelegend),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = sizelegend),
        axis.text.x=element_text(face = "bold", size = sizelegend),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))+
  annotate("text", x = 0.4, y=0.94, label = "A")

### Models and data WIHTOUT pathogen damage
mod1 <- readRDS("Output/Revisions/PestIndexModelWithoutPathogen.rds")

dfa <- read.csv("Output/Revisions/PestIndexWithoutPathogen.csv")
dfa$EMF_Production <- df$EMF_Production

# Data associated with the model ---
dfa$respvar <- dfa$EMF_PestRegulation
dfa$landcompo <- dfa$SemiNatural
dfa$landconfig <- dfa$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa$group <- dfa$Organic
dfa <- dfa[!is.na(dfa$respvar), ]

# model predictions (at mean landscape compo and config)
me <- ggpredict(mod1, c("EMF_Production", "Organic"))
me <- me[-1,]  # plot only over the range of observed values for conventional 

### Panel B --------

FSB <- ggplot(me, aes(x = x, y = predicted, colour = group), col = mycols) +
  geom_line(size = 1, lty = "solid")+
  geom_ribbon(inherit.aes = FALSE, 
              mapping = aes(x = x, y = predicted, group = group, fill = group,
                            ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.15)+
  geom_point(data = dfa, mapping = aes(x = EMF_Production, y = respvar, fill = group, shape = group), 
             size = 2.2, colour = "black")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Production")+
  ylab("Pest control excluding lack of pathogen damage")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = sizelegend),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = sizelegend),
        axis.text.x=element_text(face = "bold", size = sizelegend),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))+
  annotate("text", x = 0.4, y=0.94, label = "B")


## save a png with high res ---------
png("Figures/FigSupp_PestIndexWithoutPatho.png",
    width=20,
    height=10,
    units = "cm",
    res=ppi)

(FSA+FSB)
dev.off()

