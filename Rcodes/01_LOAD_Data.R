## This script loads the diversity and function data, calculates multifunctionality and multidiversity indices, and creates an overall dataset for the multidiv-multifun analysis

rm(list = ls())

# Functions ---------
library(dplyr)
source("Rcodes/FUN_MultiDivFunction_Allan2014.R")  # multidiv/multifun function

# 1. Load the output data site, management, landscape, diversity and ecosystem functions------
LocalManagementData <- read.csv("Data/LocalManagement_clean.csv")
LandData <- read.csv("Data/LandscapeMetrics_clean.csv")

# diversity (richness and abundance data). The taxonomic resolution  is species, genus or family depending on the taxa group (see the detailed methodology in Supplementary information associated with the manuscript and in the metadata provided in Data/METADATA.xlsx)
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
  rename(SoilPhosphatase = Phosphatase, SoilGlucosidase=Glucosidase, SoilUrease=Urease)
PredLobesiaData <- read.csv("Data/PredLobesia_clean.csv")
PredLobesiaData <- PredLobesiaData[,-1]
PredPlastiData <- read.csv("Data/PredPlasticine_clean.csv")
PredPlastiData <- PredPlastiData[,-1]
PathogenData <- read.csv("Data/PathogenDamageData_clean.csv")
PathogenData <- PathogenData[,-1]
PestData <- read.csv("Data/PestDamageData_clean.csv")
PestData <- PestData[,-1]

# 2. Calculate multidiversity -----------------------------
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
  MB_MeanScMax = multidiv(Multidiv)[,1],  # multidiversity: averages of diversities scaled by the max, 
  MB_MeanZscore = multidiv(Multidiv, cent = TRUE, sc = "sd")[,1],  # multidiversity: average of z-scores 
  MB_Thresh70 = multidiv(Multidiv, threshold = 0.7)[,1] , # multidiversity: threshold 70%
  MB_N = multidiv(Multidiv)[,2])

plot(MultidivData)
cor(MultidivData[,-1])

# dataset with species and pollinators genus richness 
Multiab <- BiodivData[,grepl("Abundance", names(BiodivData))|
                        grepl("Cover", names(BiodivData))]

MultiabData <- data.frame(
  PlotID = BiodivData$PlotID,
  # Multidiversity indices
  MA_MeanScMax = multidiv(Multiab)[,1],  # multidiversity: averages of diversities scaled by the max, 
  MA_MeanZscore = multidiv(Multiab, cent = TRUE, sc = "sd")[,1],  # multidiversity: average of z-scores 
  MA_Thresh70 = multidiv(Multiab, threshold = 0.7)[,1] , # multidiversity: threshold 70%
  MA_N = multidiv(Multiab)[,2])

plot(MultiabData)
cor(MultiabData[,-1])


# 3. Calculate multifunctionality -----------------------
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
    EMF_MeanScMax = multidiv(MultifunData %>% select(EMF_Production, EMF_Soil, EMF_Regulation))[,1],  # averages of functions scaled by the max,
    EMF_MeanZscore = multidiv(MultifunData %>% select(EMF_Production, EMF_Soil, EMF_Regulation), cent = TRUE, sc = "sd")[,1],  #  average of z-scores 
    EMF_Thresh70 = multidiv(MultifunData %>% select(EMF_Production, EMF_Soil, EMF_Regulation), threshold = 0.7)[,1],  # threshold 70%)
    
  )

plot(MultifunData)
cor(MultifunData[,-1])


# 4. Store a big dataset with all data, ready for analysis ---------------------

MultiDiversity <- list(LocalManagementData, LandData, BiodivData, MultidivData, MultiabData, FunctionData, MultifunData) %>% 
  Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = "PlotID"), .)

write.csv(MultiDiversity, "Output/MultiDiversity_ForAnalysis.csv")
