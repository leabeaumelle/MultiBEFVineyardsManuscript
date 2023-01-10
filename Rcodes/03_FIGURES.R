## This script makes the figures

rm(list = ls())

# Functions ----------------
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)


# Figure 1 - Multidiv vs multifun--------------------------------------

esdata <- read.csv("Output/Models_1OrgLandscape/ModelResults_EffectSizeOrganicAcross.csv")

esdata$ResponseVar <- esdata[,1]

# categories of response variables 
esdata$TypeResponseVar <- factor(ifelse(grepl("Richness", esdata$ResponseVar), "Diversity", 
                                        ifelse(grepl("Abundance|Cover", esdata$ResponseVar), "Abundance", 
                                               ifelse(grepl("MB", esdata$ResponseVar), "Multidiversity", 
                                                      ifelse(grepl("MA", esdata$ResponseVar), "Multiabundance", 
                                                             ifelse(grepl("EMF", esdata$ResponseVar), "Multifunctionality", 
                                                                    "Functions"))))))



library(scales)
show_col(viridis_pal()(6))

# cols v4
mycols2 <- viridis_pal()(6)
mycolsfun <- c(rep(mycols2[5], 2), # production 
               mycols2[6],  # soil
               mycols2[3],  # pests
               rep(mycols2[6],3), 
               mycols2[3],
               rep(mycols2[6],3), 
               rep(mycols2[3], 3))
mycolsfun <- ifelse(mycolsfun == "#FDE725FF", "#f9c74f", mycolsfun)

mycolsemf <- c(mycols2[5], "black", mycols2[6], mycols2[3])
mycolsemf <- ifelse(mycolsemf == "#FDE725FF", "#f9c74f", mycolsemf)

mypchfun <- c(rep(17, 2),
              15,
              16,
              rep(15, 3),
              16, 
              rep(15,3) ,
              rep(16, 3))

mypchemf <- c(17, 18, 15, 16)

### diversity panels ------
plot2 <- esdata %>% 
  filter(TypeResponseVar=="Diversity") %>% 
  mutate(ResponseVar=factor(gsub("Richness", "", ResponseVar))) %>% 
  arrange(estimate) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(ResponseVar=factor(ResponseVar, levels=ResponseVar)) %>%   # This trick update the factor levels
  ggplot(., aes(x=ResponseVar, y=estimate)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, colour="#AA4465") +
  geom_point(colour="#AA4465")+
  geom_hline(yintercept = 0)+
  ylim(-1.6, 1.6)+
  scale_x_discrete(breaks = waiver(), 
                   labels = c("Ground beetles","Earthworms","Bees","Soil microbes","Plants", "Syrphids","Collembola", "Foliage Heteroptera",   "Birds", "Butterflies", "Foliage Coleoptera","Foliage spiders", "Foliage Auchenorrhyncha", "Ground-dwelling spiders"))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=45, hjust = 1,, size=11), 
        axis.title.x = element_blank(),
        legend.position = "none")+
  ggtitle("Diversity")+
  annotate("text", x = 0.9, y=1.5, label = "A")

plot2b <- esdata %>% 
  filter(ResponseVar=="MB_MeanScMax") %>% 
  # arrange(estimate) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  # mutate(ResponseVar=factor(ResponseVar, levels=ResponseVar)) %>%   # This trick update the factor levels
  ggplot(., aes(x=ResponseVar, y=estimate)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.05, colour="#AA4465") +
  geom_point(colour="#AA4465", size = 2)+
  geom_hline(yintercept = 0)+
  scale_x_discrete(breaks = waiver(), 
                   labels = "Index")+
  ylim(-1.6, 1.6)+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=45, hjust = 1,, size=11), 
        axis.title.x = element_blank(),
        legend.position = "none", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  ggtitle("Multitrophic diversity")+
  annotate("text", x = 0.5, y=1.5, label = "B")

### fun panels ------
plot3 <- esdata %>% 
  filter(TypeResponseVar=="Functions") %>% 
  arrange(estimate) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(ResponseVar=factor(ResponseVar, levels=ResponseVar)) %>%   # This trick update the factor levels
  ggplot(., aes(x=ResponseVar, y=estimate)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, 
                colour=mycolsfun) +
  geom_point(colour=mycolsfun, shape = mypchfun, size = 2)+
  geom_hline(yintercept = 0)+
  ylim(-1.6, 1.6)+
  scale_x_discrete(breaks = waiver(),
                   labels = c("Chlorophyll", "Yield", "Decomposition : rate", "Pest damage", "Soil phosphatase activity", "Soil carbon", "Soil urease activity", "Predation: pupae", "Soil nitrogen", "Decomposition : stabilisation", "Soil beta-glucosidase activity", "Predation: model caterpillars", "Predation: eggs", "Pathogen damage"))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=45, hjust = 1,, size=11), 
        axis.title.x = element_blank(),
        legend.position = "none")+
  ggtitle("Functions and proxies of ecosystem services")+
  annotate("text", x = 0.9, y=1.5, label = "C")

plot3b <- esdata %>% 
  filter(ResponseVar %in% c("EMF_MeanScMax", "EMF_Production", "EMF_Regulation", "EMF_Soil")) %>% 
  arrange(estimate) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(ResponseVar=factor(ResponseVar, levels=ResponseVar)) %>%   # This trick update the factor levels
  ggplot(., aes(x=ResponseVar, y=estimate)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.15, colour=mycolsemf) +
  geom_point(colour=mycolsemf, shape=mypchemf, size = 2)+
  geom_hline(yintercept = 0)+
  ylim(-1.6, 1.6)+
  scale_x_discrete(breaks = waiver(),
                   labels = c("Production", "EMF Index", "Soil", "Pest control"))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=45, hjust = 1,, size=11), 
        axis.title.x = element_blank(),
        legend.position = "none", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  ggtitle("Multifunctionality")+
  annotate("text", x = 0.7, y=1.5, label = "D")


# plot2+plot2b+
#   plot_layout(widths = c(2, 1))+
#   plot3+plot3b+
#   plot_layout(widths = c(2, 1))

### save png ---------------
w = 20
ppi <- 300

png("Figures/Fig1.png",
    width=w,
    height=w,
    units = "cm",
    res=ppi)
plot2+plot2b+
  plot_layout(widths = c(2, 1))+
  plot3+plot3b+
  plot_layout(widths = c(2, 1))
dev.off()


### Supplement figure abundance panel --------

##### plot options ----
w = 20
ppi <- 300

##### ggplots ----
plot1 <- esdata %>% 
  filter(TypeResponseVar=="Abundance") %>% 
  mutate(ResponseVar=factor(gsub("Abundance|Cover", "", ResponseVar))) %>%
  arrange(estimate) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(ResponseVar=factor(ResponseVar, levels=ResponseVar)) %>%   # This trick update the factor levels
  
  ggplot(., aes(x=ResponseVar, y=estimate)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, colour="#AA4465") +
  geom_point(colour="#AA4465")+
  ylim(-1.6, 1.6) +
  geom_hline(yintercept = 0)+
  scale_x_discrete(breaks = waiver(), 
                   labels = c("Earthworms", "Ground beetles", "Plants", "Syrphids","Bees", "Birds","Foliage spiders","Collembola",  "Butterflies",  "Foliage Heteroptera", "Foliage Coleoptera",  "Foliage Auchenorrhyncha", "Soil microbes", "Ground-dwelling spiders")) +
  theme_bw()+
  theme(axis.text.x  = element_text(angle=45, hjust = 1,, size=11),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ggtitle("Abundance")


plot1b <- esdata %>% 
  filter(ResponseVar=="MA_MeanScMax") %>% 
  ggplot(., aes(x=ResponseVar, y=estimate)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.05, colour="#AA4465") +
  geom_point(colour="#AA4465")+
  geom_hline(yintercept = 0)+
  ylim(-1.6, 1.6)+
  scale_x_discrete(breaks = waiver(), 
                   labels = c("Index"))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=45, hjust = 1,, size=11), 
        axis.title.x = element_blank(),
        legend.position = "none", 
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  ggtitle("Multitrophic abundance")


##### save png ----
png("Figures/FigSupp_AbundanceOF.png",
    width=w,
    height=w/2,
    units = "cm",
    res=ppi)
plot1+plot1b+plot_layout(widths = c(2, 1))
dev.off()








# Figure 2 - Tradeoffs with agricultural production -------------------
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
library(sjPlot)
library(ggeffects)
library(ggrepel)
library(vegan)
library(scales)

# Load data -----
df <- read.csv("/Users/save/Nextcloud/Science/Postdoc_Bordeaux/MultiBEFVineyards/Output/MultiDiversity_ForAnalysis.csv")
df <- df[,-c(1:2)]

# ggplot colors and size ------------
# set sizes of text in plots
sizetext <- 10
sizelegend <- 8

ppi = 300
w = 20

show_col(viridis_pal(option="A")(6))
mycols <- viridis_pal(option = "A")(6)[c(2, 5)]


## Multitrophic diversity --------

### Models and data --------
mod2 <- readRDS("Output/Models_2ProductionTradeoffs/ModelResults_MB_MeanScMax.rds")

# Data associated with the model ---
dfa <- df
dfa$respvar <- df$MB_MeanScMax
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa$group <- dfa$Organic
dfa <- dfa[!is.na(dfa$respvar), ]

# model predictions (at mean landscape compo and config)
me <- ggpredict(mod2, c("EMF_Production", "Organic"))
me <- me[-1,]  # plot only over the range of observed values for conventional 

### Panel A - biodiv --------
F2A <- ggplot(me, aes(x = x, y = predicted, colour = group), col = mycols) +
  geom_line(size = 1, lty = "dashed")+
  # geom_ribbon(inherit.aes = FALSE, 
  #             mapping = aes(x = x, y = predicted, group = group, fill = group,
  #                           ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.15)+
  geom_point(data = dfa, mapping = aes(x = EMF_Production, y = respvar, fill = group, shape = group), 
             size = 2.2, colour = "black")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Production")+
  ylab("Multitrophic diversity")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = sizelegend),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = sizelegend),
        axis.text.x=element_text(face = "bold", size = sizelegend),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))+
  annotate("text", x = 0.4, y=0.64, label = "A")

## Multitrophic abundance -------

### Models and data --------
mod2 <- readRDS("Output/Models_2ProductionTradeoffs/ModelResults_MA_MeanScMax.rds")


# Data associated with the model ---
dfa <- df
dfa$respvar <- df$MA_MeanScMax
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa$group <- dfa$Organic
dfa <- dfa[!is.na(dfa$respvar), ]

# model predictions (at mean landscape compo and config)
me <- ggpredict(mod2, c("EMF_Production", "Organic"))
me <- me[-1,]  # plot only over the range of observed values for conventional 

### Panel B - abdc --------
F2B <- ggplot(me, aes(x = x, y = predicted, colour = group), col = mycols) +
  geom_line(size = 1, lty = "dashed")+
  # geom_ribbon(inherit.aes = FALSE, 
  #             mapping = aes(x = x, y = predicted, group = group, fill = group,
  #                           ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.15)+
  geom_point(data = dfa, mapping = aes(x = EMF_Production, y = respvar, fill = group, shape = group), 
             size = 2.2, colour = "black")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Production")+
  ylab("Multitrophic abundance")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = sizelegend),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = sizelegend),
        axis.text.x=element_text(face = "bold", size = sizelegend),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))

## EMF Regulation ------------

### Models and data --------
mod2 <- readRDS("Output/Models_2ProductionTradeoffs/ModelResults_EMF_Regulation.rds")

# Data associated with the model ---
dfa <- df
dfa$respvar <- df$EMF_Regulation
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa$group <- dfa$Organic
dfa <- dfa[!is.na(dfa$respvar), ]

# model predictions (at mean landscape compo and config)
me <- ggpredict(mod2, c("EMF_Production", "Organic"))
me <- me[-1,]  # plot only over the range of observed values for conventional 

### Panel C --------

F2C <- ggplot(me, aes(x = x, y = predicted, colour = group), col = mycols) +
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
  annotate("text", x = 0.4, y=0.94, label = "B")

## EMF Soil ------------

### Models and data --------
mod2 <- readRDS("Output/Models_2ProductionTradeoffs/ModelResults_EMF_Soil.rds")

# Data associated with the model ---
dfa <- df
dfa$respvar <- df$EMF_Soil
dfa$landcompo <- df$SemiNatural
dfa$landconfig <- df$Dist.snh
dfa$LandscapeID <- as.factor(dfa$LandscapeID)
dfa$group <- dfa$Organic
dfa <- dfa[!is.na(dfa$respvar), ]

# model predictions (at mean landscape compo and config)
me <- ggpredict(mod2, c("EMF_Production", "Organic"))
me <- me[-1,]  # plot only over the range of observed values for conventional 

### Panel D - soil --------
F2D <- ggplot(me, aes(x = x, y = predicted, colour = group), col = mycols) +
  geom_line(size = 1, lty = "dashed")+
  # geom_ribbon(inherit.aes = FALSE, 
  #             mapping = aes(x = x, y = predicted, group = group, fill = group,
  #                           ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.15)+
  geom_point(data = dfa, mapping = aes(x = EMF_Production, y = respvar, fill = group, shape = group), 
             size = 2.2, colour = "black")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Production")+
  ylab("Soil quality/fertility")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = sizelegend),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = sizelegend),
        axis.text.x=element_text(face = "bold", size = sizelegend),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))+
  annotate("text", x = 0.4, y=0.69, label = "C")

(F2A+F2B)/(F2C+F2D)+plot_annotation(title = 'Tradeoffs between biodiversity, its services, and agricultural production')

## save a png with high res ---------
png("Figures/Fig2_ProductionTradeoff.png",
    width=30,
    height=10,
    units = "cm",
    res=ppi)

(F2A+F2C+F2D)
dev.off()

## Supplement figure for multiab
png("Figures/FigSupp_ProductionTradeoff.png",
    width=12,
    height=10,
    units = "cm",
    res=ppi)

(F2B)
dev.off()

# Supplement Local-landscape interactions --------------
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
library(sjPlot)
library(ggeffects)
library(ggrepel)
library(vegan)
library(scales)

mycols <- viridis_pal(option = "A")(6)[c(2, 5)]
sizelegend <- 8
sizetext <- 12

## create a function to plot interactions with landscape

plotfun <- function(responsevar, landscapevar, data){
  # load the model
  mod <- readRDS(file.path("Output", "Models_1OrgLandscape", paste("ModelResults_", responsevar, ".rds", sep = "")))
  
  # load data
  dfa <- data
  dfa$respvar <- dfa[,responsevar]
  dfa$landcompo <- dfa$SemiNatural
  dfa$landconfig <- dfa$Dist.snh
  dfa$LandscapeID <- as.factor(dfa$LandscapeID)
  dfa$group <-  dfa$Organic
  dfa <- dfa[!is.na(dfa$respvar), ]
  
  # model predictions (at mean landscape compo and config)
  ifelse(landscapevar =="SemiNatural", 
         me <- ggemmeans(mod, c("landcompo", "Organic"), back.transform = FALSE),
         me <- ggemmeans(mod, c("landconfig", "Organic"), back.transform = FALSE))
  
  # ggplot
  p <- ggplot(me, aes(x = x, y = predicted, colour = group), col = mycols) +
    geom_line(size = 1)+
    geom_ribbon(inherit.aes = FALSE,
                mapping = aes(x = x, y = predicted, group = group, fill = group,
                              ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.15)+
    scale_fill_manual(values=mycols)+
    scale_colour_manual(values=mycols)+
    scale_shape_manual(values = c(21,24))+
    xlab(ifelse(landscapevar == "SemiNatural", "Semi-natural habitats (%)", "Distance semi-natural habitat (m)"))+
    theme_bw()+
    theme(legend.position = "none",
          legend.text = element_text(size = sizelegend),
          legend.title = element_blank(),
          axis.text.y=element_text(face = "bold", size = sizelegend),
          axis.text.x=element_text(face = "bold", size = sizelegend),
          axis.title.y = element_text(size=sizetext, face = "bold"),
          axis.title.x = element_text(size=sizetext, face = "bold"))
  
  # add data points depending on the landscape variable
  
  if(grepl("log", mod@call[2])){
    # a <- 1-min(dfa$respvar)
    if(landscapevar =="SemiNatural"){ # composition 
      p+geom_point(data = dfa, mapping = aes(x = landcompo, y = log(respvar + (1-min(respvar))), fill = group, shape = group), size = 2.2, colour = "black")}
    
    else{  # configuration
      p+geom_point(data = dfa, mapping = aes(x = landconfig, y = log(respvar + (1-min(respvar))), fill = group, shape = group), size = 2.2, colour = "black")
    }
  }
  else{
    if(landscapevar =="SemiNatural"){ # composition 
      p+geom_point(data = dfa, mapping = aes(x = landcompo, y = respvar, fill = group, shape = group), size = 2.2, colour = "black")}
    
    else{  # configuration
      p+geom_point(data = dfa, mapping = aes(x = landconfig, y = respvar, fill = group, shape = group), size = 2.2, colour = "black")
    }
    
  }
  
}



### EMF Regulation --------
p1 <- plotfun(responsevar = "EMF_Regulation", landscapevar = "SemiNatural", data = df)+
  ylab("Index")+
  ggtitle("Pest control")


## Coleoptera richness----
p2a <- plotfun(responsevar = "ColeopteraFoliageRichness", landscapevar = "SemiNatural", data = df)+
  ylab("Family richness (log)")+
  ggtitle("Foliage Coleoptera")
p2b <- plotfun(responsevar = "ColeopteraFoliageRichness", landscapevar = "Dist.snh", data = df)+
  ylab("Family richness (log)")+
  ggtitle("Foliage Coleoptera")


## Spider Foliage richness ----
p3 <- plotfun(responsevar = "SpiderFoliageRichness", landscapevar = "SemiNatural", data = df)+
  ylab("Species richness")+
  ggtitle("Foliage spiders")


## Spider Abundance ----
p4 <- plotfun(responsevar = "SpiderAbundance", landscapevar = "Dist.snh", data = df)+
  ylab("Abundance (log)")+
  ggtitle("Ground spiders")


### Predation plasticine --------
p5 <- plotfun(responsevar = "PredRatePlasticine", landscapevar = "SemiNatural", data = df)+
  ylab("Predation rate (%)")+
  ggtitle("Model caterpillars")



### Earthworm richness --------
p6 <- plotfun(responsevar = "EwormRichness", landscapevar = "Dist.snh", data = df)+
  ylab("Species richness")+
  ggtitle("Earthworms")


### Chlorophyll --------
p7 <- plotfun(responsevar = "Ntester", landscapevar = "SemiNatural", data = df)+
  ylab("Chlorophyll content")+
  ggtitle("Grapevine performance")


p1+p2a+p2b+p3+p4+p5+p6+p7

p1+p2a+p2b+p3+p4+p5+p6+p7+plot_layout(guides = "collect") & theme(legend.position = "bottom")

## save a png with high res ---------
# ppi <-300
# png("Figures/FigSupp_LocalLandInteractions.png",
#     width=30,
#     height=15,
#     units = "cm",
#     res=ppi)
# 
# (p1+p2+p3)/(p4+p5+p6)
# 
# dev.off()


# by type pf resp var
p2a+p2b+p6+
  p4+plot_spacer()+plot_spacer()+
  p1+p5+p7+plot_layout(guides = "collect") & theme(legend.position = "bottom")

# by what makes sense
p2a+p2b+p4+p1+p5+plot_spacer()+
  p6+p7+plot_layout(guides = "collect") & theme(legend.position = "bottom")



ppi <-300
png("Figures/FigSupp_LocalLandInteractions.png",
    width=24,
    height=25,
    units = "cm",
    res=ppi)

p1+p2a+p2b+p5+p3+p4+p6+p7+plot_layout(guides = "collect") & theme(legend.position = "bottom")


dev.off()


### Version with data trends only, not model resuts -------------
library(dplyr)
library(patchwork)
library(ggplot2)
library(viridis)

# Load Data
df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")
df <- df[,-c(1:2)]

mycols <- viridis_pal(option = "A")(6)[c(2, 5)]
sizetext <- 12

# Landscape effect (lack of)
p1 <- ggplot(df, aes(y = EMF_Regulation, x = SemiNatural, col = Organic)) +
  geom_point(size = 2.2)+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  geom_smooth(se = FALSE, method = "lm")+
  ylab("Index")+
  xlab("Semi-natural habitats (%)")+
  ggtitle("Pest control")+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = 8),
        axis.text.x=element_text(face = "bold", size = 8),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))

p2 <- ggplot(df, aes(y = BirdRichness, x = SemiNatural, col = Organic)) +
  geom_point(size = 2.2)+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  geom_smooth(se = FALSE, method = "lm")+
  ylab("Species richness")+
  xlab("Semi-natural habitats (%)")+
  ggtitle("Birds")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y=element_text(face = "bold", size = 8),
        axis.text.x=element_text(face = "bold", size = 8),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))

p3 <- ggplot(df, aes(y = SpiderFoliageRichness, x = SemiNatural, col = Organic)) +
  geom_point(size = 2.2)+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  geom_smooth(se = FALSE, method = "lm")+
  ylab("Species richness")+
  xlab("Semi-natural habitats (%)")+
  ggtitle("Foliage spiders")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y=element_text(face = "bold", size = 8),
        axis.text.x=element_text(face = "bold", size = 8),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))

p4 <- ggplot(df, aes(y = PredRatePlasticine, x = SemiNatural, col = Organic)) +
  geom_point(size = 2.2)+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  geom_smooth(se = FALSE, method = "lm")+
  ylab("Predation rate (%)")+
  xlab("Semi-natural habitats (%)")+
  ggtitle("Model caterpillars")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y=element_text(face = "bold", size = 8),
        axis.text.x=element_text(face = "bold", size = 8),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))

p5 <- ggplot(df, aes(y = ColeopteraFoliageAbundance, x = Dist.snh, col = Organic)) +
  geom_point(size = 2.2)+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  geom_smooth(se = FALSE, method = "lm")+
  ylab("Abundance")+
  xlab("Distance semi-natural habitat (m)")+
  ggtitle("Foliage Coleoptera")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y=element_text(face = "bold", size = 8),
        axis.text.x=element_text(face = "bold", size = 8),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))

p6 <- ggplot(df, aes(y = EwormRichness, x = Dist.snh, col = Organic)) +
  geom_point(size = 2.2)+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  geom_smooth(se = FALSE, method = "lm")+
  ylab("Species richness")+
  xlab("Distance semi-natural habitat (m)")+
  ggtitle("Earthworms")+
  theme_bw()+
  theme(legend.position = "right",
        axis.text.y=element_text(face = "bold", size = 8),
        axis.text.x=element_text(face = "bold", size = 8),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))

(p1+p2+p3)/(p4+p5+p6)


# Supplement PCA local and landscape variables - Supp Figure ----------------------------

## Functions -----------
library(ggbiplot)
library(dplyr)
library(lme4)
library(DHARMa)
library(car)
library(sjPlot)
library(vegan)
library(nlme)
library(ggplot2)
library(patchwork)


# Supplement PCA local and landscape variables - Supp Figure ----------------------------

### Functions -----------
library(ggbiplot)
library(dplyr)
library(lme4)
library(DHARMa)
library(car)
library(sjPlot)
library(vegan)
library(nlme)
library(ggplot2)
library(patchwork)

### Load Data -----------
df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")
df <- df[,-c(1:2)]

# Shannon
df$Shannon <- diversity(df %>% select(Crops, Grasslands, Forests, Vineyards, GrassStrips, HedgeRows, SoftPaths, Artificial, Water, Others), index = "shannon")

# Key landscape management variables: 
WhichLandVars <- c("Shannon", "Edge",
                   "SemiNatural", "Dist.snh", "Vineyards", "Artificial",
                   "Crops", "Grasslands", "Forests", "GrassStrips", "HedgeRows", "Water")

dfa <- df
dfa <- dfa %>% select(WhichLandVars)
rownames(dfa) <- dfa$PlotID

# PCA
PCA <- rda(dfa , scale = TRUE)

barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig))

# Calculate percent of variance explained by the two first axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 55%,
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:3]) # 68%
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:4]) # 78%

# Find summarizing vars
sort(PCA$CA$v[,"PC1"]) # A1: SNH, Shannon, Forests, Grasslands VS. Vineyards and Edge density (36%)
sort(PCA$CA$v[,"PC2"]) # A2: Hedge rows, artificial and crops VS. distance to near SNH (16%)
sort(PCA$CA$v[,"PC3"]) # A3: Water, grass strips, edhe density VS. Forests and distance to SNH (11%)

## PCA landscape metrics -----------
mycols <- viridis_pal(option = "A")(6)[c(2, 5)]
plot.pca <- prcomp(dfa, scale. = TRUE)
P1 <- ggbiplot(plot.pca, obs.scale = 1, var.scale = 1,
               groups = df$Organic, ellipse = TRUE, circle = TRUE) +
  scale_fill_manual(name = 'Organic',values=mycols)+
  scale_color_manual(name = 'Organic', values = mycols) +
  scale_shape_manual(name = 'Organic',values = c(21,24))+
  geom_point(aes(fill=df$Organic, shape=df$Organic), size = 3) +
  theme_bw()+
  theme(legend.position = 'none')


# # Make correlation table
# cor(dfa)
# # save table for supplement 
# write.csv(cor(dfa), "Tables/CorrelationsLandscape.csv")

# Key local management variables: 
WhichLocalVars <- c("TillageFreq", "MowingFreq", "SprayingFreq", "QuantityFungicide", "QuantityInsecticide", "QuantityHerbicide", "NoAI")

dfa <- df
dfa <- dfa %>% select(WhichLocalVars)
dfa <- dfa[!is.na(dfa$TillageFreq),]
rownames(dfa) <- dfa$PlotID

# rename variables for PCA
dfa <- dfa %>% 
  dplyr::rename(Tillage =  TillageFreq, 
                Mowing = MowingFreq,
                Spraying = SprayingFreq,
                Herbicides = QuantityHerbicide,
                Insecticides = QuantityInsecticide,
                Fungicides = QuantityFungicide,
                No.ActiveIngredients = NoAI)

# PCA
PCA <- rda(dfa , scale = TRUE)

barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig))

## PCA local management -----------
mycols <- viridis_pal(option = "A")(6)[c(2, 5)]
plot.pca <- prcomp(dfa, scale. = TRUE)
P2 <- ggbiplot(plot.pca, obs.scale = 1, var.scale = 1,
               groups = df$Organic[!is.na(df$TillageFreq)], ellipse = TRUE, circle = TRUE) +
  scale_fill_manual(name = 'Organic',values=mycols)+
  scale_color_manual(name = 'Organic', values = mycols) +
  scale_shape_manual(name = 'Organic',values = c(21,24))+
  geom_point(aes(fill=df$Organic[!is.na(df$TillageFreq)], shape=df$Organic[!is.na(df$TillageFreq)]), size = 3) +
  theme_bw()+
  theme(legend.position = 'right')

# save png file with PCA landscape metrics
ppi <- 300

png("Figures/FigSupp_PCA.png",
    width=30,
    height=15,
    units = "cm",
    res=ppi)

P1+P2+plot_annotation(tag_levels = "A")

dev.off()


# Supplement: Figure with raw data response variables --------------

library(rlang)  # needed to call unquoted col in ggplot in custom function

df <- read.csv("/Users/save/Nextcloud/Science/Postdoc_Bordeaux/MultiBEFVineyards/Output/MultiDiversity_ForAnalysis.csv")
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

## Biodiversity figure - create panel with 14 taxa groups-----------------------
# keep the order of the first figure

BigPlot <- 
  myfunboxplot(data = df, Yvar = CarabRichness, Xvar = Organic, mycols = mycols, mytitle = "Ground beetles", myaxistitle = "Species richness")+
  
  myfunboxplot(data = df, Yvar = EwormRichness, Xvar = Organic, mycols = mycols, mytitle = "Earthworms", myaxistitle ="Species richness")+
  
  myfunboxplot(data = df, Yvar = BeeRichness, Xvar = Organic, mycols = mycols, mytitle = "Bees", myaxistitle = "Genera richness")+
  
  myfunboxplot(data = df, Yvar = MicrobeRichness, Xvar = Organic, mycols = mycols, mytitle = "Soil bacteria", myaxistitle = "OTU richness")+
  
  myfunboxplot(data = df, Yvar = PlantRichness, Xvar = Organic, mycols = mycols, mytitle = "Plants", myaxistitle = " Species richness")+
  
  myfunboxplot(data = df, Yvar = SyrphidRichness, Xvar = Organic, mycols = mycols, mytitle = "Syrphids", myaxistitle = "Genera richness")+
  
  myfunboxplot(data = df, Yvar = CollemboRichness, Xvar = Organic, mycols = mycols, mytitle = "Collembola", myaxistitle = "Species richness")+
  
  myfunboxplot(data = df, Yvar = HeteropteraFoliageRichness, Xvar = Organic, mycols = mycols, mytitle = "Foliage Heteroptera", myaxistitle = "Family richness")+
  
  myfunboxplot(data = df, Yvar = BirdRichness, Xvar = Organic, mycols = mycols, mytitle = "Birds", myaxistitle = " Species richness")+
  
  myfunboxplot(data = df, Yvar = ButtRichness, Xvar = Organic, mycols = mycols, mytitle = "Butterflies", myaxistitle = " Species richness")+
  
  myfunboxplot(data = df, Yvar = ColeopteraFoliageRichness, Xvar = Organic, mycols = mycols, mytitle = "Foliage Coleoptera", myaxistitle = " Family richness")+
  
  myfunboxplot(data = df, Yvar = SpiderFoliageRichness, Xvar = Organic, mycols = mycols, mytitle = "Foliage spiders", myaxistitle = " Species richness")+
  plot_spacer()+
  
  myfunboxplot(data = df, Yvar = HemipteraFoliageRichness, Xvar = Organic, mycols = mycols, mytitle = "Foliage Auchenorrhyncha",myaxistitle = "Family richness")+
  
  myfunboxplot(data = df, Yvar = SpiderRichness, Xvar = Organic, mycols = mycols, mytitle = "Ground-dwelling spiders",myaxistitle = " Species richness")



## Supplement figure for raw biodiv data
ppi <- 300

png("Figures/FigSupp_RawBiodivData.png",
    width=20,
    height=15,
    units = "cm",
    res=ppi)
BigPlot
dev.off()




## Abundance figure - create panel with 14 taxa groups-----------------------
# keep the order of the first figure

BigPlot <- 
  myfunboxplot(data = df, Yvar = CarabAbundance, Xvar = Organic, mycols = mycols, mytitle = "Ground beetles", myaxistitle = "Individuals per trap")+
  
  myfunboxplot(data = df, Yvar = EwormAbundance, Xvar = Organic, mycols = mycols, mytitle = "Earthworms", myaxistitle ="Individuals per sample")+
  
  myfunboxplot(data = df, Yvar = BeeAbundance, Xvar = Organic, mycols = mycols, mytitle = "Bees", myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = MicrobeAbundance, Xvar = Organic, mycols = mycols, mytitle = "Soil bacteria", myaxistitle = "Total no. OTU per sample")+
  
  myfunboxplot(data = df, Yvar = PlantCover, Xvar = Organic, mycols = mycols, mytitle = "Plants", myaxistitle = " Plant cover (%)")+
  
  myfunboxplot(data = df, Yvar = SyrphidAbundance, Xvar = Organic, mycols = mycols, mytitle = "Syrphids", myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = CollemboAbundance, Xvar = Organic, mycols = mycols, mytitle = "Collembola", myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = HeteropteraFoliageAbundance, Xvar = Organic, mycols = mycols, mytitle = "Foliage Heteroptera", myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = BirdAbundance, Xvar = Organic, mycols = mycols, mytitle = "Birds", myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = ButtAbundance, Xvar = Organic, mycols = mycols, mytitle = "Butterflies", myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = ColeopteraFoliageAbundance, Xvar = Organic, mycols = mycols, mytitle = "Foliage Coleoptera", myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = SpiderFoliageAbundance, Xvar = Organic, mycols = mycols, mytitle = "Foliage spiders", myaxistitle = "Ind. per sampling session")+
  plot_spacer()+
  
  myfunboxplot(data = df, Yvar = HemipteraFoliageAbundance, Xvar = Organic, mycols = mycols, mytitle = "Foliage Auchenorrhyncha",myaxistitle = "Ind. per sampling session")+
  
  myfunboxplot(data = df, Yvar = SpiderAbundance, Xvar = Organic, mycols = mycols, mytitle = "Ground-dwelling spiders",myaxistitle = "Individuals per trap")


## Supplement figure for raw biodiv data
ppi <- 300

png("Figures/FigSupp_RawAbundData.png",
    width=20,
    height=15,
    units = "cm",
    res=ppi)
BigPlot
dev.off()




## Function figure - create panel with 14 taxa groups-----------------------
# keep the order of the first figure

BigPlotfun <- 
  myfunboxplot(data = df, Yvar = Ntester, Xvar = Organic, mycols = mycols, mytitle = "Chlorophyll content", myaxistitle = "SPAD index")+
  
  myfunboxplot(data = df, Yvar = YieldRealised_hlperha, Xvar = Organic, mycols = mycols, mytitle = "Yield", myaxistitle ="hl wine per ha")+
  
  myfunboxplot(data = df, Yvar = DecompokRate, Xvar = Organic, mycols = mycols, mytitle = "Decomposition k rate", myaxistitle = "day-1")+
  
  myfunboxplot(data = df, Yvar = DecompoSfactor, Xvar = Organic, mycols = mycols, mytitle = "Decomposition S factor", myaxistitle = " ")+
  
  myfunboxplot(data = df, Yvar = SoilPhosphatase, Xvar = Organic, mycols = mycols, mytitle = "Soil phosphatase", myaxistitle = "µg PNPP/g dry soil/h")+
  
  myfunboxplot(data = df, Yvar = SoilUrease, Xvar = Organic, mycols = mycols, mytitle = "Soil urease", myaxistitle = "µg NH4/g dry soil/h")+
  
  myfunboxplot(data = df, Yvar = SoilGlucosidase, Xvar = Organic, mycols = mycols, mytitle = "Soil glucosidase", myaxistitle = "µg PNP/g dry soil/h")+
  
  myfunboxplot(data = df, Yvar = SoilC, Xvar = Organic, mycols = mycols, mytitle = "Soil C", myaxistitle = "g/kg dry soil")+
  
  myfunboxplot(data = df, Yvar = SoilN, Xvar = Organic, mycols = mycols, mytitle = "Soil N", myaxistitle = "g/kg dry soil")+
  
  myfunboxplot(data = df, Yvar = PestDamage, Xvar = Organic, mycols = mycols, mytitle = "Pest damage", myaxistitle = "Proportion of damaged \n plant organs per plot")+
  
  myfunboxplot(data = df, Yvar = PredRatePupae, Xvar = Organic, mycols = mycols, mytitle = "Predation: pupae", myaxistitle = "Proportion predated")+
  
  myfunboxplot(data = df, Yvar = PredRatePlasticine, Xvar = Organic, mycols = mycols, mytitle = "Predation: model caterpillars", myaxistitle = "Percentage predated")+
  plot_spacer()+
  
  myfunboxplot(data = df, Yvar = PredRateEggs, Xvar = Organic, mycols = mycols, mytitle = "Predation: eggs",myaxistitle = "Percentage predated")+
  
  myfunboxplot(data = df, Yvar = PathogenDamage, Xvar = Organic, mycols = mycols, mytitle = "Pathogen damage",myaxistitle = "Proportion of damaged \n plant organs per plot")



## Supplement figure for raw functions data
ppi <- 300

png("Figures/FigSupp_RawFunData.png",
    width=20,
    height=15,
    units = "cm",
    res=ppi)
BigPlotfun
dev.off()



## Multidiversity, abundance and function indices-----------------------

BigPlotEMF <- 
  myfunboxplot(data = df, Yvar = MB_MeanScMax, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic diversity", myaxistitle = "index")+
  
  myfunboxplot(data = df, Yvar = MB_MeanZscore, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic diversity", myaxistitle ="index (z-scores)")+
  
  myfunboxplot(data = df, Yvar = MB_Thresh70, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic diversity", myaxistitle ="index (threshold)")+
  
  myfunboxplot(data = df, Yvar = MA_MeanScMax, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic abundance", myaxistitle = "index")+
  
  myfunboxplot(data = df, Yvar = MA_MeanZscore, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic abundance", myaxistitle = "index (z-scores)")+
  
  myfunboxplot(data = df, Yvar = MA_Thresh70, Xvar = Organic, mycols = mycols, mytitle = "Multitrophic abundance", myaxistitle = "index (threshold)")+
  
  
  myfunboxplot(data = df, Yvar = EMF_Production, Xvar = Organic, mycols = mycols, mytitle = "Production index", myaxistitle = "index")+
  
  myfunboxplot(data = df, Yvar = EMF_Soil, Xvar = Organic, mycols = mycols, mytitle = "Soil quality & fertility index", myaxistitle = "index")+
  
  myfunboxplot(data = df, Yvar = EMF_Regulation, Xvar = Organic, mycols = mycols, mytitle = "Pest control index", myaxistitle = "index")+
  # plot_spacer()+
  
  myfunboxplot(data = df, Yvar = EMF_MeanScMax, Xvar = Organic, mycols = mycols, mytitle = "Multifunctionality", myaxistitle = "index")+
  
  myfunboxplot(data = df, Yvar = EMF_MeanZscore, Xvar = Organic, mycols = mycols, mytitle = "Multifunctionality", myaxistitle = "index (z-scores)")+
  
  myfunboxplot(data = df, Yvar = EMF_Thresh70, Xvar = Organic, mycols = mycols, mytitle = "Multifunctionality", myaxistitle = "index (threshold)")+
  plot_layout(ncol = 3, nrow = 4)



## Supplement figure for raw biodiv data
ppi <- 300

png("Figures/FigSupp_RawEMFData.png",
    width=20,
    height=15,
    units = "cm",
    res=ppi)
BigPlotEMF
dev.off()



# Supplement: Synergy biodiversity-production vs amounts of insecticides-----------

# index of "synergy" between biodiv and production when both are above the mean
index <- ifelse(df$MB_MeanScMax > mean(df$MB_MeanScMax) & df$EMF_Production > mean(df$EMF_Production), 1, 0)

df$SynergyBiodivProd <- as.factor(index)


# ggplot options
mycols2 <- viridis_pal(option = "A")(6)[c(1, 3)]
sizelegend <- 8
sizetitle <- 9

# my custom function
myfunboxplot2 <- function(data, Yvar, Xvar, mycols, myaxistitle ){
  ggplot(data, aes(y = {{Yvar}}, x = {{Xvar}}, col = Xvar))+
    geom_boxplot(col = mycols2)+
    ylab(myaxistitle)+
    xlab("Synergy biodiversity - production (index)")+
    theme_bw()+
    theme(legend.position = "none",
          plot.title = element_text(face = "bold",size = sizetitle),
          axis.text.y=element_text(size = sizelegend),
          axis.text.x=element_text(size = sizelegend),
          axis.title.y = element_text(size= sizetitle),
          axis.title.x = element_blank())
}


mycols <- viridis_pal(option = "A")(6)[c(2, 5)]
sizetext <- 12

defineSynergy <- ggplot(df, aes(y = MB_MeanScMax, x = EMF_Production, col = Organic, fill = Organic, shape = SynergyBiodivProd)) +
  geom_point(size = 1.5)+
  geom_hline(yintercept=mean(df$MB_MeanScMax))+
  geom_vline(xintercept=mean(df$EMF_Production))+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  ylab("Multitrophic diversity index")+
  xlab("Production index")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "right",
        plot.title = element_text(face = "bold",size = sizelegend),
        axis.text.y=element_text(size = sizelegend),
        axis.text.x=element_text(size = sizelegend),
        axis.title.y = element_text(size= sizelegend),
        axis.title.x = element_text(size= sizelegend), 
        legend.title= element_text(size = sizelegend),
        legend.text = element_text(size=sizelegend))




BigFig <-   defineSynergy+
  myfunboxplot2(df, SemiNatural, SynergyBiodivProd, mycols2, "Prop. semi-natural habitats (%)")+
  myfunboxplot2(df, Dist.snh, SynergyBiodivProd, mycols2, "Distance to semi-natural habitat (m)")+
  myfunboxplot2(df, TillageFreq, SynergyBiodivProd, mycols2, "Tillage freq. (no. operations)")+
  myfunboxplot2(df, MowingFreq, SynergyBiodivProd, mycols2, "Mowing freq. (no. operations)")+
  myfunboxplot2(df, SprayingFreq, SynergyBiodivProd, mycols2, "Pesticide use freq. (no. sprays)")+
  myfunboxplot2(df, QuantityFungicide, SynergyBiodivProd, mycols2, "Fungicides amounts (kg ai/ha)")+
  myfunboxplot2(df, QuantityInsecticide, SynergyBiodivProd, mycols2, "Insecticides amounts (kg ai/ha)")+
  plot_layout(ncol = 2, nrow = 4)+
  plot_annotation(tag_levels = 'A')

## Supplement figure
ppi <- 300

png("Figures/FigSupp_SynergyBP2.png",
    width=20,
    height=25,
    units = "cm",
    res=ppi)
BigFig
dev.off()


# Supplement figure showing correlation with proportion of target yield achieved-------------------

df <- read.csv("Output/MultiDiversity_ForAnalysis.csv")
df <- df[,-c(1:2)]

# set sizes of text in plots
sizetext <- 10
sizelegend <- 8

ppi = 300
w = 20

mycols <- viridis_pal(option = "A")(6)[c(2, 5)]


# Data associated with the model ---
dfa <- df
dfa$PropYieldAch <- 100*dfa$YieldRealised_hlperha/dfa$YieldTarget_hlperha

PanelA <- ggplot(dfa, aes(x = PropYieldAch, y = MB_MeanScMax, colour = Organic), col = mycols) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, lty = "dashed")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Proportion of target yield achieved (%)")+
  ylab("Multitrophic diversity")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = sizelegend),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = sizelegend),
        axis.text.x=element_text(face = "bold", size = sizelegend),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))+
  annotate("text", x = 0.4, y=0.64, label = "A")

PanelB <- ggplot(dfa, aes(x = PropYieldAch, y = EMF_Regulation, colour = Organic), col = mycols) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, lty = "dashed")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Proportion of target yield achieved (%)")+
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
  annotate("text", x = 0.4, y=0.94, label = "B")

PanelC <- ggplot(dfa, aes(x = PropYieldAch, y = EMF_Soil, colour = Organic), col = mycols) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, lty = "dashed")+
  scale_fill_manual(values=mycols)+
  scale_colour_manual(values=mycols)+
  scale_shape_manual(values = c(21,24))+
  
  xlab("Proportion of target yield achieved (%)")+
  ylab("Pest control")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = sizelegend),
        legend.title = element_blank(),
        axis.text.y=element_text(face = "bold", size = sizelegend),
        axis.text.x=element_text(face = "bold", size = sizelegend),
        axis.title.y = element_text(size=sizetext, face = "bold"),
        axis.title.x = element_text(size=sizetext, face = "bold"))+
  annotate("text", x = 0.4, y=0.7, label = "C")


png("Figures/Suppl_SensiAnaProductionTradeoff.png",
    width=30,
    height=10,
    units = "cm",
    res=ppi)

(PanelA+PanelB+PanelC)
dev.off()

