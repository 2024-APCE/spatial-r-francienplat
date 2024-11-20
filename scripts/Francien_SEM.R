#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Anderson 2007 dataset
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
install.packages("lavaan")
#renv::snapshot() in console
library(lavaan)

# dataset:
#browseURL("https://docs.google.com/spreadsheets/d/1wk3UTAN7Cp7ZeoB0wpfW2C2eE_VoyKnJQpJ0Zrjk3yM/edit?usp=sharing")
# read the data from the google docs link:
SEM_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSBL-EE7iOJwHjmQhoZXMSDztviTibUwdW2MHjDPN2Xz7XiKWv-PreuQtT3hwKEWTYf8HIO1q7cACGx/pub?gid=1260469066&single=true&output=csv")

names(SEM_data)
# standardize all variables to mean 0 and standard deviation 1. to reflect the relative importance of the variables, instead of the individual slopes.
SEM_data_std <- SEM_data |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEM_data_std
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEM_data%>% dplyr::select(dist2river,elevation,rainfall,cec,burnfreq,hills,woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEM_data_std %>% dplyr::select(dist2river,elevation,rainfall,cec,burnfreq,hills,woody),
                    stars = T, ellipses = F)

#LUKTNIETggsave("./figures/pairs.panels",pairs.panels,width=18,height=18,units="cm",dpi=300)

# analyse the model (response ~ predictors) with a multiple regression approach 
multregwoody_std <- lm(woody ~ dist2river+ elevation + rainfall + hills + cec+burnfreq, data = SEM_data_std)
summary(multregwoody_std)

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
#Woody_model<-'woody~burnfreq+cec+rainfall
            #  burnfreq~rainfall
             # cec~dist2river+burnfreq+hills
             # hills~elevation
              #rainfall~elevation
              #dist2river~hills'

#Woody_model

Woody_model2<-'woody~rainfall+dist2river+hills+burnfreq
              burnfreq~rainfall+dist2river
              hills~~elevation
              rainfall~hills+elevation
              dist2river~hills+elevation'

#install.packages("lavaanPlot")
library(lavaanPlot)
lavaanPlot(model=Woody_fit,
           coefs=TRUE,
           stand=TRUE,
           graph_options=list(rankdir="LR"),
           stars= "regress")
Woody_fit<-lavaan::sem(Woody_model2, data=SEM_data_std)
# show the model results
summary(Woody_fit,standardized=TRUE,fit.measures=T,rsquare=T)
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
# if both are true, the model is good




