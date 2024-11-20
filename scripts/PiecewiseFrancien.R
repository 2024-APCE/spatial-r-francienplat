# Piecewise SEM

library(piecewiseSEM)

# read the pointdata
pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSBL-EE7iOJwHjmQhoZXMSDztviTibUwdW2MHjDPN2Xz7XiKWv-PreuQtT3hwKEWTYf8HIO1q7cACGx/pub?gid=1260469066&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woody>0, woody<20)   # remove 2 extreme values and avoid interpolated negative values

# note that you should not standardize your data for a PicewiseSEM as then eg logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))


psych::pairs.panels(pointdata,stars = T, ellipses = F)


# Define the models
# I started from this initially hypothesized causal scheme, my model 1)
browseURL("https://docs.google.com/presentation/d/1PB8rhbswyPew-FYULsw1pIl8Jyb1FFElKPf34DZrEY8/edit?usp=sharing")

# Model 1: woody predicted by burnfreq and rainfall
model_woody <- lm(woody ~ burnfreq+dist2river+rainfall+hills+elevation,
                  data = pointdata)
summary(model_woody)
p1<-ggplot(data=pointdata,aes(x=burnfreq,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p1

p2<-ggplot(data=pointdata,aes(x=dist2river,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
p2

p3<-ggplot(data=pointdata,aes(x=rainfall,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p3

p4<-ggplot(data=pointdata,aes(x=hills,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p4


p5<-ggplot(data=pointdata,aes(x=elevation,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p5

# Model_burnfreq: burning frequency predicted by dist2river and Rainfall
model_burnfreq_init <- glm(burnfreq ~ dist2river + rainfall, 
                           family=poisson, 
                           data = pointdata)
# Calculate dispersion statistic
#mean and variance are the same in poisson model. 
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat #so there is overdispersion
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
#negative binomial model
model_burnfreq <- MASS::glm.nb(burnfreq ~ dist2river + rainfall, 
                               data = pointdata)
summary(model_burnfreq)

#GLM a quasi-poisson model is used as negative binomial can not be done in ggplot. Proximate the overdispersion with a quasi poisson. Also has log link function.

p6<-ggplot(data=pointdata,aes(y=burnfreq,x=dist2river))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p6

p7<-ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p7

p8<-ggplot(data=pointdata,aes(y=burnfreq,x=hills))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p8

p9<-ggplot(data=pointdata,aes(y=burnfreq,x=elevation))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p9



# model_rainfall: rainfall predicted by elevation and hills, regular lm
model_rainfall <- lm(rainfall ~ hills+elevation, 
                     data = pointdata)
summary(model_rainfall)

p10<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p10

p11<-ggplot(data=pointdata,aes(y=rainfall,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p11

#Model Dist2River, regular lm
model_dist2river <- lm(dist2river ~ hills+elevation+rainfall, 
                       data = pointdata)

summary(model_dist2river)
p12<-ggplot(data=pointdata,aes(y=dist2river,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p12

p13<-ggplot(data=pointdata,aes(y=dist2river,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p13

p14<-ggplot(data=pointdata,aes(y=dist2river,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p14



#Model hills (glm, binomial)
model_hills <- glm(hills ~ elevation, 
                   family=binomial,
                   data = pointdata)
summary(model_hills)
p15<-ggplot(data=pointdata,aes(y=hills,x=elevation))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial(link="logit")),#because response variable is yes or no. 
              formula= y~x,
              se=T)
summary(model_hills)
p15


#model elevation, normal lm
model_elevation <- lm(elevation ~ hills, 
                      data = pointdata)
summary(model_elevation)

p16<-ggplot(data=pointdata,aes(y=elevation,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p16
# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15+p16+
  patchwork::plot_layout(ncol=5) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_dist2river,
                                 model_rainfall)

# Summarize the SEM results
summary(psem_model)



# a Significant (P<0.05) global goodness of fit means that your model does not fit well, 
# indicating potential problems like missing paths, mis-specfied relations, 
# or unaccounted-for correlations

# update the model based on your results
# significant tests of directed separation could mean a missing direct effect between the variables

# Best Practices:
# - Hypothesize Carefully:
#   Construct the initial model based on theoretical or empirical understanding.
# - Evaluate d-Separation Results:
#   Add or adjust paths based on significant independence test results.
# - Refit and Validate:
#   Rerun the model after adjustments and recheck the Fisher’s C statistic and independence claims.
# - Avoid Overfitting:
#   Add paths only when justified by theory or strong evidence, not purely to improve fit.
# Common pitfall: 
# - ignofing significant d-separation tests and failing to modify the model
# - adding too many variables and pathways, leading to overfitting and loss of parsimony


