#Violent and non-Violent crimes by state - Aggregate view
#group Violent crime and nonViolent crime by state

crimedatafile <- read.csv("~/crimedatafile.csv", na.strings=c("NA", "-", "?"), header = T, stringsAsFactors = FALSE)
install.packages("magrittr")
crimedata_state=aggregate(newdata[,c('ViolentCrimesPerPop','nonViolPerPop')], by=list(crimedatafile$state), FUN=mean)
crimedata_state=aggregate(crimedatafile[,c('ViolentCrimesPerPop','nonViolPerPop')], by=list(crimedatafile$state), FUN=mean)
library(magrittr)
library("plotly")
l <- list(color = toRGB("white"), width = 2)
g <- list(
+     scope = 'usa',
+     projection = list(type = 'albers usa'),
+     showlakes = TRUE,
+     lakecolor = toRGB('white')
+ )
g <- list(
scope = 'usa',
projection = list(type = 'albers usa'),
showlakes = TRUE,
lakecolor = toRGB('white')
)
plot_geo(crimedata_state, locationmode = 'USA-states') %>%
add_trace(
z = ~crimedata_state$nonViolPerPop,locations = ~crimedata_state$Group.1,
color = ~crimedata_state$nonViolPerPop, colors = 'Purples'
) %>%
colorbar(title = "non-Violent Crimes(Per-100K-Pop)") %>%
layout(
title = 'Aggregate view of non-Violent Crimes Per 100K Population across US',
geo = g
)

plot_geo(crimedata_state, locationmode = 'USA-states') %>%
add_trace(
z = ~crimedata_state$ViolentCrimesPerPop,locations = ~crimedata_state$Group.1,
color = ~crimedata_state$ViolentCrimesPerPop, colors = 'Purples'
) %>%
colorbar(title = "Violent Crimes(Per-100K-Pop)") %>%
layout(
title = 'Aggregate view of Violent Crimes Per 100K Population across US',
geo = g
)


#Boxplots - Exploratory Data Analysis of Response Variables
#Boxplot of non violent crime variables
Violent= crimedatafile[,c('murdPerPop','rapesPerPop','robbbPerPop','assaultPerPop','ViolentCrimesPerPop')]
nonViolent = crimedatafile[,c('burglPerPop','larcPerPop','autoTheftPerPop','arsonsPerPop','nonViolPerPop')]
nonViolentxLabels = c('burglPerPop','larcPerPop','autoTheftPerPop','arsonsPerPop','nonViolPerPop')
violentxLabels = c('murdPerPop','rapesPerPop','robbbPerPop','assaultPerPop','ViolentCrimesPerPop')
boxplot(Violent,main="Violent crimes",violentxLabels)
boxplot(nonViolent,main="Non-violent crimes",nonViolentxLabels)

###Linear Regression (Sub-Selection) [Model 1] ###

df.working <- crimedatafile
drops <- c("X", "fold","Êcommunityname","state")
df.working <- df.working[, !(names(df.working) %in% drops)]
set.seed(1)
train_ind = sample(1:nrow(df.working), 0.7 * nrow(df.working))
normalize <- function(x) {
  +     return((x - min(x))/(max(x) - min(x)))
  + }
df.working_dt <- df.working
notneededFeatures <- c("PctSpeakEnglOnlyCat", "PctNotSpeakEnglWellCat", 
                         +                        "PctHousOccupCat", "RentQrange")
possible_predictors = colnames(df.working)[!(colnames(df.working) %in% +notneededFeatures)]
df.working = df.working[, names(df.working) %in% possible_predictors]
df.norm <- as.data.frame(lapply(df.working, normalize))
install.packages("leaps")
library(leaps)
regfit.full = regsubsets(ViolentCrimesPerPop ~ ., data = df.norm[train_ind,], really.big = T, nvmax = 6)
training.mat = model.matrix(ViolentCrimesPerPop ~ ., data = df.norm[train_ind,])
training.errors = rep(NA, 6)
for (ii in 1:6) {
  coefi = coef(regfit.full, id = ii)
  pred = training.mat[, names(coefi)] %*% coefi
  training.errors[ii] = mse(df.norm[train_ind, 97], pred)
}
test.mat = model.matrix(ViolentCrimesPerPop ~ ., data = df.norm[-train_ind,])
test.errors = rep(NA, 6)
for (ii in 1:6) {
  coefi = coef(regfit.full, id = ii)
  pred = test.mat[, names(coefi)] %*% coefi
  test.errors[ii] = mse(df.norm[-train_ind, 97], pred)
}
k = which.min(test.errors)
MSE_SLM = test.errors[k]



###Linear Regression (VIF Selection) [Model 2] ###

#Correlaions
crimedata.fourth <- crimedatafile
cols = c('HousVacant','PctHousOccup','PctHousOwnOcc','PctVacantBoarded','PctVacMore6Mos','PctUnemployed','PctEmploy','murdPerPop','rapesPerPop','robbbPerPop','assaultPerPop','ViolentCrimesPerPop','burglPerPop','larcPerPop','autoTheftPerPop','arsonsPerPop')
head(crimedata.fourth)
crimedata.fourth[,cols]
crimedata.study = crimedata.fourth[,cols]
library(dplyr)
correl <- round(cor(crimedata.study),2)

library(ggcorrplot)
ggcorrplot(correl)

cor_df <- as.data.frame(as.table(correl))
cor_df <- cor_df[cor_df$Freq != 1,]
cor_df %>%  arrange(desc(abs(Freq))) %>% filter(abs(Freq)>0.5)

#there exists multicollinearity between variables.  We will use VIF to remove multicollinearity 

library(car)
library(plyr)

fit=lm(ViolentCrimesPerPop ~ . , data=crimedata.study)

vif(fit)

# Set a VIF threshold. All the variables having higher VIF than threshold are dropped from the model
threshold=2.5

# Sequentially drop the variable with the largest VIF until all variables have VIF less than threshold
drop=TRUE

aftervif=data.frame()
while(drop==TRUE) {
  vfit=vif(fit)
  aftervif=rbind.fill(aftervif,as.data.frame(t(vfit)))
  if(max(vfit)>threshold) { fit=
    update(fit,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
  else { drop=FALSE }}


# Model after removing correlated Variables
print(fit)

# How variables removed sequentially
t_aftervif= as.data.frame(t(aftervif))
edit(t_aftervif)

# Final (uncorrelated) variables with their VIFs
vfit_d= as.data.frame(vfit)

set.seed(1)
row.number <- sample(1:nrow(crimedata.study), 0.9*nrow(crimedata.study))
train = crimedata.study[row.number,]
test = crimedata.study[-row.number,]
dim(train)
dim(test)
New_Fit=lm(ViolentCrimesPerPop ~ HousVacant + PctHousOccup + PctHousOwnOcc + PctVacantBoarded + PctVacMore6Mos + PctEmploy  + murdPerPop + rapesPerPop + assaultPerPop +larcPerPop + autoTheftPerPop + arsonsPerPop  , data=train)
summary(New_Fit)
pred1 <- predict(New_Fit, newdata = test)
library(Metrics)
c(RMSE = rmse, R2=summary(New_Fit)$r.squared)
anova(New_Fit)


