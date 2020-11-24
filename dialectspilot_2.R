
setwd("~/Desktop/dialects pilot/Pilot 2/") # For mac
setwd("C:/Users/rutadesai/Desktop/dialects/") # For windows
rm(list = ls())

library(tidyverse)

# Read and clean data

dialects <- read_csv("Good panthoots anotated_final copy.csv")
dialects$`Buildup present` <- as.factor(dialects$`Buildup present`)

# Summary of number of calls
calls_by_context <- dialects %>% 
  group_by(Community, Caller, Context) %>% 
  summarise(calls = n(), screams = sum(complete.cases(`Climax scream chosen`)), buildups = sum(complete.cases(`Buildup component chosen`)))


dialects %>% 
  select_if(is.numeric) %>%
  skimr::skim()

# Exploratory plots

library(ggplot2)


dialects %>% 
  group_by(Community, Caller) %>% 
  summarise(sum(complete.cases(`Climax scream chosen`)))


dialects %>% 
  ggplot(aes(x=Caller, color = Community)) +
  geom_bar(stat = "count") + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(fill=`Buildup present`, x=Caller)) +
  geom_bar(position = "fill") + 
  facet_wrap(~Community, scales = "free")

dialects %>% 
ggplot(aes(fill=`Climax present`, x=Caller)) +
  geom_bar(position = "fill") + facet_wrap(~Community, scales = "free")

dialects %>% 
  ggplot(aes(fill=`Letdown present`, x=Caller)) +
  geom_bar(position = "fill") + facet_wrap(~Community, scales = "free")

dialects %>% 
  ggplot(aes(y=`Climax screams`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(y=`Prop of screams`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(y=`Buildup E components`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(y=`Rate of buildup`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(fill=Drumming, x=Caller)) +
  geom_bar(position = "fill") + facet_wrap(~Community, scales = "free")

dialects %>% 
  ggplot(aes(y=`Drum beats`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(y=`Rate of beats`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(y=`Beats in climax`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(fill=`Non linearities`, x=Caller)) +
  geom_bar(position = "fill") + facet_wrap(~Community, scales = "free")

# More data cleaning

dialectsLMA <- dialects[,36:171]
dialectsLMA <- cbind(dialectsLMA, dialects$Community) # Only features from LMA with community ID

climaxes <- dialectsLMA[!dialects$Climax.scream.chosen=="", 69:137]
climaxes <- climaxes[complete.cases(climaxes),] # Only features from climaxes

# PCA on climax features and plot

pca<-princomp(climaxes[,1:68], cor=TRUE)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot) #See below for descriptions of these commands.
g <- ggbiplot(pca, choices = 2:3, obs.scale=1, var.scale=1, groups=climaxes$`dialects$Community`, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
g

# PCA on buildup features and plot

dialectsLMA2 <- cbind(dialects$Community, dialectsLMA)
buildups <- dialectsLMA2[!dialects$Buildup.component.chosen=="", 1:69]
buildups <- buildups[complete.cases(buildups),]

pca2<-princomp(buildups[,2:69], cor=TRUE)

g1 <- ggbiplot(pca2, choices = 2:3, obs.scale=1, var.scale=1, groups=buildups$`dialects$Community`, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g1 <- g1 + scale_color_discrete(name = '')
g1 <- g1 + theme(legend.direction = 'horizontal', legend.position = 'top')
g1


# Data cleaning for pDFA

source("~/Desktop/Nisarg files/Dialects/rercodeforpdfa/pdfa_functions.r")
pdfa_data <- read.csv("dialects_tonality_clean.csv")
pdfa_data$Community <- as.factor(pdfa_data$Community)
pdfa_data$Caller <- as.factor(pdfa_data$Caller)
plot(pdfa_data$Caller)

vars <- names(pdfa_data[,5:29])

library(randomForest)
Community <- pdfa_data$Community
Individual <- pdfa_data$Caller

# Impute missing data with rfImpute procedure based on Individual level averages
Imputed_individual <- rfImpute(pdfa_data[,5:29], Individual)
Imputed_individual$Individual <- pdfa_data$Caller
Imputed_individual_gombe <- Imputed_individual[pdfa_data$Community != "Kanyawara",]
Imputed_individual_gombe <- droplevels(Imputed_individual_gombe)

# Impute missing data with rfImpute procedure based on community level averages
Imputed_community <- rfImpute(pdfa_data[,5:29], Community)
Imputed_community$Community <- pdfa_data$Community
Imputed_community_gombe <- Imputed_community[pdfa_data$Community != "Kanyawara",]
Imputed_community_gombe <- droplevels(Imputed_community_gombe)


pdfa_gombe <- pdfa_data[pdfa_data$Community != "Kanyawara", ]
pdfa_gombe <- droplevels(pdfa_gombe)

# Perform nested pDFA for community-level differences in Gombe chimps and chimps form all 3 communities

pdfa.res.gombe=pDFA.nested(test.fac="Community", contr.fac="Individual",
                     variables=vars, restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, pdfa.data=Imputed_data_gombe)
pdfa.res.gombe

pdfa.res.all=pDFA.nested(test.fac="Community", contr.fac="Individual",
                     variables=vars, restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, pdfa.data=Imputed_data)
pdfa.res.all

correlations <- cor(pdfa_data[,5:29], use = "complete.obs") > 0.7

pdfa_gombe_complete <- pdfa_gombe[complete.cases(pdfa_gombe),]
plot(pdfa_gombe_complete$Caller)

# Perform nested pDFA for individual-level differences in Gombe chimps and chimps form all 3 communities

pdfa.res.gombe.individual=pDFA.nested(test.fac="Individual",
                           variables=vars, restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, pdfa.data=Imputed_data_gombe)
pdfa.res.gombe.individual

pdfa.res.all.individual=pDFA.nested(test.fac="Individual",
                                      variables=vars, restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, pdfa.data=Imputed_data)
pdfa.res.all.individual

# Data cleaning for machine learning

# Split data into train and test sets
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(pdfa_data$Caller, p = .7, 
                                  list = FALSE, 
                                  times = 1)
Imputed_individual_all_train <- Imputed_individual[trainIndex,]
Imputed_individual_all_test <- Imputed_individual[-trainIndex,]

trainIndex_gombe <- createDataPartition(pdfa_gombe$Caller, p = .7, 
                                  list = FALSE, 
                                  times = 1)
Imputed_individual_gombe_train <- Imputed_individual_gombe[trainIndex_gombe,]
Imputed_individual_gombe_test <- Imputed_individual_gombe[-trainIndex_gombe,]

trainIndex <- createDataPartition(pdfa_data$Community, p = .7, 
                                  list = FALSE, 
                                  times = 1)
Imputed_community_all_train <- Imputed_community[trainIndex,]
Imputed_community_all_test <- Imputed_community[-trainIndex,]

trainIndex_gombe <- createDataPartition(pdfa_gombe$Community, p = .7, 
                                        list = FALSE, 
                                        times = 1)
Imputed_community_gombe_train <- Imputed_community_gombe[trainIndex_gombe,]
Imputed_community_gombe_test <- Imputed_community_gombe[-trainIndex_gombe,]


# Random Forest for Gombe chimps and chimps form all 3 communities

RF_all <- randomForest(Community ~ ., data = Imputed_community_all_train)
RF_all
varImpPlot(RF_all)
confusionMatrix(predict(RF_all, newdata=Imputed_community_all_test, type="response"), Imputed_community_all_test$Community)

RF_all_individial <- randomForest(Individual ~ ., data = Imputed_individual_all_train)
RF_all_individial
varImpPlot(RF_all_individial)
confusionMatrix(predict(RF_all_individial, newdata=Imputed_individual_all_test, type="response"), Imputed_individual_all_test$Individual)


RF_gombe <- randomForest(Community ~ ., data = Imputed_community_gombe_train)
RF_gombe
varImpPlot(RF_gombe)
confusionMatrix(predict(RF_gombe, newdata=Imputed_data_gombe_test, type="response"), Imputed_data_gombe_test$Community)

RF_gombe_individual <- randomForest(Individual ~ ., data = Imputed_individual_gombe_train)
RF_gombe_individual
varImpPlot(RF_gombe_individual)
