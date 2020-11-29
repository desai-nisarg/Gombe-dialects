setwd("~/Desktop/Nisarg files/Gombe-dialects/")
rm(list = ls())

library(tidyverse)

# Read and clean data

dialects <- read_csv("All_communities.csv")

# Remove individuals with number of calls < 8

number_of_calls <- dialects %>% 
  count(Caller)
number_of_calls

dialects <- 
  dialects %>% 
  filter(Caller %in% number_of_calls[number_of_calls$n >= 8,]$Caller)

manual_features <- colnames(dialects)[1:36]
buildup_features <- colnames(dialects)[37:63]
climax_features <- colnames(dialects)[64:90]

# Check
dialects %>% 
  group_by(Community, Caller) %>% count()

########---######Summary of number of calls#####---#####

number_of_calls_by_community <- dialects %>% 
  group_by(Community) %>% 
  summarise(calls = n(), screams = sum(complete.cases(`Climax scream chosen`)), buildups = sum(complete.cases(`Buildup component chosen`)))

number_of_calls_by_caller_community <- dialects %>% 
  group_by(Community, Caller) %>% 
  summarise(calls = n(), screams = sum(complete.cases(`Climax scream chosen`)), buildups = sum(complete.cases(`Buildup component chosen`)))

calls_by_context <- dialects %>% 
  group_by(Community, Caller, Context) %>% 
  summarise(calls = n(), screams = sum(complete.cases(`Climax scream chosen`)), buildups = sum(complete.cases(`Buildup component chosen`)))

dialects %>% 
  group_by(Community, Caller) %>% 
  filter(!is.na(`Climax scream chosen`) & !is.na(`Buildup component chosen`)) %>% 
  summarise(calls = n())

complete_calls <- dialects %>% 
  filter(!is.na(`Climax scream chosen`) & !is.na(`Buildup component chosen`))

complete_calls %>% group_by(Community) %>% summarise(calls = n())

number_of_complete_calls <- complete_calls %>% 
  group_by(Caller) %>% 
  summarise(calls = n())
View(number_of_complete_calls)

complete_calls %>% 
  filter(Caller %in% number_of_complete_calls[number_of_complete_calls$calls >= 5,]$Caller) %>%
  group_by(Community, Caller) %>% 
  summarize(calls = n())

complete_calls_filtered <- complete_calls %>% 
  filter(Caller %in% number_of_complete_calls[number_of_complete_calls$calls >= 5,]$Caller)


# Summarize numeric variables
dialects %>% 
  select_if(is.numeric) %>%
  skimr::skim()

########-------EXPLORATORY PLOTS-------########

# SKIP TO LINE 380 FOR ANALYSIS)

library(ggplot2)

dialects %>% group_by(Community, Caller) %>% 
  count(Caller) %>% 
  ggplot(aes(x=fct_reorder(Caller, n, .desc = T), y = n, fill = Community)) + xlab("Caller") + ylab("Number of calls") +
  geom_col(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

# ENTIRE CALL

dialects %>% 
  ggplot(aes(y=`Duration (B to L)`, x = Community, color = Community)) + ylab("Buildup to Letdown duration (s)") +
  geom_boxplot(show.legend = FALSE) + geom_jitter(show.legend = FALSE) +# facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Duration (B to L)`, x = Caller, color = Community)) + ylab("Buildup to Letdown duration (s)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))


## BUILDUP

dialects %>% 
  filter(!is.na(`Buildup present`)) %>% 
  ggplot(aes(fill=`Buildup present`, x=Community)) + ylab("Proportion of calls") +
  geom_bar(position = "fill") + #facet_wrap(~Community, scales = "free")
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  filter(!is.na(`Buildup present`)) %>% 
  ggplot(aes(fill=`Buildup present`, x=Caller)) + ylab("Proportion of calls") +
  geom_bar(position = "fill") + 
  facet_wrap(~Community, scales = "free") +
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Buildup E components`, x = Community, color = Community)) + ylab("Buildup components (exhalation)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) +# facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Buildup E components`, x = Caller, color = Community)) + ylab("Buildup components (exhalation)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Rate of buildup`, x = Community, color = Community)) +
  geom_boxplot(show.legend = FALSE) + geom_jitter(show.legend = FALSE) +# facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Rate of buildup`, x = Caller, color = Community)) +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Buildup duration`, x = Community, color = Community)) + ylab("Buildup duration (s)") +
  geom_boxplot(show.legend = FALSE) + geom_jitter(show.legend = FALSE) +# facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Buildup duration`, x = Caller, color = Community)) + ylab("Buildup duration (s)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=Acceleration, x = Caller, color = Community)) + ylab("Buildup acceleration (comp/s/s)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=Acceleration, x = Community, color = Community)) + ylab("Buildup acceleration (comp/s/s)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) +# facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

## CLIMAX

dialects %>% 
  filter(!is.na(`Climax present`)) %>%
  ggplot(aes(fill=`Climax present`, x=Caller)) +
  geom_bar(position = "fill") + 
  facet_wrap(~Community, scales = "free")

dialects %>% 
  ggplot(aes(y=`Climax screams`, x = Community, color = Community)) +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + #facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Climax screams`, x = Caller, color = Community)) +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Climax components`, x = Community, color = Community)) + ylab("Climax screams + barks + hoos") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + #facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Climax components`, x = Caller, color = Community)) + ylab("Climax screams + barks + hoos") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Durclx`, x = Caller, color = Community)) + ylab("Climax duration (s)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Durclx`, x = Community, color = Community)) + ylab("Climax duration (s)") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) +# facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Prop of screams`, x = Community, color = Community)) + ylab("Proportion of climax screams") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + #facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Prop of screams`, x = Caller, color = Community)) + ylab("Proportion of climax screams") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))


## LETDOWN

dialects %>% 
  filter(!is.na(`Letdown present`)) %>%
  ggplot(aes(fill=`Letdown present`, x=Community)) + ylab("Proportion of calls") +
  geom_bar(position = "fill") + 
  #facet_wrap(~Community, scales = "free")
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  filter(!is.na(`Letdown present`)) %>%
  ggplot(aes(fill=`Letdown present`, x=Caller)) + ylab("Proportion of calls") +
  geom_bar(position = "fill") + 
  facet_wrap(~Community, scales = "free") +
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Letdown components`, x = Community, color = Community)) +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) +# facet_wrap(~Community, scales = "free_x")
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Letdown components`, x = Caller, color = Community)) +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

## DRUMMING

dialects %>% 
  filter(!is.na(`Drumming`)) %>%
  ggplot(aes(fill=Drumming, x=Community)) + ylab("Drumming present") +
  geom_bar(position = "fill") + #facet_wrap(~Community, scales = "free") +
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  filter(!is.na(`Drumming`)) %>%
  ggplot(aes(fill=Drumming, x=Caller)) + ylab("Drumming present") +
  geom_bar(position = "fill") + facet_wrap(~Community, scales = "free") +
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Drum beats`, x = Community, color = Community)) +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + #facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Drum beats`, x = Caller, color = Community)) +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=`Rate of beats`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(y=`Beats in climax`, x = Caller, color = Community)) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Community, scales = "free_x")

dialects %>% 
  ggplot(aes(fill=`Non linearities`, x=Caller)) +
  geom_bar(position = "fill") + facet_wrap(~Community, scales = "free")

#######----ANALYSIS----###### 

##PCA on structural features and plot

manual_numeric_features <- dialects %>% dplyr::select(manual_features) %>% dplyr::select_if(is.numeric)

manual_numeric_features <- manual_numeric_features %>% dplyr::select(-contains(c("beats","drumming")))

View(manual_numeric_features %>% purrr::map_df(~sum(is.na(.))))

manual_numeric_features <- manual_numeric_features %>% dplyr::select(-contains(c("rate", "Acceleration")))

View(manual_numeric_features %>% purrr::map_df(~sum(is.na(.))))

manual_numeric_features <- manual_numeric_features %>% add_column(Community = dialects$Community, Caller = dialects$Caller)

manual_numeric_features_complete <- manual_numeric_features[complete.cases(manual_numeric_features),]

pca<-princomp(manual_numeric_features_complete[,1:11], cor=TRUE)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot) #See below for descriptions of these commands.
g <- ggbiplot(pca, choices = 1:2, obs.scale=1, var.scale=1, groups=manual_numeric_features_complete$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + ggtitle("PCA on structural features of pant-hoots")+
         theme(legend.direction = 'horizontal', 
               legend.position = 'top', 
               legend.text=element_text(size=14),
               plot.title = element_text(size=18, hjust = 0.5),
               axis.text.x = element_text(size=14),
               axis.text.y = element_text(size=14),
               axis.title.x = element_text(size=16),
               axis.title.y = element_text(size=16)) 
g



# PCA on buildup features and plot

buildup_numeric_features <- dialects %>% 
  dplyr::select(buildup_features) %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select(-durat2) %>% 
  add_column(Community = dialects$Community, Individual = dialects$Caller)

buildup_numeric_features_complete <- buildup_numeric_features[complete.cases(buildup_numeric_features),]

pca2<-princomp(buildup_numeric_features_complete[,1:24], cor=TRUE)

g1 <- ggbiplot(pca2, choices = 1:2, obs.scale=1, var.scale=1, groups=buildup_numeric_features_complete$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g1 <- g1 + scale_color_discrete(name = '')
g1 <- g1 + ggtitle("PCA on acoustic features of buildups")+
  theme(legend.direction = 'horizontal', 
        legend.position = 'top', 
        legend.text=element_text(size=14),
        plot.title = element_text(size=18, hjust = 0.5),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) 
g1

# PCA on climax features and plot

climax_numeric_features <- dialects %>% 
  dplyr::select(climax_features) %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select(-durat2_1) %>% 
  add_column(Community = dialects$Community, Individual = dialects$Caller)

climax_numeric_features_complete <- climax_numeric_features[complete.cases(climax_numeric_features),]

pca3<-princomp(climax_numeric_features_complete[,1:24], cor=TRUE)

g2 <- ggbiplot(pca3, choices = 1:2, obs.scale=1, var.scale=1, groups=climax_numeric_features_complete$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g2 <- g2 + scale_color_discrete(name = '')
g2 <- g2 + ggtitle("PCA on acoustic features of climax screams")+
  theme(legend.direction = 'horizontal', 
        legend.position = 'top', 
        legend.text=element_text(size=14),
        plot.title = element_text(size=18, hjust = 0.5),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) 
g2




library(lme4)


mod <-glmer(Community ~ . -Individual + (1|Individual), data = climax_numeric_features_complete, family = binomial)

# More data cleaning
manual_features <- colnames(dialects)[1:36]
buildup_features <- colnames(dialects)[37:63]
climax_features <- colnames(dialects)[64:90]

manual_features_chosen <- c("Community", "Caller", "Context", "Duration (B to L)", "Buildup present", 
                            "Buildup E components", "Buildup first half", "Buildup second half", 
                            "Buildup duration", "Rate of buildup", "Rate of first half",
                            "Rate of second half", "Acceleration",
                            "Climax present", "Climax components", "Climax screams", "Prop of screams",
                            "Durclx", "Letdown present", "Letdown components") 

dialects_manual <- dialects %>% 
  dplyr::select(manual_features_chosen)
dialects_manual_2 <- dialects_manual %>% mutate_if(is.character, as.factor)

# Impute missing values

library(randomForest)
Community <- dialects_manual_2$Community
Individual <- dialects_manual_2$Caller

# Impute missing data with rfImpute procedure based on Individual level averages
dialects_manual_imputed <- rfImpute(dialects_manual_2[,4:20], Individual)
Imputed_individual$Individual <- pdfa_data$Caller
Imputed_individual_gombe <- Imputed_individual[pdfa_data$Community != "Kanyawara",]
Imputed_individual_gombe <- droplevels(Imputed_individual_gombe)

# Data cleaning for pDFA

######----pDFAs-----###### 

# pDFA FOR CONTEXT
source("~/Desktop/Nisarg files/Dialects/rercodeforpdfa/pdfa_functions.r")
pdfa_data_context <- 
  dialects %>% dplyr::select(Community, Caller, Context, climax_features, -call_1, -durat2_1, -select_1, -noise_mean_1, -noise_max_1, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration_1), !is.na(Context))

pdfa_data_context <- pdfa_data_context %>% filter(Context != "Display", Context != "Resting")

View(pdfa_data_context %>% group_by(Context, Caller) %>% count())

number_by_caller <- pdfa_data_context %>% group_by(Context, Caller) %>% summarise(calls = n())

pdfa_data_context <- pdfa_data_context %>% 
  filter(!(Caller %in% number_by_caller[number_by_caller$calls < 3,]$Caller))

pdfa_data_context <- pdfa_data_context %>% 
  filter(!Caller %in% c("LON", "LAM", "PG", "TJ")) # Do only LON for a sample size of 12.

View(pdfa_data_context %>% group_by(Context, Caller) %>% count())

pdfa_data_context %>% mutate_if(is.numeric, scale) %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_context_scaled <- pdfa_data_context %>% mutate_if(is.numeric, scale)

vars <- names(pdfa_data[,4:19])



pdfa_context=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                      variables=vars, n.to.sel=NULL,
                      n.sel=100, n.perm=1000, pdfa.data=pdfa_data_context_scaled)
pdfa_context

# pDFA FOR COMMUNITY

pdfa_data_community <- 
  dialects %>% dplyr::select(Community, Caller, Context, climax_features, -call_1, -durat2_1, -select_1, -noise_mean_1, -noise_max_1) %>% #, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration_1), !is.na(Context))

pdfa_data_community %>% 
  select_if(is.numeric) %>%
  skimr::skim()

vars_pdfa_community <- names(pdfa_data_community[,4:25])

pdfa_community <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                        variables=vars_pdfa_community, 
                        restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                        n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                        pdfa.data=pdfa_data_community)

pdfa_data_community_gombe <- pdfa_data_community %>% 
  filter(Community != "Kanyawara")

pdfa_data_community_gombe %>% group_by(Community, Caller) %>% count()

pdfa_community_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                              variables=vars_pdfa_community, 
                              restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                              n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                              pdfa.data=pdfa_data_community_gombe)
pdfa_community_gombe

# ON BUILDUPS

pdfa_data_community_buildups <- 
  dialects %>% dplyr::select(Community, Caller, Context, buildup_features, -call, -durat2, -select, -noise_mean, -noise_max) %>% #, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration), !is.na(Context))

pdfa_data_community_buildups %>% group_by(Community, Caller) %>% count

pdfa_data_community_buildups <- pdfa_data_community_buildups %>% filter(Caller != "FAN")

pdfa_data_community_buildups %>% 
  select_if(is.numeric) %>%
  skimr::skim()

vars_pdfa_community_buildups <- names(pdfa_data_community_buildups[,4:25])

pdfa_community_buildups <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                              variables=vars_pdfa_community_buildups, 
                              restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                              n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                              pdfa.data=pdfa_data_community_buildups)
pdfa_community_buildups

pdfa_data_community_buildups_gombe <- pdfa_data_community_buildups %>% 
  filter(Community != "Kanyawara")

pdfa_data_community_buildups_gombe %>% group_by(Community, Caller) %>% count()

pdfa_community_buildups_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                    variables=vars_pdfa_community_buildups, 
                                    restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                    n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                    pdfa.data=pdfa_data_community_buildups_gombe)
pdfa_community_buildups_gombe

# ON COMPLETE CALLS

pdfa_data_community_complete <- 
  complete_calls %>% dplyr::select(Community, Caller, Context, buildup_features, climax_features, -call, -durat2, -select, -noise_mean, -noise_max, -call_1, -durat2_1, -select_1, -noise_mean_1, -noise_max_1) %>% #, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration), !is.na(Context))

pdfa_data_community_complete %>% group_by(Community, Caller) %>% count

pdfa_data_community_complete <- pdfa_data_community_complete %>% filter(Caller != "FAN")

pdfa_data_community_complete %>% 
  select_if(is.numeric) %>%
  skimr::skim()

vars_pdfa_community_complete <- names(pdfa_data_community_complete[,4:47])

pdfa_community_complete <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                       variables=vars_pdfa_community_complete, 
                                       restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                       n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                       pdfa.data=pdfa_data_community_complete)
pdfa_community_complete

pdfa_data_community_complete_gombe <- pdfa_data_community_complete %>% 
  filter(Community != "Kanyawara")

pdfa_data_community_complete_gombe %>% group_by(Community) %>% count()

pdfa_community_complete_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                             variables=vars_pdfa_community_complete, 
                                             restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                             n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                             pdfa.data=pdfa_data_community_complete_gombe)
pdfa_community_complete_gombe



# pDFA FOR INDIVIDUAL

pdfa_data_individual <- 
  dialects %>% dplyr::select(Community, Caller, Context, climax_features, -Pfmin_1, -call_1, -durat2_1, -select_1, -noise_mean_1, -noise_max_1, -trfak_1, -Pfmaxamp_1, -Pfminamp_1, -Pfmaxloc_1, -Pfminloc_1, -Pfmaxdif_1, -F0start_1, -trmean_1, -trmax_1, -lmmean_1, -lmmax_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration_1), !is.na(Context))

pdfa_data_individual %>% 
  select_if(is.numeric) %>%
  skimr::skim()

vars <- names(pdfa_data_individual[,4:10])

View(pdfa_data_individual %>% group_by(Caller) %>% count())

pdfa.ind <- pDFA.nested(test.fac="Caller", contr.fac = NULL,
                        variables=vars, 
                        restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                        n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                        pdfa.data=pdfa_data_individual)
pdfa.ind

##-----------------------------###---------------###------------##


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

pdfa.res.gombe=pDFA.nested(test.fac="Community", contr.fac="Context",
                     variables=vars, restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, pdfa.data=pdfa_gombe)
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


