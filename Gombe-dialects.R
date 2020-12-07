setwd("~/Desktop/Nisarg files/Gombe-dialects/")
rm(list = ls())

library(tidyverse)

# Read and clean data

dialects <- read_csv("All_communities.csv")
class(dialects)

# Remove individuals with number of calls < 8

number_of_calls <- dialects %>% 
  count(Caller)
number_of_calls

dialects <- 
  dialects %>% 
  filter(Caller %in% number_of_calls[number_of_calls$n >= 8,]$Caller)

manual_features <- colnames(dialects)[1:36]
buildup_features <- colnames(dialects)[37:63]
climax_features <- colnames(dialects)[64:91]

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
  dplyr::filter(!is.na(`Climax scream chosen`) & !is.na(`Buildup component chosen`))

complete_calls %>% group_by(Community, Caller) %>% summarise(calls = n())

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

manual_numeric_features <- dialects %>% dplyr::select(all_of(manual_features)) %>% dplyr::select_if(is.numeric)

manual_numeric_features <- manual_numeric_features %>% dplyr::select(-contains(c("beats","drumming")), -Duration)

#View(manual_numeric_features %>% purrr::map_df(~sum(is.na(.))))

#manual_numeric_features <- manual_numeric_features %>% dplyr::select(-contains(c("rate", "Acceleration")))

#View(manual_numeric_features %>% purrr::map_df(~sum(is.na(.))))

manual_numeric_features <- manual_numeric_features %>% 
  add_column(Community = dialects$Community, Caller = dialects$Caller) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::filter(Caller != "ZS")

manual_numeric_features %>% group_by(Community, Caller) %>% count

manual_features_numeric <- names(manual_numeric_features[,1:14])

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

pca_manual_numeric_features <- princomp(manual_numeric_features[,1:14], cor=TRUE)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot) #See below for descriptions of these commands.
g <- ggbiplot(pca_manual_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, groups=manual_numeric_features$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
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

buildup_numeric_features <- buildup_numeric_features %>% filter(complete.cases(.))

pca_buildup_numeric_features <- princomp(buildup_numeric_features[,1:24], cor=TRUE)

g1 <- ggbiplot(pca_buildup_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, groups=buildup_numeric_features$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
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

climax_numeric_features <-  climax_numeric_features %>% filter(complete.cases(.))

pca_climax_numeric_features <- princomp(climax_numeric_features[,1:24], cor=TRUE)

g2 <- ggbiplot(pca_climax_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, groups=climax_numeric_features$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
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


######----pDFAs-----###### 

#######------pDFA FOR CONTEXT------#######
source("~/Desktop/Nisarg files/Dialects/rercodeforpdfa/pdfa_functions.r")



# ON CLIMAXES'

pdfa_data_context_climaxes <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(climax_features), -call_1, -durat2_1, -select_1) %>%  ##, -noise_mean_1, -noise_max_1, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration_1), !is.na(Context))

pdfa_data_context_climaxes <- pdfa_data_context_climaxes %>% filter(Context != "Display", Context != "Resting")

View(pdfa_data_context_climaxes %>% group_by(Context, Caller) %>% count())

number_by_caller_climaxes <- pdfa_data_context_climaxes %>% group_by(Context, Caller) %>% summarise(calls = n())

pdfa_data_context_climaxes <- pdfa_data_context_climaxes %>% 
  filter(!(Caller %in% number_by_caller_climaxes[number_by_caller_climaxes$calls < 3,]$Caller))

pdfa_data_context_climaxes <- pdfa_data_context_climaxes %>% 
  filter(!Caller %in% c("LON", "LAM", "PG", "TJ")) # Do only LON for a sample size of 12.

View(pdfa_data_context_climaxes %>% group_by(Context, Caller) %>% count())

pdfa_data_context_climaxes %>% #mutate_if(is.numeric, scale) %>%
  select_if(is.numeric) %>%
  skimr::skim()

# pdfa_data_context_scaled <- pdfa_data_context %>% mutate_if(is.numeric, scale)


pdfa_data_context_climaxes %>% group_by(Context) %>% count

vars_pdfa_context_climaxes <- names(pdfa_data_context_climaxes[,4:28])

pdfa_context_climaxes=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                      variables=vars_pdfa_context_climaxes, n.to.sel=NULL,
                      n.sel=100, n.perm=1000, 
                      pdfa.data=as.data.frame(pdfa_data_context_climaxes))
pdfa_context_climaxes

# ON CLIMAXES OF GOMBE

pdfa_data_context_climaxes_gombe <- pdfa_data_context_climaxes %>% filter(Community != "Kanyawara")

pdfa_data_context_climaxes_gombe %>% group_by(Context, Caller) %>% count

pca_pdfa_context_climaxes_gombe <- princomp(pdfa_data_context_climaxes_gombe[,4:28], cor = T)
summary(pca_pdfa_context_climaxes_gombe)

pca_scores_pdfa_context_climaxes_gombe <- as_tibble(pca_pdfa_context_climaxes_gombe$scores)

pca_scores_pdfa_context_climaxes_gombe <- pca_scores_pdfa_context_climaxes_gombe %>%
  add_column(Community = pdfa_data_context_climaxes_gombe$Community, Caller = pdfa_data_context_climaxes_gombe$Caller,
             Context = pdfa_data_context_climaxes_gombe$Context)

vars_pdfa_context_climaxes_gombe <- names(pca_scores_pdfa_context_climaxes_gombe[,1:20])

pdfa_context_climaxes_gombe=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                         variables=vars_pdfa_context_climaxes_gombe, n.to.sel=NULL,
                                         n.sel=100, n.perm=1000, 
                                         pdfa.data=as.data.frame(pca_scores_pdfa_context_climaxes_gombe))
pdfa_context_climaxes_gombe


# ON BUILDUPS

pdfa_data_context_buildups <- 
  dialects %>% dplyr::select(Community, Caller, Context, buildup_features, -call, -durat2, -select) %>%  ##, -noise_mean_1, -noise_max_1, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration), !is.na(Context))

pdfa_data_context_buildups <- pdfa_data_context_buildups %>% filter(Context != "Display", Context != "Resting")

View(pdfa_data_context_buildups %>% group_by(Context, Caller) %>% count())

number_by_caller_buildups <- pdfa_data_context_buildups %>% group_by(Context, Caller) %>% summarise(calls = n())

pdfa_data_context_buildups <- pdfa_data_context_buildups %>% 
  filter(!(Caller %in% number_by_caller_buildups[number_by_caller_buildups$calls < 3,]$Caller))

pdfa_data_context_buildups <- pdfa_data_context_buildups %>% 
  filter(!Caller %in% c("LON"))

pdfa_data_context_buildups %>% group_by(Context, Caller) %>% count

pdfa_data_context_buildups %>% #mutate_if(is.numeric, scale) %>%
  select_if(is.numeric) %>%
  skimr::skim()

# pdfa_data_context_scaled <- pdfa_data_context %>% mutate_if(is.numeric, scale)


pdfa_data_context_buildups %>% group_by(Context) %>% count

vars_pdfa_context_buildups <- names(pdfa_data_context_buildups[,4:27])

pdfa_context_buildups=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                   variables=vars_pdfa_context_buildups, n.to.sel=NULL,
                                   n.sel=100, n.perm=1000, 
                                   pdfa.data=as.data.frame(pdfa_data_context_buildups))
pdfa_context_buildups

# ON BUILDPS OF GOMBE

pdfa_data_context_buildups_gombe <- pdfa_data_context_buildups %>% filter(Community != "Kanyawara")

pdfa_data_context_buildups_gombe %>% group_by(Context) %>% count

pca_pdfa_context_buildups_gombe <- princomp(pdfa_data_context_buildups_gombe[,4:27], cor = T)
summary(pca_pdfa_context_buildups_gombe)

pca_scores_pdfa_context_buildups_gombe <- as_tibble(pca_pdfa_context_buildups_gombe$scores)

pca_scores_pdfa_context_buildups_gombe <- pca_scores_pdfa_context_buildups_gombe %>%
  add_column(Community = pdfa_data_context_buildups_gombe$Community, Caller = pdfa_data_context_buildups_gombe$Caller,
             Context = pdfa_data_context_buildups_gombe$Context)

vars_pdfa_context_buildups_gombe <- names(pca_scores_pdfa_context_buildups_gombe[,1:20])

pdfa_context_buildups_gombe=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                   variables=vars_pdfa_context_buildups_gombe, n.to.sel=NULL,
                                   n.sel=100, n.perm=1000, 
                                   pdfa.data=as.data.frame(pca_scores_pdfa_context_buildups_gombe))
pdfa_context_buildups_gombe

# ON COMPLETE CALLS

pdfa_data_context_complete <- 
  complete_calls %>% dplyr::select(Community, Caller, Context, all_of(manual_features_numeric), all_of(buildup_features), all_of(climax_features), -call, -durat2, -select, -call_1, -durat2_1, -select_1) %>%  ##, -noise_mean_1, -noise_max_1, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(complete.cases(.))

pdfa_data_context_complete <- pdfa_data_context_complete %>% dplyr::filter(Context != "Display", Context != "Resting")

View(pdfa_data_context_complete %>% group_by(Context, Caller) %>% count())

number_by_caller_complete <- pdfa_data_context_complete %>% group_by(Context, Caller) %>% summarise(calls = n())

pdfa_data_context_complete <- pdfa_data_context_complete %>% 
  dplyr::filter(!(Caller %in% number_by_caller_complete[number_by_caller_complete$calls < 3,]$Caller))

pdfa_data_context_complete <- pdfa_data_context_complete %>% 
  filter(!Caller %in% c("LON", "YB"))

pdfa_data_context_complete %>% group_by(Context, Caller) %>% count

pdfa_data_context_complete %>% #mutate_if(is.numeric, scale) %>%
  select_if(is.numeric) %>%
  skimr::skim()

# pdfa_data_context_scaled <- pdfa_data_context %>% mutate_if(is.numeric, scale)


pdfa_data_context_complete %>% group_by(Context) %>% count

pca_pdfa_context_complete <- princomp(pdfa_data_context_complete[,4:66], cor = TRUE)
summary(pca_pdfa_context_complete)

pca_scores_pdfa_context_complete <- as_tibble(pca_pdfa_context_complete$scores) 

pca_scores_pdfa_context_complete <- pca_scores_pdfa_context_complete %>% add_column(Caller = pdfa_data_context_complete$Caller, Context = pdfa_data_context_complete$Context, Community = pdfa_data_context_complete$Community)

screeplot(pca_pdfa_context_complete, npcs = 35, type = "lines")

vars_pdfa_context_complete <- names(pca_scores_pdfa_context_complete[,1:30])

pdfa_context_complete=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                   variables=vars_pdfa_context_complete, n.to.sel=NULL,
                                   n.sel=100, n.perm=1000, 
                                   pdfa.data=as.data.frame(pca_scores_pdfa_context_complete))
pdfa_context_complete

# ON COMPLETE CALLS OF GOMBE-- CAN'T BE DONE

pdfa_data_context_complete_gombe <- pdfa_data_context_complete %>% filter(Community != "Kanyawara")

pdfa_data_context_complete_gombe %>% group_by(Context) %>% count

pca_pdfa_context_complete_gombe <- princomp(pdfa_data_context_complete_gombe[,4:61], cor = T)
summary(pca_pdfa_context_complete_gombe)

pca_scores_pdfa_context_complete_gombe <- as_tibble(pca_pdfa_context_complete_gombe$scores)

pca_scores_pdfa_context_complete_gombe <- pca_scores_pdfa_context_complete_gombe %>%
  add_column(Community = pdfa_data_context_complete_gombe$Community, Caller = pdfa_data_context_buildups_gombe$Caller,
             Context = pdfa_data_context_complete_gombe$Context)

vars_pdfa_context_complete_gombe <- names(pca_scores_pdfa_context_complete_gombe[,1:20])

pdfa_context_complete_gombe=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                         variables=vars_pdfa_context_complete_gombe, n.to.sel=NULL,
                                         n.sel=100, n.perm=1000, 
                                         pdfa.data=as.data.frame(pca_scores_pdfa_context_complete_gombe))
pdfa_context_complete_gombe


#######------pDFA FOR COMMUNITY-----#######

# ON CLIMAXES

pdfa_data_community_climaxes <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(climax_features), -call_1, -durat2_1, -select_1) %>% #, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration_1), !is.na(Context))

pdfa_data_community_climaxes %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_community_climaxes %>% group_by(Community) %>% count
pdfa_data_community_climaxes %>% group_by(Community, Caller) %>% count

vars_pdfa_community_climaxes <- names(pdfa_data_community_climaxes[,4:28])

pdfa_community_climaxes <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                        variables=vars_pdfa_community_climaxes, 
                        restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                        n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                        pdfa.data=as.data.frame(pdfa_data_community_climaxes))
pdfa_community_climaxes

# ON CLIMAXES OF GOMBE

pdfa_data_community_climaxes_gombe <- pdfa_data_community_climaxes %>% 
  filter(Community != "Kanyawara")

pdfa_data_community_climaxes_gombe %>% group_by(Community, Caller) %>% count()

pdfa_community_climaxes_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                              variables=vars_pdfa_community_climaxes, 
                              restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                              n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                              pdfa.data=as.data.frame(pdfa_data_community_climaxes_gombe))
pdfa_community_climaxes_gombe

# ON BUILDUPS

pdfa_data_community_buildups <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(buildup_features), -call, -durat2, -select) %>% #, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  filter(!is.na(duration), !is.na(Context))

pdfa_data_community_buildups %>% group_by(Community, Caller) %>% count

pdfa_data_community_buildups <- pdfa_data_community_buildups %>% filter(Caller != "FAN")

pdfa_data_community_buildups %>% 
  select_if(is.numeric) %>%
  skimr::skim()

vars_pdfa_community_buildups <- names(pdfa_data_community_buildups[,4:27])

pdfa_community_buildups <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                              variables=vars_pdfa_community_buildups, 
                              restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                              n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                              pdfa.data=as.data.frame(pdfa_data_community_buildups))
pdfa_community_buildups

# ON BUILDUPS OF GOMBE

pdfa_data_community_buildups_gombe <- pdfa_data_community_buildups %>% 
  dplyr::filter(Community != "Kanyawara")

pdfa_data_community_buildups_gombe %>% group_by(Community) %>% count()

pdfa_community_buildups_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                    variables=vars_pdfa_community_buildups, 
                                    restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                    n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                    pdfa.data=as.data.frame(pdfa_data_community_buildups_gombe))
pdfa_community_buildups_gombe

# ON COMPLETE CALLS

pdfa_data_community_complete <- 
  complete_calls %>% dplyr::select(Community, Caller, Context, all_of(manual_features_numeric), all_of(buildup_features), all_of(climax_features), -call, -durat2, -select, -call_1, -durat2_1, -select_1) %>% #, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  dplyr::filter(complete.cases(.))

pdfa_data_community_complete %>% group_by(Community, Caller) %>% count

pdfa_data_community_complete <- pdfa_data_community_complete %>% dplyr::filter(!Caller %in% c("ZS"))

pdfa_data_community_complete %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pca_pdfa_community_complete <- princomp(pdfa_data_community_complete[,4:66], cor = T)
summary(pca_pdfa_community_complete)

pca_scores_pdfa_community_complete <- as_tibble(pca_pdfa_community_complete$scores)
pca_scores_pdfa_community_complete <- pca_scores_pdfa_community_complete %>% 
  add_column(Community = pdfa_data_community_complete$Community, Caller = pdfa_data_community_complete$Caller,
             Context = pdfa_data_community_complete$Context)


vars_pdfa_community_complete <- names(pca_scores_pdfa_community_complete[,1:38])

pdfa_community_complete <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                       variables=vars_pdfa_community_complete, 
                                       restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                       n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                       pdfa.data=as.data.frame(pca_scores_pdfa_community_complete))
pdfa_community_complete

# ON COMPLETE CALLS OF GOMBE

pdfa_data_community_complete_gombe <- pdfa_data_community_complete %>% 
  filter(Community != "Kanyawara")

pdfa_data_community_complete_gombe %>% group_by(Community, Caller) %>% count()

pdfa_data_community_complete_gombe <- pdfa_data_community_complete_gombe %>% filter(Caller != "ZS")


pca_pdfa_community_complete_gombe <- princomp(pdfa_data_community_complete_gombe[,4:66], cor = T)
summary(pca_pdfa_community_complete_gombe)

pca_scores_pdfa_community_complete_gombe <- as_tibble(pca_pdfa_community_complete_gombe$scores)
pca_scores_pdfa_community_complete_gombe <- pca_scores_pdfa_community_complete_gombe %>% 
  add_column(Community = pdfa_data_community_complete_gombe$Community, Caller = pdfa_data_community_complete_gombe$Caller,
             Context = pdfa_data_community_complete_gombe$Context)


vars_pdfa_community_complete_gombe <- names(pca_scores_pdfa_community_complete_gombe[,1:35])



pdfa_community_complete_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                             variables=vars_pdfa_community_complete_gombe, 
                                             restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                             n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                             pdfa.data=as.data.frame(pca_scores_pdfa_community_complete_gombe))
pdfa_community_complete_gombe



#######------pDFA FOR INDIVIDUAL----######

# ON CLIMAXES

pdfa_data_individual_climaxes <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(climax_features), -call_1, -durat2_1, -select_1) %>%     # -noise_mean_1, -noise_max_1, -trfak_1, -Pfmaxamp_1, -Pfminamp_1, -Pfmaxloc_1, -Pfminloc_1, -Pfmaxdif_1, -F0start_1, -trmean_1, -trmax_1, -lmmean_1, -lmmax_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  dplyr::filter(!is.na(duration_1), !is.na(Context))

pdfa_data_individual_climaxes %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_individual_climaxes %>% group_by(Caller) %>% count

pca_pdfa_individual_climaxes <- princomp(pdfa_data_individual_climaxes[,4:28], cor = T)
summary(pca_pdfa_individual_climaxes)
screeplot(pca_pdfa_individual_climaxes, npcs = 10,type = "lines")

pca_scores_pdfa_individual_climaxes <- as_tibble(pca_pdfa_individual_climaxes$scores)
pca_scores_pdfa_individual_climaxes <- pca_scores_pdfa_individual_climaxes %>% 
  add_column(Community = pdfa_data_individual_climaxes$Community, Caller = pdfa_data_individual_climaxes$Caller,
             Context = pdfa_data_individual_climaxes$Context)

vars_pdfa_individual_climaxes <- names(pca_scores_pdfa_individual_climaxes[,1:8])

pdfa_individual_climaxes <- pDFA.nested(test.fac="Caller",
                                        variables=vars_pdfa_individual_climaxes, 
                                        restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_climaxes))
pdfa_individual_climaxes

# ON CLIMAXES OF GOMBE

pdfa_data_individual_climaxes_gombe <- pdfa_data_individual_climaxes %>% 
  filter(Community != "Kanyawara")

pdfa_data_individual_climaxes_gombe %>% group_by(Community, Caller) %>% count()

pca_pdfa_individual_climaxes_gombe <- princomp(pdfa_data_individual_climaxes_gombe[,4:28], cor = T)
summary(pca_pdfa_individual_climaxes_gombe)

pca_scores_pdfa_individual_climaxes_gombe <- as_tibble(pca_pdfa_individual_climaxes_gombe$scores)
pca_scores_pdfa_individual_climaxes_gombe <- pca_scores_pdfa_individual_climaxes_gombe %>% 
  add_column(Community = pdfa_data_individual_climaxes_gombe$Community, Caller = pdfa_data_individual_climaxes_gombe$Caller,
             Context = pdfa_data_individual_climaxes_gombe$Context)


vars_pdfa_individual_climaxes_gombe <- names(pca_scores_pdfa_individual_climaxes_gombe[,1:8])

pdfa_individual_climaxes_gombe <- pDFA.nested(test.fac="Caller",
                                        variables=vars_pdfa_individual_climaxes_gombe, 
                                        restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_climaxes_gombe))
pdfa_individual_climaxes_gombe

# ON BUILDUPS

pdfa_data_individual_buildups <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(buildup_features), -call, -durat2, -select) %>%     # -noise_mean_1, -noise_max_1, -trfak_1, -Pfmaxamp_1, -Pfminamp_1, -Pfmaxloc_1, -Pfminloc_1, -Pfmaxdif_1, -F0start_1, -trmean_1, -trmax_1, -lmmean_1, -lmmax_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  dplyr::filter(!is.na(duration), !is.na(Context))

pdfa_data_individual_buildups %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_individual_buildups %>% group_by(Caller) %>% count

pdfa_data_individual_buildups <- pdfa_data_individual_buildups %>% 
  dplyr::filter(Caller != "FAN")

pca_pdfa_individual_buildups <- princomp(pdfa_data_individual_buildups[,4:27], cor = T)
summary(pca_pdfa_individual_buildups)
screeplot(pca_pdfa_individual_buildups, npcs = 10,type = "lines")

pca_scores_pdfa_individual_buildups <- as_tibble(pca_pdfa_individual_buildups$scores)
pca_scores_pdfa_individual_buildups <- pca_scores_pdfa_individual_buildups %>% 
  add_column(Community = pdfa_data_individual_buildups$Community, Caller = pdfa_data_individual_buildups$Caller,
             Context = pdfa_data_individual_buildups$Context)

vars_pdfa_individual_buildups <- names(pca_scores_pdfa_individual_buildups[,1:5])

pdfa_individual_buildups <- pDFA.nested(test.fac="Caller",
                                        variables=vars_pdfa_individual_buildups, 
                                        restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_buildups))
pdfa_individual_buildups

# ON BUILDUPS OF GOMBE

pdfa_data_individual_buildups_gombe <- pdfa_data_individual_buildups %>% 
  filter(Community != "Kanyawara")

pdfa_data_individual_buildups_gombe %>% group_by(Community, Caller) %>% count()

pca_pdfa_individual_buildups_gombe <- princomp(pdfa_data_individual_buildups_gombe[,4:27], cor = T)
summary(pca_pdfa_individual_buildups_gombe)
screeplot(pca_pdfa_individual_buildups_gombe, npcs = 10,type = "lines")

pca_scores_pdfa_individual_buildups_gombe <- as_tibble(pca_pdfa_individual_buildups_gombe$scores)
pca_scores_pdfa_individual_buildups_gombe <- pca_scores_pdfa_individual_buildups_gombe %>% 
  add_column(Community = pdfa_data_individual_buildups_gombe$Community, Caller = pdfa_data_individual_buildups_gombe$Caller,
             Context = pdfa_data_individual_buildups_gombe$Context)


vars_pdfa_individual_buildups_gombe <- names(pca_scores_pdfa_individual_buildups_gombe[,1:5])

pdfa_individual_buildups_gombe <- pDFA.nested(test.fac="Caller",
                                              variables=vars_pdfa_individual_buildups_gombe, 
                                              restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                              pdfa.data=as.data.frame(pca_scores_pdfa_individual_buildups_gombe))
pdfa_individual_buildups_gombe


# ON COMPLETE CALLS

pdfa_data_individual_complete <- 
  complete_calls %>% dplyr::select(Community, Caller, Context, all_of(manual_features_numeric), all_of(buildup_features), all_of(climax_features), -call, -durat2, -select, -call_1, -durat2_1, -select_1) %>% #, -Pfmaxamp_1, -Pfminamp_1, -F0start_1, -F0end_1, -Pfstart_1, -Pfend_1) %>% 
  dplyr::filter(complete.cases(.))

pdfa_data_individual_complete %>% group_by(Community, Caller) %>% count

pdfa_data_individual_complete <- pdfa_data_individual_complete %>% filter(!Caller %in% c("FAN", "ZS"))


#pdfa_data_individual_complete <- pdfa_data_individual_complete %>% 
 # filter(complete.cases(.))

pdfa_data_individual_complete %>% group_by(Community, Caller) %>% count

pdfa_data_individual_complete %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pca_pdfa_individual_complete <- princomp(pdfa_data_individual_complete[,4:66], cor = T)
summary(pca_pdfa_individual_complete)
screeplot(pca_pdfa_individual_complete, type = "lines")

pca_scores_pdfa_individual_complete <- as_tibble(pca_pdfa_individual_complete$scores)
pca_scores_pdfa_individual_complete <- pca_scores_pdfa_individual_complete %>% 
  add_column(Community = pdfa_data_individual_complete$Community, Caller = pdfa_data_individual_complete$Caller,
             Context = pdfa_data_individual_complete$Context)


vars_pdfa_individual_complete <- names(pca_scores_pdfa_individual_complete[,1:4])

pdfa_individual_complete <- pDFA.nested(test.fac="Caller",
                                       variables=vars_pdfa_individual_complete, 
                                       restrict.by="Community", n.contr.fac.levels.to.sel=NULL, 
                                       n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                       pdfa.data=as.data.frame(pca_scores_pdfa_individual_complete))
pdfa_individual_complete

# ON COMPLETE CALLS OF GOMBE

pdfa_data_individual_complete_gombe <- pdfa_data_individual_complete %>% 
  filter(Community != "Kanyawara")

pdfa_data_individual_complete_gombe %>% group_by(Community, Caller) %>% count()


pca_pdfa_individual_complete_gombe <- princomp(pdfa_data_individual_complete_gombe[,4:66], cor = T)
summary(pca_pdfa_individual_complete_gombe)
screeplot(pca_pdfa_individual_complete_gombe, type = "lines")

pca_scores_pdfa_individual_complete_gombe <- as_tibble(pca_pdfa_individual_complete_gombe$scores)
pca_scores_pdfa_individual_complete_gombe <- pca_scores_pdfa_individual_complete_gombe %>% 
  add_column(Community = pdfa_data_individual_complete_gombe$Community, Caller = pdfa_data_individual_complete_gombe$Caller,
             Context = pdfa_data_individual_complete_gombe$Context)


vars_pdfa_individual_complete_gombe <- names(pca_scores_pdfa_individual_complete_gombe[,1:4])



pdfa_individual_complete_gombe <- pDFA.nested(test.fac="Caller",
                                             variables=vars_pdfa_individual_complete_gombe, 
                                             restrict.by="Community", n.contr.fac.levels.to.sel=NULL, 
                                             n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                             pdfa.data=as.data.frame(pca_scores_pdfa_individual_complete_gombe))
pdfa_individual_complete_gombe




########-----------#######---------------###------------#######


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


