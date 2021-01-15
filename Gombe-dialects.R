####----INTRODUCTION----#####
# Manuscript title: "Chimpanzee pant-hoots encode individual but not group differences"
# Manuscript authors: Nisarg Desai, Pawel Fedurek, Katie Slocombe, Michael Wilson
# Code author: Nisarg Desai. Utilized functions written by Roger Mundry and Christof Neumann
# Last update: 05 January 2021


# Note to the user: Must run essential code till line 62 before anything else. 
# Every other section can be run independently after the essential code is run. 
# Each new section begins with 7 #'s (#######)

#######----ESSENTIAL CODE----######
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

# Store the names of different types of acoustic features 

structural_features <- colnames(dialects)[1:36]
buildup_features <- colnames(dialects)[37:63]
climax_features <- colnames(dialects)[64:91]

# Check
dialects %>% 
  group_by(Community, Caller) %>% count()

# Filter calls that don't have a buildup or climax
complete_calls <- dialects %>% 
  dplyr::filter(!is.na(`Climax scream chosen`) & !is.na(`Buildup component chosen`))

# Data frame for only structural features that are not categorical
structural_numeric_features <- dialects %>% dplyr::select(all_of(structural_features)) %>% dplyr::select_if(is.numeric)

structural_numeric_features <- structural_numeric_features %>% dplyr::select(-contains(c("beats","drumming")), -Duration) # remove drumming related features and full duration

structural_numeric_features <- structural_numeric_features %>% 
  add_column(Community = dialects$Community, Caller = dialects$Caller, Context = dialects$Context) %>%
  dplyr::filter(complete.cases(.)) 

# Check composition of the data
structural_numeric_features %>% group_by(Community, Caller) %>% count # At least 4 calls per caller
structural_numeric_features %>% group_by(Community) %>% count

structural_features_numeric <- names(structural_numeric_features[,1:14])

# Summary
structural_numeric_features %>% skimr::skim()


#######-------EXPLORATORY PLOTS-------########

# SKIP TO LINE 374 FOR ANALYSIS

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
  ggplot(aes(y=Acceleration, x = Caller, color = Community)) + ylab("Buildup acceleration") +
  geom_boxplot(show.legend = F) + geom_jitter(show.legend = F) + facet_wrap(~Community, scales = "free_x") +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

dialects %>% 
  ggplot(aes(y=Acceleration, x = Community, color = Community)) + ylab("Buildup acceleration") +
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

#######----PRINCIPAL COMPONENTS ANALYSIS AND PLOTS FOR PUBLICATION----###### 
### SKIP TO LINE 682 FOR pDFAs
# library(devtools)
# install_github("vqv/ggbiplot")

##PCA on structural features and plot

structural_numeric_features_context <- structural_numeric_features %>% 
  dplyr::filter(Context != "Display", Context != "Resting")

pca_structural_numeric_features <- princomp(structural_numeric_features_context[,1:14], cor=TRUE)
summary(pca_structural_numeric_features)
pca_structural_numeric_features$loadings

library(ggbiplot) 
g2.1 <- ggbiplot(pca_structural_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, 
              groups=structural_numeric_features_context$Context,
              var.axes = F, ellipse = TRUE, circle = F, varname.size=4, varname.adjust=2)
g2.1 <- g2.1 + ggtitle("(a) Structural features")+ 
  scale_color_manual(name="Context", values=c("orange", "purple")) +  
  scale_shape_manual(name="Context", values=c(17,18)) +
  geom_point(aes(colour=structural_numeric_features_context$Context, shape=structural_numeric_features_context$Context), size = 3) +
  #geom_text(aes(label = structural_numeric_features_context$Filename)) +
  theme_bw() +
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_rect(colour = "black", size=1),
               legend.direction = 'vertical', 
               legend.position = 'right',
               legend.background = element_rect(colour = 'black', size = 0.5),
               legend.title = element_text(size=18),
               legend.text=element_text(size=18),
               plot.title = element_text(size=18),
               axis.text.x = element_text(size=14),
               axis.text.y = element_text(size=14),
               axis.title.x = element_text(size=16),
               axis.title.y = element_text(size=16)) 
g2.1

g3.1 <- ggbiplot(pca_structural_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, 
                 groups=structural_numeric_features_context$Community,
                 var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g3.1 <- g3.1 + ggtitle("(a) Structural features")+ 
  scale_color_manual(name="Community", values=c("red", "#008080", "yellowgreen")) +  
  scale_shape_manual(name="Community", values=c(17,18,19)) +
  geom_point(aes(colour=structural_numeric_features_context$Community, shape=structural_numeric_features_context$Community), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1),
        legend.direction = 'vertical', 
        legend.position = 'right',
        legend.background = element_rect(colour = 'black', size = 0.5),
        legend.title = element_text(size=18),
        legend.text=element_text(size=18),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) 
g3.1

# loading ggbiplot loads plyr which causes dplyr features to misbehave. So detach plyr and ggbiplot once you're done with it
detach(package:ggbiplot)
detach(package:plyr)


# PCA on buildup features and plot

buildup_numeric_features <- dialects %>% 
  dplyr::select(all_of(buildup_features)) %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select(-durat2) %>% 
  add_column(Community = dialects$Community, Individual = dialects$Caller, Context = dialects$Context)

buildup_numeric_features <- buildup_numeric_features %>% dplyr::filter(complete.cases(.))
buildup_numeric_features_context <- buildup_numeric_features %>% dplyr::filter(Context != "Display", Context != "Resting")

pca_buildup_numeric_features <- princomp(buildup_numeric_features[,1:24], cor=TRUE)
pca_buildup_numeric_features_context <- princomp(buildup_numeric_features_context[,1:24], cor=TRUE)
library(ggbiplot)

g3.2 <- ggbiplot(pca_buildup_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, groups=buildup_numeric_features$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g3.2 <- g3.2 + ggtitle("(b) Build-up features")+
  scale_color_manual(name="Community", values=c("red", "#008080", "yellowgreen")) +  
  scale_shape_manual(name="Community", values=c(17,18,19)) +
  geom_point(aes(colour=buildup_numeric_features$Community, shape=buildup_numeric_features$Community), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1),
        #legend.direction = 'horizontal', 
        legend.position = 'none', 
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) + ylim(-7.5,6)
g3.2

g2.2 <- ggbiplot(pca_buildup_numeric_features_context, choices = 1:2, obs.scale=1, var.scale=1, groups=buildup_numeric_features_context$Context, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g2.2 <- g2.2 + ggtitle("(b) Build-up features")+
  scale_color_manual(name="Context", values=c("orange", "purple")) +  
  scale_shape_manual(name="Context", values=c(17,18)) +
  geom_point(aes(colour=buildup_numeric_features_context$Context, shape=buildup_numeric_features_context$Context), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.direction = 'vertical', 
        legend.position = 'none', 
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        panel.border = element_rect(colour = "black", size=1)) #+ ylim(-7,9)
g2.2


# PCA on climax features and plot
detach(package:ggbiplot)
detach(package:plyr)
climax_numeric_features <- dialects %>% 
  dplyr::select(all_of(climax_features)) %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select(-durat2_1) %>% 
  add_column(Community = dialects$Community, Individual = dialects$Caller, Context = dialects$Context)

climax_numeric_features <-  climax_numeric_features %>% filter(complete.cases(.))
climax_numeric_features_context <- climax_numeric_features %>% 
  dplyr::filter(Context != "Display", Context != "Resting")

pca_climax_numeric_features <- princomp(climax_numeric_features[,1:25], cor=TRUE)
pca_climax_numeric_features$loadings
pca_climax_numeric_features_context <- princomp(climax_numeric_features_context[,1:25], cor=TRUE)

library(ggbiplot)
g3.3 <- ggbiplot(pca_climax_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, groups=climax_numeric_features$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g3.3 <- g3.3 + ggtitle("(c) Climax scream features")+
  scale_color_manual(name="Community", values=c("red", "#008080", "yellowgreen")) +  
  scale_shape_manual(name="Community", values=c(17,18,19)) +
  geom_point(aes(colour=climax_numeric_features$Community, shape=climax_numeric_features$Community), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1),
        #legend.direction = 'horizontal', 
        legend.position = 'none', 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) + ylim(-9,4.5)
g3.3

g2.3 <- ggbiplot(pca_climax_numeric_features_context, choices = 1:2, obs.scale=1, var.scale=1, groups=climax_numeric_features_context$Context, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g2.3 <- g2.3 + ggtitle("(c) Climax scream features")+
  scale_color_manual(name="Context", values=c("orange", "purple")) +  
  scale_shape_manual(name="Context", values=c(17,18)) +
  geom_point(aes(colour=climax_numeric_features_context$Context, shape=climax_numeric_features_context$Context), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1),
        #legend.direction = 'horizontal', 
        legend.position = 'none', 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) + ylim(-4,12)
g2.3

# PCA on complete features and plots
detach(package:ggbiplot)
detach(package:plyr)
complete_numeric_features <- complete_calls %>% 
  dplyr::select(all_of(structural_features_numeric), all_of(climax_features), all_of(buildup_features)) %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select(-durat2_1, -durat2) %>% 
  add_column(Community = complete_calls$Community, Individual = complete_calls$Caller, Context = complete_calls$Context)

complete_numeric_features <-  complete_numeric_features %>% filter(complete.cases(.))
complete_numeric_features_context <- complete_numeric_features %>% 
  dplyr::filter(Context != "Display", Context != "Resting")
complete_numeric_features_individual_kasekela <- complete_numeric_features %>% 
  dplyr::filter(Community == "Kasekela")
complete_numeric_features_individual_mitumba <- complete_numeric_features %>% 
  dplyr::filter(Community == "Mitumba")
complete_numeric_features_individual_kanyawara <- complete_numeric_features %>% 
  dplyr::filter(Community == "Kanyawara")

pca_complete_numeric_features <- princomp(complete_numeric_features[,1:63], cor=TRUE)
pca_complete_numeric_features_context <- princomp(complete_numeric_features_context[,1:63], cor=TRUE)
pca_complete_numeric_features_individual_kasekela <- princomp(complete_numeric_features_individual_kasekela[,1:63], cor=TRUE)
pca_complete_numeric_features_individual_mitumba <- princomp(complete_numeric_features_individual_mitumba[,1:41], cor=TRUE)
pca_complete_numeric_features_individual_kanyawara <- princomp(complete_numeric_features_individual_kanyawara[,1:63], cor=TRUE)

library(ggbiplot)
g2.4 <- ggbiplot(pca_complete_numeric_features_context, choices = 1:2, obs.scale=1, var.scale=1, groups=complete_numeric_features_context$Context, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g2.4 <- g2.4 + ggtitle("(d) All acoustic features")+
  scale_color_manual(name="Context", values=c("orange", "purple")) +  
  scale_shape_manual(name="Context", values=c(17,18)) +
  geom_point(aes(colour=complete_numeric_features_context$Context, shape=complete_numeric_features_context$Context), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1),
        #legend.direction = 'vertical', 
        legend.position = 'none', 
        legend.title = element_blank(),
        legend.text=element_text(size=18),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))
g2.4

g3.4 <- ggbiplot(pca_complete_numeric_features, choices = 1:2, obs.scale=1, var.scale=1, groups=complete_numeric_features$Community, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g3.4 <- g3.4 + ggtitle("(d) All acoustic features")+
  scale_color_manual(name="Community", values=c("red", "#008080", "yellowgreen")) +  
  scale_shape_manual(name="Community", values=c(17,18,19)) +
  geom_point(aes(colour=complete_numeric_features$Community, shape=complete_numeric_features$Community), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1),
        #legend.direction = 'vertical', 
        legend.position = 'none', 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) #+ xlim(-18, 4)
g3.4

g5.1 <- ggbiplot(pca_complete_numeric_features_individual_kasekela, choices = 1:2, obs.scale=1, var.scale=1, groups=complete_numeric_features_individual_kasekela$Individual, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g5.1 <- g5.1 + ggtitle("(a) Kasekela")+
  scale_color_manual(name="Caller", values=c("#E69F00", "#56B4E9", "#009E73",
                                             "#F0E442", "#0072B2", "#D55E00")) +  
  scale_shape_manual(name="Caller", values=c(17,18,19,20,21,22)) +
  geom_point(aes(colour=complete_numeric_features_individual_kasekela$Individual, shape=complete_numeric_features_individual_kasekela$Individual), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.direction = 'horizontal', 
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) + ylim(-6,8) +
  guides(color = guide_legend(nrow = 1))
g5.1

g5.2 <- ggbiplot(pca_complete_numeric_features_individual_mitumba, choices = 1:2, obs.scale=1, var.scale=1, groups=complete_numeric_features_individual_mitumba$Individual, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g5.2 <- g5.2 + ggtitle("(b) Mitumba")+
  scale_color_manual(name="Caller", values=c("#E69F00", "#56B4E9", "#009E73",
                                             "#F0E442", "#0072B2")) +  
  scale_shape_manual(name="Caller", values=c(17,18,19,20,21,22)) +
  geom_point(aes(colour=complete_numeric_features_individual_mitumba$Individual, shape=complete_numeric_features_individual_mitumba$Individual), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.direction = 'horizontal', 
        legend.position = 'top', 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) + ylim(-7,7)
g5.2

g5.3 <- ggbiplot(pca_complete_numeric_features_individual_kanyawara, choices = 1:2, obs.scale=1, var.scale=1, groups=complete_numeric_features_individual_kanyawara$Individual, var.axes = FALSE, ellipse = TRUE, circle = TRUE)
g5.3 <- g5.3 + ggtitle("(c) Kanyawara")+
  scale_color_manual(name="Caller", values=c("#E69F00", "#56B4E9", "#009E73",
                                             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +  
  scale_shape_manual(name="Caller", values=c(17,18,19,20,21,22,23)) +
  geom_point(aes(colour=complete_numeric_features_individual_kanyawara$Individual, shape=complete_numeric_features_individual_kanyawara$Individual), size = 3) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.direction = 'vertical', 
        legend.position = 'right', 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        plot.title = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) + coord_flip() + xlim(-4,4)
g5.3

#######----pDFAs-----###### 
detach(package:ggbiplot)
detach(package:plyr)
source("~/Desktop/Nisarg files/Dialects/rercodeforpdfa/pdfa_functions.r")
devtools::install_github("gobbios/cfp")
library(cfp)

#######------pDFA FOR CONTEXT------#######

# ON STRUCTURAL NUMERIC FEATURES

# Data cleaning
structural_numeric_features_context <- as.data.frame(structural_numeric_features)

names(structural_numeric_features_context) <- make.names(names(structural_numeric_features_context))

# Remove calls from Display and Resting contexts
structural_numeric_features_context <- structural_numeric_features_context %>% 
  dplyr::filter(Context != "Display", Context != "Resting")

number_by_caller_context_structural <- structural_numeric_features_context %>% dplyr::group_by(Context, Caller) %>% count()

# Remove callers with less than 3 recordings in each context
structural_numeric_features_context <- structural_numeric_features_context %>% 
  dplyr::filter(!(Caller %in% number_by_caller_context_structural[number_by_caller_context_structural$n < 3,]$Caller))

structural_numeric_features_context <- structural_numeric_features_context %>% 
  filter(!Caller %in% c("ZS")) 

a<-structural_numeric_features_context %>% group_by(Context, Caller) %>% count
a # 11 individuals with at least 3 calls in each context
a %>% ungroup() %>% group_by(Context) %>% 
  summarize(mean = mean(n), median = median(n), range = range(n))

# PCA to reduce number of features
pca_pdfa_context_structural <- princomp(structural_numeric_features_context[,1:14], cor = T)
screeplot(pca_pdfa_context_structural, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_context_structural <- as_tibble(pca_pdfa_context_structural$scores)
pca_scores_pdfa_context_structural <- pca_scores_pdfa_context_structural %>% 
  add_column(Caller = structural_numeric_features_context$Caller, Context = structural_numeric_features_context$Context,
             Community = structural_numeric_features_context$Community)

# Variable names to be used in the pDFA
vars_pdfa_context_structural <- names(pca_scores_pdfa_context_structural[,1:7])

# Perform the pDFA
set.seed(46) # For repeatable results; p=.0440
pdfa_context_structural=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                   variables=vars_pdfa_context_structural, n.to.sel=NULL,
                                   n.sel=100, n.perm=1000, 
                                   pdfa.data=as.data.frame(pca_scores_pdfa_context_structural))
pdfa_context_structural

# Extract the features that distinguish contexts in DFA
ImpVars_context_structural <- repDFA(as.data.frame(pca_scores_pdfa_context_structural), testfactor = "Context", balancefactor = c("Context", "Caller"), varnames = vars_pdfa_context_structural,
              npercomb = 3, nrand = 1000)
table(ImpVars_context_structural$df1_best) # PC6 and PC1 are best differentiators
sort(abs(loadings(pca_pdfa_context_structural)[,6]), decreasing = T)
sort(abs(loadings(pca_pdfa_context_structural)[,1]), decreasing = T)


# ON SRRUCTURAL FEATURES OF GOMBE

# Data cleaning
structural_numeric_features_context_gombe <- structural_numeric_features_context %>% 
  filter(Community != "Kanyawara")

a<-structural_numeric_features_context_gombe %>% group_by(Context, Caller) %>% count
a %>% ungroup() %>% group_by(Context) %>% 
  summarize(mean = mean(n), median = median(n), range = range(n))

# PCA to reduce number of features
pca_pdfa_context_structural_gombe <- princomp(structural_numeric_features_context_gombe[,1:14], cor = T)
screeplot(pca_pdfa_context_structural_gombe, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_context_structural_gombe <- as_tibble(pca_pdfa_context_structural_gombe$scores)
pca_scores_pdfa_context_structural_gombe <- pca_scores_pdfa_context_structural_gombe %>% 
  add_column(Caller = structural_numeric_features_context_gombe$Caller, Context = structural_numeric_features_context_gombe$Context,
             Community = structural_numeric_features_context_gombe$Community)

# Variable names to be used in the pDFA
vars_pdfa_context_structural_gombe <- names(pca_scores_pdfa_context_structural_gombe[,1:8])

# Perform the pDFA
set.seed(23) # For repeatable results; p=0.291
pdfa_context_structural_gombe=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                     variables=vars_pdfa_context_structural_gombe, n.to.sel=NULL,
                                     n.sel=100, n.perm=1000, 
                                     pdfa.data=as.data.frame(pca_scores_pdfa_context_structural_gombe))

pdfa_context_structural_gombe

# ON CLIMAXES

# Data cleaning
pdfa_data_context_climaxes <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(climax_features), -call_1, -durat2_1, -select_1) %>%
  filter(!is.na(duration_1), !is.na(Context))

pdfa_data_context_climaxes <- pdfa_data_context_climaxes %>% filter(Context != "Display", Context != "Resting")

View(pdfa_data_context_climaxes %>% group_by(Context, Caller) %>% count())

number_by_caller_climaxes <- pdfa_data_context_climaxes %>% group_by(Context, Caller) %>% summarise(calls = n())

# Remove callers with less than 3 calls
pdfa_data_context_climaxes <- pdfa_data_context_climaxes %>% 
  filter(!(Caller %in% number_by_caller_climaxes[number_by_caller_climaxes$calls < 3,]$Caller))

pdfa_data_context_climaxes <- pdfa_data_context_climaxes %>% 
  filter(!Caller %in% c("LON")) 

a<-pdfa_data_context_climaxes %>% group_by(Context, Caller) %>% count
a # 12 individuals with at least 3 calls in both contexts
a %>% ungroup() %>% group_by(Context) %>% 
  summarize(mean = mean(n), median = median(n), range = range(n))

# Feature summary
pdfa_data_context_climaxes %>% #mutate_if(is.numeric, scale) %>%
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_context_climaxes %>% group_by(Context) %>% count

# PCA to reduce number of features
pca_pdfa_context_climaxes <- princomp(pdfa_data_context_climaxes[,4:28], cor = T)
summary(pca_pdfa_context_climaxes)
screeplot(pca_pdfa_context_climaxes, npcs = 20, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_context_climaxes <- as_tibble(pca_pdfa_context_climaxes$scores)
pca_scores_pdfa_context_climaxes <- pca_scores_pdfa_context_climaxes %>% 
  add_column(Caller = pdfa_data_context_climaxes$Caller, Context = pdfa_data_context_climaxes$Context,
             Community = pdfa_data_context_climaxes$Community)

# Variable names to be used in the pDFA
vars_pdfa_context_climaxes <- names(pca_scores_pdfa_context_climaxes[,1:12])

# Perform the pDFA
set.seed(5432) # For repeatable results; p=0.337
pdfa_context_climaxes=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                      variables=vars_pdfa_context_climaxes, n.to.sel=NULL,
                      n.sel=100, n.perm=1000, 
                      pdfa.data=as.data.frame(pca_scores_pdfa_context_climaxes))
pdfa_context_climaxes

# ON CLIMAXES OF GOMBE

# Data cleaning
pdfa_data_context_climaxes_gombe <- pdfa_data_context_climaxes %>% filter(Community != "Kanyawara")

a<-pdfa_data_context_climaxes_gombe %>% group_by(Context, Caller) %>% count 
a # 6 individuals with at least 3 calls in both contexts
a %>% ungroup() %>% group_by(Context) %>% 
  summarize(mean = mean(n), median = median(n), range = range(n))

# PCA to reduce number of features
pca_pdfa_context_climaxes_gombe <- princomp(pdfa_data_context_climaxes_gombe[,4:28], cor = T)
summary(pca_pdfa_context_climaxes_gombe)

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_context_climaxes_gombe <- as_tibble(pca_pdfa_context_climaxes_gombe$scores)
pca_scores_pdfa_context_climaxes_gombe <- pca_scores_pdfa_context_climaxes_gombe %>%
  add_column(Community = pdfa_data_context_climaxes_gombe$Community, Caller = pdfa_data_context_climaxes_gombe$Caller,
             Context = pdfa_data_context_climaxes_gombe$Context)

# Variable names to be used in the pDFA
vars_pdfa_context_climaxes_gombe <- names(pca_scores_pdfa_context_climaxes_gombe[,1:20])

# Perform the pDFA
set.seed(98) # For repeatable results; p=0.394
pdfa_context_climaxes_gombe=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                         variables=vars_pdfa_context_climaxes_gombe, n.to.sel=NULL,
                                         n.sel=100, n.perm=1000, 
                                         pdfa.data=as.data.frame(pca_scores_pdfa_context_climaxes_gombe))
pdfa_context_climaxes_gombe


# ON BUILDUPS

# Data cleaning
pdfa_data_context_buildups <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(buildup_features), -call, -durat2, -select) %>% 
  filter(!is.na(duration), !is.na(Context))

pdfa_data_context_buildups <- pdfa_data_context_buildups %>% filter(Context != "Display", Context != "Resting")

View(pdfa_data_context_buildups %>% group_by(Context, Caller) %>% count())

number_by_caller_buildups <- pdfa_data_context_buildups %>% group_by(Context, Caller) %>% summarise(calls = n())

# Remove callers with less than 3 calls in both contexts
pdfa_data_context_buildups <- pdfa_data_context_buildups %>% 
  filter(!(Caller %in% number_by_caller_buildups[number_by_caller_buildups$calls < 3,]$Caller))

pdfa_data_context_buildups <- pdfa_data_context_buildups %>% 
  filter(!Caller %in% c("LON"))

a<-pdfa_data_context_buildups %>% group_by(Context, Caller) %>% count
a # 9 individuals with at least 3 calls in both contexts
a %>% ungroup() %>% group_by(Context) %>% 
  summarize(mean = mean(n), median = median(n), range = range(n))

# Feature summary
pdfa_data_context_buildups %>% #mutate_if(is.numeric, scale) %>%
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_context_buildups %>% group_by(Context) %>% count

# Variable names to be used in the pDFA
vars_pdfa_context_buildups <- names(pdfa_data_context_buildups[,4:27])

# Perform the pDFA
set.seed(6324) # For repeatable results; p = 0.376
pdfa_context_buildups=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                   variables=vars_pdfa_context_buildups, n.to.sel=NULL,
                                   n.sel=100, n.perm=1000, 
                                   pdfa.data=as.data.frame(pdfa_data_context_buildups))
pdfa_context_buildups

# ON BUILDPS OF GOMBE

# Data cleaning
pdfa_data_context_buildups_gombe <- pdfa_data_context_buildups %>% filter(Community != "Kanyawara")

a<-pdfa_data_context_buildups_gombe %>% group_by(Context, Caller) %>% count
a # 5 individuals with at least 3 calls in both contexts
a %>% ungroup() %>% group_by(Context) %>% 
  summarize(mean = mean(n), median = median(n), range = range(n))


# PCA to reduce number of features
pca_pdfa_context_buildups_gombe <- princomp(pdfa_data_context_buildups_gombe[,4:27], cor = T)
summary(pca_pdfa_context_buildups_gombe)

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_context_buildups_gombe <- as_tibble(pca_pdfa_context_buildups_gombe$scores)
pca_scores_pdfa_context_buildups_gombe <- pca_scores_pdfa_context_buildups_gombe %>%
  add_column(Community = pdfa_data_context_buildups_gombe$Community, Caller = pdfa_data_context_buildups_gombe$Caller,
             Context = pdfa_data_context_buildups_gombe$Context)

# Variable names to be used in the pDFA
vars_pdfa_context_buildups_gombe <- names(pca_scores_pdfa_context_buildups_gombe[,1:20])

# Perform the pDFA
set.seed(312) # For repeatable results; p=0.382
pdfa_context_buildups_gombe=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                   variables=vars_pdfa_context_buildups_gombe, n.to.sel=NULL,
                                   n.sel=100, n.perm=1000, 
                                   pdfa.data=as.data.frame(pca_scores_pdfa_context_buildups_gombe))
pdfa_context_buildups_gombe

# ON COMPLETE CALLS

# Data cleaning
pdfa_data_context_complete <- 
  complete_calls %>% dplyr::select(Community, Caller, Context, all_of(structural_features_numeric), all_of(buildup_features), all_of(climax_features), -call, -durat2, -select, -call_1, -durat2_1, -select_1) %>%
  filter(complete.cases(.))

pdfa_data_context_complete <- pdfa_data_context_complete %>% dplyr::filter(Context != "Display", Context != "Resting")

View(pdfa_data_context_complete %>% group_by(Context, Caller) %>% count())

number_by_caller_complete <- pdfa_data_context_complete %>% group_by(Context, Caller) %>% summarise(calls = n())

# Remove callers with less than 3 calls per context
pdfa_data_context_complete <- pdfa_data_context_complete %>% 
  dplyr::filter(!(Caller %in% number_by_caller_complete[number_by_caller_complete$calls < 3,]$Caller))

pdfa_data_context_complete <- pdfa_data_context_complete %>% 
  filter(!Caller %in% c("LON", "YB"))

a<-pdfa_data_context_complete %>% group_by(Context, Caller) %>% count
a # 6 individuals with at least 3 calls in both contexts
a %>% ungroup() %>% group_by(Context) %>% 
  summarize(mean = mean(n), median = median(n), range = range(n))

# Feature summary
pdfa_data_context_complete %>% #mutate_if(is.numeric, scale) %>%
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_context_complete %>% group_by(Context) %>% count

# PCA to reduce number of features
pca_pdfa_context_complete <- princomp(pdfa_data_context_complete[,4:66], cor = TRUE)
summary(pca_pdfa_context_complete)
screeplot(pca_pdfa_context_complete, npcs = 35, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_context_complete <- as_tibble(pca_pdfa_context_complete$scores) 
pca_scores_pdfa_context_complete <- pca_scores_pdfa_context_complete %>% 
  add_column(Caller = pdfa_data_context_complete$Caller, 
             Context = pdfa_data_context_complete$Context, 
             Community = pdfa_data_context_complete$Community)

# Variable names to be used in the pDFA
vars_pdfa_context_complete <- names(pca_scores_pdfa_context_complete[,1:20])

# Perform the pDFA
set.seed(124) # For repeatable results; p=0.37
pdfa_context_complete=pDFA.crossed(test.fac="Context", contr.fac="Caller",
                                   variables=vars_pdfa_context_complete, n.to.sel=NULL,
                                   n.sel=100, n.perm=1000, 
                                   pdfa.data=as.data.frame(pca_scores_pdfa_context_complete))
pdfa_context_complete

# ON COMPLETE CALLS OF GOMBE-- CAN'T BE DONE DUE TO LOW SAMPLE SIZES


#######------pDFA FOR COMMUNITY-----#######

# ON STRUCTURAL NUMERIC FEATURES

# Data cleaning
structural_numeric_features_community <- as.data.frame(structural_numeric_features)

names(structural_numeric_features_community) <- make.names(names(structural_numeric_features_community))

structural_numeric_features_community <- structural_numeric_features_community %>% 
  filter(Context != "Display", Context != "Resting")

number_by_caller_community_structural <- structural_numeric_features_community %>% group_by(Community, Caller) %>% count

# Remove callers with less than 4 calls
structural_numeric_features_community <- structural_numeric_features_community %>% 
  dplyr::filter(!(Caller %in% number_by_caller_community_structural[number_by_caller_community_structural$n < 4,]$Caller))

a <- structural_numeric_features_community %>% group_by(Community, Caller) %>% count
a %>% ungroup() %>% group_by(Community) %>% summarise(range = range(n), median = median(n))

# PCA to reduce number of features
pca_pdfa_community_structural <- princomp(structural_numeric_features_community[,1:14], cor = T)
screeplot(pca_pdfa_community_structural, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_community_structural <- as_tibble(pca_pdfa_community_structural$scores)
pca_scores_pdfa_community_structural <- pca_scores_pdfa_community_structural %>% 
  add_column(Caller = structural_numeric_features_community$Caller, Context = structural_numeric_features_community$Context,
             Community = structural_numeric_features_community$Community)

# Variable names to be used in the pDFA
vars_pdfa_community_structural <- names(pca_scores_pdfa_community_structural[,1:7])

# Perform pDFA while controlling for context
set.seed(345) # For repeatable results; p=0.322
pdfa_community_structural_ContextControlled=pDFA.crossed(test.fac="Community", contr.fac="Context",
                                 variables=vars_pdfa_community_structural, n.to.sel=NULL,
                                 n.sel=100, n.perm=1000, 
                                 pdfa.data=as.data.frame(pca_scores_pdfa_community_structural))
pdfa_community_structural_ContextControlled

# Perform pDFA while controlling for caller identity
set.seed(67) # For repeatable results; p=0.729
pdfa_community_structural_CallerControlled=pDFA.nested(test.fac="Community", contr.fac="Caller",
                                          variables=vars_pdfa_community_structural, n.to.sel=NULL,
                                          n.sel=100, n.perm=1000, 
                                          pdfa.data=as.data.frame(pca_scores_pdfa_community_structural))
pdfa_community_structural_CallerControlled

# ON STRUCTURAL FEATURES OF GOMBE

# Data cleaning
structural_numeric_features_community_gombe <- structural_numeric_features_community %>% 
  filter(Community != "Kanyawara")

# PCA to reduce number of features
pca_pdfa_community_structural_gombe <- princomp(structural_numeric_features_community_gombe[,1:14], cor = T)
screeplot(pca_pdfa_community_structural_gombe, type = "lines")
summary(pca_pdfa_community_structural_gombe)

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_community_structural_gombe <- as_tibble(pca_pdfa_community_structural_gombe$scores)
pca_scores_pdfa_community_structural_gombe <- pca_scores_pdfa_community_structural_gombe %>% 
  add_column(Caller = structural_numeric_features_community_gombe$Caller, Context = structural_numeric_features_community_gombe$Context,
             Community = structural_numeric_features_community_gombe$Community)

# Variable names to be used in the pDFA
vars_pdfa_community_structural_gombe <- names(pca_scores_pdfa_community_structural_gombe[,1:7])

# Perform pDFA while controlling for context
set.seed(77) # For repeatable results; p=0.412
pdfa_community_structural_gombe_ContextControlled=pDFA.crossed(test.fac="Community", contr.fac="Context",
                                      variables=vars_pdfa_community_structural_gombe, n.to.sel=NULL,
                                      n.sel=100, n.perm=1000, 
                                      pdfa.data=as.data.frame(pca_scores_pdfa_community_structural_gombe))
pdfa_community_structural_gombe_ContextControlled

# Perform pDFA while controlling for caller identity
set.seed(52) # For repeatable results; p=0.639
pdfa_community_structural_gombe_CallerControlled=pDFA.nested(test.fac="Community", contr.fac="Caller",
                                                       variables=vars_pdfa_community_structural_gombe, n.to.sel=NULL,
                                                       n.sel=100, n.perm=1000, 
                                                       pdfa.data=as.data.frame(pca_scores_pdfa_community_structural_gombe))
pdfa_community_structural_gombe_CallerControlled 


# ON CLIMAXES

# Data cleaning
pdfa_data_community_climaxes <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(climax_features), -call_1, -durat2_1, -select_1) %>% 
  filter(!is.na(duration_1), !is.na(Context))

# Feature summary
pdfa_data_community_climaxes %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_community_climaxes %>% group_by(Community) %>% count
a<-pdfa_data_community_climaxes %>% group_by(Community, Caller) %>% count
a # At least 8 calls per caller
a %>% ungroup() %>% group_by(Community) %>% summarise(range = range(n), median = median(n))

# PCA to reduce number of features
pca_pdfa_community_climaxes <- princomp(pdfa_data_community_climaxes[,4:28], cor = T)
screeplot(pca_pdfa_community_climaxes, npcs = 20, type = "lines")
summary(pca_pdfa_community_climaxes)

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_community_climaxes <- as_tibble(pca_pdfa_community_climaxes$scores)
pca_scores_pdfa_community_climaxes <- pca_scores_pdfa_community_climaxes %>% 
  add_column(Caller = pdfa_data_community_climaxes$Caller, Community = pdfa_data_community_climaxes$Community)

# Variable names to be used in the pDFA
vars_pdfa_community_climaxes <- names(pca_scores_pdfa_community_climaxes[,1:13])

# Perform the pDFA
set.seed(99) # For repeatable results; p=0.017
pdfa_community_climaxes <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                        variables=vars_pdfa_community_climaxes, 
                        restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                        n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                        pdfa.data=as.data.frame(pca_scores_pdfa_community_climaxes))
pdfa_community_climaxes


# Extract the features that distinguish communities in DFA
source("repDFA_nested.r")
ImpVars_community_climaxes <- repDFA_nested(as.data.frame(pca_scores_pdfa_community_climaxes), testfactor = "Community", balancefactor = c("Community", "Caller"), varnames = vars_pdfa_community_climaxes,
                                     npercomb = 8, nrand = 1000)
table(ImpVars_community_climaxes$df1_best)
table(ImpVars_community_climaxes$df2_best)
sort(abs(loadings(pca_pdfa_community_climaxes)[,4]), decreasing = T)
sort(abs(loadings(pca_pdfa_community_climaxes)[,2]), decreasing = T)
ImpVars_community_climaxes

# Tests for significance of best features 

# Check distributions
hist(climax_numeric_features$trfak_1)
hist(climax_numeric_features$duration_1)
hist(climax_numeric_features$F0loc_1)
hist(log(climax_numeric_features$Pfmaxdif_1))
hist(climax_numeric_features$noise_mean_1)
hist(climax_numeric_features$noise_max_1)

library(lmerTest)

Clitrfak <- lmer(trfak_1 ~ Community + (1|Individual), data = climax_numeric_features)
summary(Clitrfak)

Clidur <- lmer(duration_1 ~ Community + (1|Individual), data = climax_numeric_features)
summary(Clidur)

CliF0loc <- lmer(F0loc_1 ~ Community + (1|Individual), data = climax_numeric_features)
summary(CliF0loc)

Clinoisemean <- lmer(noise_mean_1 ~ Community + (1|Individual), data = climax_numeric_features)
summary(Clinoisemean)

Clinoisemax <- lmer(noise_max_1 ~ Community + (1|Individual), data = climax_numeric_features)
summary(Clinoisemax)

CliPfmaxdif <- lmer(Pfmaxdif_1 ~ Community + (1|Individual), data = climax_numeric_features)
summary(CliPfmaxdif)

# Correct p-values for multiple comparisons
p<- c(0.8347,0.0392,0.612,0.179,0.5726,0.0864,0.00443,5.85e-07,3.55e-06,2.67e-08,0.000216,0.662041)
p.adjust(p, "BH")


# ON CLIMAXES OF GOMBE

# Data cleaning
pdfa_data_community_climaxes_gombe <- pdfa_data_community_climaxes %>% 
  filter(Community != "Kanyawara")

a<-pdfa_data_community_climaxes_gombe %>% group_by(Community, Caller) %>% count()
a %>% ungroup() %>% group_by(Community) %>% summarise(range = range(n), median = median(n))

# PCA to reduce number of features
pca_pdfa_community_climaxes_gombe <- princomp(pdfa_data_community_climaxes_gombe[,4:28], cor = T)
screeplot(pca_pdfa_community_climaxes_gombe, npcs = 20, type = "lines")
summary(pca_pdfa_community_climaxes_gombe)

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_community_climaxes_gombe <- as_tibble(pca_pdfa_community_climaxes_gombe$scores)
pca_scores_pdfa_community_climaxes_gombe <- pca_scores_pdfa_community_climaxes_gombe %>% 
  add_column(Caller = pdfa_data_community_climaxes_gombe$Caller, Community = pdfa_data_community_climaxes_gombe$Community)

# Variable names to be used in the pDFA
vars_pdfa_community_climaxes_gombe <- names(pca_scores_pdfa_community_climaxes_gombe[,1:12])

# Perform the pDFA
set.seed(21) # For repeatable results; p=0.089
pdfa_community_climaxes_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                              variables=vars_pdfa_community_climaxes_gombe, 
                              restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                              n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                              pdfa.data=as.data.frame(pca_scores_pdfa_community_climaxes_gombe))
pdfa_community_climaxes_gombe

# ON BUILDUPS

# Data cleaning
pdfa_data_community_buildups <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(buildup_features), -call, -durat2, -select) %>%
  filter(!is.na(duration), !is.na(Context))

a<-pdfa_data_community_buildups %>% group_by(Community, Caller) %>% count
a %>% ungroup() %>% group_by(Community) %>% summarise(range = range(n), median = median(n))

pdfa_data_community_buildups <- pdfa_data_community_buildups %>% filter(Caller != "FAN")

# Feature summary
pdfa_data_community_buildups %>% 
  select_if(is.numeric) %>%
  skimr::skim()

# Variable names to be used in the pDFA
vars_pdfa_community_buildups <- names(pdfa_data_community_buildups[,4:27])

# Perform the pDFA
set.seed(95) # For repeatable results; p=0.08
pdfa_community_buildups <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                              variables=vars_pdfa_community_buildups, 
                              restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                              n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                              pdfa.data=as.data.frame(pdfa_data_community_buildups))
pdfa_community_buildups

# ON BUILDUPS OF GOMBE

# Data cleaning
pdfa_data_community_buildups_gombe <- pdfa_data_community_buildups %>% 
  dplyr::filter(Community != "Kanyawara")

a<-pdfa_data_community_buildups_gombe %>% group_by(Community, Caller) %>% count
a %>% ungroup() %>% group_by(Community) %>% summarise(range = range(n), median = median(n))

# Perform the pDFA
set.seed(14) # For repeatable results; p=0.255
pdfa_community_buildups_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                    variables=vars_pdfa_community_buildups, 
                                    restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                    n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                    pdfa.data=as.data.frame(pdfa_data_community_buildups_gombe))
pdfa_community_buildups_gombe

# ON COMPLETE CALLS

# Data cleaning
pdfa_data_community_complete <- 
  complete_calls %>% dplyr::select(Community, Caller, Context, all_of(structural_features_numeric), all_of(buildup_features), all_of(climax_features), -call, -durat2, -select, -call_1, -durat2_1, -select_1) %>%
  dplyr::filter(complete.cases(.))

pdfa_data_community_complete %>% group_by(Community) %>% count

pdfa_data_community_complete <- pdfa_data_community_complete %>% dplyr::filter(!Caller %in% c("ZS"))

a<-pdfa_data_community_complete %>% group_by(Community, Caller) %>% count
a %>% ungroup() %>% group_by(Community) %>% summarise(range = range(n), median = median(n))

# Feature summary
pdfa_data_community_complete %>% 
  select_if(is.numeric) %>%
  skimr::skim()

# PCA to reduce number of features
pca_pdfa_community_complete <- princomp(pdfa_data_community_complete[,4:66], cor = T)
summary(pca_pdfa_community_complete)
screeplot(pca_pdfa_community_complete, npcs = 40, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_community_complete <- as_tibble(pca_pdfa_community_complete$scores)
pca_scores_pdfa_community_complete <- pca_scores_pdfa_community_complete %>% 
  add_column(Community = pdfa_data_community_complete$Community, Caller = pdfa_data_community_complete$Caller,
             Context = pdfa_data_community_complete$Context)

# Variable names to be used in the pDFA
vars_pdfa_community_complete <- names(pca_scores_pdfa_community_complete[,1:25])

# Perform the pDFA
set.seed(8655) # For repeatable results; p=0.079 
pdfa_community_complete <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                       variables=vars_pdfa_community_complete, 
                                       restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                       n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                       pdfa.data=as.data.frame(pca_scores_pdfa_community_complete))
pdfa_community_complete

# ON COMPLETE CALLS OF GOMBE

# Data cleaning
pdfa_data_community_complete_gombe <- pdfa_data_community_complete %>% 
  filter(Community != "Kanyawara")

pdfa_data_community_complete_gombe %>% group_by(Community, Caller) %>% count()

pdfa_data_community_complete_gombe <- pdfa_data_community_complete_gombe %>% filter(Caller != "ZS")

# PCA to reduce number of features
pca_pdfa_community_complete_gombe <- princomp(pdfa_data_community_complete_gombe[,4:66], cor = T)
summary(pca_pdfa_community_complete_gombe)

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_community_complete_gombe <- as_tibble(pca_pdfa_community_complete_gombe$scores)
pca_scores_pdfa_community_complete_gombe <- pca_scores_pdfa_community_complete_gombe %>% 
  add_column(Community = pdfa_data_community_complete_gombe$Community, Caller = pdfa_data_community_complete_gombe$Caller,
             Context = pdfa_data_community_complete_gombe$Context)

# Variable names to be used in the pDFA
vars_pdfa_community_complete_gombe <- names(pca_scores_pdfa_community_complete_gombe[,1:24])

# Perform the pDFA
set.seed(324) # For repeatable results; p=0.272
pdfa_community_complete_gombe <- pDFA.nested(test.fac="Community", contr.fac = "Caller",
                                             variables=vars_pdfa_community_complete_gombe, 
                                             restrict.by=NULL, n.contr.fac.levels.to.sel=NULL, 
                                             n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                             pdfa.data=as.data.frame(pca_scores_pdfa_community_complete_gombe))
pdfa_community_complete_gombe



#######------pDFA FOR INDIVIDUAL----######

# ON STRUCTURAL NUMERIC FEATURES

# Data cleaning
structural_numeric_features_individual <- as.data.frame(structural_numeric_features)

names(structural_numeric_features_individual) <- make.names(names(structural_numeric_features_individual))

structural_numeric_features_individual <- structural_numeric_features_individual %>% 
  dplyr::filter(Context != "Display", Context != "Resting")

structural_numeric_features_individual <- structural_numeric_features_individual %>% 
  filter(!Caller %in% c("ZS")) 

number_by_caller_individual_structural <- structural_numeric_features_individual %>% group_by(Context, Caller) %>% count

structural_numeric_features_individual <- structural_numeric_features_individual %>% 
  dplyr::filter(!(Caller %in% number_by_caller_individual_structural[number_by_caller_individual_structural$n < 4,]$Caller))

b<-structural_numeric_features_individual %>% group_by(Caller) %>% count
median(b$n)
range(b$n)

# PCA to reduce number of features
pca_pdfa_individual_structural <- princomp(structural_numeric_features_individual[,1:14], cor = T)
screeplot(pca_pdfa_individual_structural, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_structural <- as_tibble(pca_pdfa_individual_structural$scores)
pca_scores_pdfa_individual_structural <- pca_scores_pdfa_individual_structural %>% 
  add_column(Caller = structural_numeric_features_individual$Caller, Context = structural_numeric_features_individual$Context,
             Community = structural_numeric_features_individual$Community)

# Variable names to be used in the pDFA
vars_pdfa_individual_structural <- names(pca_scores_pdfa_individual_structural[,1:9])

# Perform the pDFA while controlling for context
set.seed(63) # For repeatable results; p=0.043
pdfa_individual_structural_ContextControlled=pDFA.crossed(test.fac="Caller", contr.fac = "Context",
                                      variables=vars_pdfa_individual_structural, n.to.sel=NULL,
                                      n.sel=100, n.perm=1000, 
                                      pdfa.data=as.data.frame(pca_scores_pdfa_individual_structural))
pdfa_individual_structural_ContextControlled

# pDFA with no control and community identity as restriction factor

# Data cleaning
structural_numeric_features_individual <- as.data.frame(structural_numeric_features)

names(structural_numeric_features_individual) <- make.names(names(structural_numeric_features_individual))

b<-structural_numeric_features_individual %>% group_by(Caller) %>% count
median(b$n)
range(b$n)

# PCA to reduce number of features
pca_pdfa_individual_structural <- princomp(structural_numeric_features_individual[,1:14], cor = T)
screeplot(pca_pdfa_individual_structural, type = "lines")
pca_pdfa_individual_structural$loadings

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_structural <- as_tibble(pca_pdfa_individual_structural$scores)
pca_scores_pdfa_individual_structural <- pca_scores_pdfa_individual_structural %>% 
  add_column(Caller = structural_numeric_features_individual$Caller, Context = structural_numeric_features_individual$Context,
             Community = structural_numeric_features_individual$Community)

# Variable names to be used in the pDFA
vars_pdfa_individual_structural <- names(pca_scores_pdfa_individual_structural[,1:9])

# Perform the pDFA
set.seed(89) # For repeatable results; p=0.001
pdfa_individual_structural_NoControl=pDFA.nested(test.fac="Caller", 
                                       restrict.by = "Community",
                                        variables=vars_pdfa_individual_structural, n.to.sel=NULL,
                                        n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_structural))
pdfa_individual_structural_NoControl

# Extract the structural features that distinguish individuals in DFA
ImpVars_individual_structural <- repDFA_nested(as.data.frame(pca_scores_pdfa_individual_structural), testfactor = "Caller", balancefactor = c("Caller"), varnames = vars_pdfa_individual_structural,
                                       npercomb = 4, nrand = 1000)
table(ImpVars_individual_structural$df1_best)
table(ImpVars_individual_structural$df2_best)
sort(abs(loadings(pca_pdfa_individual_structural)[,2]), decreasing = T)
sort(abs(loadings(pca_pdfa_individual_structural)[,7]), decreasing = T)


# ON STRUCTURAL NUMERIC FEATURES OF GOMBE

# Data cleaning
structural_numeric_features_individual_gombe <- as.data.frame(structural_numeric_features)

names(structural_numeric_features_individual_gombe) <- make.names(names(structural_numeric_features_individual_gombe))

structural_numeric_features_individual_gombe <- structural_numeric_features_individual_gombe %>% 
  filter(Community != "Kanyawara") 

number_by_caller_individual_structural_gombe <- structural_numeric_features_individual_gombe %>% group_by(Caller) %>% count

b<-structural_numeric_features_individual_gombe %>% group_by(Caller) %>% count
median(b$n)
range(b$n)

# PCA to reduce number of features
pca_pdfa_individual_structural_gombe <- princomp(structural_numeric_features_individual_gombe[,1:14], cor = T)
screeplot(pca_pdfa_individual_structural_gombe, type = "lines")
pca_pdfa_individual_structural_gombe$loadings

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_structural_gombe <- as_tibble(pca_pdfa_individual_structural_gombe$scores)
pca_scores_pdfa_individual_structural_gombe <- pca_scores_pdfa_individual_structural_gombe %>% 
  add_column(Caller = structural_numeric_features_individual_gombe$Caller, Context = structural_numeric_features_individual_gombe$Context,
             Community = structural_numeric_features_individual_gombe$Community)

# Variable names to be used in the pDFA
vars_pdfa_individual_structural_gombe <- names(pca_scores_pdfa_individual_structural_gombe[,1:7])

# Perform the pDFA
set.seed(71) # For repeatable results; p=0.001
pdfa_individual_structural_gombe=pDFA.nested(test.fac="Caller", 
                                        restrict.by = "Community",
                                        variables=vars_pdfa_individual_structural_gombe, n.to.sel=NULL,
                                        n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_structural_gombe))

pdfa_individual_structural_gombe #Context controlled is not possible due to low sample sizes


# ON CLIMAXES

# Data cleaning
pdfa_data_individual_climaxes <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(climax_features), -call_1, -durat2_1, -select_1) %>%
  dplyr::filter(!is.na(duration_1), !is.na(Context))

# Feature summary
pdfa_data_individual_climaxes %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_individual_climaxes %>% group_by(Community, Caller) %>% count

# PCA to reduce number of features
pca_pdfa_individual_climaxes <- princomp(pdfa_data_individual_climaxes[,4:28], cor = T)
summary(pca_pdfa_individual_climaxes)
screeplot(pca_pdfa_individual_climaxes, npcs = 10,type = "lines")
pca_pdfa_individual_climaxes$loadings

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_climaxes <- as_tibble(pca_pdfa_individual_climaxes$scores)
pca_scores_pdfa_individual_climaxes <- pca_scores_pdfa_individual_climaxes %>% 
  add_column(Community = pdfa_data_individual_climaxes$Community, Caller = pdfa_data_individual_climaxes$Caller,
             Context = pdfa_data_individual_climaxes$Context)

# Variable names to be used in the pDFA
vars_pdfa_individual_climaxes <- names(pca_scores_pdfa_individual_climaxes[,1:8])

# Perform the pDFA
set.seed(22) # For repeatable results; p=0.001
pdfa_individual_climaxes <- pDFA.nested(test.fac="Caller",
                                        variables=vars_pdfa_individual_climaxes, 
                                        restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_climaxes))
pdfa_individual_climaxes

# Extract the climax features that distinguish individuals in DFA
ImpVars_individual_climaxes <- repDFA_nested(as.data.frame(pca_scores_pdfa_individual_climaxes), testfactor = "Caller", balancefactor = c("Caller"), varnames = vars_pdfa_individual_climaxes,
                                                npercomb = 8, nrand = 1000)
table(ImpVars_individual_climaxes$df1_best)
table(ImpVars_individual_climaxes$df2_best)
sort(abs(loadings(pca_pdfa_individual_climaxes)[,1]), decreasing = T)
sort(abs(loadings(pca_pdfa_individual_climaxes)[,3]), decreasing = T)


# ON CLIMAXES OF GOMBE

# Data cleaning
pdfa_data_individual_climaxes_gombe <- pdfa_data_individual_climaxes %>% 
  filter(Community != "Kanyawara")

pdfa_data_individual_climaxes_gombe %>% group_by(Community, Caller) %>% count()

# PCA to reduce number of features
pca_pdfa_individual_climaxes_gombe <- princomp(pdfa_data_individual_climaxes_gombe[,4:28], cor = T)
summary(pca_pdfa_individual_climaxes_gombe)

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_climaxes_gombe <- as_tibble(pca_pdfa_individual_climaxes_gombe$scores)
pca_scores_pdfa_individual_climaxes_gombe <- pca_scores_pdfa_individual_climaxes_gombe %>% 
  add_column(Community = pdfa_data_individual_climaxes_gombe$Community, Caller = pdfa_data_individual_climaxes_gombe$Caller,
             Context = pdfa_data_individual_climaxes_gombe$Context)

# Variable names to be used in the pDFA
vars_pdfa_individual_climaxes_gombe <- names(pca_scores_pdfa_individual_climaxes_gombe[,1:8])

set.seed(501) # For repeatable results; p=0.005
pdfa_individual_climaxes_gombe <- pDFA.nested(test.fac="Caller",
                                        variables=vars_pdfa_individual_climaxes_gombe, 
                                        restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_climaxes_gombe))
pdfa_individual_climaxes_gombe

# ON BUILDUPS

# Data cleaning
pdfa_data_individual_buildups <- 
  dialects %>% dplyr::select(Community, Caller, Context, all_of(buildup_features), -call, -durat2, -select) %>%
  dplyr::filter(!is.na(duration), !is.na(Context))

# Feature summary
pdfa_data_individual_buildups %>% 
  select_if(is.numeric) %>%
  skimr::skim()

pdfa_data_individual_buildups %>% group_by(Community, Caller) %>% count

pdfa_data_individual_buildups <- pdfa_data_individual_buildups %>% 
  dplyr::filter(Caller != "FAN")

# PCA to reduce number of features
pca_pdfa_individual_buildups <- princomp(pdfa_data_individual_buildups[,4:27], cor = T)
summary(pca_pdfa_individual_buildups)
screeplot(pca_pdfa_individual_buildups, npcs = 10,type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_buildups <- as_tibble(pca_pdfa_individual_buildups$scores)
pca_scores_pdfa_individual_buildups <- pca_scores_pdfa_individual_buildups %>% 
  add_column(Community = pdfa_data_individual_buildups$Community, Caller = pdfa_data_individual_buildups$Caller,
             Context = pdfa_data_individual_buildups$Context)

# Variable names to be used in the pDFA
vars_pdfa_individual_buildups <- names(pca_scores_pdfa_individual_buildups[,1:5])

# Perform the pDFA
set.seed(873) # For repeatable results; p=0.17
pdfa_individual_buildups <- pDFA.nested(test.fac="Caller",
                                        variables=vars_pdfa_individual_buildups, 
                                        restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                        pdfa.data=as.data.frame(pca_scores_pdfa_individual_buildups))
pdfa_individual_buildups

# ON BUILDUPS OF GOMBE

# Data cleaning
pdfa_data_individual_buildups_gombe <- pdfa_data_individual_buildups %>% 
  filter(Community != "Kanyawara")

pdfa_data_individual_buildups_gombe %>% group_by(Community, Caller) %>% count()

# PCA to reduce number of features
pca_pdfa_individual_buildups_gombe <- princomp(pdfa_data_individual_buildups_gombe[,4:27], cor = T)
summary(pca_pdfa_individual_buildups_gombe)
screeplot(pca_pdfa_individual_buildups_gombe, npcs = 10,type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_buildups_gombe <- as_tibble(pca_pdfa_individual_buildups_gombe$scores)
pca_scores_pdfa_individual_buildups_gombe <- pca_scores_pdfa_individual_buildups_gombe %>% 
  add_column(Community = pdfa_data_individual_buildups_gombe$Community, Caller = pdfa_data_individual_buildups_gombe$Caller,
             Context = pdfa_data_individual_buildups_gombe$Context)

# Variable names to be used in the pDFA
vars_pdfa_individual_buildups_gombe <- names(pca_scores_pdfa_individual_buildups_gombe[,1:5])

# Perform the pDFA
set.seed(235) # For repeatable results; p=0.13
pdfa_individual_buildups_gombe <- pDFA.nested(test.fac="Caller",
                                              variables=vars_pdfa_individual_buildups_gombe, 
                                              restrict.by="Community", n.contr.fac.levels.to.sel=NULL, n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                              pdfa.data=as.data.frame(pca_scores_pdfa_individual_buildups_gombe))
pdfa_individual_buildups_gombe


# ON COMPLETE CALLS

# Data cleaning
pdfa_data_individual_complete <- 
  complete_calls %>% dplyr::select(Community, Caller, Context, all_of(structural_features_numeric), all_of(buildup_features), all_of(climax_features), -call, -durat2, -select, -call_1, -durat2_1, -select_1) %>%
  dplyr::filter(complete.cases(.))

pdfa_data_individual_complete %>% group_by(Community, Caller) %>% count

pdfa_data_individual_complete <- pdfa_data_individual_complete %>% filter(!Caller %in% c("ZS"))

pdfa_data_individual_complete %>% group_by(Community, Caller) %>% count

# Feature summary
pdfa_data_individual_complete %>% 
  select_if(is.numeric) %>%
  skimr::skim()

# PCA to reduce number of features
pca_pdfa_individual_complete <- princomp(pdfa_data_individual_complete[,4:66], cor = T)
summary(pca_pdfa_individual_complete)
screeplot(pca_pdfa_individual_complete, type = "lines")
pca_pdfa_individual_complete$loadings

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_complete <- as_tibble(pca_pdfa_individual_complete$scores)
pca_scores_pdfa_individual_complete <- pca_scores_pdfa_individual_complete %>% 
  add_column(Community = pdfa_data_individual_complete$Community, Caller = pdfa_data_individual_complete$Caller,
             Context = pdfa_data_individual_complete$Context)

# Variable names to be used in the pDFA
vars_pdfa_individual_complete <- names(pca_scores_pdfa_individual_complete[,1:3])

# Perform the pDFA
set.seed(922) # For repeatable results; p=0.007
pdfa_individual_complete <- pDFA.nested(test.fac="Caller",
                                       variables=vars_pdfa_individual_complete, 
                                       restrict.by="Community", n.contr.fac.levels.to.sel=NULL, 
                                       n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                       pdfa.data=as.data.frame(pca_scores_pdfa_individual_complete))
pdfa_individual_complete


# Extract the features that distinguish individuals in DFA
ImpVars_individual_complete <- repDFA_nested(as.data.frame(pca_scores_pdfa_individual_complete), testfactor = "Caller", balancefactor = c("Caller"), varnames = vars_pdfa_individual_complete,
                                             npercomb = 3, nrand = 1000)
table(ImpVars_individual_complete$df1_best)
sort(abs(loadings(pca_pdfa_individual_complete)[,2]), decreasing = T)


# ON COMPLETE CALLS OF GOMBE

# Data cleaning
pdfa_data_individual_complete_gombe <- pdfa_data_individual_complete %>% 
  filter(Community != "Kanyawara")

pdfa_data_individual_complete_gombe %>% group_by(Community, Caller) %>% count()

# PCA to reduce number of features
pca_pdfa_individual_complete_gombe <- princomp(pdfa_data_individual_complete_gombe[,4:66], cor = T)
summary(pca_pdfa_individual_complete_gombe)
screeplot(pca_pdfa_individual_complete_gombe, type = "lines")

# Extract PC scores to use as features in pDFA
pca_scores_pdfa_individual_complete_gombe <- as_tibble(pca_pdfa_individual_complete_gombe$scores)
pca_scores_pdfa_individual_complete_gombe <- pca_scores_pdfa_individual_complete_gombe %>% 
  add_column(Community = pdfa_data_individual_complete_gombe$Community, Caller = pdfa_data_individual_complete_gombe$Caller,
             Context = pdfa_data_individual_complete_gombe$Context)

# Variable names to be used in the pDFA
vars_pdfa_individual_complete_gombe <- names(pca_scores_pdfa_individual_complete_gombe[,1:3])

# Perform the pDFA
set.seed(9831) # For repeatable results; p=0.023
pdfa_individual_complete_gombe <- pDFA.nested(test.fac="Caller",
                                             variables=vars_pdfa_individual_complete_gombe, 
                                             restrict.by="Community", n.contr.fac.levels.to.sel=NULL, 
                                             n.to.sel.per.contr.fac.level=NULL, n.sel=100, n.perm=1000, 
                                             pdfa.data=as.data.frame(pca_scores_pdfa_individual_complete_gombe))
pdfa_individual_complete_gombe

