#R-code for Media Lengua written Vowel Variation
#Published in Written Language & Literacy - https://doi.org/10.1075/wll
#By Jesse Stewart, PhD
#2025-12-15

#libraries
library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggrepel)
library(scales)

#load dataset
dataset <- read_excel("WVV.xlsx")

DS1=dataset
View(DS1)

#remove blank rows/na's
DS1 <- DS1[!is.na(DS1$SD), ]
DS1 <- DS1[!is.na(DS1$Standard_V), ]
DS1 <- DS1[!is.na(DS1$Position), ]
DS1 <- DS1[!is.na(DS1$ML_V), ]
DS1 <- DS1[!is.na(DS1$Annotator), ]
DS1 <- DS1[!is.na(DS1$Origin), ]

DS1$Freq <- ifelse(DS1$SD=="Same",1,0)

#Binary columns for the Models (medial switch i.e., root-final)
DS1$Mid_S=ifelse(DS1$Position=="medial_S","TRUE","FALSE")

######
#Subsets for Modelling
######
#Monopthongs
Mono_i_Sp=subset(DS1, Standard_V=="i")
Mono_e_Sp=subset(DS1, Standard_V=="e")
Mono_u_Sp=subset(DS1, Standard_V=="u")
Mono_o_Sp=subset(DS1, Standard_V=="o")

#Diphthongs
Dipth_ie_Sp=subset(DS1, Standard_V=="ie")
Dipth_ae_Sp=subset(DS1, Standard_V=="ae")
Dipth_ea_Sp=subset(DS1, Standard_V=="ea")
Dipth_io_Sp=subset(DS1, Standard_V=="io")
Dipth_ia_Sp=subset(DS1, Standard_V=="ia")
Dipth_au_Sp=subset(DS1, Standard_V=="au")

############
#GLMM Models
############
#Monophthongs
#/i/
#Backwards Elimination for /i/
Model_i=glmer(Freq~Origin+Mid_S+Annotator+(1|Word),family=binomial,data=Mono_i_Sp)
summary(Model_i)
#Closest non-significant z-value to zero is Origin is 'Mid_S'
Model_i=glmer(Freq~Mid_S+Annotator+(1|Word),family=binomial,data=Mono_i_Sp)
summary(Model_i)
#Closest non-signficant z-value to zero is 'Annotator'
#Final /i/ model (only significant predictors remaining)
Model_i=glmer(Freq~Mid_S+(1|Word),family=binomial,data=Mono_i_Sp)
summary(Model_i)
#Likelihood-ratio ANOVA testing for /i/
Model_i_NW=glm(Freq~Mid_S,family=binomial,data=Mono_i_Sp)
anova(Model_i_NW, Model_i, test = "Chisq")
#Random effects dump
ranef(Model_i)

#/e/
#Backwards Elimination for /e/
#'Origin' for /e/ only has one level as it is only found in Spanish
#Both 'Mid_S' and 'Annotator' are significant. No further reduction is warranted 
Model_e=glmer(Freq~Annotator+Mid_S+(1|Word),family=binomial,data=Mono_e_Sp)
summary(Model_e)
#Likelihood-ratio ANOVA testing for /e/
Model_e_NW=glm(Freq~Annotator+Mid_S,family=binomial,data=Mono_e_Sp)
anova(Model_e_NW, Model_e, test = "Chisq")
#Random effects dump
ranef(Model_e)

#/u/
#Backwards Elimination for /u/
Model_u=glmer(Freq~Mid_S+Annotator+Origin+(1|Word),family=binomial,data=Mono_u_Sp)
summary(Model_u)
#Closest non-significant z-value to zero is Origin is 'Mid_S'
#Final /u/ model (only significant predictors remaining)
Model_u=glmer(Freq~Annotator+Origin+(1|Word),family=binomial,data=Mono_u_Sp)
summary(Model_u)
#Likelihood-ratio ANOVA testing for /e/
Model_u_NW=glm(Freq~Annotator+Mid_S,family=binomial,data=Mono_u_Sp)
anova(Model_u_NW, Model_u, test = "Chisq")
#Random effects dump
ranef(Model_u)

#/o/
#Backwards Elimination for /o/
#'Origin' for /o/ only has one level as it is only found in Spanish
Model_o=glmer(Freq~Mid_S+Annotator+(1|Word),family=binomial,data=Mono_o_Sp)
summary(Model_o)
#Closest non-significant z-value to zero is Origin is 'Annotator'
#Final /o/ model (only significant predictors remaining)
Model_o=glmer(Freq~Mid_S+(1|Word),family=binomial,data=Mono_o_Sp)
summary(Model_o)
#Likelihood-ratio ANOVA testing for /o/
Model_o_NW=glm(Freq~Mid_S,family=binomial,data=Mono_o_Sp)
anova(Model_o_NW, Model_o, test = "Chisq")
#Random effects dump
ranef(Model_o)

#Vowel Sequencies
#/ie/
#Backwards Elimination for /ie/
#'Origin' for /ie/ has only one level as it is only found in Spanish, and /ie/ does not exist in 'root-final' position in the dataset
Model_ie=glmer(Freq~Annotator+(1|Word),family=binomial,data=Dipth_ie_Sp)
summary(Model_ie)
#Closest non-significant z-value to zero is Origin is 'Annotator'
#Final /ie/ model (No predictors remaining)
Model_ie=glmer(Freq~(1|Word),family=binomial,data=Dipth_ie_Sp)
summary(Model_ie)
#Likelihood-ratio ANOVA testing for /ie/ can't be run as there are no fixed effects
#Random effects dump
ranef(Model_ie)

#/ia/
#Backwards Elimination for /ie/
#'Origin' for /ia/ has only one level as it is only found in Spanish, and /ia/ does not exist in 'root-final' position in the dataset
Model_ia=glmer(Freq~Annotator+(1|Word),family=binomial,data=Dipth_ia_Sp)
summary(Model_ia)
#Closest non-significant z-value to zero is Origin is 'Annotator'
#Final /ia/ model (No predictors remaining)
Model_ia=glmer(Freq~(1|Word),family=binomial,data=Dipth_ia_Sp)
summary(Model_ia)
#Likelihood-ratio ANOVA testing for /ia/ can't be run as there are no fixed effects
#Random effects dump
ranef(Model_ia)

#/ea/
#Backwards Elimination for /ie/
#Origin for /ea/ has only one level as it is only found in Spanish
Model_ea=glmer(Freq~Annotator+Mid_S+(1|Word),family=binomial,data=Dipth_ea_Sp)
summary(Model_ea)
#Closest non-significant z-value to zero is Origin is 'Mid_S'
Model_ea=glmer(Freq~Annotator+(1|Word),family=binomial,data=Dipth_ea_Sp)
summary(Model_ea)
#Closest non-significant z-value to zero is Origin is 'Annotator'
#Final /ea/ model (No predictors remaining)
Model_ea=glmer(Freq~(1|Word),family=binomial,data=Dipth_ea_Sp)
summary(Model_ea)
#Likelihood-ratio ANOVA testing for /ia/ can't be run as there are no fixed effects
#Random effects dump
ranef(Model_ea)

############
#Graphing
############
#Datasets
gi <- read_excel("i.xlsx")
ge <- read_excel("e.xlsx")
gu <- read_excel("u.xlsx")
go <- read_excel("o.xlsx")
gvs <- read_excel("vs.xlsx")

#/i/
ggplot(gi, aes(x = Vowel_RF, y = Vowel_RF)) +
    
    ## vertical reference lines at 10%, 20%, 80%, 90%
    geom_vline(
        xintercept = c(0.1, 0.2, 0.8, 0.9),
        linetype = "dashed",
        colour = "blue",
        linewidth = 0.8
    ) +
    
    ## lighter, larger points
    geom_point(
        aes(size = Count, colour = Origin),
        alpha = 0.45
    ) +
    
    ## black labels, repelled and enlarged
    geom_text_repel(
        aes(label = Trans),
        colour = "black",
        size = 4.2,
        box.padding = 0.1,
        point.padding = 0.3,
        max.overlaps = Inf
    ) +
    
     #Colour palette
     scale_colour_manual(
         values = c(
             "Spanish" = "#D896FF",
             "Kichwa"  = "#FFD6A5")
     )+
    
    ## percent axes with 0–100% by 10s
    scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    
    ## larger bubble scale
    scale_size_continuous(range = c(4, 16)) +
    
    guides(
  colour = guide_legend(
    override.aes = list(size = 6, alpha = 1)
  	)
	) +
    
    ## clean minimal theme with larger text
    theme_minimal(base_size = 16) +
    theme(
        axis.title   = element_text(size = 18),
        axis.text    = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
    ) +
    
    ## labels
    labs(
        x = "Probability of <i> Realization",
        y = "Probability of <i> Realization",
        colour = "Lexical origin",
        size   = "Token count"
    )

#/e/
ggplot(ge, aes(x = Vowel_RF, y = Vowel_RF)) +
    
    ## vertical reference lines at 10%, 20%, 80%, 90%
    geom_vline(
        xintercept = c(0.1, 0.2, 0.8, 0.9),
        linetype = "dashed",
        colour = "blue",
        linewidth = 0.8
    ) +
    
    ## lighter, larger points
    geom_point(
        aes(size = Count, colour = Origin),
        alpha = 0.45
    ) +
    
    ## black labels, repelled and enlarged
    geom_text_repel(
        aes(label = Trans),
        colour = "black",
        size = 4.2,
        box.padding = 0.1,
        point.padding = 0.3,
        max.overlaps = Inf
    ) +
    
    #Colour palette
     scale_colour_manual(
         values = c(
             "Spanish" = "#D896FF")
     )+
    
    ## percent axes with 0–100% by 10s
    scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    
    ## larger bubble scale
    scale_size_continuous(range = c(4, 16)) +
    
    guides(
  colour = guide_legend(
    override.aes = list(size = 6, alpha = 1)
  	)
	) +
    
    ## clean minimal theme with larger text
    theme_minimal(base_size = 16) +
    theme(
        axis.title   = element_text(size = 18),
        axis.text    = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
    ) +
    
    ## labels
    labs(
        x = "Probability of <e> Realization",
        y = "Probability of <e> Realization",
        colour = "Lexical origin",
        size   = "Token count"
    )

#/u/
ggplot(gu, aes(x = Vowel, y = Vowel)) +
    
    ## vertical reference lines at 10%, 20%, 80%, 90%
    geom_vline(
        xintercept = c(0.1, 0.2, 0.8, 0.9),
        linetype = "dashed",
        colour = "blue",
        linewidth = 0.8
    ) +
    
    ## lighter, larger points
    geom_point(
        aes(size = Count, colour = Origin),
        alpha = 0.45
    ) +
    
    ## black labels, repelled and enlarged
    geom_text_repel(
        aes(label = Trans),
        colour = "black",
        size = 4.2,
        box.padding = 0.01,
        point.padding = 0.3,
        max.overlaps = Inf
    ) +

     #Colour palette
     scale_colour_manual(
         values = c(
             "Spanish" = "#D896FF",
             "Kichwa"  = "#FFD6A5")
     )+
    
    ## percent axes with 0–100% by 10s
    scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    
    ## larger bubble scale
    scale_size_continuous(range = c(4, 16)) +

    guides(
  colour = guide_legend(
    override.aes = list(size = 6, alpha = 1)
  	)
	) +
    
    ## clean minimal theme with larger text
    theme_minimal(base_size = 16) +
    theme(
        axis.title   = element_text(size = 18),
        axis.text    = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
    ) +
    
    ## labels
    labs(
        x = "Probability of <u> Realization",
        y = "Probability of <u> Realization",
        colour = "Lexical origin",
        size   = "Token count"
    )
    
#/o/
ggplot(go, aes(x = Vowel_RF, y = Vowel_RF)) +
    
    ## vertical reference lines at 10%, 20%, 80%, 90%
    geom_vline(
        xintercept = c(0.1, 0.2, 0.8, 0.9),
        linetype = "dashed",
        colour = "blue",
        linewidth = 0.8
    ) +
    
    ## lighter, larger points
    geom_point(
        aes(size = Count, colour = Origin),
        alpha = 0.45
    ) +
    
    ## black labels, repelled and enlarged
    geom_text_repel(
        aes(label = Trans),
        colour = "black",
        size = 4.2,
        box.padding = 0.1,
        point.padding = 0.3,
        max.overlaps = Inf
    ) +

     #Colour palette
     scale_colour_manual(
         values = c(
             "Spanish" = "#D896FF")
     )+
    
    ## percent axes with 0–100% by 10s
    scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    
    ## larger bubble scale
    scale_size_continuous(range = c(4, 16)) +
    
    guides(
  colour = guide_legend(
    override.aes = list(size = 6, alpha = 1)
  	)
	) +
    
    ## clean minimal theme with larger text
    theme_minimal(base_size = 16) +
    theme(
        axis.title   = element_text(size = 18),
        axis.text    = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
    ) +
    
    ## labels
    labs(
        x = "Probability of <o> Realization",
        y = "Probability of <o> Realization",
        colour = "Lexical origin",
        size   = "Token count"
    )
    
#Vowel sequencies
ggplot(gvs, aes(x = Vowel, y = Vowel)) +
    
    ## vertical reference lines at 10%, 20%, 80%, 90%
    geom_vline(
        xintercept = c(0.1, 0.2, 0.8, 0.9),
        linetype = "dashed",
        colour = "blue",
        linewidth = 0.8
    ) +
    
    ## lighter, larger points
    geom_point(
        aes(size = Count, colour = V),
        alpha = 0.45
    ) +
    
    ## black labels, repelled and enlarged
    geom_text_repel(
        aes(label = Trans),
        colour = "black",
        size = 4.2,
        box.padding = 0.1,
        point.padding = 0.3,
        max.overlaps = Inf
    ) +
    
    ## percent axes with 0–100% by 10s
    scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1),
        labels = percent_format(accuracy = 1)
    ) +
    
    ## larger bubble scale
    scale_size_continuous(range = c(4, 16)) +
    
    guides(
  colour = guide_legend(
    override.aes = list(size = 6, alpha = 1)
  	)
	) +
    
    ## clean minimal theme with larger text
    theme_minimal(base_size = 16) +
    theme(
        axis.title   = element_text(size = 18),
        axis.text    = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
    ) +
    
    ## labels
    labs(
        x = "Probability of Vowel Sequence Realizations",
        y = "Probability of Vowel Sequence Realizations",
        colour = "Lexical origin",
        size   = "Token count"
    )