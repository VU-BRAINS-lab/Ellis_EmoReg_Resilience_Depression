#Load libraries
library(Hmisc)
library(ggplot2)
library(interactions)

#Cronbach's Alpha (read in item-level data)
library(ltm)
SurveyData <- read.csv("~/Desktop/Survey_cleaned.csv") 

#Cognitive Reappraisal
cog_items <- SurveyData[c("erq_1", "erq_3", "erq_5", "erq_7", "erq_8", "erq_10")]

cronbach.alpha(cog_items, na.rm = TRUE)

#Expressive Suppression
exp_items <- SurveyData[c("erq_2", "erq_4", "erq_6", "erq_9")]

#ders_sf_strategies
ders_strategies  <- SurveyData[c("ders_sf_10", "ders_sf_15", "ders_sf_18")]

cronbach.alpha(ders_strategies, na.rm = TRUE)

#ders_sf_nonacceptance
ders_nonacceptance <- SurveyData[c("ders_sf_7", "ders_sf_12", "ders_sf_16")]

cronbach.alpha(ders_nonacceptance, na.rm = TRUE)

#ders_sf_impulse
ders_impulse <- SurveyData[c("ders_sf_9", "ders_sf_14", "ders_sf_17")]

cronbach.alpha(ders_impulse, na.rm = TRUE)

#ders_sf_goals
ders_goals <- SurveyData[c("ders_sf_8", "ders_sf_11", "ders_sf_13")]

cronbach.alpha(ders_goals, na.rm = TRUE)

#ders_sf_awareness
ders_awareness <- SurveyData[c("ders_sf_1", "ders_sf_4", "ders_sf_6")]

cronbach.alpha(ders_awareness, na.rm = TRUE)

#ders_sf_clarity
ders_clarity <- SurveyData[c("ders_sf_2", "ders_sf_3", "ders_sf_5")]

cronbach.alpha(ders_clarity, na.rm = TRUE)

#BRS
brs_items <- SurveyData[c("brs_1", "brs_2", "brs_3", "brs_4", "brs_5", "brs_6")]

cronbach.alpha(brs_items, na.rm = TRUE)

#BDI
bdi_items <- SurveyData[c("bdi_1", "bdi_2", "bdi_3", "bdi_4", "bdi_5", "bdi_6", "bdi_7", "bdi_8", "bdi_9", "bdi_10", "bdi_11", "bdi_12", "bdi_13", "bdi_14", "bdi_15", "bdi_16", "bdi_17", "bdi_18", "bdi_19", "bdi_20", "bdi_21")]

cronbach.alpha(bdi_items, na.rm = TRUE)

#Read in data for analyses
EmoData <- readRDS("~/Desktop/EmoReg_BRS_BDI.rds") 

##First, compute a correlation matrix, which is the correlation between all variables of interest.
#Trim to just the variables that should be in the correlation matrix
EmoData_subset <- EmoData[c(grep("bdi_total|brs_raw_score|erq_cog_total|erq_expressive_total|ders_sf_strategies_total|ders_sf_nonacceptance_tot|ders_sf_impulse_total|ders_sf_goals_total|ders_sf_awareness_total|ders_sf_clarity_total",names(EmoData)))]

#Check that it worked
names(EmoData_subset)

#Run correlation matrix
EmoData_corr <- rcorr(as.matrix(EmoData_subset))

#Print correlation matrix
EmoData_corr

#Check histograms and boxplots
#Histogram
hist(EmoData$ders_sf_strategies_total,
     xlab = "Limited access to emotion regulation strategies",
     breaks = sqrt(length(EmoData$ders_sf_strategies_total)) # set number of bins
)

hist(EmoData$ders_sf_nonacceptance_tot,
     xlab = "Nonacceptance of emotional responses",
     breaks = sqrt(length(EmoData$ders_sf_nonacceptance_tot)) # set number of bins
)

hist(EmoData$ders_sf_impulse_total,
     xlab = "Impulse control difficulties",
     breaks = sqrt(length(EmoData$ders_sf_impulse_total)) # set number of bins
)

hist(EmoData$ders_sf_goals_total,
     xlab = "Difficulty engaging in goal-directed behavior",
     breaks = sqrt(length(EmoData$ders_sf_goals_total)) # set number of bins
)

hist(EmoData$ders_sf_awareness_total,
     xlab = "Lack of emotional awareness",
     breaks = sqrt(length(EmoData$ders_sf_awareness_total)) # set number of bins
)

hist(EmoData$ders_sf_clarity_total,
     xlab = "Lack of emotional clarity",
     breaks = sqrt(length(EmoData$ders_sf_clarity_total)) # set number of bins
)

hist(EmoData$erq_cog_total,
     xlab = "Cognitive Reappraisal",
     breaks = sqrt(length(EmoData$erq_cog_total)) # set number of bins
)

hist(EmoData$erq_expressive_total,
     xlab = "Expressive Suppression",
     breaks = sqrt(length(EmoData$erq_expressive_total)) # set number of bins
)

hist(EmoData$bdi_total,
     xlab = "Depressive symptoms",
     breaks = sqrt(length(EmoData$bdi_total)) # set number of bins
)

hist(EmoData$brs_raw_score,
     xlab = "Resilience",
     breaks = sqrt(length(EmoData$brs_raw_score)) # set number of bins
)

#Boxplot
boxplot(EmoData$ders_sf_strategies_total,
        ylab = "Limited access to emotion regulation strategies"
)

boxplot(EmoData$ders_sf_nonacceptance_tot,
        ylab = "Nonacceptance of emotional responses"
)

boxplot(EmoData$ders_sf_impulse_total,
        ylab = "Lack of emotional clarity"
)

boxplot(EmoData$ders_sf_goals_total,
        ylab = "Difficulty engaging in goal-directed behavior"
)

boxplot(EmoData$ders_sf_awareness_total,
        ylab = "Lack of emotional awareness"
)

boxplot(EmoData$ders_sf_clarity_total,
        ylab = "Lack of emotional clarity"
)

boxplot(EmoData$erq_cog_total,
        ylab = "Cognitive Reappraisal"
)

boxplot(EmoData$erq_expressive_total,
        ylab = "Expressive Suppression"
)

boxplot(EmoData$bdi_total,
        ylab = "Depressive symptoms"
)

boxplot(EmoData$brs_raw_score,
        ylab = "Resilience"
)

#Based on visual inspection, log transform the BDI (adding 1 to all scores is needed so we don't try and take the log of 0)
library(dplyr)
EmoData <- EmoData %>%
  mutate(bdi_log = log(bdi_total + 1))

hist(EmoData$bdi_log,
     xlab = "Depressive symptoms log transformed",
     breaks = sqrt(length(EmoData$bdi_log)) # set number of bins
)

boxplot(EmoData$bdi_log,
        ylab = "Depressive symptoms log transformed"
)

#BDI log Moderation Analysis
mod1 <- lm(brs_raw_score ~ age + ders_sf_strategies_total + bdi_log + ders_sf_strategies_total*bdi_log, data=EmoData)
mod2 <- lm(brs_raw_score ~ age + ders_sf_nonacceptance_tot + bdi_log + ders_sf_nonacceptance_tot*bdi_log, data=EmoData)
mod3 <- lm(brs_raw_score ~ age + ders_sf_impulse_total + bdi_log + ders_sf_impulse_total*bdi_log, data=EmoData)
mod4 <- lm(brs_raw_score ~ age + ders_sf_goals_total + bdi_log + ders_sf_goals_total*bdi_log, data=EmoData)
mod5 <- lm(brs_raw_score ~ age + ders_sf_awareness_total + bdi_log + ders_sf_awareness_total*bdi_log, data=EmoData)
mod6 <- lm(brs_raw_score ~ age + ders_sf_clarity_total + bdi_log + ders_sf_clarity_total*bdi_log, data=EmoData)
mod7 <- lm(brs_raw_score ~ age + erq_cog_total + bdi_log + erq_cog_total*bdi_log, data=EmoData)
mod8 <- lm(brs_raw_score ~ age + erq_expressive_total + bdi_log + erq_expressive_total*bdi_log, data=EmoData)

#Print the model results
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
summary(mod7)
summary(mod8)

#standardized betas
library(lm.beta)
lm.beta(mod1)
lm.beta(mod2)
lm.beta(mod3)
lm.beta(mod4)
lm.beta(mod5)
lm.beta(mod6)
lm.beta(mod7)
lm.beta(mod8)

#BDI log FDR
pvalues <- c(0.000576, 0.0198, 0.0758, 3.5e-08, 0.375, 0.2562, 0.02506, 0.2998)
p.adjust(pvalues, method="fdr")

#Create a new variable with only males and females because there are not enough intersex individuals
EmoData$sex <- ifelse(EmoData$gender %in% c(1, 2), EmoData$gender, NA)
EmoData$sex <- as.factor(EmoData$sex)

##Three way interaction emotion regulation, depressive symptoms, and sex.
mod_interaction1 <- lm(brs_raw_score ~ ders_sf_strategies_total*bdi_log*sex, data=EmoData)
mod_interaction2 <- lm(brs_raw_score ~ ders_sf_nonacceptance_tot*bdi_log*sex, data=EmoData)
mod_interaction3 <- lm(brs_raw_score ~ ders_sf_impulse_total*bdi_log*sex, data=EmoData)
mod_interaction4 <- lm(brs_raw_score ~ ders_sf_goals_total*bdi_log*sex, data=EmoData)
mod_interaction5 <- lm(brs_raw_score ~ ders_sf_awareness_total*bdi_log*sex, data=EmoData)
mod_interaction6 <- lm(brs_raw_score ~ ders_sf_clarity_total*bdi_log*sex, data=EmoData)
mod_interaction7 <- lm(brs_raw_score ~ erq_cog_total*bdi_log*sex, data=EmoData)
mod_interaction8 <- lm(brs_raw_score ~ erq_expressive_total*bdi_log*sex, data=EmoData)

#Print the model results
summary(mod_interaction1)
summary(mod_interaction2)
summary(mod_interaction3)
summary(mod_interaction4)
summary(mod_interaction5)
summary(mod_interaction6)
summary(mod_interaction7)
summary(mod_interaction8)

#FDR Correction 3-way Interaction
pvalues <- c(0.17864, 0.8549, 0.1101, 0.79118, 0.219, 0.5140, 0.707899, 0.031944)
p.adjust(pvalues, method="fdr")

#Plot significant interactions for figures
plot1 <- lm(brs_raw_score ~ age + ders_sf_strategies_total + bdi_log + ders_sf_strategies_total*bdi_log, data=EmoData)
interact_plot(plot1, pred = ders_sf_strategies_total, modx = bdi_log, modx.values = "terciles", interval = TRUE,
              int.type = "confidence", int.width = .95, 
              x.label = "Limited access to regulation strategies", 
              y.label = "Resilience", 
              legend.main = "Depressive symptoms") +
  scale_y_continuous(limits = c(10, 25), breaks = seq(10, 25, 5)) +  # Set y-axis limits and breaks
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),    # Increase axis label size
        axis.text = element_text(size = 15),     # Increase tick label size
        legend.text = element_text(size = 13),    # Increase legend text size
        legend.title = element_text(size = 14))   # Increase legend title size

plot2 <-  lm(brs_raw_score ~ age + ders_sf_goals_total + bdi_log + ders_sf_goals_total*bdi_log, data=EmoData)
interact_plot(plot2, pred = ders_sf_goals_total, modx = bdi_log, modx.values = "terciles", interval = TRUE,
              int.type = "confidence", int.width = .95, 
              x.label = "Difficulty engaging in goal-directed behavior", 
              y.label = "Resilience", 
              legend.main = "Depressive symptoms") +
  scale_y_continuous(limits = c(10, 25), breaks = seq(10, 25, 5)) +  # Set y-axis limits and breaks
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15),    # Increase axis label size
        axis.text = element_text(size = 15),     # Increase tick label size
        legend.text = element_text(size = 13),    # Increase legend text size
        legend.title = element_text(size = 14))   # Increase legend title size
