## Tutorial - Time-varing\longitudinal\dynamic\measured-level-1 predictors ##


## Load relevant packages:
library(lmerTest)
library(performance)
library(tidyverse)
library(parameters)

# The first example will serve to illustrate the role of time-varing predictors 
# (e.g., mood) in longitudinal models for outcomes that show within-person FLUCTUATION.

# The second example will serve to illustrate the role of time-varing predictors 
# (e.g., parents monitoring behaviors) in longitudinal GROWTH models (change over time),
# as in the last tutorial.

## Example 1 - Time-Variant Predictors in Models for Within-Person Fluctuation: ------------

data <- read.csv("4 - Time-varying predictors-20230620/Example_DailyMoodStressSymptoms_Hoffman_Chap.78.csv") 
data <-read.csv(file.choose()) # If you are not working with r.project

# Data comes from the Cognition, Health, and Aging Project, in which 6 assessments 
# were collected over two weeks.
# Besides RT of a cognitive task, the study also collected a variety of other
# measures (of physical, cognitive, and emotional well-being).
# Because of concerns about initial reactivity to the assessments, only sessions 2 to 6
# (i.e. 5 obs.) will be used.
# The outcome 'symptoms': the number of physical symptoms participants reported 
#                         experiencing in the past 24 hours.

# The sample includes 525 lines of observations from 105 older adults:
head(data,n=20)
# Can you tell which predictors are time-invariant?
# Can you tell which predictors are time-varying?






# Specifically we have in this data:

# 2 invariant level-2 variables: baseage, women
# and 7 WP level-1 variables, but 4 of them are some indexes of time (session , studyday ,
# dayofweek , weekend ), AND 3 are MEASURED time variant predictors (symptoms, mood , stressor )

# we will start our example for predicting daily physical symptoms by first examining a 
# time-varying predictor of daily negative mood as computed as the mean of five
# items: sad, annoyed, worried, irritated, and depressed, each rated on a 5-point scale

## Data Prep - Centering -----------------------------------------------------------------------------

# Let's see how to properly center each type of variables.

# We will center our variables according to their level. 

# For example, centering \ coding of Time-Invariant Predictors  will be the same as we do in GLM:
# i.e, treating factors correctly (e.g. dummy coded) and centering continuous variables
# around the sample's mean.

# women (gender) is a binary variable:
# 0- man; 1- women
# (can be left as is, or centered such that 0 will be genders' mean).
data$women

# age can be centered to the sample's mean (=grand mean) - baseage_gmc:
data <- data %>% mutate(baseage_gmc = baseage - mean(baseage)) 

data %>% select(baseage, baseage_gmc)

### Centering MEASURED time-varying Predictors (that's new!):

# symptoms, mood, and stressor, however, should be treated differently!

# We will use symptoms (= daily physical symptoms) as our outcome, and we won't center 
# the outcome. But, if in another model, we would want to use symptoms
# as a predictor we will have to center it.


## Centering mood (our time varying predictor):
                                                                                                                                                                                                                                      
# Although time-varying predictors are measured at the time level (i.e., at each occasion), they usually
# contain systematic variation at higher levels of sampling as well (i.e., time-invariant variation). 
# -> they contain both BP and WP information.
#    Both sources of variation must be represented explicitly (and centered appropriately)!

## Two variables (instead of one) for each variation:
# 1- BP effect at level 2 - a trait \ chronic effect:
#    In the example of stress and negative mood, people with higher stress on average (than other people)
#    may report greater negative mood on average. 
# 2- WP effect at level 1 - a state \ acute effect:
#    At times when stress is higher than usual, negative mood may be greater than usual as well.


### [Not part of centering but part of the process] Check heterogeneity in predictor:-----

# How much of a time-varying predictor's variation is due to each source?  
  # -> treat the time-varying predictor as an outcome and use an empty random model to quantify ICC:

mood_model <- lmer(mood ~ 1 + (1 | PersonID), data = data)
summary(mood_model)
# Mood's BP (random intercept) variance= 0.05236
# Mood's Residual variance= 0.09516

icc(mood_model)
# Here, 35.5% of mood's variance was due to BP mean differences
# ICC size dictates which effects the time-varying predictor potentially show. 
# -> if there is significant BP variance then that BP variation can potentially show a
#    corresponding BP effect. 
# -> Alternatively, this means it has only WP variation, and thus the time-varying 
#    predictor could only show a WP effect.

# We can also:
check_heterogeneity_bias(data, 
                         select = c("mood", "baseage"), 
                         group = "PersonID")
# Indeed, our time varying predictor has Possible heterogeneity bias 

### Computing the two new mood variables ---------------------

# mood_WP - the time-varying mood value at time t for person i.
# mood_PM  - the person mean of time-varying negative mood across days.
data <- data %>%
  group_by(PersonID) %>% 
  mutate(
    mood_PM = mean(mood,na.rm=T),    # person mean
    mood_WP = mood - mood_PM # person mean centering!
  ) 

data %>% select(PersonID,mood,mood_PM,mood_WP)
# See also:
?datawizard::demean

# mood_WP is centered at a variable (not a constant): person's usual level of
# daily negative mood, as represented by each person's mean across occasions.


## variable 2: level-2 (BP, time-invariant) predictor = mood_PM - mood sample mean\ grand mean
?summarise_at
mean(data$mood_PM) #1.206095

#Or if we had non balanced data
M<-data %>% group_by(PersonID) %>% summarize(mean=mean(mood_PM))
mean(M$mean) #1.206095


data <- data %>% mutate(mood_BP = mood_PM - 1.206095) 



# All in all, these are our "mood variables"
data |> 
  select(PersonID, mood, mood_PM, mood_WP, mood_BP) |> 
  head(n = 15)
# Due to the variables centering - mood_WP will be uncorrelated with the mood_BP
# ->  because they operate at separate levels of the model, the effects of mood_WP
#     and mood_BP can be evaluated simultaneously. 

# mood_WP can be called also mood_pmc
# mood_BP can be called also pm_mood_gmc


## Modeling - WP fluctuation effects- 2 main effects for mood----------------------

# we now continue the example by examining the effect of mood time-varying predictor.
# Note that time is not part of the model (not a growth model):
# Here, what we need to know is what the mood value was at each occasion, and not 
# necessarily when that value was reported. 

## model 1a- adding the BP and WP effects of mood 

# FIXED: intercept, WPmood, BPmood
# RANDOM: intercept
  
# Level 1: symptomsti = b0i +b1i(mood_WPi)+eti
#                   the WP variable is in level 1!

# Level 2: 
# intercept: b0i = gamma00 + gamma01(mood_BPi)+ U0i
#                   the BP variable is in level 2! (Modeling the intercept)
# WPmood:    b1i = gamma10  -> For now- WPmood is not random

# Composite: symptomsti = gamma00 + U0i + gamma01(mood_BPi) + gamma10(mood_WPti)  + eti


M1a <- lmer(symptoms ~  mood_BP + mood_WP + # mood new vars as fixed effects
                      (1|PersonID), 
                      data = data)
model_parameters(M1a, ci_method = "S")

# The fixed intercept gamma00 = 1.30  is now the expected number of physical symptoms 
# when all predictors have a value of 0: for a person with average mood equal to the 
# average mood in the sample, and on  on a day when he is at his average negative mood. 

## Negative mood effects:

# The sig. BP main effect of mood (gamma01 = 2.10 ) indicates that for every one-unit
# higher person ~mean mood, the symptoms reported across days is expected to be higher by 2.10 
# The n.s. WP main effect of mood (gamma10 =0.13 ) indicates that for every one-unit
# more negative mood than usual (i.e.,relative to the person's mean), that specific
# day's symptoms are expected to be non significantly higher by 0.13 

# To summarize, grumpier people report more symptoms across days, but being in a
# worse mood than usual doesn't predict reporting more physical symptoms that day.
# -> the BP and WP effects of the same variable appear to differ. 


### Computing mood standardized coef. using:

# BP-standardized coef.= (BP-ustandardized coef.)*
#                        (BPpredictorStd.Dev)/(BPoutcomeStd.Dev)
# WP-standardized coef.= (WP-ustandardized coef.)*
#                        (WPpredictorStd.Dev)/(WPoutcomeStd.Dev)

# That is,
# mood_BP-standardized coef.= (mood_BP-ustandardized coef.)*
#                              (Mood's random intercept Std.Dev)/(Symptoms's random intercept Std.Dev)
# mood_WP-standardized coef.= (mood_WP-ustandardized coef.)*
#                              (Mood's residual Std.Dev)/(Symptoms's residual Std.Dev)

# We will use the WP and BP variances from the unconditional empty mood models:
mood_model <- lmer(mood ~ 1 + (1|PersonID),data = data)
summary(mood_model)
# Mood's BP (random intercept) Std.Dev.= 0.2288  
# Mood's Residual variance= 0.3085  
symptoms_model <- lmer(symptoms ~ 1 + (1|PersonID),data = data)
summary(symptoms_model)
# symptoms's BP (random intercept) Std.Dev.= 1.1105  
# symptoms's Residual Std.Dev.= 0.7831  

# The unstandardized coef:
summary(M1a)
# mood_BP = 2.1030     
# mood_WP = 0.1247     

# standardized coef.:
# mood_BP-standardized coef.:
(2.1030)*(0.2288)/(1.1105) 
#0.4332881

# mood_WP-standardized coef. :
(0.1247)*(0.3085)/(0.7831)
#0.04912521

# Or- use standardize_parametes() function from effect size lib.
parameters::standardize_parameters(M1a, method = "pseudo")


# Calculations are done slightly differently, so results don't 100% match - but
# they are pretty close!
# (* Why? For between-variables, shrinkage reduces the estimated intercept
#   variance of the X (here, mood) compared to the observed variance, resulting in
#   smaller betas [but this has 0 effect on significance testing!])

## Effect size: 

# 1. Pseudo-R2 for the proportion reduction in each variance component using :
#    (Variancefewer - Variancemore) / Variancefewer. 

#    Let's compare Pseudo-R2 of this model in comparison to the empty model
 
  summary(M1a)
  # New model: Residual variance  = 0.6190       
  #            Intercept variance = .9227     
  summary(symptoms_model) 
  # Baseline model: Residual variance = 0.6133   
  #                 Intercept variance: 1.2333   

# Each pile of variance in the model will have its own pseudo-R2.

  # The BP main effect of mood explained an additional 25% of the level-2 
  # random intercept variance that remained after controlling for gender and age:
  # Pseudo-R2 for intercept variance: 
  (1.2333-.9227) / 1.2333 

  # The WP main effect of mood didn't explained any additional variance of the
  # level-1 residual variance., as 0.6133~=0.6190
  # Note- There are a number of issues that plague the use of pseudo-R^2 in practice that can 
  #       make it less useful for assessing effect size, especially in models with time-varying 
  #       predictors (see part 1.F in chap. 8). 
  
# 2. r2_nakagawa() from performance lib.
    r2_nakagawa(symptoms_model) 
#   Conditional R2 was 0.66 - 66.8% of the total outcome variance was explained by random
#   and fixed effects (i.e.,by the random inercept)
#   see- 
    icc(symptoms_model)
#   Marginal R2 was 0 - 0% of the total outcome variance was explained by fixed effects 
#   (there where no fixed effects).
    
    r2_nakagawa(M1a)
#   Now 17% of the total outcome variance is explained by fixed effects.
#   But still- 66% of total variance explained-
    # ->  Since we saw there was no additional explained variance by WP main effect
    #     17% of the variance is probably explained by BP effect, which explained some of the 
    #     already explained variance by the random intercept. 

    
    
### adding the mood_WP var RANDOM EFFECT ----------------------

# FIXED: intercept, WPmood, BPmood
# RANDOM: intercept, WPmood

# That is, in level 2: b1i = gamma10 + U1i
# such that:
# Composite: symptomsti = gamma00 + gamma10(mood_BPi) + (gamma10 + U1i)(mood_WPti) + U0i + eti
    
# now every ID will have its own slope of WP changes in mood effect on symptoms.

M1b <- lmer(symptoms ~  mood_BP + mood_WP +
              (mood_WP|PersonID), # setting WP mood changes as random + COV with ths intercept
              data = data)
model_parameters(M1b, ci_method = "S")
anova(M1a, M1b)
# Including the random effect didn't improved the model

# maybe without the COV?
M1b.noCOV <- lmer(symptoms ~  mood_BP + mood_WP +
              (mood_WP||PersonID), 
            data = data)
anova(M1a,M1b.noCOV)

# no....

# Note- mood_BP can't get a random effect since it is constant for each ID. 



### dichotomous time-varying variable- stressor -----------------------------------------

## model 2- adding the BP and WP main effects of stressor

# We have considered how to examine the effects of a continuous time varying
# predictor - negative mood. 

# We now continue by examining a categorical time-varying predictor: 
# presence of a daily stressor (Stressorti), which was assessed as a dichotomous
# variable for whether any of five questions about stressors relating to relationships,
# events, and health had been endorsed (0 = stressor free day; 1 = stressor day). 
summary(data$stressor)
  # A stressor was reported on 45% of the total sample days.

## Centering the stressors variable:

# Although PMC can be an intuitive way to examine the WP and BP effects of continuous
# time-varying predictors, it is less intuitive for categorical predictors that already
# have a natural 0 point and a limited range of possible values.

# However, PMC is appropriate also in this case. Moreover, if we want prevent smushing 
# of the effects of stressors across levels, we need to include the person mean of stressors:

# Computing the two new Stressor variables:

# Stressorti - presence of Stressor at time t for person i.
# Stessori - the person mean of Stressors' presence across days (proportion of sterssors).
data <- data %>% group_by(PersonID) %>% 
  mutate(PMstressor = mean(stressor,na.rm=TRUE)) # person mean (=stressori)

## variable 1: level-1 (WP) predictor      
data <- data %>% mutate(stressor_WP = stressor - PMstressor) 
# How this pmc variable is interperated?

data %>% select(stressor,PMstressor,stressor_WP)
# The WP effect of stressor in reltion to one's total proportion of stressor presence
# (that is, in relation to one's mean tendency to report stressors above days)

## variable 2: level-2 (BP, time-invariant) predictor

mean(data$PMstressor,na.rm=TRUE) #0.4495238
#OR (if number of measurments differ per ID):
#Or if we had non balanced data
M<-data %>% group_by(PersonID) %>% summarize(mean=mean(PMstressor))
mean(M$mean) #.4495238 (same...)

# -> The reference point will be a person who reported a stressor on 44.9% of their days.
data <- data %>% mutate(stressor_BP = PMstressor - 0.4495238) 

# All in all:
head(data[,c("PersonID","stressor","PMstressor","stressor_WP","stressor_BP")],n = 20L)  

# Adding to our model the effects of daily stressors:

# FIXED: intercept, 
#         WPmood, BPmood, 
#         WPstressor, BPstressor
# RANDOM: intercept

M2a <- lmer(symptoms ~ 
              mood_BP + mood_WP +
              stressor_BP + stressor_WP + # new BP and WP stressor vars
              (1|PersonID), data = data)
summary(M2a)

# -> the fixed intercept 1.29815     - is the intercept for a person with the sample's average mood
#     (mood_BP=0) on days when he has mean mood (mood_WP = 0)+
#     for a person who reported stressors on 44.9% of  days (stressor_BP = 0) on a
#    relative to self- non-stressor day (stressor_WP = 0). 

#    - The WP effect for Stressorti of 0.07037  (n.s) indicates that, holding the proportion of
#      stressor days constant, symptoms were expected to be n.s. higher by 0.07037 on days
#      with stressors compared to a day with no stressors. 
#    - The level-2 effect for BPstressor of 1.00661 (p < .01) indicates that, controlling for 
#      daily stressors, symptoms were expected to be sig. higher by 1.00661  for every unit of PMstressor. 


# Although the WP fixed effect of stressors is not significant, this effect could still
# vary across persons. We can test this by adding a random slope:

## Model 2b- adding stressors random slope
M2b <- lmer(symptoms ~ 
              mood_BP + mood_WP +
              stressor_BP + stressor_WP +
              (stressor_WP|PersonID), data = data)
summary(M2b)
# R is not happy an we've got an "?isSingular" warning
# Trying without cov:
M2bNew<- lmer(symptoms ~
                mood_BP + mood_WP +
                stressor_BP + stressor_WP +
              (stressor_WP||PersonID), data = data)
summary(M2bNew)
# The warning remains and the stressor_WP variance is really close to 0 (1.931e-14)


# Thus, whether a stressor occurs on a given day does not appear to relate to that days
# physical symptoms (i.e., no fixed WP effect), and the difference between stressor and
# non-stressor days is the same across individuals (i.e., no random level-1 effect).
# However, persons who experience stressors on a higher proportion are more likely to report
# greater symptoms (i.e., a fixed BP effect), even after controlling for that day's stressors.







# Exercise 3- Example 2 - time-varying predictors that show within-person change over time (in GROWTH models)------------

# THIS EXAMPLE WILL BE AN EXERCISE- Follow the instructions in the code. 
# Answers are provided in the R code: "Example 2- Time-Varing Predictors - answers"

data2 <- read.csv("4 - Time-varying predictors-20230620/ChangeInRiskyBehaviorAdolescence_Hoffman_Chap.7.csv") 

head(data2)
# Seven (approximately annual) assessments were collected from 200 adolescent girls 
# age 12 to 18 (1,400 total observations).

# The variables:
# The outcome variable: Risky (12-18)- a sum of 10 items indexing frequency of risky behavior 
#                       (e.g., smoking, drinking, shoplifting, skipping school).
# Time-invariant predictor: Attitude12- an index of their mothers' attitudes about
#                           smoking and drinking (measured only in the first time point).
# Time-variant predictors: Age (12-18)- the exact age in each time point.
#                          Monitor (12-18)- Now we will use it! Monitoring was measured as
#                          the mean across 10 items on a 1 to 5 scale.


## Data Prep (explained in the first tutorial):

# Creating a to long (stacked) format of the data:
data.long <- data2 %>% 
  tidyr::pivot_longer(
    cols = c(starts_with("Age"), starts_with("Risky"), starts_with("Monitor")),
    names_pattern = "(.*)([0-9][0-9])",
    names_to = c(".value", "Age_rounded")
  )
head(data.long)
# we centered age to 18:
data.long <- data.long %>% mutate(YearsPre18 = Age-18)

head(data.long)




############################################
############## Your exercise ##############
############################################

# Let's day you have already found that the best unconditional growth model 
# included fixed quadratic and random linear effects of age:

GrowthQuadFix<-lmer(Risky~poly(YearsPre18,2,raw = TRUE)+
                          (YearsPre18|PersonID),
                          data=data.long)
summary(GrowthQuadFix)

# As seen in the plot:
ggplot(data.long, aes(x=Age,y=Risky)) + geom_line() + 
  guides(colour=FALSE) + xlab("Age")+ylab("Risky behavior") +
  aes(colour = factor(PersonID)) + geom_smooth(se=FALSE, colour="black", size=2)

# Then you also discovered a conditional model in which mother attitudes (level 2, invariant
# predictor) moderated the  linear (though not quadratic) age effects
AttitudesByLinT<-lmer(Risky~YearsPre18*Attitude12 + I(YearsPre18^2)+
                        (YearsPre18|PersonID),data=data.long)
summary(AttitudesByLinT) 

psych::describe(data.long$Attitude12)

emmeans::emtrends(AttitudesByLinT, ~ Attitude12, var = "YearsPre18", 
         at= list(Attitude12=c(3.35,3.95,4.55)),
         lmer.df = "s") |> 
  summary(infer = TRUE)

# We now turn to examine how girls risky behavior might be predicted by
# mothers' monitoring behavior, as collected at the same occasions. 
# That is, monitoring behavior is a time varying predictor in growth model.

# A. Compute monitoring ICC. 

monitoring_model0 <- lmer(Monitor ~ 1 + (1 | PersonID), data = data.long)

icc(monitoring_model0)

# B. Although monitoring will be a time-varying *predictor*,given the developmental context of
#    this study, it will be useful to examine to what extent monitoring itself shows individual
#    change in interpreting its effects predicting risky behavior in the models that follow. 
#    More generally, examination of unconditional models of change is always 
#    recommended for any time-varying variable, regardless of whether it is labeled
#    as a predictor or an outcome.
#     Hence, fit a quadratic model in which the outcome is monitor.
#     That is, fit monitor~ YearsPre18 + YearsPre18^2.
#     Use a random intercept, and add random effects of linear\ quadratic time only if they 
#     improve the model.

monitor_model1 <- lmer(Monitor ~ YearsPre18 + I(YearsPre18^2) + (1 | PersonID), data = data.long)

model_parameters(monitor_model1)

monitor_model2 <- lmer(Monitor ~ YearsPre18 + I(YearsPre18^2) + (YearsPre18 | PersonID), data = data.long)
monitor_model3 <- lmer(Monitor ~ YearsPre18 + I(YearsPre18^2) + (YearsPre18 + I(YearsPre18^2) | PersonID), data = data.long)

anova(monitor_model1, monitor_model2) # model 2 is better
anova(monitor_model1, monitor_model3) # model 3 is better
anova(monitor_model2, monitor_model3) # model 3 is not better - choosing the simpler model 2

# C. we will now add time-varying effects of monitor to the 'AttitudesByLinT' model.
#    i.e. add the WP monitor main effect to the model.
#    To do so, start with person mean centering. Add the pmc monitor to  
#    'AttitudesByLinT' as main fixed effect, and then also as random. compare models.
AttitudesByLinT <- lmer(Risky ~ YearsPre18 * Attitude12 + I(YearsPre18^2) +
                        (YearsPre18|PersonID), data = data.long)

model_parameters(AttitudesByLinT)

## Person-mean centering monitor
data.long <- data.long |>
  group_by(PersonID) |>
  mutate(monitor_pmc = Monitor - mean(Monitor, na.rm = T)) |>
  ungroup()

AttitudesByLinT_fixed1 <- lmer(Risky ~ YearsPre18 * Attitude12 + I(YearsPre18^2) + monitor_pmc +
                                     (YearsPre18|PersonID), data = data.long)

model_parameters(AttitudesByLinT_fixed1)

AttitudesByLinT_fixed2 <- lmer(Risky ~ YearsPre18 * Attitude12 + I(YearsPre18^2) + monitor_pmc +
                                        (YearsPre18 + monitor_pmc|PersonID), data = data.long)

model_parameters(AttitudesByLinT_fixed2)

anova(AttitudesByLinT_fixed1, AttitudesByLinT_fixed2) # adding the random effect is not significantly better

# D. Add the BP effect of monitoring to the best model from C.
#    Use monitor grand mean as the centering point.

## Adding person-mean and BP monitor
data.long <- data.long |>
  group_by(PersonID) |>
  mutate(monitor_pm = mean(Monitor, na.rm = T)) |>
  ungroup() |>
  mutate(monitor_bp = monitor_pm - mean(Monitor, na.rm = T))

AttitudesByLinT_fixed3 <- lmer(Risky ~ YearsPre18 * Attitude12 + I(YearsPre18^2) + monitor_pmc + monitor_bp +
                                        (YearsPre18|PersonID), data = data.long)

model_parameters(AttitudesByLinT_fixed3)

# E. Lastly, does the WP and BP monitoring effects on risky behavior interacts with 
#    linear effect of age? add the monitor variables interactions with age (for now, 
#    only linear).

AttitudesByLinT_fixed4 <- lmer(Risky ~ YearsPre18 * Attitude12 + I(YearsPre18^2) + monitor_pmc * YearsPre18 + monitor_bp * YearsPre18 +
                                 (YearsPre18|PersonID), data = data.long)

model_parameters(AttitudesByLinT_fixed4)

anova(AttitudesByLinT_fixed3, AttitudesByLinT_fixed4) # interaction terms doesnt add significant explained variance

# FOR MORE OF TIME VARYING PREDICTORS IN GROWTH MODELS- READ CHAPTER 9.