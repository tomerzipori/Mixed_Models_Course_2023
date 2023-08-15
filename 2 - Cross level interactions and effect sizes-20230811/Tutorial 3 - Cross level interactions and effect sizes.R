##  Cross-level interactions and pseudo R square  ##

# Cross-level interactions are interaction between:

# 1. Time-variant predictors = within person = level 1 predictors
# 2. Time-invariant          = Between person\ person-level   = level-2 predictors

# * This is true when obs. are nested within persons, but it is not always like that.

# The Data ------------------------------------------------------------------

Nemo <- read.csv("Nemo.csv") # The child-Flanker task
Nemo <-read.csv(file.choose()) # If you are not working with r.project

head(Nemo) 
# ID - subject ID (child)
# RT - reaction time in the task
# Condition - the Flanker condition - See "Nemo Flanker.png"


Symptoms <- read.csv("Symptoms.csv")
Symptoms <-read.csv(file.choose())

head(Symptoms)
# ID      - the child's ID
# inatten - inattention symptoms (as per Mom)
# hyper   - hyper activity symptoms (as per Mom)
# ADHD    - total symptoms (inatten + hyper)


# This data is based on real data collected as part of the BIDS research project.


## Our research question:
# We are interested in how the number of inattention symptoms affect the
# Facilitation and Interference effects. That is, inattention X Condition effect.

# This is a cross-level int. Why?
# inattention is measured in level 2
# Condition is measured in level 1

## Data prep: coding + centering ----------------------------------------------

library(dplyr)

# We first need to merge the level 1 (Nemo) and level 2 (Symptoms) data

ADHD_data <- left_join(Nemo,Symptoms, by = "ID")
head(ADHD_data)


# Next we need to code Condition as a factor. 
# We will use the Neutral condition as the reference group - that way we 
# have parameters for:
# 1. The Facilitation effect: {Congruent - Neutral}
# 2. The Interference effect: {Incongruent - Neutral}
ADHD_data <- ADHD_data  %>%  
  mutate(
    Condition = factor(Condition),
    Condition = relevel(Condition, ref = "Neutral")
  )

levels(ADHD_data$Condition)
contrasts(ADHD_data$Condition)

# We *could* use effects coding if we wanted the intercept to be the overall
# mean, but that would make interpretation of the coefficients harder.


# Since we are interested in how the number of inattention symptoms affect the
# Facilitation and Interference effects = moderation.
# So we will also center the inattention symptoms, to aid in interpretation.
# As we know, the scale of all model predictors should have a meaningful 0. 
# --> If the original scale of the predictor doesn't include 0, or if 0 is of
# little value or interest, then a constant should be subtracted or added-> a
# procedure known as CENTERING.
ADHD_data <- ADHD_data  %>%  
  mutate(inatten = scale(inatten, center = TRUE, scale = FALSE))


# Modeling ----------------------------------------------------------------

library(lmerTest) #for MLMs
library(performance) #for icc
library(parameters)
library(emmeans)


## Random intercept model/ empty model -----------------------------------

# The starting point is to estimate the WP empty model* in which:

# FIXED EFFECTS- intercept
# RANDOM EFFECTS- intercept

# * With regard to the name of the model, we can drop the WP part of the name and stay with "empty model"
#   because all MLM will include both BP and WP variance. Otherwise, we could use regular GLMs.

# The model's equations (Hoffman's notation):
# level-1 equation:   RTji = b0i + eji 
# level-2 equation:   b0i = gamma00 + U0i
# Composite equation: RTji = gamma00 + U0i + eji
#            [Reaction Time of child i during measurement j= sample's mean (fixed intercept)+ 
#                                                the individual difference in child i's intercept +
#                                                error for child i in measurement j]

# fit:
EmptyWP <- lmer(RT ~ 1 + (1|ID), data = ADHD_data)
summary(EmptyWP)

icc(EmptyWP)
# ~10% of the variance in RT is due to person mean differences.

ranova(EmptyWP)
# and it seems this is substantial enough to model...


## RT ~ Condition model (Level 1 predictor model) ---------------------------------------------------

# adding the effect of Condition:
# Does children's RT change in different condition?

# FIXED EFFECTS- intercept, condition dummy variables (Cong and Incong in reference to Neutral)
# RANDOM EFFECTS- intercept, condition dummy variables, and their co-variances

# * NOTE: We will directly model fixed and random effects (+ their COV) to save some time
#         But you should try to model random effects sequentially and use only random effects
#         that help with model fit (by comparing models), as we taught in the previous tutorial

## The model's equations:

## level-1 equation:
# RTji =  b0i + b1i(Cong_dummyij) + b2i(Incong_dummyij) + eji 

## That is...
# RT = mean neutral + b1i*0+ b2i*0 = mean neutral       <- neutral
# RT = mean neutral + b1i*1+ b2i*0 = mean neutral + b1i <- cong
# RT = mean neutral + b1i*0+ b2i*1 = mean neutral + b2i <- INcong

# child i's RT in observation j = 
#   child's mean RT + 
#   Cong effect for child i * Cong_dummy value at obs. j (1- if obs. j was under Cong condition, 0 otherwise ) +
#   Incong effect for child i * Incong_dummy value at obs. j (1- if obs. j was under Incong condition, 0 otherwise )+
#   residual error for child i's at obs. j

## level-2 equations: b0i = gamma00 + U0i <- same as before
#                     b1i = gamma10 + U1i [Cong effect for child i = 
#                                         the general Cong effect in the sample
#                                          + the individual deviation in child i Cong effect]
#                     b2i = gamma20 + U2i [Incong effect for child i = 
#                                         the general Incong effect in the sample
#                                          + the individual deviation in child i Incong effect]

## Composite: RTji = gamma00 + (gamma10 + U1i)(Cong_dummyij) + (gamma20 + U2i)(Incong_dummyij) +U0i + eji
#             [RT of child i during obs. j= sample's mean RT+ 
#                                          (Cong effect based on the general+ individual effects) * Cong_dummy value at obs. j +
#                                          (Incong effect based on the general+ individual effects) * Cong_dummy value at obs. j +
#                                          the individual difference in child i's intercept +
#                                          error for child i in obs. j]


WPCondition <- lmer(RT ~ Condition + (Condition|ID), data = ADHD_data)

model_parameters(WPCondition, ci_method = "S")
summary(WPCondition)
# We can see that there is a large [-414.06,  -23.03] interference effect (ConditionIncong effect).

# This function shows us the random (co)variance components:
VarCorr(WPCondition)

# The interference effect (ConditionIncong) is related to the intercept
# (r=-0.72). This means that individuals who are slower in the neutral
# condition, tend to show larger (more negative) interference effects!


# We can also see there is *a lot* of within-person variation that is not
# explained by the Flanker task.

# And indeed, <1% in RTs is explained by the Flanker condition.
r2(WPCondition)

# The marginal r-squared considers only the variance of the fixed effects, 
# while the conditional r-squared takes both the fixed and random effects 
#into account.


## RT ~ Condition+ Symptoms model-> Adding level 2 predictor ---------------------------------------------

# We will now add ADHD to the model. For now, only as main effect (we will get to the moderation...)

# Can ADHD be random? 
# Not in this data!

# so...
# FIXED EFFECTS- intercept, condition dummy variables , symptoms
# RANDOM EFFECTS- intercept, condition dummy variables, and their co-variances

## The model's equations:

# level-1 equation: RTji =  b0i + b1i(Cong_dummyij) + b2i(Incong_dummyij) + eji  
#                   same as before!

# level-2 equations:  b0i = gamma00 + gamma01(Symptomsi) +U0i 
#                           Now the child i's intercept is modeled also by the child's ADHD effect
#                           i.e, Child i's mean RT= sample's mean RT + 
#                                                   ADHD Symptoms effect on RT * child i's Symptoms +
#                                                   individual difference in child i's intercept (after accounting for symptoms)
#                     b1i = gamma10 + U1i <- same as before
#                     b2i = gamma20 + U2i <- same as before

## Composite: RTji = gamma00 + gamma01(Symptomsi)+ (gamma10 + U1i)(Cong_dummyij) + (gamma20 + U2i)(Incong_dummyij)  +U0i + eji
#             [RT of child i during obs. j= sample's mean RT+ 
#                                           ADHD Symptoms effect on RT * child i's Symptoms +
#                                          (Cong effect based on the general+ individual effects) * Cong_dummy value at obs. j +
#                                          (Incong effect based on the general+ individual effects) * Cong_dummy value at obs. j +
#                                          the individual difference in child i's intercept (after accounting for symptoms) +
#                                          error for child i in obs. j]


BP_ADHD <- lmer(RT ~ Condition + inatten + (Condition|ID), data = ADHD_data)

anova(BP_ADHD, WPCondition, refit = TRUE) # fit isn't better

(BP_ADHD_mp <- model_parameters(BP_ADHD, ci_method = "S", effects = "fixed"))
# We can see that the effect for inatten is rather small (an increase in 1 symptom
# amounts to -14--+34 ms change in reaction times - in children).


# We can also quantify this addition to the model with Pseudo R2!




### Pseudo R2 ---------------

# Pseudo R2 is calculated when adding FIXED effects!
# we won't check Pseudo R2 after adding random effects!

# Pseudo R2 is calculated in relation to the specific variance it aims to explain-
# That is, if the fixed time-invariant effect models:
# The intercept: -> if the intercept is random, we will examine the change in the intercept
#                   variance (Tau square), according to the relevant level 2 equation.
#                -> if the intercept is not random, we will examine the change in 
#                   the residual variance (sigma square)
# Another predictor:-> if the interacting predictor is random, we will examine the change in 
#                      the predictor slope variance (Tau square), according to the relevant level
#                      2 equation.
#                   -> if the interacting predictor is not random, we will examine the 
#                      change in the residual variance (sigma square)


# We have added a level 2/ person-level/ BP variable->
# We are looking at the change in variance to the random intercepts.
VarCorr(BP_ADHD)
# Now it is SD(Intercept) = 471.84
# Before it was SD(Intercept) = 477.99

(Psd_R2 <- 1 - (471.84^2)/(477.99^2))
# Therefore, 2.5% of the between-person variance in the neutral condition is
# accounted for by ADHD symptoms (at least in this data set).

# Compare to:
r2(BP_ADHD)

# When explained variance was quite similar in the previous model:
r2(WPCondition)

# same as: 
r2_nakagawa(BP_ADHD)
r2_nakagawa(WPCondition)

# Extra Note:
# It appears only ~0.1% of the total variance in RTs was related to person mean
# differences predictable by inattention, a small effect size at best. It makes
# sense when thinking about the fit indices that were not very definitive
# regarding the improvement in fit vs. number of predictors trade-off (and even
# supported the previous model).


# r2_nakagawa and pseudo r2 are 2 approaches out of more. As this example illustrates the methods
# for calculating the explained variance not converge on the same estimates
# and this is why it will be important to always describe exactly how any R2
# values were obtained in reporting your results.




### Pseudo beta ---------------

# We can also look at the pseudo-standardized coefficients.
# BP-standardized coef. = (BP-ustandardized coef.) * (BPpredictorStd.Dev) / (BPoutcomeStd.Dev)
# WP-standardized coef. = (WP-ustandardized coef.) * (WPpredictorStd.Dev) / (WPoutcomeStd.Dev)

# We first need the variances from the first model:
VarCorr(EmptyWP)
sd_within <- 1178.48
sd_between <- 387.38


# There are our coefficients:
BP_ADHD_mp
# e.g., the standardized effect of inatten:
9.60 * sd(Symptoms$inatten) / sd_between

# e.g., the standardized effect of the interference effect is harder (because it
# is a factor), but we can find it:
ADHD_data %>% 
  mutate(dummy_interference = ifelse(Condition == "Incong", 1, 0)) %>% 
  summarise(interference_SD = sd(dummy_interference))

-216.86 * 0.4872474 / sd_within

# We can also find the SDs using:
standardise_info(BP_ADHD, include_pseudo = TRUE)[,c("Parameter", "Deviation_Response_Pseudo", "Deviation_Pseudo")]

# Or we can just get the whole thing directly (*not always accurate):
model_parameters(BP_ADHD, ci_method = "S", effects = "fixed",
                 standardize = "pseudo")

# Note that the level 2 effect of inatten (0.11) is stronger than the level 1
# effect (-0.09) - how can it be the weaker effect is the significant one??

# It is because of POWER - the level 1 predictor as all the repeated measures
# from all the subjects to support the effect. Whereas the level 2 predictor has
# only the single measures from each subject to support an effect.




## Cross level interaction  ------------------------

# Now we want to see how inatten moderated the Flanker effects!

# FIXED EFFECTS- intercept, condition dummy variables , symptoms, 
#                AND symptoms interactions with condition dummy variables
# RANDOM EFFECTS- intercept, condition dummy variables, and their co-variances


CLI_model <- lmer(RT ~ Condition + inatten + Condition:inatten + (Condition|ID), data = ADHD_data)
# Or
CLI_model <- lmer(RT ~ Condition * inatten + (Condition|ID), data = ADHD_data)


## The model's equations:

# level-1 equation: RTji =  b0i + b1i(Cong_dummyij) + b2i(Incong_dummyij) + eji  <-same as before!

# level-2 equations:  b0i = gamma00 + gamma01(Symptomsi) + U0i <-same as before!
#                     b1i = gamma10 + gamma11(Symptomsi) + U1i 
#                     b2i = gamma20 + gamma21(Symptomsi) + U2i 

#                           Now Cong and Incong effects are modeled also by Symptom!
#                           e.g., the b1i effect (Cong effect): child i's intercept is modeled also by the child's ADHD effect
#                           i.e, Child i's Cong effect= sample's Cong effect + 
#                                                   ADHD Symptoms effect on Cong effect * child i's Symptoms +
#                                                   individual difference in child i's Cong effect 
#                                                   (after accounting for symptoms moderation effect)


# But where we can see the interaction if it's cross-leveled? 
# in the composite equation!
# just placing level-2 equations in level 1:
# RTji = gamma00 + gamma01(Symptomsi)+ (gamma10 + gamma11(Symptomsi) + U1i)(Cong_dummyij) + 
#                 (gamma20 + gamma21(Symptomsi) + U2i)(Incong_dummyij)  +U0i + eji

# (partially) Opening this equation:
# RTji = gamma00 + gamma01(Symptomsi)+ (gamma10 + U1i)(Cong_dummyij) + gamma11(Symptomsi)(Cong_dummyij)
#                 (gamma20 + U2i)(Incong_dummyij) + gamma21(Symptomsi)(Incong_dummyij) +U0i + eji

# gamma11 and gamma21 are the interaction estimates

model_parameters(CLI_model, ci_method = "S", effects = "fixed") #dummy var interactions not sig
anova(CLI_model) #anova table- the "main" interaction is not sig

#comparing with model with no int.:

anova(CLI_model, BP_ADHD)
# All arrow point at no interaction....

### Effect sizes --------------------

r2_nakagawa(CLI_model) # total explained variance

# Looking Pseudo R2:
VarCorr(CLI_model)
VarCorr(BP_ADHD)

(PSD_R2_interference <- 1 - (600.27 / 600.45) ^ 2)

# Very little compared to the previous model...

# And Pseudo Std. Coef:
model_parameters(CLI_model, ci_method = "S", effects = "fixed",
                 standardize = "pseudo")

### Extra (nothing new...)- Follow up analysis --------------

# Just like with linear models, we can conduct followup analyses here too:
# (Yes, interaction wasn't significant, so we open it just for the sport...)


#### Simple slopes (Condition as moderator)

emtrends(CLI_model, ~ Condition, var = "inatten",
         lmer.df = "s") %>%  
  summary(infer = TRUE)
# The (NS) trends for the between person effects are:
# - In Neutral and Cong, subjects with *more* inattentive symptoms show slower RTs.
# - In the Incong condition, subjects with *less* inattentive symptoms show slower RTs.


#### Simple contrasts (inatten as moderator) ------


mean_sd <- function(x) mean(x) + c(-1, 0, 1) * sd(x)

ems <- emmeans(CLI_model, ~ Condition + inatten,
               cov.reduce = list(inatten = mean_sd),
               lmer.df = "s")
ems

contrast(ems, method = "trt.vs.ctrl", by = "inatten")
# For low inatten, interference effect is weak (and not significant).
# For mean inatten, interference effect is larger (and marginally significant).
# For high inatten, interference effect is largest (and significant!).
#
# (However the interaction = the difference between them, is not significant.)

# Exercise (2) ----------------------------------------------------------------


datat <- read.csv("Bolger and Laurenceau Ch. 4.csv")
datat <-read.csv(file.choose())

# this data is from Bolger and Laurenceau (2013), chapter 4 (csv is included in
# this R project). It's simulated data that is intended to represent a study of
# wives from 50 heterosexual married couples, randomly assigned to a 16-week
# marital therapy treatment condition (n = 25) or a 16-week wait-list condition
# (n = 25). In both groups participants completed web diary on a fixed day each
# week for 15 time points. One questionnaire in this survey measured intimacy.

head(datat)

# There are 16 lines of data for each subject, resulting in 16 * 50 = 800 rows in all. 
# There are 4 variables: id, time, intimacy, and treatment (0 for control and 1 for intervention).

## Spaghetti Plots
library(ggplot2)
ggplot(datat, aes(time, intimacy, colour = factor(id))) + 
  geom_line() +
  geom_smooth(method = 'lm', se = FALSE,
              colour = "black", linewidth = 1) +
  facet_grid(cols = vars(treatment)) + 
  guides(colour = "none")
# What do you see here? which effects?




## Your mission- Build and compare the following models:
# 1. empty model for predicting intimacy
#    - find the unconditional ICC
# 2. the random linear time model for predicting intimacy (time both as fixed
#    and random)
#    - DON'T model random effects COV.
#    - check whether the BP variance in the effect of time is sig.
#    - How is the random effect of time related to BP outcome levels at the
#      start of the study?
#    - Re-code the data to obtain the correlation between the slope of time and
#      the outcome at the *end* of the study.
#    * Bonus: is the correlation is significantly different from 0?
# 3. Add (to the random linear time model) treatment as a predictor
#    - Compute the pseudo-R2
#    - Compute pseudo std. coefs (pseudo betas)
# 4. Add the time * treatment interaction
#    - Compute the pseudo-R2
#    - Use the emmeans package for simple slope analysis (compute the slope for
#      each group)

# For each model- compare it to the previous model and interpret the results.
# Did the parameter you add improved the model?


# (we will have exercises on pseudo R square and stand. estimates in another time...)
