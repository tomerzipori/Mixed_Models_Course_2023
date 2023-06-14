# Exercise 2 --------------------------------------------------------------------------

# (This exercise will combine grrowth models with cross-level interaction)
library(ggplot2)
library(lmerTest)
library(parameters)
library(performance)
library(emmeans)

data <- read.csv("ex3 - growth models/Bolger and Laurenceau Ch. 4.csv")


# this data is from Bolger and Laurenceau (2013), chapter 4 (csv is included in
# this R project). It's simulated data that is intended to represent a study of
# wives from 50 heterosexual married couples, randomly assigned to a 16-week
# marital therapy treatment condition (n = 25) or a 16-week wait-list condition
# (n = 25). In both groups participants completed web diary on a fixed day each
# week for 15 time points. One questionnaire in this survey measured intimacy.

head(data)

# There are 16 lines of data for each subject, resulting in 16 * 50 = 800 rows in all. 
# There are 4 variables: id, time, intimacy, and treatment (0 for control and 1 for intervention).

## Spaghetti Plots

ggplot(data, aes(time, intimacy, colour = factor(id))) + 
  geom_line() +
  geom_smooth(method = 'loess', se = FALSE,
              colour = "black", linewidth = 1) +
  guides(colour = "none")
# What do you see here? which effects?

# And per group?
ggplot(data, aes(time, intimacy, colour = factor(id))) + 
  geom_line() +
  geom_smooth(method = 'loess', se = FALSE,
              colour = "black", linewidth = 1) +
  facet_grid(cols = vars(treatment)) + 
  guides(colour = "none")




## Your mission- Build and COMPARE (!) the following models:

# 1. empty model (=with fixed and random intercept) for predicting intimacy
#    + find the unconditional ICC and descibe its' meaning
model0 <- lmer(intimacy ~ 1 + (1|id), data = data)

model_parameters(model0)

## ICC - measure of explained variance by individual differences
icc(model0) # 24.1%


# 2. the linear time growth model for predicting intimacy 
#    try all combinations of random effects and state which is the best option 
#    (a) only random intercept
model1 <- lmer(intimacy ~ time + (1|id), data = data)

model_parameters(model1)

#    (b) random intercept + random time slope
model2 <- lmer(intimacy ~ time + (time||id), data = data)

model_parameters(model2)

#    (c) random intercept + random time slope + their cov
model3 <- lmer(intimacy ~ time + (time|id), data = data)

model_parameters(model3)

# Model comparison
anova(model1, model2)
anova(model2, model3)
anova(model1, model3)

# Model 3 - random time slope, intercept and their cov is the best model
## Model equations:
### level-1 equation:  RTij = b0i + b1i * time + e_i
### level-2 equations: b0i = gamma00 + U0i
###                    b1i = gamma01 + U1i
## The parameter of time is 0.08, it is the mean rate of change in intimacy in the sample.
## It is also noted by 'gamma01' in the models equations.


# 3. the quadratic time growth model for predicting intimacy
#    try 2 models and state which is the best option :
#    (a) keep the same random effects as in the best model you found in (2)
model4 <- lmer(intimacy ~ time + poly(time, 2) + (time|id), data = data)

model_parameters(model4)
#    (b) add the random quadratic effect (no need to check for the cov with other
#        random effects now)
model5 <- lmer(intimacy ~ time + poly(time, 2) + (time|id) + (0 + I(time^2)||id), data = data)

model_parameters(model5)


anova(model4, model5) # The addition of random slope for the quadratic effect is not significant

# The quadratic effect in model4 is -0.66. This means that the rate of change in
# intimacy is decreasing (by 2 * -0.66 per session) as the participant advaces in the sessions.


## Adding treatment to the model:

# 4. Use the best LINEAR model (even if you found the quadratic as better in (3), 
#     we want to keep it simple for now.)
#     Add treatment as a predictor to the best linear time model (start with adding
#     treatment as a main effect, with out the interaction)
#     - what can you tell from the fixed effects?

model3.1 <- lmer(intimacy ~ treatment + time + (time|id), data = data)

model_parameters(model3.1)

# The coefficient of 'treatment' is 0.27, and it is the difference in mean intimacy
# between the treatment groups. treatment group 1 is 0.27 higher in intimacy than group 0.



# 5. Add the time X treatment cross-level interaction (moderated linear growth model)
model3.2 <- lmer(intimacy ~ time * treatment + (time|id), data = data)

model_parameters(model3.2)

# The interaction is not significant...

#    - If the interaction is sig, use the emmeans package for simple slope analysis 
#     (compute the slope for each group)
