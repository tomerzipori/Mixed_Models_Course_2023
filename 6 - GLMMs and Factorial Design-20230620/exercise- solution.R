


# Exercise ---------------------------------------------------------------

library(ggplot2)
library(lme4)
library(performance)
library(parameters)

SFON_data <- haven::read_sav("6 - GLMMs and Factorial Design-20230620/SFON.sav")
SFON_data$Task <- factor(SFON_data$Task, labels = c("Bird", "Truck"))


# 1. Think about that data ---------------------------------------------
# - Which effects do we have?
# - What level are each of these effect in?
# - How is the data nested?


# The 8 observations are nested in 2 Tasks and in 46 children. For now, we will
# ignore Task.
# The effect for Age is a between-person effect (level 2), as is the effect for
# weberFr. There are no level 1 predictors.






# 2. Plot the data somehow ---------------------------------------------

ggplot(SFON_data, aes(weberFr, Attend)) + 
  stat_summary(aes(group = ID), geom = "point")
# Each point is a person - looks like people with larger weberFr do show a
# smaller probability of attending to quantitative information.









# 3. Fit a Binomial (logistic) random intercepts model ------------------

m1 <- glmer(Attend ~ 1 + (1| ID), 
            family = binomial(),
            data = SFON_data)


# - Does it have a better fit than a fixed intercept only model?
m0 <- glm(Attend ~ 1, 
          family = binomial(),
          data = SFON_data)
anova(m1, m0)
# Yes: X^2(1) = 139.93, p<.001, BIC and AIC are smaller for m1.


# - What is the ICC?
icc(m1)
# ICC = 0.718
# Indicating strong behavioral consistency between "games".

# - On average, do children tend to attend more or less?
(b0 <- fixef(m1))
plogis(b0) # convert log(Odds) to Pr(y=1)
# Overall, attend only 17% of the times.







# 4. Fit a conditional model with a fixed effect for Age -----------------
m2 <- glmer(Attend ~ Age + (1| ID), 
            family = binomial(),
            data = SFON_data)



# - How much variation in the random intercept is explained by Age?
VarCorr(m1)
VarCorr(m2)

c(pseudo.R2 = 1 - (2.6068/2.8961)^2)
# 18% of between person variation in the tenancy to attend quant. info is
# attributable to age.




# 5. Fit a conditional model with a fixed effect for weberFr (on top of Age) ----------------
m3 <- glmer(Attend ~ weberFr + Age + (1| ID), 
            family = binomial(),
            data = SFON_data)



# - Does it have a better fit than the previous model?
anova(m3, m2)
# X^2(1) = 5.2134, p = .022
# BIC smaller for m2, while AIC is smaller for m3.


# - What what is the slope of weberFr? Is it in the expected direction?
model_parameters(m3, exponentiate = TRUE, standardize = "pseudo")
# An increase of 1 SD in weberFr is related to a 3-time reduction the odds of
# attending quant. info.



# - How much additional variation in the random intercept is explained by weberFr?
VarCorr(m3)

pR2 <- c(pseudo.R2_m2 = 1 - (2.6068/2.8961)^2,
         pseudo.R2_m3 = 1 - (2.4988/2.8961)^2)
pR2
# m3 accounts for 25% of the variance between children, but this includes also
# the variance accounted for by age.
# The difference between the pseudo-R2 is:
diff(pR2)
# So we can say that an *additional* 6.5% of the variance is explained uniquely
# by weberFr.



