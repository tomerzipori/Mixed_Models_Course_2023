# Exercise --------------------------------------------------------------------------

library(ggplot2)
library(lmerTest)
library(parameters)
library(emmeans)


# This is longitudinal data* from a study where a new teaching program at inner-city schools
# was developed to help teachers engage with parents.

par_teach <- read.csv("parent-teach.csv")
par_teach <-read.csv(file.choose()) # If you are not working with r.project

head(par_teach)
# Teacher - Teacher ID
# time - time (weeks) since start of school year.
#        The program started at time = 6.
# Eval - the DV, how parents rated their children's teachers 
# Group - control (old) or new program



ggplot(par_teach, aes(time, Eval, color = Group)) + 
  geom_point(alpha = 0.4, shape = 16) + 
  geom_smooth(se = FALSE)




# 1. Fit a full fixed+random linear growth model.
#    What is the correlation between the slopes and the Eval values at the start
#    of the program (time = 6)?

# We need to center `time` to be 0 when time=6 so that we get the correct
# correlation.
# We can do that in the formula with I(time - 6)
m1 <- lmer(Eval ~ I(time - 6) + (I(time - 6) | Teacher),
           data = par_teach)
VarCorr(m1)
# The correlation is 0.28 - teachers who had higher Evals at the start of the
# program tended to show a better improvement.
model_parameters(m1, effects = "random")
# However, this correlation is not significantly different from 0:
# r = 0.28, 95% CI [-0.13, 0.61]



# 2. Does the linear growth differ between the programs?
#    (What is the relevant pseudo-R2?)


m2 <- lmer(Eval ~ I(time - 6) * Group + (I(time - 6) | Teacher),
           data = par_teach)
VarCorr(m1) # sigma{time}=0.095
VarCorr(m2) # sigma{time}=0.091
(Pseudo_R2_time <- 1 - (0.091 / 0.095)^2)
# 8% of the variance in the linear effect of time is explained by the different
# programs the teachers were in.

model_parameters(m2, ci_method = "S")
# However, the effect of the program on the linear growth (i.e., the
# interaction) is not significant:
# b = -0.06, SE = 0.03, 95% CI [-0.13, 0.00], t(48) = -1.88, p = .067






# 3. Does a quadratic growth model better describe the data?
#    - Compare to the fixed-quadratic random-linear model to the full linear growth model.
# m2.quad <- lmer(Eval ~ poly(time, 2) + (time | Teacher),
#                 data = par_teach)
m2.quad <- lmer(Eval ~ time + I(time ^ 2) + (time | Teacher),
                data = par_teach)
anova(m2, m2.quad)
# AIC supports the linear growth model
# BIC supports the quadratic growth model
# NHST is significant, supporting the quadratic growth model
# The next step would be to add a random slope for `I(time ^ 2)`

#    - What is the slope of time at week 0, week 6 and week 15?
b <- fixef(m2.quad)
time <- c(0, 6, 15)
b["time"] + 2 * time * b["I(time^2)"] # first derivative

# We can also use emmeans:
emtrends(m2.quad, ~ time, var = "time", 
         at = list(time = c(0, 6, 15)))




# 4. Does a piece-wise discontinuity model fit better than a quadratic model?
#    Note: these are NOT nested models!
par_teach$event <- par_teach$time > 6
m2.pw <- lmer(Eval ~ time + event + (time | Teacher),
              data = par_teach)
BIC(m2.pw, m2.quad) # the PW has a smaller BIC, so it supported:
# The next step would be to add a random slope for `event`

# Population level
ggplot(par_teach, aes(time, Eval)) + 
  geom_line(aes(y = predict(m2.quad, re.form = NA), color = "Quadratic")) +
  geom_line(aes(y = predict(m2.pw, re.form = NA), color = "Piece-wise"))

# Subject level
ggplot(par_teach, aes(time, Eval, group = Teacher)) + 
  geom_point(alpha = 0.4, shape = 16) +
  geom_line(aes(y = predict(m2.quad), color = "Quadratic")) +
  geom_line(aes(y = predict(m2.pw), color = "Piece-wise"))



