SFON_data <- haven::read_sav("SFON.sav")
SFON_data$Task <- factor(SFON_data$Task, labels = c("Bird", "Truck"))
# This data was collected from 46 children aged 3-5. 
# They were given two versions (Truck/Bird) of the same task: In each version,
# they played with a toy together with an experimenter. During each of "game",
# they're behavior was codded as either showing some attention to the
# quantitative properties of the game (counting, saying numbers, etc.) or not.
# Additionally, the children ability to discriminate between quantities was
# measured.
head(SFON_data)
#      ID - Child ID.
#    Task - Which of the tasks (Bird or Truck)
#     Age - Child's age in years
# weberFr - quantitative discrimination ability (smaller numbers are better)
#  Attend - Did the child attend to the quantitative properties of the game?

# Data from here:
# https://psyarxiv.com/mt3n9/


# We are interested in:
# Do children with better quantitative discrimination (smaller weberFr) tend to
# attend to quantitative information with a higher probability than those with
# worse quantitative discrimination?
# We must also control for Age.



# Exercise ---------------------------------------------------------------

# 1. Think about that data:
# - Which effects do we have?
# - What level are each of these effect in?
# - How is the data nested?

# 2. Plot the data somehow.

# 3. Fit a Binomial (logistic) random intercepts model.
# - Does it have a better fit than a fixed intercept only model?
# - What is the ICC?
# - On average, do children tend to attend more or less?

# 4. Fit a conditional model with a fixed effect for Age.
# - How much variation in the random intercept is explained by Age?

# 5. Fit a conditional model with a fixed effect for weberFr (on top of Age).
# - Does it have a better fit than the previous model?
# - What what is the slope of weberFr? Is it in the expected direction?
# - How much additional variation in the random intercept is explained by weberFr?
  
