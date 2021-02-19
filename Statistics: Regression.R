library(tidyverse)

wood_density <- read_csv("Data/wood_density.csv")

### Scatter Plot
wood_density %>% ggplot(aes(x=Density,
                            y=Hardness))+
  geom_point()+
  geom_smooth(method="lm")

### Linear Model
density_model <- lm(Hardness~Density, data=wood_density)
density_model
# Note: This output means that Hardness = -1160.50 + 57.51 (Density)
# The value of the intercept is the timber hardness when wood density = zero.

### TASK. For wood density value of 24.7, calculate the predicted hardness.
(24.7*57.507)+-1160.5 # = 259.9229

coef(density_model)[1]+
  coef(density_model)[2]*
  24.7 # = 259.9152 (prevents rounding errors)

### Add predicted values to model, can be compared to real values.
fitted(density_model)


### Residuals
# the difference between these predictions or model-fitted values and the observed values
413-409.4325
427-265.6658
484-259.9152

### TASK. You can use the mutate() function to build a pipe which will add the models predictions 
# and residuals back onto our original dataframe.

wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions)