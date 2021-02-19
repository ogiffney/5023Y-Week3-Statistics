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

### Checking the overall explanatory power of a model

library(patchwork)
p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")
p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")
p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")
p1+p2+p3

### Broom Package

# Glance
broom::glance(density_model)
#Tidy
broom::tidy(density_model, conf.int=TRUE)
#Augment
broom::augment(density_model, wood_density, interval="confidence") 

# Plot to see confidence in the slope.
plot1 <- broom::augment(density_model, wood_density, interval="confidence") %>% ggplot(aes(x=Density, y=Hardness))+geom_line(aes(x=Density, y=.fitted))+geom_line(aes(x=Density, y=.upper), linetype="dashed")+geom_line(aes(x=Density, y=.lower), linetype="dashed")+geom_point() +ggtitle("Manually fitting linear model \n and confidence intervals")
plot2 <- wood_density %>% ggplot(aes(x=Density, y=Hardness))+geom_smooth(method=lm)+geom_point()+ggtitle("Geom smooth method to plotting \n a linear model")
plot1+plot2

### Write Up
# Timber Hardness can be accurately predicted by Wood Density, as is seen in the final two graphs where the slope matches perfectly and the confidence intervals are close to the line.