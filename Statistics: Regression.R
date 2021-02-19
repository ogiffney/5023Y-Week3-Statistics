library(tidyverse)

wood_density <- read_csv("Data/wood_density.csv")

wood_density %>% ggplot(aes(x=Density,
                            y=Hardness))+
  geom_point()+
  geom_smooth(method="lm")

