## Null hypothesis: There is no correlation between the height of the seedling and if the plant was self or cross fertilised

library(skimr)
library(tidyverse)

darwin <- read_csv("Data/darwin.csv")
skim(darwin)

darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"),names_to="type",values_to="height")

darwin %>% 
  ggplot(aes(x=height))+geom_histogram()

darwin %>% 
  ggplot(aes(x=type, y=height))+
  geom_jitter(width=0.1, pch=21, aes(fill=type))+
  theme_classic()

model1 <- lm(height~1, data=darwin)
model1

darwin %>% 
  ggplot(aes(x=type,y=height))+
  geom_jitter(width=0.1,pch=21,aes(fill=type))+
  theme_classic()+
  geom_abline(intercept=18.88,slope=0,linetype="dashed")

darwin %>% 
  summarise(mean=mean(height))

model2 <- lm(height~type, data=darwin)
model2