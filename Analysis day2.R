library(ggplot2)
library(dplyr)
library(gapminder)
glimpse(mtcars)
glimpse(gapminder)

glimpse(starwars)

mtcars %>%
  ggplot(aes(mpg))+
  geom_histogram(bins = 10)+
  facet_wrap(~cyl)

gapminder %>%
  filter(year==2007) %>%
  ggplot(aes(gdpPercap,col=continent))+
  geom_boxplot()

gapminder %>%
  filter(year==2007) %>%
  ggplot(aes(gdpPercap))+
  geom_boxplot()+
  facet_wrap(~continent)

library(tidyverse)
library(palmerpenguins)
library(ggthemes)

data(package = 'palmerpenguins')
glimpse(penguins)

ggplot (
  data= penguins,
  mapping = (aes(x=flipper_length_mm,y=body_mass_g))
)+
  geom_point(aes( color=species,shape=species))+
  geom_smooth(method="lm")+
  labs(
    title="Body mass and flipper length",
      subtitle="Dimentions for different species",
      x="fliper Length (mm)",y="body mass(g)",
      color= "species",shape="species"
      )+
  scale_color_colorblind()

# visualise distribution

ggplot(penguins,aes(x=species))+
geom_bar()

ggplot(penguins,aes(x=fct_infreq(species)))+
  geom_bar(width = 0.5, color = "blue") +
  labs(x ="Worker", y = "Answers") +
  geom_text(stat ='count', aes(label = after_stat(count), vjust =-0.5)) +
  ggtitle("Total iterations")

ggplot(penguins,aes(x=body_mass_g))+
  geom_histogram(binwidth = 200)

#mtcars

correlation_matrix<- cor(mtcars) 
print(correlation_matrix)

pairs(mtcars)
library(tidyr)
library(tribble)

cor_df=as.data.frame(correlation_matrix)

##regression
linear_model<- lm(mpg~hp, data = mtcars)

summary(linear_model)

