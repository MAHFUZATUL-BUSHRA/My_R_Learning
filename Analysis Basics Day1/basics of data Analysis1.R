colnames(gapminder)
library(gapminder)
library(DataExplorer)
create_report(gapminder,y="lifeExp")
library(SmartEDA)
ExpReport(airquality,op_file='smarteda_airquality_html')
library(dlookr)
diagnose_web_report(gapminder)

gapminder %>%
  eda_web_report(
    target = "lifeExp",
    output_format="html",
    output_file="eda_report_dlookr"
  )
install.packages("dplyr")
library(dplyr)
library(ISLR)
glimpse(Wage)
install.packages("ggstatsplot")
library(ggstatsplot)
ggbarstats(data=Wage, x=jobclass, y=education, label="both")

install.packages("flextable")
install.packages("expss")
library(expss)

available.packages()

data <- data.frame(
  numeric_col = c(1.5, 2.3, 3.7, 4.2, 5.5),
  category_col = c("A", "B", "A", "B", "C")
)
install.packages("skimr")
library(skimr)
skim(data$numeric_col)
?expss

help(package = "expss")

# Univariate analysis for numeric variables
result_numeric <- univar_numeric(data$numeric_col)
print(result_numeric)

# Diagnostic statistics for the dataset
result_diagnose <- univar_diagnose(data)
print(result_diagnose)

library(flextable)
dlookr:: describe(Wage) %>% flextable()

Wage %>%
  group_by(education) %>%
  univar_numeric()

gapminder %>%
  diagnose_numeric() %>%
  flextable()

gapminder %>%
  Select(-c(1:3)) %>%
  diagnose_numeric() %>%
  flextable()

install.packages("gtsummary")
library(gtsummary)
glimpse(trial)
colnames(trial)


subset<- trial %>%
  select(trt,age,grade,response)
tbl_summary(subset, by= trt)

install.packags("report")
library(report)
install.packages("sjPlot")
library(sjPlot)

str(Wage)

t.test(Wage ~ health, Data=Wage) %>%
  report()


library(tidyr)
install.packages("tidyr")

#variable assignment
my_var<- 5
my_var
library(gapminder)
gapminder
view(gapminder)

head(gapminder)
tail(gapminder)
str(gapminder)

name<- c("Mercury","Venus","earth","mars","jupiter","Saturn","Uranus","naptune")

type<-c("Terrestrial planet","Terrestrial planet","Terrestrial planet","Terrestrial planet",
        "Giant gas","Giant gas","Giant gas","Giant gas")
diameter<-c(0.382,0.949,1,0.532,11.209,9.449,4.007,3.883)
rotation<-c(58.64,-243.02,1,1.03,0.41,0.43,-0.72,0.67)
rings<-c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE)
planet_df<-data.frame(name,type,diameter,rotation,rings)
planet_df
View(planet_df)
planet_df[4,]

my_vector<- 1:9
my_vector
my_matrix <- matrix(1:9,ncol=3)
my_matrix

my_df <- mtcars[1:10,]
my_df

my_list <-list(vec=my_vector,mat=my_matrix,df=my_df)
my_list

my_list["vec"]


data <- read.csv("WHR2024.csv",header=TRUE)
data
View(data)

data2 <- read.csv("WHR2023.csv",header=TRUE)
data2
View(data2)
library(vcdExtra)
library(gapminder)

glimpse(data)
str(data)

#mutate

happiness_2023 <- data %>%
  mutate(Year=2023)

colnames(happiness_2024)
happiness_2024 <- data2 %>%
  mutate(Year=2024)

happiness_2023 %>%
   select (-c(Explained.by..Log.GDP.per.capita,          
               Explained.by..Social.support,              
               Explained.by..Healthy.life.expectancy,     
               Explained.by..Freedom.to.make.life.choices,
               Explained.by..Generosity,                  
               Explained.by..Perceptions.of.corruption)) %>% 
   View()
colnames(happiness_2024)
 
happiness_2024 %>%
  select (-c(Logged.GDP.per.capita,                     
             Social.support,                            
             Healthy.life.expectancy,                   
             Freedom.to.make.life.choices,              
             Generosity,
             Ladder.score.in.Dystopia,
             Perceptions.of.corruption,
             Standard.error.of.ladder.score,
             Explained.by..Log.GDP.per.capita,          
             Explained.by..Social.support,              
             Explained.by..Healthy.life.expectancy,     
             Explained.by..Freedom.to.make.life.choices,
             Explained.by..Generosity,                  
             Explained.by..Perceptions.of.corruption)) %>% 
  View()    

happiness<-rbind(happiness_2023,happiness_2024) # should same num of columns

View(happiness)
library(ggplot2)
gapminder %>%
  filter(year==2007) %>%
  count(continent,sort= TRUE) %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_col()+
  ggtitle("Frequency distribution of continents") %>% View()
View(gapminder)

gapminder %>%
  filter(year %in% c(2007,1952)) %>%
  group_by(year) %>% 
  summarize(mean_life_expentency=mean(lifeExp),
            median_life_expentency=median(lifeExp),
            std_life_expentency=sd(lifeExp))
gapminder %>%
  group_by(country) %>% 
  summarize(mean_life_expentency=mean(lifeExp),
            median_life_expentency=median(lifeExp),
            std_life_expentency=sd(lifeExp)) %>% View()

hist(gapminder$lifeExp, breaks=20, main="life expectancy frequency")
boxplot(gapminder$lifeExp, main="life expectancy frequency")
RlE=range(gapminder$lifeExp)
IQR_LE=IQR(gapminder$lifeExp)
var_lf=var(gapminder$lifeExp)
correlation_le=cor(gapminder$gdpPercap,gapminder$lifeExp)

library(moments)
life_freq= table(gapminder$lifeExp)
print(life_freq)

cof_of_var=sd(gapminder$lifeExp)/mean(gapminder$lifeExp)
skewness=skewness(gapminder$lifeExp)
kurtosis=kurtosis(gapminder$lifeExp)

