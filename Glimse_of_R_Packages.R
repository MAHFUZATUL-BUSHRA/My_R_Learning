######## Automated Exploratory Data Analysis (EDA) Reports  ##########
#Importing the Required Library
#install.packages("dplyr") #If the package is not installed yet then install the package first

#Then load the package
library(dplyr)

#install.packages("gapminder")
library(gapminder)

# We will be using two built in datasets (diamonds & gapminder) that are loaded in 
# "ggplot2"(tidyverse) and "gapminder" package respectively.

#Have a look at the top rows of the datasets
head(gapminder)


head(diamonds) #You get an error as'diamons' data frame in loaded in 'ggplot2' package

#let's first install the "ggplot2" package
install.packages("ggplot2")

library(ggplot2)
head(diamonds)

#To find out more details we can call the str (structure) function
str(gapminder)
str(diamonds)

#To look at the summary of the datasets let's call "summary" function
summary(gapminder)

#How about we perform the Exploratory Data Analysis using a single line of code?

install.packages("DataExplorer")
library(DataExplorer)

create_report(gapminder) #Here you go! With writing a single line of codes you have the missing value information
                        # Data Structure, Univariate Distributions, Correlation Analysis, and even PCA.
                        # Check the report in your browser
#It also shows the output of Principle Component Analysis


# Let's create the report with a response variable 
# In Regression Analysis Type Scenario where say, 'lifeExp' is the Response/Dependent Variable

colnames(gapminder) #checking the column names of the dataset
create_report(gapminder, y = "lifeExp") #you have some extra information like boxplots and scatter plots


### Let's try "SmartEDA" package to achieve something similar, but better
install.packages("SmartEDA")
library(SmartEDA)

ExpReport(airquality, op_file = 'smarteda_airquality.html') #We're using the 'airquality' dataset here

#This shows density plot and scatter plot along with the other features


### Let's checkout another similar package "dlookr"
#install.packages("dlookr")
library(dlookr)

diagnose_web_report(gapminder)

gapminder %>% 
  eda_web_report(
    target = "lifeExp",
    output_format = "html",
    output_file = "EDA_gapminder_dlookr.html"
  )

### Let's load the 'Wage' dataset
install.packages("ISLR")
library(ISLR)
glimpse(Wage)

install.packages("ggstatsplot")
library(ggstatsplot)

ggbarstats(data = Wage, x = jobclass, y = education, label = "both")

install.packages("flextable")
library(flextable)

dlookr::describe(Wage) %>% flextable()

Wage %>% 
  group_by(education) %>% 
  univar_numeric()

gapminder %>% 
  diagnose_numeric() %>% 
  flextable()

gapminder %>% 
  select(-c(1:3)) %>% 
  diagnose_numeric() %>% 
  flextable()







######### Let's use one of the most important package built for Publication Standard Output   ############
#gtsummary
#Resource: https://yuzar-blog.netlify.app/posts/2022-10-31-gtsummary/

installed.packages("gtsummary")
library(gtsummary)
#It has a loaded dataset called 'trial'

glimpse(trial)
str(trial)

?trial #help function to learn more about trial
colnames(trial)

subset <- trial %>% 
  select(trt, age, grade, response)

#summarize data
tbl_summary(subset, by = trt)


subset %>% 
  tbl_strata(
    strata = grade,
    ~ .x %>% 
      tbl_summary(by = trt)
  )

# summarize statistical tests
subset %>% 
  tbl_summary(by = trt) %>% 
  add_p()

# There is more to this

subset %>% 
  tbl_summary(by = trt) %>% 
  add_p() %>% 
  add_overall() %>% 
  add_n() %>% 
  add_ci() %>% 
  add_stat_label(
    label = all_continuous() ~ "Median (IQR)"
  )

# Instead of add_p(), we can use the add_difference() to test the difference between two groups

subset %>% 
  tbl_summary(by = trt) %>% 
  add_difference() %>% 
  add_overall() %>% 
  add_n() %>% 
  add_ci() %>% 
  add_stat_label(
    label = all_continuous() ~ "Median (IQR)"
  )


# If our data follows Normal Distribution

trial %>% 
  tbl_summary(
    by = trt,
    statistic = all_continuous() ~ "{mean} ({sd})") %>% 
  add_p (test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "fisher.test"
                    )
    )

# We can use any statistic

trial %>% 
  tbl_summary(
    by = trt,
    statistic = list(
      age ~ "{mean} ({sd})",
      marker ~ "{mean}({min}, {p25}, {p75}, {max})",
      stage ~ "{n} / {N} ({p}%)"
                    )
             ) %>% 
  add_p (test = list(
    age ~ "t.test",
    marker ~ "wilcox.test",
    stage ~ "fisher.test"
                    )
        ) %>% 
  separate_p_footnotes()

trial_paired <-
  trial %>%
  select(trt, marker, response) %>%
  group_by(trt) %>%
  mutate(id = row_number()) %>%
  ungroup()

trial_paired %>%
  filter(complete.cases(.)) %>%
  group_by(id) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  tbl_summary(by = trt, include = -id) %>%
  add_p(test = list(
    marker   ~ "paired.t.test",
    response ~ "mcnemar.test"), 
    group    = id)



###Logistic Regression
uvlm_table <- trial %>%
  select(response, trt, age, grade) %>%
  tbl_uvregression(
    method       = glm,
    y            = response,
    method.args  = list(family = binomial),
    exponentiate = TRUE
  ) 

uvlm_table

# Saving the tables
library(flextable)

uvlm_table %>%
  as_flex_table() %>% 
  save_as_image(path = "lr_output.png")

uvlm_table %>%
  as_flex_table() %>%
  save_as_docx(path = "lr_output.docx") 





### Now look at the "report" package for Reporting Statistical Results of Tests, Models
#It provides interpretation

install.packages(c("ISLR", "report", "sjPlot"))
library(ISLR)
library(report)

str(Wage)

t.test(wage ~ jobclass, data = Wage) %>% 
  report()

wilc_test <- wilcox.test(Wage$wage ~ Wage$jobclass)
report(wilc_test)
  

cor.test(mtcars$mpg, mtcars$wt,
         method = "spearman", exact = F) %>% 
  report()

#If we want to report in table
cor.test(mtcars$mpg, mtcars$wt,
         method = "spearman", exact = F) %>% 
  report_table()


###How it handles models

#Usual way,
model <- lm(mpg ~ am + hp, data = mtcars)
summary(model)

#using 'repor' package
model <- lm(mpg ~ am + hp, data = mtcars)
report(model)

#If we want only the essential informaion
report(model) %>% summary()



### Let's checkout the 'sjPlot' package
library(sjPlot)
view_df(Wage, show.frq = T, show.prc = T, show.na = T)

Wage %>% 
  plot_frq(education)

Wage %>% 
  group_by(race) %>% 
  plot_frq(education) %>% 
  plot_grid()

#Clustered Bar Chart
plot_grpfrq(
  var.cnt = Wage$education,
  var.grp = Wage$jobclass
)

# as stacked proportional bars
plot_xtab(
  x   = Wage$education, 
  grp = Wage$jobclass, 
  margin  = "row", 
  bar.pos = "stack",
  show.summary = TRUE,
  coord.flip   = TRUE)


# Crosstab

tab_xtab(
  var.row = Wage$education, 
  var.col = Wage$jobclass, 
  show.row.prc = T)


# Histogram
Wage %>% 
  group_by(jobclass) %>% 
  plot_frq(wage, type = "histogram", show.mean = TRUE, normal.curve = TRUE) %>% 
  plot_grid()


#Plot likert scales Data
install.packages("likert")
library(likert)

data(pisaitems)
head(pisaitems)

d <- pisaitems %>% 
  dplyr::select(starts_with("ST25Q"))

view_df(d, show.frq = T, show.prc = T)

plot_likert(d)



###Plotting Models

m <- lm(wage ~ education, data = Wage)
plot_model(m, type = "pred")

summary(m)

plot_model(m, show.values = TRUE, width = 0.1)+
  ylab("Increase in salary as compared to no education")

tab_model(m, 
          show.reflvl = T, 
          show.intercept = F, 
          p.style = "numeric_stars")
