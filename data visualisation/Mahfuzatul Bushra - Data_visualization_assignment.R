#Data Visualization Assignment 
#The following section of code is to run the dataset
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(highcharter)
library(stringr)

dataset <- read.csv(file.choose()) #if you run this line of code
#you will be asked to choose the csv file. Please choose the file that is 
#given for your assignment and run. This data will be used for rest of the visualization work

str(dataset)


#Task_1: Visualize the performance of the students based on their absence days. Based on your visualization make 
#a decision that is being present in the class is an important factor for being a good student?
#{Hint: Here you have one column named "class" that column shows the performance of the students
#M- Medium, L-Lower, H-Higher}

ggplot(
     data=dataset , aes(x=Class, fill= StudentAbsenceDays)) + geom_bar()+
       labs(title = "The performance of the students based on absence days")





#Task_2: We all know a sentence that "School starts at home". So relation with our parents affects our study as well. 
#So, here in the dataset there is a column named "relation" where it is stated you have relation with whom. Now your work is to 
#analyze and visualize that which students has higher performance index, the one who has good relation with mom or the one has good relation with dad?
#{Hint: Here you have one column named "class" that column shows the performance of the students
#M- Medium, L-Lower, H-Higher}

ggplot(
  data=dataset , aes(x=Class, fill= Relation)) + geom_bar()+
  labs(title = "The performance of the students based on relation with parents ")




#Task_3: "The more the resources, the better the student is performing" - you have to prove if this statement is correct 
#or not by analyzing the dataset. Analyze and visualize that relationship between the student's class and Visited Resources.
ggplot(
  data=dataset ,aes(x=Class, y= VisITedResources ,fill= Class)) +geom_boxplot() +
  labs(title="The performance of the students based on most visited Resources ")


#Task_4: Based on gender find out where are the most of the students from. Visualize the result.

country_based_on_gender <- dataset %>%
  filter(!is.na(gender)) %>%
  filter(gender %in% c('M','F')) %>%
  select( gender ,NationalITy) %>%
 # mutate(gender = str_split(gender, pattern = ":")) %>%
  unnest(gender) %>%
  group_by(gender, NationalITy) %>%
  summarise(Count= n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(gender = reorder(gender,Count)) %>%
  hchart('column',hcaes('gender','Count',group= 'NationalITy')) %>%
  hc_title(text = "Nationality based on Gender")

country_based_on_gender
  


#Task_5: Find out relationship between Gender and the topic they are studying. Find out who is dominating. 



Topic_based_on_gender <- dataset %>%
  filter(!is.na(gender)) %>%
  filter(gender %in% c('M','F')) %>%
  select( gender ,Topic) %>%
  mutate(gender = str_split(gender, pattern =":")) %>%
  unnest(gender) %>%
  group_by(gender, Topic) %>%
  summarise(Count= n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(gender = reorder(gender,Count)) %>%
  hchart('column',hcaes('gender','Count',group= 'Topic')) %>%
  hc_title(text = "Topic Studying based on Gender")

Topic_based_on_gender
