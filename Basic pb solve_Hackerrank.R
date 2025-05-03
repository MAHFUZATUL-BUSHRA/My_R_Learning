#Q1:Monthly Revenue Summary

#You are given a dataset containing order information for an e-commerce platform. Each record contains:

#order_id: A unique identifier for the order

#date: The date the order was placed (YYYY-MM-DD format)

#revenue: The revenue generated from the order

#Your task is to create a report that:

#Extracts the year and month from the date.

#Calculates total revenue per year for the following months only: January, February, June, July, October, and December.

#Displays each year as a row and the selected months as columns.

#Fills in 0 for any year-month combination that has no orders.

#Adds a final row labeled "Mean" that shows the average revenue for each of the selected months across all years.

# Load required libraries
library(dplyr)
library(lubridate)
library(tidyr)

# Read input
df <- read.csv(file("stdin"))

# Extract year and month
df <- df %>%
  mutate(
    date = as.Date(date),
    Year = year(date),
    Month = month(date, label = TRUE, abbr = TRUE)
  )

# Summarize revenue by Year and Month
monthly_summary <- df %>%
  group_by(Year, Month) %>%
  summarise(revenue = sum(revenue), .groups = 'drop')

# Pivot the table
pivot_df <- monthly_summary %>%
  pivot_wider(names_from = Month, values_from = revenue, values_fill = 0)

# Convert Year to character to avoid bind_rows error
pivot_df$Year <- as.character(pivot_df$Year)

# Create the mean row
mean_row <- pivot_df %>%
  select(-Year) %>%
  summarise(across(everything(), ~ round(mean(.), 0))) %>%
  mutate(Year = "Mean") %>%
  select(Year, everything())

# Combine and order columns
final_df <- bind_rows(pivot_df, mean_row)

# Order months correctly
month_order <- month.abb
final_df <- final_df %>%
  select(Year, all_of(intersect(month_order, names(final_df))))

# Output as CSV with quotes
write.csv(final_df, row.names = FALSE, quote = TRUE)

# Input

#"tweet_id","tweet_text","tweet_sentiment"
#"40b149d273","poor girl","negative"
#"e61d64740e","Yeah but it doesnt sound indie enough i need2learn some other tunes and then pick up mo style =] 1hour! Ill c u then ;)","neutral"
#"431ed00e5d","hey beautiful","positive"
#"fe530042f7","I shudder at the thought of what she was thinking shed do with it if shed managed to reclaim it...","neutral"
#"2ca5660e66","May 10 is Human Kindness Day.","neutral"
#"b59a33cbaf","needs 1000 words for 502 part 2 .... then freedom ..... until another few assignments and exams","neutral"
#"7c843939bb","ned to go to beathroom, dont know if i can reach/find clothes. need to walk outsie to get to bathroom. ok at night, not ok in daylight.","neutral"
#"9fb39d3c8c","Only has under 200 words left to write on her assignment","neutral"
#"c481433b33","hm... i dont I can recommend any white chocolates though.. you have to move to the dark side first..","neutral"
#"1a34426d09","my grandpa was telling me how they used to cut up human bodies in med s{-truncated-}

#Output: 

"word","positive","neutral","negative"
"clothes",0,1,0
"dress",0,0,1


# Q:Compute the number of tweets per sentiment category, and output the result in descending order of frequency, or alphabetically if tied .


# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(tm)  # for stopwords

# Read input
df <- read.csv(file("stdin"))

# Normalize and tokenize
df_words <- df %>%
  mutate(tweet_text = tolower(tweet_text)) %>%
  mutate(tweet_text = str_replace_all(tweet_text, "[^a-z\\s]", " ")) %>%  # remove punctuation
  separate_rows(tweet_text, sep = "\\s+") %>%
  rename(word = tweet_text, sentiment = tweet_sentiment) %>%
  filter(word != "") %>%
  filter(!word %in% stopwords("en"))  # remove stopwords

# Count words by sentiment
word_counts <- df_words %>%
  group_by(word, sentiment) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = sentiment, values_from = count, values_fill = 0) %>%
  arrange(word)

# Output result
write.csv(word_counts, row.names = FALSE, quote = TRUE)

#input Sample
tweet_id,tweet_text,tweet_sentiment
"40b149d273","poor girl","negative"
"e61d64740e","... pick up mo style ...","neutral"
"431ed00e5d","hey beautiful","positive"
...
"7c843939bb","ned to go to beathroom, don`t know if i can reach/find clothes.","neutral"

# Output
"word","positive","neutral","negative"
"beautiful",1,0,0
"beathroom",0,1,0
"clothes",0,1,0
"dress",0,0,1
"girl",0,0,1
"hey",1,0,0
"poor",0,0,1
"style",0,1,0
