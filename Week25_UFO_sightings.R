#required pacakges

library(tidyverse)
#install.packages("stylo")
library(stylo)
library(png)
library(ggplot2)
#library(devtools)
#devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

#Importing Data
ufo_sightings <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
head(ufo_sightings)

#-----------------#
# Count the occurrences of each word in the summary column
word_counts <- ufo_sightings %>%
  mutate(words = strsplit(summary, " ")) %>%
  unnest(words) %>%
  count(words)

# Rename the count column to accurately represent word count
names(word_counts)[2] <- "count"

#-----------------#
# Function to remove special characters and numbers
remove_special_chars_numbers <- function(text) {
  gsub("[^A-Za-z]", "", gsub("[0-9]", "", text))
}
#-----------------#
# Apply the function to the 'words' column
word_counts$words <- sapply(word_counts$words, remove_special_chars_numbers)
#-----------------#
#remove blank rows
new_df <-word_counts[!(is.na(word_counts[1]) | word_counts[1]==""), ]
new_df <- new_df %>% arrange(desc(count))
#-----------------#
#delete pronouns 
delete.stop.words(new_df$words, 
                  stop.words = stylo.pronouns(corpus.lang = "English"))
#-----------------#
#removing words with less than 4 char
new_df = new_df[(which(nchar(new_df$words) >= 4)),]
#-----------------#
#removing low frequency words
new_df =new_df[!rowSums(new_df<700),]
#-----------------#
#removing words from list
remove.list <-  paste(c("from","were", "about","seen","then","like","with","into","This", "until", "which","between","this","each","that","very","Note","than","have","wife", "NUFORC"), collapse = '|')
new_df<-new_df[!grepl(remove.list, new_df$words),]
#-----------------#
#replacing some words
new_df$words[new_df$words == 'light'] <- 'lights'
#-----------------#
#merging and summing rows with same words
new_df$count=with(new_df, c(count[1],count[-1]+count[-nrow(new_df)]))
new_df = new_df[-1,]

#-----------------#
#Drawing a lettercloud with Wordcloud2 package
set.seed(123)
letterCloud(new_df,word="UFOs", color="blue", size=1.5, backgroundColor="white")