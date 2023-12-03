# Author: Bryan D'Amico
# Date: March 21, 2023
# Purpose: Data Processing and Plot Creation for Final Project Poster

#######################################################
#
# Loading required packages
#
#######################################################

library(tidyverse)
library(RColorBrewer)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)

#######################################################
#
# Importing the data set
#
#######################################################

#Choosing VideoGamesSales.csv file downloaded from Kaggle
fname <- file.choose()

#Importing the data
games <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE
                  , na.strings = "NA")

#######################################################
#
# Confirming Data Size Requirement
#
#######################################################

#Checking structure of data frame
str(games)

#Checking the dimension of the data frame
dim(games)
#There are 1907 rows and 13 columns

#Note that I will be using 12 for the number of variables since the index column
#is just a unique identifier and not actual data.
num_cols <- 12
num_rows <- 1907

Size_Requirement <- (num_cols * 4) * (num_rows / 100)

if (Size_Requirement >= 100) {
  print("good to go")
}
Size_Requirement

#######################################################
#
# Data Preparation and Cleaning
#
#######################################################

#############################
#Filling in Missing Year Values 
# and Publisher Names
#############################

#These were looked up individually online based on the game title and platform

games[games$index == 143, "Year"] <- 2003
games[games$index == 253, "Year"] <- 1999
games[games$index == 301, "Year"] <- 2003
games[games$index == 373, "Year"] <- 2005
games[games$index == 434, "Year"] <- 2008
games[games$index == 523, "Year"] <- 2007
games[games$index == 524, "Year"] <- 2001
games[games$index == 573, "Year"] <- 2008
games[games$index == 582, "Year"] <- 2006
games[games$index == 646, "Year"] <- 2008
games[games$index == 746, "Year"] <- 2002
games[games$index == 936, "Year"] <- 2007
games[games$index == 1016, "Year"] <- 1998
games[games$index == 1030, "Year"] <- 2010
games[games$index == 1275, "Year"] <- 2002
games[games$index == 1318, "Year"] <- 2002
games[games$index == 1326, "Year"] <- 2013
games[games$index == 1386, "Year"] <- 2008
games[games$index == 1424, "Year"] <- 2007
games[games$index == 1526, "Year"] <- 1999
games[games$index == 1554, "Year"] <- 1997
games[games$index == 1631, "Year"] <- 2002
games[games$index == 1676, "Year"] <- 2003
games[games$index == 1716, "Year"] <- 2003
games[games$index == 1752, "Year"] <- 2005
games[games$index == 1759, "Year"] <- 2011
games[games$index == 1784, "Year"] <- 2005
games[games$index == 1819, "Year"] <- 2010
games[games$index == 1899, "Year"] <- 2006
games[games$index == 373, "Publisher"] <- "THQ"
games[games$index == 1016, "Publisher"] <- "Electronic Arts"

#############################
# Creating the Generations Variable
#############################

#The data is cut based on the starting year of each console generation
#Based on the intervals created, games within those years are assigned the
#corresponding generation
games$Generation <- cut(games$Year, breaks = c(1983, 1987, 1993
                                               , 1998, 2005, 2013),
                        labels = c("Gen3", "Gen4", "Gen5", "Gen6", "Gen7"),
                        right = FALSE)

#############################
# Creating the Ecosystem Variable
#############################

#Looking at the list of platforms contained in the data set
unique(games$Platform) 

#Making a vector of all the Nintendo consoles in the data
Nintendo_Platforms <- c("NES", "GB", "Wii", "DS", "GBA", "N64", "3DS", "GC"
                        , "WiiU", "SNES")

#Making a vector of all the Sony consoles in the data
Sony_Platforms <- c("PS2", "PS3", "PS", "PSP", "PSV")

#Making a vector of all the Microsoft consoles in the data
Microsoft_Platforms <- c("XB", "X360")

#Making a vector of all the Sega conosles in the data
Sega_Platforms <- c("GEN", "DC", "SAT", "SCD")

#Using nested if-else statements to assign the ecosystem variable based on which
#platform each game title was released on
games <- games %>%
  transform(Ecosystem = ifelse(Platform %in% Sony_Platforms, "Sony", 
                               ifelse(Platform %in% Microsoft_Platforms, "Microsoft",
                                      ifelse(Platform %in% Nintendo_Platforms, "Nintendo",
                                             ifelse(Platform %in% Sega_Platforms, "Sega", "PC")))))

#############################
# Removing PC games
#############################

#This analysis is only about console games
games <- games %>%
  filter(Ecosystem != "PC")

#######################################################
#
# Main Question and Multidimensional Plot
#
#######################################################

#Creating a new data frame based on Ecosystem sales
Ecosystem_Popularity <- games %>%
  group_by(Ecosystem, Year, Generation) %>%
  summarize(total_sales = sum(Global)) %>%
  filter(!is.na(Generation))

#If an ecosystem had no sales in a given year that row is missing instead of 
#showing a row with a sales figure of 0, this creates gaps in the plot
#Creating missing rows for Sega no sales years
Ecosystem_Popularity2 <- data.frame(Ecosystem = c("Sega", "Sega"),
                                    Year = c(1996, 1997),
                                    Generation = c("Gen5", "Gen5"),
                                    total_sales = c(0, 0),
                                    stringsAsFactors = FALSE) 
#Appending this information to the data frame
Ecosystem_Popularity <- rbind(Ecosystem_Popularity, Ecosystem_Popularity2)

#Turning Ecosystem into a factor to control the vertical order in which they
#appear on the plot
Ecosystem_Popularity$Ecosystem <- factor(Ecosystem_Popularity$Ecosystem
                                         , levels = c("Microsoft", "Sony", "Sega", "Nintendo"))

#Creating the main visualization
ggplot(Ecosystem_Popularity, aes(x = Year, y = total_sales, fill = Ecosystem)) +
  geom_area() +
  scale_fill_manual(values=c("olivedrab2",
                                         "steelblue1",
                                         "#CC66FF",
                                         "#E75C19")) +
                                           labs(x = "Release Year", y = "Sales in Millions of Units") +
  scale_x_continuous(breaks = seq(1983, 2012, 1)
                     , labels = seq(1983, 2012, 1))

#######################################################
#
# Supporting Questions and Multidimensional Plots
#
#######################################################

#Question 2:
# Who were the major publishers in each console generation?

#Gen 3
##########################################
Gen3_Pubs <- games %>%
  filter(Generation == "Gen3") %>%
  group_by(Publisher) %>%
  summarize(sales = sum(Global)) %>%
  arrange(desc(sales))

pie(Gen3_Pubs$sales, labels = Gen3_Pubs$Publisher
    , col = gray.colors(4))
##########################################

#Gen 4
##########################################
Gen4_Pubs <- games %>%
  filter(Generation == "Gen4") %>%
  group_by(Publisher) %>%
  summarize(sales = sum(Global)) %>%
  arrange(desc(sales))

Gen4_Pubs$Publisher_Names <- ifelse(Gen4_Pubs$sales > 9
                                    , Gen4_Pubs$Publisher, "Others")

Gen4_Pubs <- Gen4_Pubs %>%
  group_by(Publisher_Names) %>%
  summarize(sales = sum(sales))

pie(Gen4_Pubs$sales, labels = Gen4_Pubs$Publisher_Names
    , col = gray.colors(5))
##########################################

#Gen 5
##########################################
Gen5_Pubs <- games %>%
  filter(Generation == "Gen5") %>%
  group_by(Publisher) %>%
  summarize(sales = sum(Global)) %>%
  arrange(desc(sales))

Gen5_Pubs$Publisher_Names <- ifelse(Gen5_Pubs$sales > 15
                                    , Gen5_Pubs$Publisher, "Others")

Gen5_Pubs <- Gen5_Pubs %>%
  group_by(Publisher_Names) %>%
  summarize(sales = sum(sales))

pie(Gen5_Pubs$sales, labels = Gen5_Pubs$Publisher_Names
    , col = gray.colors(5))
##########################################

#Gen 6
##########################################
Gen6_Pubs <- games %>%
  filter(Generation == "Gen6") %>%
  group_by(Publisher) %>%
  summarize(sales = sum(Global)) %>%
  arrange(desc(sales)) 

Gen6_Pubs$Publisher_Names <- ifelse(Gen6_Pubs$sales > 80
                                    , Gen6_Pubs$Publisher, "Others")

Gen6_Pubs <- Gen6_Pubs %>%
  group_by(Publisher_Names) %>%
  summarize(sales = sum(sales))

pie(Gen6_Pubs$sales, labels = Gen6_Pubs$Publisher_Names
    , col = gray.colors(5))
##########################################

#Gen 7
##########################################
Gen7_Pubs <- games %>%
  filter(Generation == "Gen7") %>%
  group_by(Publisher) %>%
  summarize(sales = sum(Global)) %>%
  arrange(desc(sales))

Gen7_Pubs$Publisher_Names <- ifelse(Gen7_Pubs$sales > 150
                                    , Gen7_Pubs$Publisher, "Others")

Gen7_Pubs <- Gen7_Pubs %>%
  group_by(Publisher_Names) %>%
  summarize(sales = sum(sales))

pie(Gen7_Pubs$sales, labels = Gen7_Pubs$Publisher_Names
    , col = gray.colors(5))
##########################################


# Question 3: 
# What were the most popular genres by sales in each region?

#Grouping by genre and finding the sum of sales in each region
Genre_sales <- games %>%
  group_by(Genre) %>%
  summarize(NA_sales = sum(North.America),
            EU_sales = sum(Europe),
            JP_sales = sum(Japan),
            Rest_sales = sum(Rest.of.World))

#Transforming the wide data set to a long format for plotting
Genre_sales <- gather(Genre_sales, Region, Sales, NA_sales:Rest_sales, factor_key = TRUE)

#Plotting sales by genre for North America
Genre_sales %>%
  filter(Region == "NA_sales") %>%
  ggplot(aes(x = fct_reorder(Genre, Sales), y = Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ggtitle("Sales in North America by Genre") +
  labs(x = "Units Sold in Millions", y = "Genre") +
  scale_y_continuous(limits = c(0, 400))

#Plotting sales by genre for Europe
Genre_sales %>%
  filter(Region == "EU_sales") %>%
  ggplot(aes(x = fct_reorder(Genre, Sales), y = Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ggtitle("Sales in Europe by Genre") +
  labs(x = "Units Sold in Millions", y = "Genre") + 
  scale_y_continuous(limits = c(0, 400))

#Plotting sales by genre for Japan
Genre_sales %>%
  filter(Region == "JP_sales") %>%
  ggplot(aes(x = fct_reorder(Genre, Sales), y = Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ggtitle("Sales in Japan by Genre") +
  labs(x = "Units Sold in Millions", y = "Genre") +
  scale_y_continuous(limits = c(0, 400))


# Question 4: 
#   What consoles had most highly reviewed games?

#Creating ordered boxplots of review scores for each console
ggplot(games, aes(x = fct_reorder(Platform, Review), y = Review)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Distribution of Review Scores by Console") +
  labs(x = "Console", y = "Review Score") 

#######################################################
#
# One Dimensional Plots and Supporting Visualization
#
#######################################################

#Games per console
games %>%
  group_by(Platform) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Platform = factor(Platform, levels = unique(Platform))) %>%
  ggplot(aes(x = Platform, y = count)) +
  geom_bar(color = "black", fill = "lightblue", stat = "identity") +
  ggtitle("Number of Games Per Console") +
  labs(x = "Console", y = "Number of Games")

#Games per Genre
games %>%
  group_by(Genre) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Genre = factor(Genre, levels = unique(Genre))) %>%
  ggplot(aes(x = Genre, y = count)) +
  geom_bar(color = "black", fill = "lightblue", stat = "identity") +
  ggtitle("Number of Games Per Genre") +
  labs(x = "Genre", y = "Number of Games")

#Games per Generation
games %>%
  group_by(Generation) %>%
  summarize(count = n()) %>%
  filter(!is.na(Generation)) %>%
  arrange(count) %>%
  mutate(Generation = factor(Generation, levels = unique(Generation))) %>%
  ggplot(aes(x = Generation, y = count)) +
  geom_bar(color = "black", fill = "lightblue", stat = "identity") +
  coord_flip() +
  ggtitle("Number of Games Per Console Generation") +
  labs(x = "Console Generation", y = "Number of Games")

#Word Cloud to show prominent series

#Creating a corpus from all game titles
titleCorpus <- corpus(games$Game.Title)
#Tokenizing the game titles and removing punctuation, symbols, and numbers
titleTokens <- tokens(titleCorpus, what = "word", 
                     remove_punct = TRUE, remove_numbers = TRUE, 
                     remove_symbols = TRUE)
#Making all of the tokens all lowercase
titleTokens <- tokens_tolower(titleTokens, keep_acronyms = FALSE)
#Removing any stop words and requiring 2 minimum characters in each word
titleNoStop <- tokens_select(titleTokens, stopwords("english"), selection = "remove", 
                            min_nchar = 2)
#Creating a document-feature matrix to make the word cloud
titleDFM <- dfm(titleNoStop)
#Generating a word cloud where the words must appear at least 10 times to be
#included
textplot_wordcloud(titleDFM, min_count = 10)
