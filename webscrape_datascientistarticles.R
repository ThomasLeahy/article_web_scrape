######
# An example of webscraping. Suppose we want to get some of the keywords
# that come up in articles about Data Science or being a Data Scientist.
# Below we take 5 articles online scrape their text and look at the most
# frequent words.
# Note that when scraping different websites, they do not all use the 
# same format and so below notice that we tidy each article indiviually.
######
# libraries
library(rvest)
library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)
library(tm)
library(RColorBrewer)



urls <- c("https://www.cio.com/article/3263790/data-science/the-essential-skills-and-traits-of-an-expert-data-scientist.html", "https://www.thebalancecareers.com/list-of-data-scientist-skills-2062381",
          "https://www.kdnuggets.com/2018/05/simplilearn-9-must-have-skills-data-scientist.html", "https://www.forbes.com/sites/quora/2017/06/15/what-are-the-top-five-skills-data-scientists-need/", 
          "https://bigdata-madesimple.com/eight-key-skills-of-a-data-scientist/")

url1 <- read_html(urls[1])  # read in webpage
txt1 <- url1 %>% html_nodes("p:not(p.dateline):not(p.name):not(p.msg-sent):not(p.addl-text)") %>% # select only paragraphs with some limitations
  html_text %>%
  as.character %>%
  toString
fileText_1 <- gsub("\\$", "", txt1) # remove $ sign as special character in R


url2 <- read_html(urls[2])
txt2 <- url2 %>% html_nodes("p") %>% 
  html_text %>%
  as.character %>%
  toString
fileText_2 <- gsub("\\$", "", txt2) 


url3 <- read_html(urls[3])
txt3 <- url3 %>% html_nodes("p") %>% 
  html_text %>%
 as.character %>%
 toString
fileText_3 <- gsub("\\$", "", txt3) 

url4 <- read_html(urls[4])
txt4 <- url4 %>% html_nodes("p:not(p.dateline):not(p.name):not(p.msg-sent):not(p.addl-text)") %>% 
  html_text %>%
  as.character %>%
  toString
fileText_4 <- gsub("\\$", "", txt4) 

url5 <- read_html(urls[5])
txt5 <- url5 %>% html_nodes("p:not(p.dateline):not(p.name):not(p.msg-sent):not(p.addl-text)") %>% 
  html_text %>%
  as.character %>%
  toString
fileText_5 <- gsub("\\$", "", txt5) 

x <- paste(fileText_1,fileText_2,fileText_3,fileText_4,fileText_5,sep=" ") # concatenate the 5 texts



docs <- Corpus(VectorSource(x))  # Corpus class

# strip back text, removing common english words, numbers etc
docs <-  tm_map(docs, content_transformer(tolower)) # all text lower case
docs <- tm_map(docs, removeNumbers)    # Remove numbers
docs <-  tm_map(docs, removeWords, stopwords("english"))   # Remove english common stopwords
docs <- tm_map(docs, removePunctuation) # Remove punctuations
docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces

doc_to_mat <- TermDocumentMatrix(docs)
mat <- as.matrix(doc_to_mat)  # create a matrix of word use
sort_freq <- sort(rowSums(mat),decreasing=TRUE)  # calculate frequency and sort 
df <- data.frame(word = names(sort_freq),freq=sort_freq) 

set.seed(204)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,   # create and plot word cloud
          max.words=25, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
