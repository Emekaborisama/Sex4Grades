library(readr)
library(tm)
library(SnowballC)
Sex4Grades <- read_csv("Data/Sex4Grades.csv")
##View (Sex4Grades)

Clean_sex_for_grades <- as.character(Sex4Grades$`1`)
##View(Clean_sex_for_grades)

## remove usernames and replace them with "USER"
Clean_sex_for_grades<- gsub("@\\w*"," USER",   Clean_sex_for_grades)

Clean_sex_for_grades<- Corpus(VectorSource(Clean_sex_for_grades))

# convert to lower case
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, content_transformer(tolower))

#remove ������ what would be emojis
Clean_sex_for_grades<-tm_map(Clean_sex_for_grades, content_transformer(gsub), pattern="\\W",replace=" ")

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, content_transformer(removeURL)
)

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, content_transformer(removeNumPunct))

# remove stopwords
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, removeWords, stopwords("english"))


Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, removeWords, c("rt","user"))

# remove extra whitespace
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, stripWhitespace)

# Remove numbers
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, removeNumbers)

# Remove punctuations
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades, removePunctuation)

#stemming
Clean_sex_for_grades <- tm_map(Clean_sex_for_grades , stemDocument)

#create a term matrix 
Clean_sex_for_grades_dtm <- TermDocumentMatrix(Clean_sex_for_grades)
#remove sparse terms, set threshhold at 90%
#Clean_sex_for_grades_dtm <- removeSparseTerms(Clean_sex_for_grades_dtm, 0.90)

#create matrix

Clean_sex_for_grades_matrix = as.matrix(Clean_sex_for_grades_dtm)
v <- sort(rowSums(Clean_sex_for_grades_matrix),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

write.csv(d, file = "tweets.csv")

