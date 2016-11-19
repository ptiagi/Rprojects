# Q. 1) (3 points)
# Write down a general regular expression to match the following:
#  (a) Words with @ symbols in them, e.g., h@te or v|c0din
w <- c("h@te", "v!c0din")
grep("[:punct:]",w, ignore.case = TRUE)

# (b)An IP address (Four sets of 1 to 3 digits separated by periods, e.g., 100.12.162.0)
wrod <- c("11.22.33.44", "0.0.0.0", "1111.222.4444.8888","wer.333.tg.777")
grep('^[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}$', wrod)

# (c) An email address that ends with .com, .edu, .net, .org, or .gov
word <- c("adc@gmail.gov", "adc@gmail.com","adc@gmail.org","adc@gmail.edu","adc@gmail.net")

grep('[[:alnum:][:punct:]+@[:alnum:]]+.com|.gov|.org|.edu|.net', word)


# Q. 2) (19 points) Carry out the following exercises on the State of the Union
# speeches database (available in moodle).
# (a) Use readLines() to read in the speeches (available as a text file in
# moodle) where the return value is: character vector with one element/
# character string per line in the file.

filename <- "stateoftheunion1790-2012.txt"
getwd()
setwd("/Users/Dexter/Downloads")
con <- file(description = filename, open = "r")
speeches <- readLines(con)
head(speeches)

# (b) Use regular expressions to find ***

grep("\\*\\*\\*", speeches)

# (c) Use *** to identify the date of the speech.
dates <- speeches[grep("\\*\\*\\*", speeches) + 4]
head(dates)

# (d) Use regular expressions to extract the year.
year <- as.numeric(sapply(dates, function(x) substr(x, nchar(x)-3, nchar(x))))
year

# (e) Use regular expressions to extract the month.
dateSplit <- strsplit(dates, split=" ")
dateSplit
speech_month <- sapply(dateSplit, "[", 1)
speech_month

# (f) Use *** to extract the name of the president State of the union speeches.
president_speech<- speeches[grep("\\*\\*\\*", speeches) + 3]
president_speech

# (g) Use regular expressions and R to return the number of speeches in
# the dataset, and the number of presidents that gave speeches.
numberOfSpeeches <- grep("\\*\\*\\*", speeches)
length(numberOfSpeeches)
length(unique(president_speech))

# (h) Chop the speeches up into a list there is one element for each speech.
# Each element is a character vector. Check: does your number of list
# elements match your answer above?

speech_list <- list()
br <- grep("\\*\\*\\*", speeches)
breaks <- c(grep("\\*\\*\\*", speeches), length(speeches)-2)
for(i in 1:length(br)){
  str <- paste(speeches[(breaks[i]+6):(breaks[i+1]-1)], sep=" ", collapse=" ") 
  speech_list[[i]] <- unlist(str)
}
length(speech_list)

# (i) Eliminate apostrophes, numbers, and the phrase: (Applause.)
new_list <- gsub("[0-9]+$*", "", speech_list)
new_list <- gsub("'", "", new_list)
new_list <- gsub("\\([Applause]\\)", "", new_list)

# (j) Make all the characters lower case.
new_list <- tolower(new_list)

# (k) Split the sentences up where there are blanks and punctuation to
# create “words”.
words <- strsplit(new_list, "[[:punct:][:blank:]]+")
head(words)

# (l) Drop any empty words that resulted from this split.
words = words[words != ""]

# (m) Create a word vector for each speech.
word <- unlist(words)
length(word)

# (n) Normalize the word vectors to get term frequencies.
length(word)
frequencies <- sapply(words, function(x) length(unique(x)))
normalised_vector <- frequencies/(length(unique(word)))

# (o) (5 points) Carry out some exploratory analysis of the data and term
# frequencies. For example, find the number of sentences, extract the
# long words, and the political party. Plot and interpret the term
# frequencies. What are your observations?

# Finding number of sentences
sentences <- gregexpr('[[:alnum:] ][.!?]', new_list)
numberOfSentences <- lapply(sentences, length)

# Extracting long words
# Finding Length of each word
lengthOfWords <- sapply(word, function(x) sum(nchar(x))) 
# Average Length of words
average_length_of_words <- sum(lengthOfWords)/length(word)
# Extracting long words i.e. words with length greater than the average length
long_words <- word[lengthOfWords > average_length_of_words]

# Plotting frequency
plot(frequencies, type = 'l')
plot(normalised_vector, type = 'l')
plot(density(frequencies), main = "Density Plot for Number of words in Speeches", xlab = "Number of words", ylab = "Density")

# Most of the speeches had 2000 maximum number of words. Very less speeches had 3000-5000 words.
# The density curve seems to be a normal curve

