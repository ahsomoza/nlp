library(NLP)
library(tm)  # make sure to load this prior to openNLP
library(openNLP)
library(openNLPdata)
library(stringr)
#We define the function to tag the text with POS tagging process using Penn Treebank tagset
POStag <- function(object){
  # define paths to corpus files
  corpus.tmp <- object
  # define sentence annotator
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  # define word annotator
  word_token_annotator <- Maxent_Word_Token_Annotator()
  # define pos annotator
  pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en", probs = FALSE)
  # convert all file content to strings
  Corpus <- lapply(corpus.tmp, function(x){
    x <- as.String(x)  }  )
  # loop over file contents
  lapply(Corpus, function(x){
    y1 <- NLP::annotate(x, list(sent_token_annotator, word_token_annotator))
    y2<- NLP::annotate(x, pos_tag_annotator, y1)
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", x[y2w], tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)  }  )
}

# load corpus data
fileName <- "data/peterpan.txt"
text <- readChar(fileName, file.info(fileName)$size)
# inspect data
str(text)

#Proceed with POS tagging process
textpos <- POStag(object = text)
textpos <- textpos[[1]]
#Split the result of tagging process to format as an array with each pair word/tag
sp = strsplit(textpos, " +")
#Format result as dataframe
myDf <- as.data.frame(sp) 
#Define an empty matrix to fill in the loop
output <- matrix(ncol=2, nrow=nrow(myDf))
for(i in 1:nrow(myDf)){
  #For each word/tag pair we slit it and add it to the output matrix
  pair <- myDf[i,1]
  sp1 <- strsplit(pair, "/", fixed=TRUE)
  output[i,] <- c(sp1[[1]][1],sp1[[1]][2])
}  
#We format the output as dataset and set the columns name
output <- data.frame(output)
colnames(output) <- c("Word", "Tag")
#We filter the outout to mantain only words tagged as adjectives
output <- output[output$Tag == 'JJ',]
output = subset(output, select = -c(Tag) )
#We create another dataset aggreging the data to get repetitions of each word
dfTemp<-aggregate(output$Word, output, length)
#We order it by number of appearances and get the top 15
countAdjectives <-dfTemp[order(-dfTemp$x),]
countAdjectives1 <-head(countAdjectives,20)
#Represent the result data
barplot(height = countAdjectives1$x, names.arg = countAdjectives1$Word, main="Peter Pan most common adjectives")
write.csv(countAdjectives1,"data/peterPanOutput.csv", row.names = FALSE)



# load corpus data
fileName <- "data/alice's adventures.txt"
text <- readChar(fileName, file.info(fileName)$size)
# inspect data
str(text)

#Proceed with POS tagging process
textpos <- POStag(object = text)
textpos <- textpos[[1]]
#Split the result of tagging process to format as an array with each pair word/tag
sp = strsplit(textpos, " +")
#Format result as dataframe
myDf <- as.data.frame(sp) 
#Define an empty matrix to fill in the loop
output <- matrix(ncol=2, nrow=nrow(myDf))
for(i in 1:nrow(myDf)){
  #For each word/tag pair we slit it and add it to the output matrix
  pair <- myDf[i,1]
  sp1 <- strsplit(pair, "/", fixed=TRUE)
  output[i,] <- c(sp1[[1]][1],sp1[[1]][2])
}  
#We format the output as dataset and set the columns name
output <- data.frame(output)
colnames(output) <- c("Word", "Tag")
#We filter the outout to mantain only words tagged as adjectives
output <- output[output$Tag == 'JJ',]
output = subset(output, select = -c(Tag) )
#We create another dataset aggreging the data to get repetitions of each word
dfTemp<-aggregate(output$Word, output, length)
#We order it by number of appearances and get the top 15
countAdjectives <-dfTemp[order(-dfTemp$x),]
countAdjectives2 <-head(countAdjectives,20)
#Represent the result data
barplot(height = countAdjectives2$x, names.arg = countAdjectives2$Word, main="Alice in Wonderland most common adjectives")
write.csv(countAdjectives2,"data/aliceOutput.csv", row.names = FALSE)


# load corpus data
fileName <- "data/frankestein.txt"
text <- readChar(fileName, file.info(fileName)$size)
# inspect data
str(text)

#Proceed with POS tagging process
textpos <- POStag(object = text)
textpos <- textpos[[1]]
#Split the result of tagging process to format as an array with each pair word/tag
sp = strsplit(textpos, " +")
#Format result as dataframe
myDf <- as.data.frame(sp) 
#Define an empty matrix to fill in the loop
output <- matrix(ncol=2, nrow=nrow(myDf))
for(i in 1:nrow(myDf)){
  #For each word/tag pair we slit it and add it to the output matrix
  pair <- myDf[i,1]
  sp1 <- strsplit(pair, "/", fixed=TRUE)
  output[i,] <- c(sp1[[1]][1],sp1[[1]][2])
}  
#We format the output as dataset and set the columns name
output <- data.frame(output)
colnames(output) <- c("Word", "Tag")
#We filter the outout to mantain only words tagged as adjectives
output <- output[output$Tag == 'JJ',]
output = subset(output, select = -c(Tag) )
#We create another dataset aggreging the data to get repetitions of each word
dfTemp<-aggregate(output$Word, output, length)
#We order it by number of appearances and get the top 15
countAdjectives <-dfTemp[order(-dfTemp$x),]
countAdjectives3 <-head(countAdjectives,20)
#Represent the result data
barplot(height = countAdjectives3$x, names.arg = countAdjectives3$Word, main="Frankestein most common adjectives")
write.csv(countAdjectives3,"data/frankesteinOutput.csv", row.names = FALSE)

# merge the three dataframes
total3 <- merge(countAdjectives1,countAdjectives2,by="Word")
total3 <- merge(total3, countAdjectives3,by="Word")
total3 <-total3[order(-total3$x),]
write.csv(total3,"data/TotalOutputInner.csv", row.names = FALSE)
total3
total <- merge(countAdjectives1,countAdjectives2,by="Word", all=TRUE)
total <- merge(total, countAdjectives3,by="Word", all=TRUE)
total <-total[order(-total$x),]
write.csv(total,"data/TotalOutputFull.csv", row.names = FALSE)
total
