library(NLP)
library(tm)  # make sure to load this prior to openNLP
library(openNLP)
library(openNLPdata)
library(stringr)

# load corpus data
#text <- readLines("https://slcladal.github.io/data/testcorpus/linguistics07.txt", skipNul = T)
fileName <- "data/peterpan.txt"
text <- readChar(fileName, file.info(fileName)$size)
# inspect data
str(text)
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

textpos <- POStag(object = text)
textpos <- textpos[[1]]
sp = strsplit(textpos, " +")
myDf <- as.data.frame(sp) 
output <- matrix(ncol=2, nrow=nrow(myDf))
for(i in 1:nrow(myDf)){
  pair <- myDf[i,1]
  sp1 <- strsplit(pair, "/", fixed=TRUE)
  output[i,] <- c(sp1[[1]][1],sp1[[1]][2])
  }  
output <- data.frame(output)
colnames(output) <- c("Word", "Tag")
output[output$Tag == 'JJ',]

