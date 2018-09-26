setwd("/home/aurora/Documents/School/rstudio/Sociology_Test_Data")
library("stm")
library("genderizeR")
library("dplyr")
library("quanteda")

options(stringsAsFactors = FALSE)

text_data <- read.csv("OUTPUT_Python_MSSQL_Article_Text_XA.csv")
name_data <- read.csv("OUTPUT_Python_MSSQL_Names_XA.csv")
nameAndText <- merge(text_data,name_data, by="Article_Key")

data <- nameAndText[1:1000,]

first1000 <- data[,7] #7 is column 7
#first1000corrected <- gsub("[^A-z]+.*","",first1000)
#https://www.rdocumentation.org/packages/genderizeR/versions/2.0.0/topics/genderize
givenNames = findGivenNames(first1000, progress = FALSE, apikey="c0d6eabb501298b0b57595710f5b0d92")

# I need to figure out a way to count the repeated names in order to get the same number of columns so that the first 1000 can be genderized and then sent through STM
#need to clean data. Too many NA
genFirst1000 = data.frame(name=character(), gender=character(), 
                          probability=character(),stringsAsFactors = FALSE)
for(name in first1000){
    pos = which(givenNames$name == tolower(name))
    if(length(pos) == 0){
      genFirst1000[nrow(genFirst1000) + 1, ] =c(name, NA,NA)
    }
    else{
      gender = givenNames$gender[pos]
      probability = givenNames$probability[pos]
      genFirst1000[nrow(genFirst1000) + 1, ] =c(name, gender, probability)
    }
}
#combined genFirst1000 and data
useData <- cbind(data,genFirst1000)

# data$documents is a column of text in each article
processed <- textProcessor(useData$Abstract, metadata = NULL, 
                           lowercase = TRUE,removestopwords = TRUE,
                           removenumbers = TRUE, removepunctuation = TRUE,
                           stem = TRUE, wordLengths = c(3, Inf), 
                           sparselevel = 1,language = "en", 
                           verbose = TRUE,onlycharacter = FALSE,
                           striphtml = FALSE, customstopwords = NULL, v1 = FALSE)
#converts to the number indexes. When the word was first seen and how many times
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab,processed$meta, lower.thresh = 15)
socSTM <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence = NULL, content = NULL, data = NULL,
              init.type = c("Spectral", "LDA", "Random", "Custom"), seed = NULL,
              max.em.its = 500, emtol = 1e-05, verbose = TRUE, reportevery = 5,
              LDAbeta = TRUE, interactions = TRUE, ngroups = 1, model = NULL)