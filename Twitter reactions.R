# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator
#install.packages("RColorBrewer") # color palettes
#install.packages("wordcloud2") # Word-cloud generator

#Importing Packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

#Importing csv of tweets
IPL_Tweets <- read.csv("/Users/eswar/Documents/Module 4/Using R for Analytics 59000/Project/tweets.csv")

#Removing NAs
anyNA(IPL_Tweets) #Checking for NAs
IPL_Tweets = IPL_Tweets[which(!is.na(IPL_Tweets)),] #Removing NAs

#Removing Duplicates
sum(duplicated(IPL_Tweets))
IPL_Tweets=IPL_Tweets[duplicated(IPL_Tweets)!= "TRUE",]

#Extracting Tweets and Hashtags to different data frames
rownumbers=c(1:nrow(IPL_Tweets))
Tweets=data.frame(doc_id=rownumbers,text=IPL_Tweets$tweet) #Tweets
Hashtags= data.frame(doc_id=rownumbers, text=IPL_Tweets$hashtags) #Hashtags

#Removing Empty Hashtags
Hashtags=Hashtags[Hashtags$text!= "[]",]

#Extracting Corpus
TextCorpus= Corpus(DataframeSource(Tweets)) #Corpus for Tweets  
Hashcorpus=Corpus(DataframeSource(Hashtags)) #Corpus for Hashtags

#Converting to lowercase
TextCorpus1=tm_map(TextCorpus,content_transformer(tolower)) # for Tweets
Hashcorpus1=tm_map(Hashcorpus,content_transformer(tolower)) #for Hashtags

#removing punctuation
TextCorpus2=tm_map(TextCorpus1,content_transformer(removePunctuation))
Hashcorpus2=tm_map(Hashcorpus1,content_transformer(removePunctuation))
#removing number
TextCorpus3=tm_map(TextCorpus2,content_transformer(removeNumbers))
Hashcorpus3=tm_map(Hashcorpus2,content_transformer(removeNumbers))

#removing stopwords
TextCorpus4=tm_map(TextCorpus3,content_transformer(removeWords),stopwords())
HashCorpus4=tm_map(Hashcorpus3,content_transformer(removeWords), stopwords())

#Removing Spaces between words
TextCorpus5 = tm_map (TextCorpus4, stripWhitespace)
HashCorpus5= tm_map(HashCorpus4,stripWhitespace)

# Text stemming - which reduces words to their root form
TextCorpus6= tm_map(TextCorpus5,stemDocument)
HashCorpus6= tm_map(HashCorpus5,stemDocument)

#Building the Term Document Matrix for Tweets
DTM= DocumentTermMatrix(TextCorpus6)
DTM=as.matrix(DTM) #Converting into a double matrix
DTM.totalfreq=colSums(DTM) # calculate the term frequencies
summary(DTM.totalfreq) #summary calculation

#Building the Term Document Matrix for Hashtags
DTM1= DocumentTermMatrix(HashCorpus6)
DTM1=as.matrix(DTM1)
DTM1.totalfreq=colSums(DTM1)
summary(DTM1.totalfreq)

#Finding out the most frequent words in Tweets 
#Sort by descending value of frequency
DTM_V=sort(DTM.totalfreq,decreasing=TRUE)
DTM_D=data.frame(word = names(DTM_V),freq=DTM_V)
# Display the top 10 most frequent words
head(DTM_D, 10)
# Plot the most frequent words
barplot(DTM_D[1:10,]$freq, las = 2, names.arg = DTM_D[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words in Tweets",
        ylab = "Word frequencies")

wordcloud <- wordcloud(words = colnames(DTM),freq=DTM.totalfreq,
                       max.words=1000,rot.per=0.30,colors=brewer.pal(n=8,"Dark2"), random.order=F)

