require("twitteR")
require("ggplot2")
require('sentimentr')
require('syuzhet')
require("tm")
require("wordcloud")

# Function for data cleaning
f_clean_tweets <- function (tweets) {
  
  clean_tweets = sapply(tweets, function(x) x$getText())
  # remove retweet entities
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_tweets)
  # remove at people
  clean_tweets = gsub('@\\w+', '', clean_tweets)
  # remove punctuation
  clean_tweets = gsub('[[:punct:]]', '', clean_tweets)
  # remove numbers
  clean_tweets = gsub('[[:digit:]]', '', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', '', clean_tweets)
  # remove unnecessary spaces
  clean_tweets = gsub('[ \t]{2,}', '', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', '', clean_tweets)
  # remove emojis or special characters
  clean_tweets = gsub('<.*>', '', enc2native(clean_tweets))
  
  clean_tweets = tolower(clean_tweets)
  
  clean_tweets
}

# Function for plotting wordcloud
f_plot_wordcloud <- function (tweet_texts, nrc_sentiment) {
  
  all = c(
    paste(tweet_texts[nrc_sentiment$anger > 0], collapse=" "),
    paste(tweet_texts[nrc_sentiment$anticipation > 0], collapse=" "),
    paste(tweet_texts[nrc_sentiment$disgust > 0], collapse=" "),
    paste(tweet_texts[nrc_sentiment$fear > 0], collapse=" "),
    paste(tweet_texts[nrc_sentiment$joy > 0], collapse=" "),
    paste(tweet_texts[nrc_sentiment$sadness > 0], collapse=" "),
    paste(tweet_texts[nrc_sentiment$surprise > 0], collapse=" "),
    paste(tweet_texts[nrc_sentiment$trust > 0], collapse=" ")
  )
  all <- removeWords(all, stopwords("english"))
  # create corpus
  corpus <- Corpus(VectorSource(all))
  #
  # create term-document matrix
  tdm <- TermDocumentMatrix(corpus)
  #
  # convert as matrix
  tdm <- as.matrix(tdm)
  tdm1 <- tdm[nchar(rownames(tdm)) < 15,]
  #
  # add column names
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdm1) <- colnames(tdm)
  comparison.cloud(tdm1, random.order=FALSE,
                   colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"), title.size=1.8, max.words=300, scale=c(2,.8),rot.per=0.4)
}


consumer_key <- '...'
consumer_secret <- '...'

access_token <- '...'
access_secret <- '...'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

liverpool_color <- "#C8102E"
tottenham_color <- "#132257"
raptors_color <- "#CE1141"
gsw_color <- "#006BB6"
default_color <-"#06D6A0"

# Champons League Final
liv_search <- "#LFC+#UCLFinal"
liv_tweets <- searchTwitter(liv_search, n = 1000, lang = "en", resultType = "recent")
liv_tweet_texts <- f_clean_tweets(liv_tweets)
liv_sentiment <- sentiment_by(liv_tweet_texts)
qplot(liv_sentiment$ave_sentiment,
      geom="histogram",
      binwidth=0.1, 
      main=paste(liv_search, "Sentiment Histogram", sep=" "),
      xlim = c(-0.9, 0.9),
      ylim = c(0, 800),
      colour=I("black"),
      fill=I(liverpool_color))

tot_search <- "#COYS+#UCLFinal"
tot_tweets <- searchTwitter(tot_search, n = 1000, lang = "en", resultType = "recent")
tot_tweet_texts <- f_clean_tweets(tot_tweets)
tot_sentiment <- sentiment_by(tot_tweet_texts)
qplot(tot_sentiment$ave_sentiment,
      geom="histogram",
      binwidth=0.1, 
      main=paste(tot_search, "Sentiment Histogram", sep=" "),
      xlim = c(-0.9, 0.9),
      ylim = c(0, 800),
      colour=I("black"),
      fill=I(tottenham_color))

summary(liv_sentiment$ave_sentiment)
summary(tot_sentiment$ave_sentiment)

# Emotion Vectors
liv_nrc_sentiment <-get_nrc_sentiment(liv_tweet_texts)
tot_nrc_sentiment <-get_nrc_sentiment(tot_tweet_texts)
barplot(
  sort(colSums(prop.table(liv_nrc_sentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Liverpool Tweets", xlab="Percentage",
  col = liverpool_color
)
barplot(
  sort(colSums(prop.table(tot_nrc_sentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Tottenham Tweets", xlab="Percentage",
  col = tottenham_color
)

# Subtracting Emotions
barplot(
  sort(colSums(prop.table(tot_nrc_sentiment[, 1:8])) - colSums(prop.table(liv_nrc_sentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Tottenham Emotions - Liverpool Emotions", xlab="Percentage",
  col = default_color
)

# Positive-Negative Emotions
barplot(
  sort(colSums(prop.table(liv_nrc_sentiment[, 9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Pos-Neg Emotions: Liverpool", xlab="Percentage",
  col = liverpool_color
)

barplot(
  sort(colSums(prop.table(tot_nrc_sentiment[, 9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Pos-Neg Emotions: Tottenham", xlab="Percentage",
  col = tottenham_color
)

f_plot_wordcloud(liv_tweet_texts,liv_nrc_sentiment)
f_plot_wordcloud(tot_tweet_texts,tot_nrc_sentiment)




# NBA Final
raptor_search <- "#WeTheNorth+#NBAFinals"
raptor_tweets <- searchTwitter(raptor_search, n = 1000, lang = "en", resultType = "recent")
raptor_tweet_texts <- f_clean_tweets(raptor_tweets)
raptor_sentiment <- sentiment_by(raptor_tweet_texts)
qplot(raptor_sentiment$ave_sentiment,
      geom="histogram",
      binwidth=0.1, 
      main=paste(raptor_search, "Sentiment Histogram", sep=" "),
      xlim = c(-1.4, 1.4),
      ylim = c(0, 550),
      colour=I("black"),
      fill=I(raptors_color))

gsw_search <- "#DubNation+#NBAFinals"
gsw_tweets <- searchTwitter(gsw_search, n = 1000, lang = "en", resultType = "recent")
gsw_tweet_texts <- f_clean_tweets(gsw_tweets)
gsw_sentiment <- sentiment_by(gsw_tweet_texts)
qplot(gsw_sentiment$ave_sentiment,
      geom="histogram",
      binwidth=0.1, 
      main=paste(raptor_search, "Sentiment Histogram", sep=" "),
      xlim = c(-1.4, 1.4),
      ylim = c(0, 550),
      colour=I("black"),
      fill=I(gsw_color))

summary(raptor_sentiment$ave_sentiment)
summary(gsw_sentiment$ave_sentiment)

# Emotion Vectors
raptor_nrc_sentiment <- get_nrc_sentiment(raptor_tweet_texts)
gsw_nrc_sentiment <- get_nrc_sentiment(gsw_tweet_texts)
barplot(
  sort(colSums(prop.table(raptor_nrc_sentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Toronto Raptors Tweets", xlab="Percentage",
  col = raptors_color
)
barplot(
  sort(colSums(prop.table(gsw_nrc_sentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in GSW Tweets", xlab="Percentage",
  col = gsw_color
)

# Subtracting Emotions
barplot(
  sort(colSums(prop.table(raptor_nrc_sentiment[, 1:8])) - colSums(prop.table(gsw_nrc_sentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Raptors Emotions - GSW Emotions", xlab="Percentage",
  col = default_color
)

# Positive-Negative Emotions
barplot(
  sort(colSums(prop.table(raptor_nrc_sentiment[, 9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Pos-Neg Emotions: Raptors", xlab="Percentage",
  col = raptors_color
)
barplot(
  sort(colSums(prop.table(gsw_nrc_sentiment[, 9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Pos-Neg Emotions: GSW", xlab="Percentage",
  col = gsw_color
)

f_plot_wordcloud(raptor_tweet_texts,raptor_nrc_sentiment)
f_plot_wordcloud(gsw_tweet_texts,gsw_nrc_sentiment)
