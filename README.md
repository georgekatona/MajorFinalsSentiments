# Champions League

On the First of June, it is the Champions League Finals, Liverpool vs Tottenham, the first time two English teams play the finals since 2008. In the Premier League both teams qualified themselves for the next seasons, but Liverpool also got really close to winning the title, finished only one point behind Manchester City. There has been no other team before losing the trophy with this many points (97), and the nightmare of the team - and its supporters - is that they could finish this wonderful season without winning any trophies. It is fair to say that Liverpool is the team under the higher pressure in the finals.

### Positive-Negative Scale

I am using the [twitteR](https://cran.r-project.org/web/packages/twitteR/README.html) package to get the 1000 most recent English tweets containing the official hashtag of the team along with "#UCLFinal". In order to analyse the text, first we have to clean it by removing links, punctuation, numbers, emojis and special characters. The library [sentimentr](https://github.com/trinker/sentimentr) is designed to take into account valence shifters like amplifiers and negators, which results in a more accurate sentiment score. This library returns not only the sentiment classes positive/negative for each tweet, but also its value. A higher sentiment value represents a stronger positive emotion in the post. The figure below shows the distribution of the sentiment values for the two teams.

```
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
```

![liv_tot_histo](https://georgekatona.com/img/finals/liv_tot_histo.png)
<p align="center">Figure 1: Histogram of the sentiment scores for Liverpool and Tottenham</p>

```
> summary(liv_sentiment$ave_sentiment)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
 -1.255   0.000   0.000   0.126   0.250   0.900
> summary(tot_sentiment$ave_sentiment)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
-0.37889  0.00000  0.00000 -0.01251  0.00000  0.61968
```

### Multiple Discrete Sentiments

With the library [syuzhet]() we can access multiple emotions (joy, fear, trust, etc.), but only its binary state, which means that one twitter message can be categorized as expressing both joy and surprise, but we don’t get their extent. In Figure 2 you can see the emotions expressed by the tweets for each team.

```
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
```

![liv_tot_emotions](https://georgekatona.com/img/finals/liv_tot_emotions.png)
<p align="center">Figure 2: Emotions expressed by the tweets for each team</p>

To make comparison easier, I subtracted the relative presence of the emotions. Figure 3 shows us that Tottenham fans are expressing much more fear and anger, while Liverpool fans have more trust.

![sub_emotions_cl](https://georgekatona.com/img/finals/sub_emotions_cl.png)
<p align="center">Figure 3: Difference of sentiments</p>

With syuzhet we can also classify the tweets based on their positive or negative content (Figure 4). It is interesting to see how much more optimistic Liverpool fans are.

![liv_tot_pos_neg](https://georgekatona.com/img/finals/liv_tot_pos_neg.png)
<p align="center">Figure 4: Discrete binary classification of the tweets</p>


### Word Clouds

To get an idea about the content of the tweets, we use word clouds (Figure 5).

![wordcloud_cl](https://georgekatona.com/img/finals/wordcloud_cl.png)
<p align="center">Figure 5: Word clouds for each teams</p>

# NBA

In the NBA Finals the two-time defending champion Golden State Warriors play against the Toronto Raptors. It is no surprise that Golden State is the finals, but it might be surprising for some how the Raptors overcame Milwaukee Bucks led by Giannis Antetokounmpo. Most still think that GSW has the better chance in winning, but we also shouldn’t underestimate the Raptors, we are looking forward to an exciting battle. Let’s see what the fan tweets are telling us.

### Positive-Negative Scale

![rap_gsw_histo](https://georgekatona.com/img/finals/rap_gsw_histo.png)
<p align="center">Figure 6: Histogram of the sentiment scores for Toronto Raptors and Golden State Warriors</p>

```
> summary(raptor_sentiment$ave_sentiment)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
-0.5729  0.0000  0.0000  0.0769  0.1664  0.8609
> summary(gsw_sentiment$ave_sentiment)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
-0.8074  0.0000  0.0000  0.0965  0.1809  0.6971
```

Based on the histograms of Figure 6, GSW fans look slightly more optimistic, but it is difficult to draw conclusions based on only these results.

### Multiple Discrete Sentiments

![rap_gsw_emotions](https://georgekatona.com/img/finals/rap_gsw_emotions.png)
<p align="center">Figure 7: Emotions expressed by the tweets for each team</p>

![sub_emotions_nba](https://georgekatona.com/img/finals/sub_emotions_nba.png)
<p align="center">Figure 8: Difference of sentiments</p>

While investigating multiple sentiments, it might be surprising, that the Raptor tweets contain more trust. We should note, that NBA playoff is much more frequent than the Champions League, the emotions of the previous matches are probably much more present in the fans, than in the previous case. The analysis also tells us that Raptors fans have more fear, they are more surprised. On the other hand Golden State fans feeling more joy and they have more anticipation.

![raptors_gsw_pos_neg](https://georgekatona.com/img/finals/raptors_gsw_pos_neg.png)
<p align="center">Figure 9: Discrete binary classification of the tweets</p>

In general, the fans of both teams are optimistic, Golden State tweets are slightly more positive than the Raptors posts.

### Word Clouds

Let’s see the most frequent words grouped by the 6 sentiments.

![wordcloud_nba](https://georgekatona.com/img/finals/wordcloud_nba.png)
<p align="center">Figure 10: Word clouds for each teams</p>
