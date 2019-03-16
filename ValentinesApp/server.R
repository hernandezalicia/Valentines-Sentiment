library(shiny)


############################################


### Server file for app.

shinyServer(function(input, output) {
   
  #### Word Analysis tab.

  
  #Creating reactive event to generate vocabulary for word cloud.
  #This happens each time the "generate" button is pressed".
  data <- eventReactive(input$cloud_update, {
    data <- create_vocabulary(tweet_tokens, ngram = c(ngram_min = as.numeric(input$cloud_length), ngram_max = as.numeric(input$cloud_length)),
                      sep_ngram = " ", stopwords = total_stopwords)
    
    
    #### DOCUMENT PROPORTION DOES NOT WORK FOR TRI-GRAM. NEED TO FIND A WAY TO ADJUST THIS IN FUNCTION.
    data <- prune_vocabulary(data, term_count_min = 10, doc_proportion_max = .7)
  })

  
   #Creating wordcloud output.
   output$cloudPlot <- renderPlot({
  
     terms <- data()
     terms$lemmatized <- lemmatize_strings(terms$term)
     
     terms <- terms %>%
       group_by(lemmatized) %>%
       summarize(term_count = sum(term_count))
  
     wordcloud(terms$lemmatized, terms$term_count, min.freq= input$cloud_freq, scale =c(4, .5),
               max.words=input$cloud_number, rot.per = .25, random.order = TRUE, family = 'mono',
               colors=c( 'tomato2', 'red3', 'violetred3', 'maroon', 'darkred', 'violetred4', 'firebrick2'))

   })
   
   
   

   #Creating reactive event which observes TF-IDF switch.
   idfdata <- eventReactive(input$tfswitch, {
     idfdata <- tweet_tfidf

   })
   
   #TF-IDF wordcloud plot.
   output$IDFplot <- renderPlot({
     
     tweet_tfidf <- idfdata()
     
     wordcloud(tweet_tfidf$term, tweet_tfidf$weighted_count, min.freq= input$cloud_freq, scale =c(3.5, .1),
               max.words=input$cloud_number, rot.per = .2, random.order = TRUE, family = 'mono',
               colors=c( 'tomato2', 'red3', 'violetred3', 'maroon', 'darkred', 'violetred4', 'firebrick2'))
     
   })
   
   
   
   ##### Commonality/Comparison Analysis.
   #Comparison Cloud output.
   output$compareCloud <- renderPlot({
     
     comparison.cloud(pos_neg, scale=c(5.5, .5), max.words=300,
                      random.order=FALSE, rot.per=.2,
                      colors= c('firebrick3', 'slategray'),
                      use.r.layout=FALSE, title.size=2, family = 'mono',
                      match.colors=TRUE, title.bg.colors="grey90")
     
   })
   
   #Comonality Cloud output.
   output$commonCloud <- renderPlot({
     
     commonality.cloud(pos_neg, max.words = 300, scale = c(3.5, .5), random.order = FALSE, family = 'mono',
                       colors= c('red3', 'pink3', 'violetred3', 'deeppink4', 'maroon', 'firebrick3', 'darkred'))
     
   })
   
   
   
   
   
   
   ##### Sentiment analysis tab.
   
   #Reactive element which filters based on .
   trenddata <- eventReactive(input$sentitype, {
     
     if (input$sentitype != "Overall"){
     tenddata <- categorysent %>%
       filter(category == input$sentitype)
     
     }
     
     else {trenddata <- vsent %>%
       group_by(date, hour) %>%
       summarize(avg_sentiment = mean(tweet_sentiment))}
     
   })
   
   #Creating plots for various sentiments.
   output$sentitrendPlot <- renderPlot({
     
     categorysent <- trenddata()
     
     #Creating date-hour column for plotting purposes.
     categorysent$date_hour <- ymd_h(paste(categorysent$date, categorysent$hour))
     
     
     #Creating line graph which shows sentiment trend over time.
     ggplot(aes(x = date_hour, y = avg_sentiment), data = categorysent) +
       geom_line(stat = "identity", size = .8, color = 'firebrick') +
       scale_y_continuous(breaks = round(seq(min(categorysent$avg_sentiment),
                                             max(categorysent$avg_sentiment), .1), 1)) +
       scale_x_datetime(date_labels = "%b %d", date_breaks = "24 hours") +
       theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=18)) + xlab("Date") +
       ylab("Average Sentiment") + ggtitle(paste(input$sentitype, "Sentiment Trends", sep = " "))
     
     
     
   })
   
   
   
   #Creating reactive data for sentiment bar plot.
   thesentidata <- eventReactive(input$sentitype, {
     if (input$sentitype != "Overall"){
       thesentidata <- dailygroup %>%
         filter(category == input$sentitype)
     }
     
     else {thesentidata <- dailygroup}
     
   })
   
   
   
   output$sentimenttotals <- renderPlot({
     
     
     dailygroup <- thesentidata()
     
     #Stacked bar plot.
     ggplot(aes(x = date, y = group_total), data = dailygroup) +
       geom_bar(stat = 'identity', aes(fill = category)) +
       scale_fill_manual(values= senticolors) +
       ylab("Number of Tweets") + xlab("Date") + ggtitle("Daily Tweets Included in Analysis") +
       scale_y_continuous(limits = c(0, 30000), breaks = seq(0, 50000, 5000)) +
       theme(axis.text.x=element_text(angle=90, hjust=1), text = element_text(size=18))
     
   })
   
   
   
   
   
   ##### Emotion analysis tab.
   
   #Output for radar chart.
   output$emoradar <- renderChartJSRadar({
     
     chartJSRadar(overall_emotions)
     
   })
   
   
   #Output for emotion trend lines.
   output$emotrends <- renderPlot({
     
     #Creating data filter for inputs of emotion selection.
     emotions <- daily_emotion %>%
       filter(sentiment %in% tolower(input$emotionSelect))
     
     
     #Emotion trend line plot.
     ggplot(aes(x = y, y = emo_pct, color = sentiment), data = emotions) +
       geom_line(aes(group = sentiment), stat = "identity", size = 1.2) +
       scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 2.5)) +
       scale_color_manual(name = "Sentiment", values = emotioncolors) +
       scale_x_datetime(date_labels = "%b %d", date_breaks = "24 hours") +
       theme(axis.text.x=element_text(angle=90, hjust=1, vjust = .5), text = element_text(size=14)) +
       xlab("Date") + ylab("Percentage of Daily Emotions") + ggtitle("Emotion Trends") +
       geom_dl(aes(label = sentiment), method = list(dl.combine("first.points"),
                                                     cex = 1, dl.trans(y = y+0.3, x = x+1.7)))
     
   })
   
   
   
   #Generating random tweet to output on README page.
   output$randomTweet <- renderPrint({
     
     cat(as.character(vsent$text[[tweetnum]]))
     
   })
   
  
})