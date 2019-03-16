library(rtweet)

######### Setting up Twitter API ########################

appname <- ""

## api key
key <- ""

## api secret
secret <- ""

##access token and secret
atoken <- ""
asecret <- ""

## creating token
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = atoken,
  access_secret = asecret)

#########################################################

## Generating variable for today's date for naming convention and limit of dates tweets can be gathered from.
todaysdate <- Sys.Date()

#Scraping tweets from twitter, limit is 18000 per 15 minutes.
tweets <- search_tweets("Valentine's Day OR Valentines Day", n = 50000, lang = "en", full_text = TRUE, include_rts = FALSE,
                        since = todaysdate, resulttype = 'popular', include_entities = FALSE, retryonratelimit = TRUE)


#Saving tweets as csv file name format as today's date.
save_as_csv(tweets, paste(".\\Data\\", todaysdate, ".csv", sep = ""), prepend_ids = TRUE, na = 'N/A',
            fileEncoding = "UTF-8")
