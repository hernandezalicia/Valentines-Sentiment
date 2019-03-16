###### This code outlines the iterative cleaning process done on tweets for this project.
# Tweets we would like to work with contain the words "Valentine's Day" or "Valentines Day"
# The ideal tweets for this project do not promote a business or the sale of any product or service.
# The code works to eliminate as many of the tweets as possible for a sentiment analysis.
# Code also works to clean the tweets to ready them for sentiment analysis.

setwd('.\\Data\\')

file_names <- list.files() #where you have your files

#Reading in and combining all data files to one dataframe.
valentweets <- do.call(rbind,lapply(file_names,read.csv))
valentweets$text <- iconv(valentweets$text, from="UTF-8", to = "ASCII", sub="") #Changing encoding to clean column.
valentweets$source <- iconv(valentweets$source, from="UTF-8", to = "ASCII", sub="") #Changing encoding to clean column.


#####################

#Importing needed libraries.
library(dplyr)
library(lubridate)
library(rtweet)
library(tm)
library(text2vec)
library(qdap)
library(textclean)

library(qdapDictionaries)
#####################



##########  Data Cleaning   ##########

#Original data is approximately 740k rows.

#Removing duplicate rows that appeared from scraping each day due to time zone used by Twitter API.
### Removing based on unique tweet status id.
valentweets <- valentweets[!duplicated(valentweets$status_id),]




#######################

## Removing spam.


#Identifying users that are contributing large amounts of spam tweets.
# users that have greater than 5 tweets, assumption no one tweets about the subject this much.
# If users tweet about excessively about topic, we want to limit the influence of the overall sentiment.
usercounts <- valentweets %>%
  group_by(screen_name) %>%
  summarize(n = n()) %>%
  filter(n >= 5)

#Removing these usernames using an anit-join.
valentweets <- anti_join(valentweets, usercounts, by= "screen_name")


#Grouping by username and by tweet to show users who are sending repeated tweets.
repeattweets <- valentweets %>%
  group_by(screen_name, text) %>%
  summarize(n = n()) %>%
  filter(n >= 2)

#Removing users which have repeatedly sent the same tweet.
#It was found the vast majority of these tweets were obviously spam.
valentweets <- anti_join(valentweets, repeattweets)




#Removing tweets sent by different users by common spam words.
#Filtering some tweets with common spam words/hashtags.
spamwords <- valentweets %>% 
  filter(grepl(paste0('#amazon|location|purchase|link|carnival|details|visit|giveaway|entered|book now|', 
                      'enter to win|chance to win|click|via|limited offer|limited time|offer ends|call now|check out|',
                      'hyatt|place an order|order now|discount code|all orders|use code|free shipping|checkout|',
                      'place your order|place your orders|shop now|with code|offer code|enter code'), valentweets$text,
               ignore.case = TRUE))

#Removing tweets with common words.
valentweets <- anti_join(valentweets, spamwords, by = 'text')




######## Examining and removing sources. ############
#Removing sources contributing majority spam tweets.
sourcecounts <- valentweets %>%
  group_by(source) %>%
  summarize(n = n()) %>%
  filter(n < 5000)

#Removing these sources through an anti-join.
valentweets <- anti_join(valentweets, sourcecounts, by = 'source')

#Adding additional sources to list that were considered to post spam tweets upon further examination.
#Also removed tweets posted from Facebook after originally only removing links as too many proved to be spam-like.
sourcelist <- list("WordPress.com", "MailChimp", "dlvr.it", "Sprout Social", "TweetDeck", "Buffer", "IFTTT",
                   "Hootsuite Inc.", "Instagram", "Facebook")

#Removing tweets posted by 'spam' sources.
valentweets <- valentweets[! valentweets$source %in% sourcelist,]





#### Final process of tweet removal, this list was created by manually examning the dataframe.
### As username contributing spam was found it was added to this list and later removed.
### This was a very iterative process and does not guarantee all spam was found.

#Adding additional usernames found to be contributing spam.
userlist <- list("TH_Fans_Club", "_Anonymo7", "see_want_buy", "CatsExclusive", "T_raine", "sweetremediesuk", "Sweetmilkteaaa",
                 "SweetHolyMofo", "sweetheartstj", "SweetDeeJadore", "swampmusicinfo", "SunnySweetDays", "sunita1210",
                 "sunita_omguru", "SucreNewOrleans", "studiovickn", "stonewrapper", "stlmommy1", "stfoodcinema",
                 "SnowcatCinema", "SmartDecor1", "SMAntonuccio", "smallcakespland", "sizzling_silver", "sirenshaven",
                 "SpeclSalesDeals", "CouponProBlog", "BridgeHealt", "BKbyPassionateO", "Ketann7", "UtProsim540", "thedextazlab",
                 "PortalProphecy", "OnyxYouth", "__cocochantel", "_ashleybrionne_", "_Harmonyxo", "_jon_games", "1027KIISFM",
                 "105River", "1063CowboyCntry", "1065WYRK", "1079YYD", "108_jakarta", "1800flowers", "1800SLAYYYTER",
                 "2kidsandacoupon", "2peasandadog", "31i55a", "3womenandanoven", "4cbenamis", "50PlusNow", "8thStJewelry",
                 "925TheJewel", "937NOW_", "99beersonwall", "AbbeyGraphics", "Absolover", "adailydoseofmom", "adamsappleclub",
                 "AddiesWorld", "adidasyeezy2017", "AdeleGutman", "AdeleJFoster", "adidassbot", "After5Detroit", "AlayneCurtiss",
                 "alexanderdiez", "AlexisSilverton", "AlexisTarraz", "AllThingsHairUK", "AlsonJewelers", "amadorn", "AmandaJacklin6",
                 "AmericanSwiss", "AmznDealAlert", "AndersonMIX97", "AndreaCoventry", "DanceandFeet", "TCArthurMurray",
                 "EncourageFitnss", "ContempConcepts", "loveinsanelynow", "DecorbyDS", "RobinReno", "BADD_Batter", "TXStarsRingo",
                 "ProvinceCanada", "frugalfreebies", "itsbinkybee", "cmovkids", "HardFactorNews", "mountainskysoap",
                 "thepastashop", "KcSclassof2021", "JoeFreshest", "MeenuRa23133866", "joyofmemories", "SmartArtProduct",
                 "LyndeHouse", "hopecoffee", "HeywoodSpa", "DearCreatives", "SimoneDavid_SEA", "veggingvegan", "ilovejwhairco",
                 "meh_hu_vicky", "DCsportsGrl", "LeonArt_2017", "psilocybae", "MedGrillVic", "TinaJoEvans2", "Bagzton",
                 "nevermrsmartin", "MashingIn", "TheInfirmaryFCU", "atwater_terri", "FarmingCannabis", "perkband",
                 "DiscountComputr", "Goodcount", "docuplusja", "salay_handmade", "SwagsSportShoes", "TheFrailNinja",
                 "VTurner8", "Katelyn072699", "RodanFields_TK", "Karingarrido2", "lunarflower_pro", "Alopecia_Barbie",
                 "WrappedByDeee", "Shafer4867", "wiggett7", "tribecalledess", "DeeRolon", "Ons_Booi", "KhanyaMzendana",
                 "Ziiya_M", "Shoun_B", "niaazuri", "snufflelove", "GoLLLion", "Mayasmojitoland", "MAINEnatics_BU",
                 "whfarmchardon", "renea_mua", "haldar_anjana", "NehaHaldar2", "cannabombz", "AAbayomi9", "NehaHaldar2",
                 "kevinchambers19", "avaroseagency", "TEAM_OPH", "GammaRhoZPB", "kylenwicks", "arts4youngstars",
                 "CharlieDaniels", "_yannnaaaaa", "StephD311", "OkstateSHPE", "thejusmekamil", "samtuckeryoung",
                 "ShaunaEYoung", "cfa_richmondky", "Rareradar", "UptownDrug", "Fancygirlnyc", "PrimitivesLTD",
                 "mdscollections", "BrandiB79", "TownshipTALK", "donna_garcia21", "Back_to_Eden", "currycottage73",
                 "FrankTrainor", "beshcollections", "TerraGallery", "antoniasrestaur", "antoniasrestaur", "0514tic",
                 "dlc917", "HalleelujahMom", "iulai12", "AvenirCentre", "LindatheBraLady", "UESLibrary",
                 "TheMessiahOmen", "TheRomeBraves", "auqeno", "concupisco_com", "5NEWS", "DawnSweetTreats", "NOLAnews",
                 "RedlandsNews", "CHCottage", "downtownsinyc", "Flowerbagscouk")

#Removing tweets of users on list.
valentweets <- valentweets[! valentweets$screen_name %in% userlist,]



### Seeing how many times same tweet appears.

sametweet <- valentweets %>%
  group_by(text) %>%
  summarize(n= n())


######
#Creating dataframe with features we will use.
valentweets <- valentweets %>%
  select(user_id, status_id, created_at, screen_name, text, source, display_text_width, verified)

#Changing format of ID columns so that they do not appear as N/As when written to csv.
valentweets$user_id <- as.character(valentweets$user_id)
valentweets$status_id <- as.character(valentweets$status_id)

#Writing dataframe of non-spam tweets to csv.
#For possible later use.
write_as_csv(valentweets, 'Valentweets.csv')


##################################################
##################################################
#######    Additional Cleaning Steps    ##########
##################################################
##################################################

#### Cleaning the text column of the data. ####
valentweets$text <- gsub("http\\S+", "",  valentweets$text) #Removing links.
valentweets$text <- gsub("@\\w+", "", valentweets$text) #Removing usernames.
valentweets$text <- gsub('@', "at", valentweets$text) #Replacing when users enter @ symbol meaning the word 'at'.
valentweets$text <- gsub('(#\\w+)', "", valentweets$text)   #Removing hashtags.
valentweets$text <- gsub('#', "", valentweets$text)   #Removing stray number signs in front of emojis etc.
valentweets$text <- gsub("<.*?>", "", valentweets$text) #Removing encoded emojis.
valentweets$text <- removeNumbers(valentweets$text) #Removing numbers.
valentweets$text <- stripWhitespace(valentweets$text) #Removing any additional white space.


#Transforming the everything to lowercase.
valentweets$text <- tolower(valentweets$text)


#Separating date and time column.
valentweets$created_at <- ymd_hms(valentweets$created_at)
valentweets$time <-  strftime(valentweets$created_at ,format="%H:%M:%S")
valentweets$date <- as.character(strptime(valentweets$created_at, format='%Y-%m-%d'))





#Removing elongated words. This only appears to work when done at the last step. Otherwise it produces some NAs.
valentweets$text <- replace_word_elongation(valentweets$text) #Removing elongated words.






###################

#Creating lexicon for commonly used internet slang in this context.
internet_slang <- data.frame(x = c("bf", "gf", "omg", "tmr", "tmrw", "tmrow", "rlly", "rly", "ik",
                                "ikr", "aka", "bc", "bby", "gotta", "lol", "gtg", "u", "thx", "pls",
                                "rn", "plz", "fyi", "jk", "fr", "til", "w", "gonna", "b", "u",
                                "wtf", "tbh", "n", "bday", "tryna", "pic", "hmu", "lil", "bff",
                                "bffs", "dunno", "wut", "wuts", "thru", "cmon", "luv", "luvs", "r",
                                "wanna", "rlly", "ilysm", "ily", "lmao", "gm", "psa", "ur", "gon", "sumn",
                                "tf", "bouta", "boutta", "cuz", "wyd", "becuz", "y", "goin", "ur", "tht",
                                "thts", "feb", "whos", "amp", "yal", "yall", "ight", "yr", "bfs", "gfs", "tbf",
                                "s/o", "fo", "dat", "sum", "imma", "idc", "bcuz", "bcos", "yr", "yrs"),
                          
                          y = c("boyfriend", "girlfriend", "oh my god", "tomorrow", "tomorrow",
                                "tomorrow", "really", "really", "i know", "i know right", "also known as",
                                "because", "baby", "have to", "laugh out loud", "got to go", "you", "thanks",
                                "please", "right now", "please", "for your information",
                                "just kidding", "for real", "until", "with", "going to", "be", "you",
                                "what the fuck", "to be honest", "and", "birthday", "trying to", "picture",
                                "hit me up", "little", "best friend forever", "best friends forever",
                                "do not know", "what", "what is", "through", "come on", "love", "loves", "are",
                                "want to", "really", "i love you so much", "i love you", "laughing my ass off",
                                "good morning", "public service announcement", "your", "going to", "something",
                                "the fuck", "about to", "about to", "because", "what are you doing",
                                "because", "why", "going", "your", "that", "that is", "february", "who is",
                                "and", "you all", "you all", "alright", "year", "boyfriends",
                                "girlfriends", "to be fair", "significant other", "for", "that", "some",
                                "i am going to", "i do not care", "because", "bcos", "year", "years"))



#Creating function to replace internet slang words words.
replaceWords <- function(string){
  for(x in 1:nrow(internet_slang))
    string <- gsub(paste0("\\b", internet_slang[x,"x"], "\\b"), paste0(" ",internet_slang[x,"y"]," "), string)
  
  #Ensuring no leading or trailing white space.
  string <- gsub("^\\s+|\\s+$", "", string)
  
  return(string)
}


#Applying above function.
valentweets$text<- replaceWords(valentweets$text)





#Cleaning up words/typos etc.

#Creating contraction function.
replaceContracts <- function(string, dictionary){
  colnames(dictionary)[1] <- 'x'
  colnames(dictionary)[2] <- 'y'
  
  for(x in 1:nrow(dictionary))
    string <- gsub(paste0("\\b", dictionary[x,"x"], "\\b"), paste0(dictionary[x,"y"]), string)
  

  return(string)
}

#Applying the contraction function.
valentweets$text <- replaceContracts(valentweets$text, contractions)
      #Contractions are replaced above with punctuation, we will do this again except for we're and we'll because
      #those are words without the punctiation and will make a large number of tweets not make sense.

#Second round of replacing contractions.
contractions2less <- contractions[-c(47, 48), ]
contractions2less$contraction <- removePunctuation(as.character(contractions2less$contraction))

valentweets$text <- replaceContracts(valentweets$text, contractions2less)



#Replacing slashes with a space before removing punctuation so it does not create joined words.
valentweets$text <- gsub('/', " ", valentweets$text)


#Removing punctuation as final step keeping hyphens betweet words.
valentweets$text <- removePunctuation(valentweets$text, preserve_intra_word_dashes = TRUE)


#Removing white space one last time to verify there is none.
valentweets$text <- stripWhitespace(valentweets$text)




######
#Creating clean dataframe with features we will use.
cleantweets <- valentweets %>%
  select(user_id, status_id, created_at, screen_name, text, source, display_text_width, verified, time, date)



### Writing clean data csv file for ShinyApp creation.
write_as_csv(cleantweets, 'CleanTweets.csv')