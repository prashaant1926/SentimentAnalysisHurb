setwd("~/Desktop/Hurb Projects/Sentiment Analysis")

remotes::install_github("amrrs/itunesr")
install.packages("devtools")
library(devtools)
install.packages("tm")
install_github("computationalstylistics/stylo")

## Custom function uploaded to Github, check there for the raw code
source_url("https://raw.githubusercontent.com/prashaant1926/SentimentAnalysisHurb/prashaant1926-ptPolarityCalculator/PortuguesePolarityCalculator.R")
devtools::install_github("amrrs/itunesr")
install.packages("lexiconPT")
install.packages("wordcloud")
devtools::install_github("sillasgonzaga/lexiconPT")

## Installing Packages
library(itunesr)
library(googleLanguageR)
library(openxlsx)
library(ggplot2)
library(writexl)
library(tidyr)
library(dplyr)
library(sentimentr)
library(scales)
library(tidyverse)
library(tidytext)
library(DataExplorer)
library(tm)
library(lexiconPT)


##Getting Hurb's iOS App ID

Expedia <- 427916203
hurb <- 780640863 
booking <- 367003839
hopper <- 904052407
appStoreID = hurb

##### HURB USA DATA   #####

df_HurbUSApp <- getReviews(appStoreID, 'us', 1)
# Get information about the Hurb App in the US


### Getting country codes for Hurb App here ###

appMarketMeta <- c("br")

### Each page has about 51 reviews, We are going get reviews from US, Brazil and Portugal and bind them together and make a master dataframe
## hurb_reviews <- getReviews(appStoreID,'us',1)

no_ratings <- 500
no_reviews_per_page <-51
pages <- no_ratings/no_reviews_per_page #pages = number of ratings/ number of reviews per page
df_App_allMarkets = NULL

## Rounding up pagination and settings variables for second and last 

ratings <- ceiling(pages)
reviewStartPage <- 2
reviewEndPage <- ratings


## Loop for all App markets US, Brazil, Portugal

for( appMarket in appMarketMeta){
  
  #Create a data frame with the first page of reviews
  df_App <- getReviews(appStoreID, appMarket, 1)
  
  ## a loop for merging all the table data into one data frame
  for (page in reviewStartPage:reviewEndPage){
    df_App <- rbind(df_App, getReviews(appStoreID,appMarket, page))
  }
  
  ## Change 'Date' from POSIXt to date and sort df by ascending order date
  df_App$Date <-as.Date(df_App$Date)
  df_App <- df_App[order(df_App$Date),]
  
  ## Reordering columns in the dataframe by column index
  
  df_App <- df_App[c(7,4,5,1,6,2,3)]
  df_App$Market = appMarket
  
  ## Create a data frame for each local market
  df_App_Market <- print(appMarket)
  df_App_Market <- df_App
  
  ## Bind all markets together
  df_App_allMarkets <- rbind(df_App_allMarkets, df_App_Market)
  
  # Remove dublicated reviews
  df_App_allMarkets <- unique(df_App_allMarkets)
  ### End loop
  
}

plot_str(df_App_allMarkets) # plot the different variable trees in the dataset


#merging brazilian and US market data for hurb
df_HurbUSApp$Market <- "us" 
df_App_allMarkets <- rbind.data.frame(df_App_allMarkets, df_HurbUSApp)

# write_xlsx(df_App_allMarkets, path = ("BookingAppStoreReviewsUSandBr.xlsx"), col_names = TRUE)

qplot(df_App_allMarkets$Date)

# Sort data frame by ratin 1 -5 
df_App_allMarkets$Rating <- factor(df_App_allMarkets$Rating, 
                                   levels = c("1", "2", "3", "4", "5"))

## Convert to date format
df_App_allMarkets$Date <- as.Date(df_App_allMarkets$Date)


## FINDING OUT DETRACTORS AND PROMOTER DISTRIBUTION OF THE APP

ggplot(df_App_allMarkets, aes(x=as.factor(Rating), fill=as.factor(Rating))) +
  geom_bar(col="white") + scale_fill_manual("Ratings", values = c("1" = "#DA393B", "2" = "#EE6D45", "3" = "#F7E458", "4" = "#68E194", "5" = "#2E8B57"))+
  facet_wrap(~Market, scales = 'free_x')


####################

##Creating a new data frame based on All Markets where ratings are converted from numeric to string values

df_Ratings_Simplified <- data.frame("Date" = df_App_allMarkets$Date, "Rating" = df_App_allMarkets$Rating, "AppVersion" = df_App_allMarkets$App_Version, "Market" = df_App_allMarkets$Market)

df_Ratings_Simplified$Rating <- as.character(df_Ratings_Simplified$Rating)


## Removing all ratings with 3 star or neutral reviews

# Remove all ratings with 3-stars from df
df_Ratings_Simplified <- df_Ratings_Simplified[!df_Ratings_Simplified$Rating == "3", ]

# Replace 1-2 star ratings with text Negative, and 4-5 stars with text Positive
df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '1'] <- 'Negative'
  df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '2'] <- 'Negative'
  
  df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '4'] <- 'Positive'
  df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '5'] <- 'Positive'

  
  # Plot user feelings for each market (THIS CODE WAS TAKEN FROM AN ONLINE SOURCE)
  ggplot(df_Ratings_Simplified, aes(Rating, group = Market)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
    geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), size = 4, stat= "count", vjust = -0.4) +
    theme_bw() +
    theme(legend.position="none")+
    scale_fill_manual("Ratings", values = c("1" = "#ED5540", "2" = "#68E194"))+
    labs(y = "Rating", fill="Rating") +
    scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
    ylab("relative frequencies") +
    xlab("Procent") +  labs(title="User feeling in the Market", x="Reviews", y="Amount")+
    labs(caption = "(Negative = 1-2 stars, Positive = 4-5 stars)")+
    facet_wrap(~Market, scales = 'free_x')
  
  
  ### Plot feelings by weekday 
  df_Ratings_Feeling_Week <- df_Ratings_Simplified
  df_Ratings_Feeling_Week$Date <- format(as.Date(df_Ratings_Feeling_Week$Date), '%A')
  ggplot(df_Ratings_Feeling_Week, aes(x = as.factor(Date), fill = Rating, label = Rating)) +
    geom_bar(stat = "count")+
    theme_bw() +
    scale_fill_manual("Ratings", values = c("Positive" = "#2E8B57", "Negative" = "#ED5540"))+
    theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0))

 #### Converting characters to String ####
  
  df_App_allMarkets$Rating <- as.numeric(df_App_allMarkets$Rating)
  
   
  
  ##### Average Ratings per app version #####
  
  df_MeanRatingsVersion <- aggregate(df_App_allMarkets$Rating, list(df_App_allMarkets$App_Version), mean)
  
  
  ###  Renaming the columns in the dataframe ###
  
  names(df_MeanRatingsVersion)[1]<- "Version"
  names(df_MeanRatingsVersion)[2]<- "Rating"
  
  
  ## Rounding Mean Values of Ratings
  
  df_MeanRatingsVersion$Rating <- 
    round(df_MeanRatingsVersion$Rating, digits = 0)

  # Plot average ratings for each app version
  ggplot(df_MeanRatingsVersion, aes(x = Version, y = Rating, label=Rating)) +
    geom_bar(fill = "#32CD32", stat = "identity")+
    geom_text(position = 'identity', stat = 'identity', size = 4, vjust = -0.1)+
    
    theme_bw() +
    labs(title="Average ratings for each App Version", size=60) +
    labs(x="App Version", y="Avg. Rating") 
  
 
  
   
  #### Calculating Average Ratings for Each Day of the Month ####
  
  
  df_MeanRatingsDays <- aggregate(df_App_allMarkets$Rating, list(df_App_allMarkets$Date), mean)
  names(df_MeanRatingsDays)[1]<- "Date"
  names(df_MeanRatingsDays)[2]<- "Rating"
  
  # Convert dates to day of  month
  df_MeanRatingsDays$Date <- unclass(as.POSIXlt(df_MeanRatingsDays$Date))$mday
  

  
  ### AGGREGATE DAY OF THE MONTH AND find the rating mean
  
  names(df_MeanRatingsDays)[1]<- "Day"
  names(df_MeanRatingsDays)[2]<- "Rating"

  
  df_MeanRatingsDays <- aggregate(df_MeanRatingsDays$Rating, list(df_MeanRatingsDays$Day), mean)

  names(df_MeanRatingsDays)[1]<- "Day"
  names(df_MeanRatingsDays)[2]<- "Rating"
 
  # Round Ratings to 1 digit

  df_MeanRatingsDays$Rating <- lapply(df_MeanRatingsDays$Rating, round)

  
  # Plot mean ratings for each day of month
  df_MeanRatingsDays$Rating=as.numeric(df_MeanRatingsDays$Rating)
  
  ggplot(df_MeanRatingsDays, aes(x = Day, y = Rating, label = Rating))+
    geom_bar(fill = "#008000", stat = "identity")+
    theme_bw() +
    geom_text(position = 'identity', stat = 'identity', size = 4, vjust = -0.4)+
    labs(title="Average ratings by day of month", size=60) +
    theme(plot.title = element_text (color="black", face="bold", size=26, hjust=0)) +
    labs(x="Day of Month", y="Avg. Rating")+
    scale_x_discrete(limits=df_MeanRatingsDays$Day)+
    scale_y_continuous(limits = c(0,5))
  
  
  ### ###### EXPLORATORY DATA ANALYSIS, Average review length #########
  
  
  
  df_App_allMarkets$Review <- as.character(df_App_allMarkets$Review)
  df_App_allMarkets$ReviewLength <- nchar(df_App_allMarkets$Review)
  
  
  ### Calculate review length for each market BR, US, FR .... etc. ######
  
  df_MeanLengthMarket <- aggregate((df_App_allMarkets$ReviewLength), list(df_App_allMarkets$Market), mean)
  
  names(df_MeanLengthMarket)[1]<- "Market"
  names(df_MeanLengthMarket)[2]<- "AvgReviewLength"
  
  ## Creating a new all market dataframe for the avg review ####
  
  df2_App_allMarkets <- merge(df_App_allMarkets,df_MeanLengthMarket, by= "Market")
  
  ggplot(data=df2_App_allMarkets, aes(x=ReviewLength)) + 
    geom_density(aes(y = ..count..), color="#32CD32", fill = "#008000", alpha=0.6) +
    geom_vline(aes(xintercept = df2_App_allMarkets$AvgReviewLength), linetype = "dashed", size = 0.5)+
    
    facet_wrap(~Market, scales = 'free')+
    geom_text(data=df2_App_allMarkets, mapping=aes(x=AvgReviewLength, y=2, label=AvgReviewLength), check_overlap = TRUE, size=5, angle=0, vjust=1, hjust=-0.5)+
    ylim(0,5)+
    xlim(5,600)+
    theme_minimal()+
    labs(title="Review Character Length", subtitle = "The average length per review for each market", x="Review Length", y="Rating Average")+
    theme(plot.title = element_text( color="black", face="bold", size=22, hjust=0)) +
    theme(axis.title = element_text( color="black", face="bold", size=12)) +
    theme(plot.subtitle = element_text(family = "Helvetica", color="black", face="plain", size=10))+
    theme(strip.text = element_text(face="bold", size=12))  +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  
  ################################                           #######################################
  ###############################                              ######################################
  ################################                             ####################################
  ################################ TEXT SENTIMENT ANALYSIS    ####################################
  
  
  # Convert Reviews to character vector
  df2_App_allMarkets$Review <- as.character(df2_App_allMarkets$Review)
  df_ReviewSentiment <- df2_App_allMarkets
  
  
  ##REMOVING Columns that I don't need
  names(df_ReviewSentiment)
  df_ReviewSentiment <- subset(df_ReviewSentiment, select = -
                                 c(Author_URL, Author_Name, ReviewLength, Title))

  
  # Perform sentiment analysis and round values, Here we are finding the polarity score of each review
  # I had to create a custom function to calculate the polarity of a portuguese sentence see gitHub for the code
  
  
  df_ReviewSentiment$ReviewSentimentPolarity <- lapply(df_ReviewSentiment$Review,ptPolarityCalculator)

  df_ReviewSentiment$ReviewSentimentPolarity <- as.numeric(df_ReviewSentiment$ReviewSentimentPolarity)
  
  # Check first three and last 3 objects

  head(df_ReviewSentiment$ReviewSentimentPolarity, 3)
  tail(df_ReviewSentiment$ReviewSentimentPolarity, 3)
  
  
  ggplot(df_ReviewSentiment, aes(Rating,ReviewSentimentPolarity, group = Rating)) +
    geom_boxplot(fill="#3CB371") +
    theme_minimal()+
    labs(title="App reviews sentiment score per market", y="Average sentiment score")+
    geom_jitter(shape=16, size=0.7, position=position_jitter(0.3))
  
  
  ######  REVIEW SENTIMENT FOR EACH MARKET   ########
  
  # App reviews sentiment score
  ggplot(df_ReviewSentiment, aes(x = Date, y = ReviewSentimentPolarity, fill=Market)) + 
    geom_smooth(colour="black", size=1) +
    theme_bw() +
    theme_minimal()+
    labs(title="App reviews sentiment score per market", 
         subtitle = "Time period differs due to the amount of reviews in the near future", 
         x="Date", 
         y="Reviews Sentiment Scores")+
    facet_wrap(~Market, scales = "free_x")

  ###################### TEXT MINING #####################################
  
  # Here I am extracting words from the reviews and mapping them 
  
  textSentimentAnalysis <- as.vector(df_ReviewSentiment$Review)
  
  #removing stop words using the text mining package
  
  textSentimentAnalysis <- removeWords(textSentimentAnalysis, stopwords("portuguese"))
  textSentimentAnalysis <- removeWords(textSentimentAnalysis, "é")
  
  textSentimentAnalysis <- data.frame( text = textSentimentAnalysis)

  #Split reviews into individual words (Tokenization)  
  tidy_df <- textSentimentAnalysis %>%
    unnest_tokens(word, text)
  
  #Removing Stop Words NOTE THIS WORKS FOR ENGLISH ONLY 
  
  data("stop_words")
  
  tidy_df <- tidy_df %>%
    anti_join(stop_words)
  
  tidy_df <- data.frame(tidy_df)
  
  tidy_df %>%
    count(tidy_df$word, sort = TRUE)
 
 # Visualize words that occur 40+ or 100+ times
 tidy_df %>%
   count(word, sort = TRUE) %>%
   filter(n > 60) %>%
   mutate(word = reorder(word, n)) %>%
   ggplot(aes(word, n), group_by(tidy_df$df_ReviewSentiment.Market)) +
   theme_minimal()+
   labs(title="Words that occur more than 100 times", subtitle = "Occurring individual words in our sampled reviews", x="", y="Contribution to sentiment")+
   geom_col() +
   xlab(NULL) +
   coord_flip()
  
 # Add sentiment scores to each word in PORTUGUESE
 vectorPtAnalysis <- as.vector(t(tidy_df))
 
 
 
 #copy of tidy_df
 dfCopyTidyDf <- tidy_df
 names(dfCopyTidyDf)[1] <- "term"
 
 # Getting Portuguese lexicon 
 data("oplexicon_v3.0")
 
 #Merging the data based on the word sentiment
 newdfPT <- merge(dfCopyTidyDf, oplexicon_v3.0, by="term", sort = FALSE)
 
 # Removing unwanted columns
 drops <- c("polarity_revision","type")
 newdfPT <- newdfPT[ , !(names(newdfPT) %in% drops)]
 
 ##### Add sentiment to each word in English
 Polarity_word_counts <- newdfPT %>%  #counting the occurances of all the word while keeping the polarity intact
   count(term, polarity, sort = TRUE) %>%
   ungroup()
  
 
 Polarity_word_counts
 Polarity_word_counts %>%
   group_by(polarity) %>%
   top_n(25) %>%
   ungroup() %>%
   mutate(term = reorder(term, n)) %>%
   # Visualize the distrubution of word sentiment
   ggplot(aes(term, n, fill = polarity)) +
   geom_col(show.legend = FALSE) +
   theme_minimal()+
   labs(title="Distribution of word sentiment", subtitle = "Words that contribute to positive and negative sentiment", x="", y="Contribution to sentiment")+
   facet_wrap(~polarity, scales = "free_y") +
   coord_flip()
 
 
 library(reshape2)
 library(wordcloud)
 
 # Word cloud showing 200 words
 Polarity_word_counts %>%
   with(wordcloud(term, n, use.r.layout=FALSE,max.words = 100))
 
 # Word cloud showing 200 words by sentiment score
 Polarity_word_counts %>%
   acast(term ~ polarity, value.var = "n", fill = 1) %>%
   comparison.cloud(colors = c("#D9383A", "#2E8B57"),
                    use.r.layout=FALSE,
                    max.words = 300 )
    
 