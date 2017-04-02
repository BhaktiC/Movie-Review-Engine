#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  load("twitter authentication.Rdata")
  #once you run the code for the forst time, you can start from here onwards
  setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, 
                      access_token=accesstoken, access_secret=accesssecret)
  
  #harvest the tweets
  
   output$plot1<-renderPlot({ 
     
     #plot distribution of polarity
     
     all_tweets<-searchTwitter(input$text,1000, lang="en")
     
     tweets_df<- twListToDF(all_tweets)
     write.csv(tweets_df,"AllTweets.csv")
     
     tweet_txt=sapply(all_tweets,function(x) x$getText())
     
     #data cleaning using regex
     #remove retweet
     tweet_txt = gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweet_txt)
     #gsub-get substring
     #remove at people
     tewwt_txt=gsub("@\\w+","",tweet_txt)
     #remove punctuation
     tweet_txt=gsub("[[:punct:]]","",tweet_txt)
     #remove numbers
     tweet_txt=gsub("[[:digit:]]","",tweet_txt)
     #remove html links
     tweet_txt=gsub("http\\w+","",tweet_txt)
     #remove unnecessary spaces
     tweet_text=gsub("[\t]{2,}","",tweet_txt)
     tweet_txt=gsub("^\\s+|\\s+$","",tweet_txt)
     
     try.error=function(x)
     {
       #create missing values
       y=NA
       #try catch error
       try_error=tryCatch(tolower(x),error=function(e)e)
       
       #if not an error
       if(!inherits(try_error,"error"))
         y=tolower(x)
       return(y)
     }
     
     #lower case using try.error with sapply
     tweet_txt=sapply(tweet_txt,try.error)
     
     #remove NAs in tweet_txt
     tweet_txt=tweet_txt[!is.na(tweet_txt)]
     names(tweet_txt)=NULL
     
     #sentiment analysis
     class_emo=classify_emotion(tweet_txt, algorithm="bayes", prior=1.0)
     
     #get emotion best fit
     emotion=class_emo[,7]
     
     #substitute NAs by unknown
     emotion[is.na(emotion)]="unknown"
     
     #write.csv(class_emo,"AllTweetsAnalysis.csv")
     
     #classify polarity
     class_pol=classify_polarity(tweet_txt, algorithm="bayes")
     #get polarity best fit
     polarity=class_pol[,4]
     
     #data frame with result
     sent_df = data.frame(text=tweet_txt,
                          emotion=emotion, polarity=polarity,
                          stringsAsFactors = FALSE)
     sent_df = within(sent_df,
                      emotion<- factor(emotion,
                                       levels = names(sort(table(emotion),
                                                           decreasing = TRUE))))
     sent_df1 =  within(sent_df,
                        polarity<- factor(polarity,
                                          levels= names(sort(table(polarity),
                                                             decreasing = TRUE))))
     
     
     
     
     ggplot(sent_df1,aes(x=polarity))+
       geom_bar(aes(y=..count..,fill=polarity))+
       scale_fill_brewer(palette = "Dark2")+
       labs(x="polarity category", y="number of tweets",
            title="classification based on polarity")
  
  
})})
