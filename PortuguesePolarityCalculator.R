library(lexiconPT)
ptPolarityCalculator <- function( sentencePt ){
  score <- 0
  ctr <- 0
  sentencePt <-  gsub("[[:punct:]]", "", sentencePt) #removing all punctuation 
  sentencePt <- gsub("[\r\n]", " ", sentencePt)
  sentencePt <- unlist(strsplit(sentencePt, " "))
  sentencePt <- as.vector(sentencePt)
  for (st in sentencePt){
    wordPolarity <- as.data.frame(get_word_sentiment(st))
    ctr <- ctr + 1
    if(is.null(wordPolarity$oplexicon_v3.0.polarity)){
      score <- 0 + score
    }
    else{
      score <- wordPolarity$oplexicon_v3.0.polarity+ score
    }
  }
  totalScore<- score/ctr
  score <- 0
  ctr <- 0
  
  print(totalScore)
  return(totalScore)
}
