# Input tweets for the 4 teams with an additinal tag of FIFA

library(twitteR)
library(ROAuth)
library(ggplot2)

ARG.list <- searchTwitter('#ARG #FIFA', n=1000, cainfo="cacert.pem")  
ARG.df = twListToDF(ARG.list)  

BRA.list <- searchTwitter('#BRA #FIFA', n=1000, cainfo="cacert.pem")  
BRA.df = twListToDF(BRA.list)  

head(BRA.list)
GER.list <- searchTwitter('#GER #FIFA', n=1000, cainfo="cacert.pem")  
GER.df = twListToDF(GER.list)  

NED.list <- searchTwitter('#NED #FIFA', n=1000, cainfo="cacert.pem")  
NED.df = twListToDF(NED.list) 

# Import all other libraries
library (plyr)
library (stringr)
#Generate the function
score.sentiment = function(sentences, pos.words, neg.words,.progress='none')  
{  
  require(plyr)  
  require(stringr)       
  
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  
  good.smiley <- c(":)")
  bad.smiley <- c(":(",";)",":'",":P") 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    
    sentence = gsub(":)", 'awsum', sentence)
     
    sentence = gsub('[[:punct:]]', '', sentence)  
    
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    
    sentence = gsub('\\d+', '', sentence)  
    
    # and convert to lower case:  
    
    sentence = tolower(sentence)  
    
    # split into words. str_split is in the stringr package  
    
    word.list = str_split(sentence, '\\s+')  
    
    # sometimes a list() is one level of hierarchy too much  
    
    words = unlist(word.list)  
    
    # compare our words to the dictionaries of positive & negative terms  
    
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    
    # match() returns the position of the matched term or NA  
    # we just want a TRUE/FALSE:  
    
    pos.matches = !is.na(pos.matches)  
    
    neg.matches = !is.na(neg.matches)  
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
    
    score = sum(pos.matches) - sum(neg.matches)  
    
    return(score)  
    
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 



#Load sentiment word lists
hu.liu.pos = scan('C:/temp/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C:/temp/negative-words.txt', what='character', comment.char=';')

#Add words to list
pos.words = c(hu.liu.pos, 'upgrade', 'awsum')
neg.words = c(hu.liu.neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical',"suspension","no")
team = c("vs.","vs","versus")

#convert text to factor
ARG.df$text<-as.factor(ARG.df$text)
BRA.df$text<-as.factor(BRA.df$text)
NED.df$text<-as.factor(NED.df$text)
GER.df$text<-as.factor(GER.df$text)

#calculate all the scores
ARG.scores = score.sentiment(ARG.df$text, pos.words,neg.words, .progress='text')
BRA.scores = score.sentiment(BRA.df$text, pos.words,neg.words, .progress='text')

NED.scores = score.sentiment(NED.df$text,pos.words,neg.words, .progress='text')

GER.scores = score.sentiment(GER.df$text,pos.words,neg.words, .progress='text')

ARG.scores$Team = 'Argentina'
BRA.scores$Team = 'Brazil'
NED.scores$Team = 'Netherland'
GER.scores$Team = 'Germany'

#Check the negative tweets. What made them negative
ARG.scores.2 = subset(ARG.scores,ARG.scores$score < 0)

head(ARG.scores.2)


# Final outputs
hist(ARG.scores$score)

hist(BRA.scores$score)

hist(NED.scores$score)
hist(GER.scores$score)


table(ARG.scores$score)
table(BRA.scores$score)
table(NED.scores$score)
table(GER.scores$score)



head(all.scores)
all.scores = rbind(ARG.scores, NED.scores, GER.scores,BRA.scores)

table(all.scores$score,all.scores$Team)

ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=Team), binwidth=1) +
  facet_grid(Team~.) + # make a separate plot for each hashtag
  theme_bw() + scale_fill_brewer() # plain display, nicer colors

