# needed libraries 
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(tidytext)
library(stringr)

# FUNCTIONS CREATED:
# 1. lengthsWithStopWords
# 2. rm_words
# 3. lengthsWithoutStopWords
# 4. firstPersonFrequency
# 5. thirdPersonFrequency
# 6. promptFrequency

#----------------------------------------------------------------------#
# LENGTH OF TRANSCRIPT WITH STOP WORDS 
lengthsWithStopWords <- function(){
  lengths <- c()
  for (i in 1:length(transcript)){
    lengths <- c(lengths, sapply(strsplit(transcript[i], " "), length))
  }
  return(lengths) 
}




#----------------------------------------------------------------------#
# LENGTH OF TRANSCRIPT WITHOUT STOP WORDS 
lengthsWithoutStopWords <- function(){
  lengthsNoStopWords <- c()
  for (i in 1:length(noStopWordsTranscript)){
    lengthsNoStopWords <- c(lengthsNoStopWords, sapply(strsplit(noStopWordsTranscript[i], " "), length))
  }
  # making the lengths calculated with no stop words global 
  lengthsNoStopWords <- lengthsNoStopWords 
}

#----------------------------------------------------------------------#
# FIRST PERSON PRONOUNS FREQUENCY 
firstPersonFrequency <- function(){
  firstPersonPronouns <- c(" me ", " my ", " mine ", " myself ", " i "," im ")
  countFirstPerson <- c()
  frequencyFirstPerson <- c()
  for (i in 1:length(transcript)) {
    sumOfPronouns <- sum(str_count(transcript[i], pattern = firstPersonPronouns))
    countFirstPerson <- c(countFirstPerson, sumOfPronouns)
    frequencyCalculation <- (sumOfPronouns / lengths[i]) * 100
    frequencyFirstPerson <- c(frequencyFirstPerson, frequencyCalculation)
  }
  countFirstPerson <<- countFirstPerson
  frequencyFirstPerson <<- frequencyFirstPerson
}

#----------------------------------------------------------------------#
# THIRD PERSON PRONOUNS FREQUENCY 
thirdPersonFrequency <- function(){
  thirdPersonPronouns <- c(" he ", " him ", " his" ," she ", " her ", "  hers ", " it ", " its ", " they ", " them ", " their ", " theirs ")
  countThirdPerson <- c() # the total number of third person pronouns used 
  frequencyThirdPerson <- c() # the frequency of third person pronouns 
  # for loop to go through transcript 
  for (i in 1:length(transcript)) {
    sumOfPronouns <- sum(str_count(transcript[i], pattern = thirdPersonPronouns))
    countThirdPerson <- c(countThirdPerson, sumOfPronouns)
    frequencyCalculation <- (sumOfPronouns / lengths[i]) * 100
    frequencyThirdPerson <- c(frequencyThirdPerson, frequencyCalculation)
  }
  countThirdPerson <<- countThirdPerson
  frequencyThirdPerson <<- frequencyThirdPerson
}

#----------------------------------------------------------------------#
# PROMPT FREQUENCY 
promptFrequency <- function(){
  prompts <- c(" snake ", " snakes ", " mistake " ," map ", " cup ", 
               "  tea ", " stamp ", " stamps ", " collection ", " birthday ", 
               " stapler ", " missing ", " dog ", " fire ", " kitchen", " tow ",
               "truck", "abandoned", "house", "locked", "box", "love", "song", 
               "dance", "competition")
  countPrompts <- c() # the total number of third person pronouns used 
  frequencyPrompts <- c() # the frequency of third person pronouns 
  # for loop to go through transcript 
  for (i in 1:length(transcript)) {
    sumOfPrompts <- sum(str_count(transcript[i], pattern = prompts))
    countPrompts <- c(countPrompts, sumOfPrompts)
    frequencyCalculation <- (sumOfPrompts / lengths[i]) * 100
    frequencyPrompts <- c(frequencyPrompts, frequencyCalculation)
  }
  countPrompts <<- countPrompts
  frequencyPrompts <<- frequencyPrompts
}

#----------------------------------------------------------------------#
# MAIN
main <- function(){
  # getting the original csv -> make sure you put in the correct file path if you are running this 
  mainFile <<-  read.csv("data.csv", stringsAsFactors=FALSE) 
  sessionID <<- mainFile[,2] # making IDs global 
  transcript <- mainFile[,3] # the transcipts 
  transcript <- removePunctuation(transcript) #removing punctuation 
  transcript <<- tolower(transcript) #making script all lower case 
  promptGiven <<- mainFile[,6] #prompts 
  
  lengthsWithStopWords()
  noStopWordsTranscript <<- rm_words(transcript, tm::stopwords("en")) 
  lengthsWithoutStopWords()
  firstPersonFrequency()
  thirdPersonFrequency()
  promptFrequency()
  
  #----------------------------------------------------------------------#
  
  frequencyPromptsFrame <- data.frame(sessionID, prompt, transcript, noStopWordsTranscript, lengths,
                                      countFirstPerson, frequencyFirstPerson, countThirdPerson,
                                      frequencyThirdPerson, countPrompts, frequencyPrompts)
  # writing to a csv in the directory, the new data 
  write.csv(frequencyPromptsFrame,"frequencyPromptsFrame.csv", row.names = FALSE)
  
  # ---------------------------GRAPHICS--------------------------- #
  # BOX PLOTS 
  boxplot(lengths, col = "green", main = "With (green) vs. Without Stop Words (red)")
  boxplot(lengthsNoStopWords, col = "red")
}
main()


