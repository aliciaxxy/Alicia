library(readr)
library(tidyr)
library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2); 
library(ggthemes)
library(qdap)
library(tm)
library(gridExtra)
library(corrplot)

#we import mydata_clean.csv to Rstudio from 'Import Dataset' on the upper right 'Environment'
#then choose 'From Text(readr)', and select mydata_clean.csv

#Question1
#What is the rating trend of different companies from 2009 to 2018? 
mydata_clean$time <- (substr(mydata_clean$dates,8,12))
mydata_clean$time <- gsub('\\s+', '', mydata_clean$time)
#split mydata_clean by commpany
company1 <- mydata_clean%>%
  filter(company == 'google')%>%
  group_by(time)%>%
  summarize(google=length(`job-title`))
company2 <- mydata_clean%>%
  filter(company == 'amazon')%>%
  group_by(time)%>%
  summarize(amazon=length(`job-title`))
company3 <- mydata_clean%>%
  filter(company == 'netflix')%>%
  group_by(time)%>%
  summarize(netflix=length(`job-title`))
company4 <- mydata_clean%>%
  filter(company == 'facebook')%>%
  group_by(time)%>%
  summarize(facebook=length(`job-title`))
company5 <- mydata_clean%>%
  filter(company == 'apple')%>%
  group_by(time)%>%
  summarize(apple=length(`job-title`))
company6 <- mydata_clean%>%
  filter(company == 'microsoft')%>%
  group_by(time)%>%
  summarize(microsoft=length(`job-title`))
companyAll<-merge(company1,company2,all=T)
companyAll<-merge(companyAll,company3,all=T)
companyAll<-merge(companyAll,company4,all=T)
companyAll<-merge(companyAll,company5,all=T)
companyAll<-merge(companyAll,company6,all=T)
companyAll<-companyAll[-c(1:2),]
companyAll<-companyAll%>%
  gather('condition','number',-time)

mydata1<-mydata_clean%>%
  group_by(time)%>%
  summarize(n=length(`job-title`))

mydata1<-mydata1[-1,]
mydata1<-mydata1[-1,]

ggplot(mydata1, aes(x=time,y=n,group=100))+geom_line(colour='violet')+geom_point(size=4,shape=20,colour='violet',fill='white')


ggplot(data=companyAll,mapping=aes(x=time,y=number,group=condition))+geom_line(aes(linetype=condition,color=condition))+theme(legend.position='top')

#Question1
#What is the rating trend of different companies from 2009 to 2018? 
mydata_clean$time <- (substr(mydata_clean$dates,8,12))
mydata_clean$time <- gsub('\\s+', '', mydata_clean$time)
#split mydata_clean by commpany
company1 <- mydata_clean%>%
  filter(company == 'google')%>%
  group_by(time)%>%
  summarize(google=length(`job-title`))
company2 <- mydata_clean%>%
  filter(company == 'amazon')%>%
  group_by(time)%>%
  summarize(amazon=length(`job-title`))
company3 <- mydata_clean%>%
  filter(company == 'netflix')%>%
  group_by(time)%>%
  summarize(netflix=length(`job-title`))
company4 <- mydata_clean%>%
  filter(company == 'facebook')%>%
  group_by(time)%>%
  summarize(facebook=length(`job-title`))
company5 <- mydata_clean%>%
  filter(company == 'apple')%>%
  group_by(time)%>%
  summarize(apple=length(`job-title`))
company6 <- mydata_clean%>%
  filter(company == 'microsoft')%>%
  group_by(time)%>%
  summarize(microsoft=length(`job-title`))
companyAll<-merge(company1,company2,all=T)
companyAll<-merge(companyAll,company3,all=T)
companyAll<-merge(companyAll,company4,all=T)
companyAll<-merge(companyAll,company5,all=T)
companyAll<-merge(companyAll,company6,all=T)
companyAll<-companyAll[-c(1:2),]
companyAll<-companyAll%>%
  gather('condition','number',-time)

mydata1<-mydata_clean%>%
  group_by(time)%>%
  summarize(n=length(`job-title`))

mydata1<-mydata1[-1,]
mydata1<-mydata1[-1,]

ggplot(mydata1, aes(x=time,y=n,group=100))+geom_line(colour='violet')+geom_point(size=4,shape=20,colour='violet',fill='white')


ggplot(data=companyAll,mapping=aes(x=time,y=number,group=condition))+geom_line(aes(linetype=condition,color=condition))+theme(legend.position='top')

#Question2
##What company performs best given the ratings from employees? What is the correlation #between overall ratings and detailed ratings such as work balance and culture values? And #what is the rating that values most for employees?

#overall rating for different companies
total.rate  <- select(mydata_clean, company,`overall-ratings`) %>%
  group_by(company) %>%
  summarise(average.rate = round(mean(`overall-ratings`),2)) 

ggplot(data = total.rate, mapping = aes(x = company, y = average.rate)) + 
  geom_bar(stat = 'identity',width = 0.5,color = "green") + 
  ggtitle('Average Overall Ratings of All Five Companies') + 
  xlab('Companies') + 
  ylab('Average Overall Rating')

#histogram
ggplot(mydata_clean, aes(x=`overall-ratings`))+
  geom_histogram(binwidth = .5, colour='violet',fill='white')+
  facet_grid(company~.)
#boxplot is better to show the distribution
ggplot(mydata_clean, aes(x=company, y=`overall-ratings`,fill=company))+
  geom_boxplot(colour='grey')

#mydata_clean$`work-balance-stars`
workload.rate <- select(mydata_clean, company,`work-balance-stars`) %>%
  filter(`work-balance-stars` != 'none') %>%
  group_by(company) %>%
  summarise(work.rate = round(mean(as.numeric(as.character(`work-balance-stars`)),2)))
workload.rate

ggplot(data = workload.rate, mapping = aes(x = company, y = work.rate)) + 
  geom_bar(stat = 'identity',width = 0.5,color = "green") + 
  ggtitle('Average work-balance Ratings of All Five Companies') + 
  xlab('Companies') + 
  ylab('Average work-balance Rating')

#correlation
sum(is.na(mydata_clean))
full_all <- mydata_clean[,c(9:(ncol(mydata_clean)-1))];full_all
sum(is.na(full_all))

#class(full_all$`overall-ratings`)

#class(full_all$`work-balance-stars`)
#(full_all$`culture-values-stars`)
#class(full_all$`carrer-opportunities-stars`)
#class(full_all$`comp-benefit-stars`)
#class(full_all$`senior-mangemnet-stars`)



full_all$`overall-ratings`<-as.numeric(full_all$`overall-ratings`)

full_all$`work-balance-stars`<-as.numeric(as.character(full_all$`work-balance-stars`))
full_all$`culture-values-stars`<-as.numeric(as.character(full_all$`culture-values-stars`))
full_all$`carrer-opportunities-stars`<-as.numeric(as.character(full_all$`carrer-opportunities-stars`))
full_all$`comp-benefit-stars`<-as.numeric(as.character(full_all$`comp-benefit-stars`))
full_all$`senior-mangemnet-stars`<-as.numeric(as.character(full_all$`senior-mangemnet-stars`))

sum(is.na(full_all))

full_all_clean<-na.omit(full_all)
sum(is.na(full_all_clean))

#class(full_all_clean$`overall-ratings`)
#class(full_all_clean$`work-balance-stars`)
#class(full_all_clean$`culture-values-stars`)
#(full_all_clean$`carrer-opportunities-stars`)
#class(full_all_clean$`comp-benefit-stars`)
#class(full_all_clean$`senior-mangemnet-stars`)


#The function corrplot() takes the correlation matrix as the first argument. The second argument (type="upper") is used to display only the upper triangular of the correlation matrix.
corrplot(cor(full_all_clean), method="color", type = "upper", tl.col="red",order='hclust',
         tl.srt=45, addCoef.col = "gray8", diag = T)

#3Considering the reviews of former employees, what are the driving reasons for them to leave? And how can the company improve?

#divdie employee to leave(former employee) and now(current employee)
leave <- mydata_clean[grep("Former Employee", mydata_clean$`job-title`),]
now <- mydata_clean[grep("Current Employee", mydata_clean$`job-title`),]

#former employee overall ratings
ggplot(data=leave,aes(x=leave$`overall-ratings`))+
  geom_histogram(fill='sienna')+
  theme_economist()

#current employee overall ratings
ggplot(data=now,aes(x=now$`overall-ratings`))+
  geom_histogram(fill='sienna')+
  theme_economist()

#former employees bad words
cons_leave_terms<-freq_terms(text.var = leave$`cons`,top = 25,stopwords = Top200Words)
#remove unrelevant words eg:"company","it's","job","working","sometimes","really","teams"
cons_leave_terms<-cons_leave_terms%>%
  filter(!WORD %in% c("company","it's","job","working","sometimes","really","teams"))
#graph
ggplot(cons_leave_terms, aes(x =reorder((WORD),FREQ), y = FREQ)) +
  geom_bar(stat = "identity",fill="lightskyblue") +
  geom_text(aes(label =FREQ), vjust = 0.5, colour = "black", position = position_dodge(0), size = 3) +
  labs(x="WORD in Summary",y="Word Frequency",title = "Former Employee Most Common Cons Words") +
  theme(panel.grid = element_blank(),panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),title=element_text(size=13, face= "bold", vjust=1, hjust=0.5),axis.text.x = element_text(size = 7,angle = 10,hjust = 1))+coord_flip()

#current employees bad words
cons_now_terms<-freq_terms(text.var = now$`cons`,top = 25,stopwords = Top200Words)
#remove unrelevant words eg:"company","it's","job","working","sometimes","really","teams"
cons_now_terms<-cons_now_terms%>%
  filter(!WORD %in% c("company","it's","job","working","sometimes","really","teams"))
#graph
ggplot(cons_now_terms, aes(x =reorder((WORD),FREQ), y = FREQ)) +
  geom_bar(stat = "identity",fill="tan2") +
  geom_text(aes(label =FREQ), vjust = 0.5, colour = "black", position = position_dodge(0), size = 3) +
  labs(x="WORD in Summary",y="Word Frequency",title = "Current Employee Most Common Cons Words") +
  theme(panel.grid = element_blank(),panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),title=element_text(size=10, face= "bold", vjust=1, hjust=0.5),axis.text.x = element_text(size = 7,angle = 10,hjust = 1))+coord_flip()

#3 What are the important words in the pros and cons that companies should pay attention to?
# And are there overlaps between the advice to management and negative comments from former #employees? Therefore, what companies can do to improve those factors?

#former employee advices to management
leave_terms_ad<-freq_terms(text.var = leave$`advice-to-mgmt`,top = 25,stopwords = Top200Words)
leave_terms_ad<-leave_terms_ad%>%
  filter(!WORD %in% c("company","it's","keep","don't","doing","really","none"))
ggplot(leave_terms_ad, aes(x =reorder((WORD),FREQ), y = FREQ)) +
  geom_bar(stat = "identity",fill="red2") +
  geom_text(aes(label =FREQ), vjust = 0.5, colour = "black", position = position_dodge(0), size = 3) +
  labs(x="WORD in Summary",y="Word Frequency",title = "Former Employee Most Common Advice Words") +
  theme(panel.grid = element_blank(),panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),title=element_text(size=10, face= "bold", vjust=1, hjust=0.5),axis.text.x = element_text(size = 7,angle = 10,hjust = 1))+coord_flip()

#current employee advices
now_terms_ad<-freq_terms(text.var = now$`advice-to-mgmt`,top = 25,stopwords = Top200Words)
now_terms_ad<-now_terms_ad%>%
  filter(!WORD %in% c("company","it's","keep","don't","doing","really","none"))
ggplot(now_terms_ad, aes(x =reorder((WORD),FREQ), y = FREQ)) +
  geom_bar(stat = "identity",fill="green") +
  geom_text(aes(label =FREQ), vjust = 0.5, colour = "black", position = position_dodge(0), size = 3) +
  labs(x="WORD in Summary",y="Word Frequency",title = "Current Employee Most Common Advice Words") +
  theme(panel.grid = element_blank(),panel.background = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),title=element_text(size=10, face= "bold", vjust=1, hjust=0.5),axis.text.x = element_text(size = 7,angle = 10,hjust = 1))+coord_flip()

#4:
# number of characters in the pros and cons of different companies
mydata_nchar_pros <- nchar(as.vector(mydata_clean$pros))
mydata_nchar_cons <- nchar(as.vector(mydata_clean$cons))
mydata_nchar_adivce  <- nchar(as.vector(mydata_clean$summary))
mydata_nchar <- data.frame(company = mydata_clean$company, pros = mydata_nchar_pros, cons = mydata_nchar_cons, advice = mydata_nchar_adivce)

c25 <- mydata_nchar %>%
  group_by(company) %>%
  summarise(mean = mean(pros)) %>%
  ggplot(., aes(company, mean))+
  geom_bar(stat = "identity", colour = "black", fill = "red1")+
  coord_flip()+
  labs(x= "", y = "Mean number of character", title = "Pros")+
  geom_text(aes(y = mean/2, label = round(mean,0)))

c26 <- mydata_nchar %>%
  group_by(company) %>%
  summarise(mean = mean(cons)) %>%
  ggplot(., aes(company, mean))+
  geom_bar(stat = "identity", colour = "black", fill = "lightskyblue")+
  coord_flip()+
  labs(x= "", y = "Mean number of character", title = "Cons")+
  geom_text(aes(y = mean/2, label = round(mean,0)))

grid.arrange(c25, c26, ncol = 1)


# what are the important words in the pros and cons that companies should pay attention to?
# Amazon: important words in pros and cons
amazon <- mydata %>% filter(company == "amazon")
pros_amazon <- amazon$pros
cons_amazon <- amazon$cons

pros_amazon1 <- Corpus(VectorSource(pros_amazon))
pros_amazon1 <- tm_map(pros_amazon1, content_transformer(tolower))
pros_amazon1 <- tm_map(pros_amazon1, removeNumbers)
pros_amazon1 <- tm_map(pros_amazon1, removeWords, stopwords("english"))
pros_amazon1 <- tm_map(pros_amazon1, removeWords, c("amazon", "work", "get", "can", "will", "company","good","great","perks","lot","amazing"))
pros_amazon1 <- tm_map(pros_amazon1, removePunctuation)
pros_amazon1 <- tm_map(pros_amazon1, stripWhitespace)
tdm1 <- TermDocumentMatrix(pros_amazon1)
pros_amazon1 <- as.matrix(tdm1)
pros_amazon1 <- sort(rowSums(pros_amazon1),decreasing=T)
pros_amazon2 <- data.frame(Word = names(pros_amazon1),Number=pros_amazon1)

cons_amazon1 <- Corpus(VectorSource(cons_amazon))
cons_amazon1 <- tm_map(cons_amazon1, content_transformer(tolower))
cons_amazon1 <- tm_map(cons_amazon1, removeNumbers)
cons_amazon1 <- tm_map(cons_amazon1, removeWords, stopwords("english"))
cons_amazon1 <- tm_map(cons_amazon1, removeWords, c("amazon", "work", "get", "can", "will", "company","really","hard","much","many"))
cons_amazon1 <- tm_map(cons_amazon1, removePunctuation)
cons_amazon1 <- tm_map(cons_amazon1, stripWhitespace)
tdm2 <- TermDocumentMatrix(cons_amazon1)
cons_amazon1 <- as.matrix(tdm2)
cons_amazon1 <- sort(rowSums(cons_amazon1),decreasing=T)
cons_amazon2 <- data.frame(Word = names(cons_amazon1),Number=cons_amazon1)

c13 <- pros_amazon2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "forestgreen")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Amazon: Pros")+
  geom_text(aes(y = Number/2, label = Number))

c14 <- cons_amazon2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "orangered3")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Amazon: Cons")+
  geom_text(aes(y = Number/2, label = Number))

# Apple: important words in pros and cons
apple <- mydata %>% filter(company == "apple")
pros_apple <- apple$pros
cons_apple <- apple$cons

pros_apple1 <- Corpus(VectorSource(pros_apple))
pros_apple1 <- tm_map(pros_apple1, content_transformer(tolower))
pros_apple1 <- tm_map(pros_apple1, removeNumbers)
pros_apple1 <- tm_map(pros_apple1, removeWords, stopwords("english"))
pros_apple1 <- tm_map(pros_apple1, removeWords, c("apple", "work", "get", "can", "will", "company","good","great","perks","lot","amazing"))
pros_apple1 <- tm_map(pros_apple1, removePunctuation)
pros_apple1 <- tm_map(pros_apple1, stripWhitespace)
tdm3 <- TermDocumentMatrix(pros_apple1)
pros_apple1 <- as.matrix(tdm3)
pros_apple1 <- sort(rowSums(pros_apple1),decreasing=T)
pros_apple2 <- data.frame(Word = names(pros_apple1),Number=pros_apple1)

cons_apple1 <- Corpus(VectorSource(cons_apple))
cons_apple1 <- tm_map(cons_apple1, content_transformer(tolower))
cons_apple1 <- tm_map(cons_apple1, removeNumbers)
cons_apple1 <- tm_map(cons_apple1, removeWords, stopwords("english"))
cons_apple1 <- tm_map(cons_apple1, removeWords, c("apple", "work", "get", "can", "will", "company","really","hard","much","many"))
cons_apple1 <- tm_map(cons_apple1, removePunctuation)
cons_apple1 <- tm_map(cons_apple1, stripWhitespace)
tdm4 <- TermDocumentMatrix(cons_apple1)
cons_apple1 <- as.matrix(tdm4)
cons_apple1 <- sort(rowSums(cons_apple1),decreasing=T)
cons_apple2 <- data.frame(Word = names(cons_apple1),Number=cons_apple1)

c15 <- pros_apple2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "forestgreen")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Apple: Pros")+
  geom_text(aes(y = Number/2, label = Number))

c16 <- cons_apple2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "orangered3")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Apple: Cons")+
  geom_text(aes(y = Number/2, label = Number))

# Facebook: important words in pros and cons
facebook <- mydata %>% filter(company == "facebook")
pros_facebook <- facebook$pros
cons_facebook <- facebook$cons

pros_facebook1 <- Corpus(VectorSource(pros_facebook))
pros_facebook1 <- tm_map(pros_facebook1, content_transformer(tolower))
pros_facebook1 <- tm_map(pros_facebook1, removeNumbers)
pros_facebook1 <- tm_map(pros_facebook1, removeWords, stopwords("english"))
pros_facebook1 <- tm_map(pros_facebook1, removeWords, c("facebook", "work", "get", "can", "will", "company","good","great","perks","lot","amazing"))
pros_facebook1 <- tm_map(pros_facebook1, removePunctuation)
pros_facebook1 <- tm_map(pros_facebook1, stripWhitespace)
tdm5 <- TermDocumentMatrix(pros_facebook1)
pros_facebook1 <- as.matrix(tdm5)
pros_facebook1 <- sort(rowSums(pros_facebook1),decreasing=T)
pros_facebook2 <- data.frame(Word = names(pros_facebook1),Number=pros_facebook1)

cons_facebook1 <- Corpus(VectorSource(cons_facebook))
cons_facebook1 <- tm_map(cons_facebook1, content_transformer(tolower))
cons_facebook1 <- tm_map(cons_facebook1, removeNumbers)
cons_facebook1 <- tm_map(cons_facebook1, removeWords, stopwords("english"))
cons_facebook1 <- tm_map(cons_facebook1, removeWords, c("facebook", "work", "get", "can", "will", "company","really","hard","much","many"))
cons_facebook1 <- tm_map(cons_facebook1, removePunctuation)
cons_facebook1 <- tm_map(cons_facebook1, stripWhitespace)
tdm6 <- TermDocumentMatrix(cons_facebook1)
cons_facebook1 <- as.matrix(tdm6)
cons_facebook1 <- sort(rowSums(cons_facebook1),decreasing=T)
cons_facebook2 <- data.frame(Word = names(cons_facebook1),Number=cons_facebook1)

c17 <- pros_facebook2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "forestgreen")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Facebook: Pros")+
  geom_text(aes(y = Number/2, label = Number))

c18 <- cons_facebook2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "orangered3")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Facebook: Cons")+
  geom_text(aes(y = Number/2, label = Number))

# Google: important words in pros and cons
google <- mydata %>% filter(company == "google")
pros_google <- google$pros
cons_google <- google$cons

pros_google1 <- Corpus(VectorSource(pros_google))
pros_google1 <- tm_map(pros_google1, content_transformer(tolower))
pros_google1 <- tm_map(pros_google1, removeNumbers)
pros_google1 <- tm_map(pros_google1, removeWords, stopwords("english"))
pros_google1 <- tm_map(pros_google1, removeWords, c("google", "work", "get", "can", "will", "company","good","great","perks","lot","amazing"))
pros_google1 <- tm_map(pros_google1, removePunctuation)
pros_google1 <- tm_map(pros_google1, stripWhitespace)
tdm7 <- TermDocumentMatrix(pros_google1)
pros_google1 <- as.matrix(tdm7)
pros_google1 <- sort(rowSums(pros_google1),decreasing=T)
pros_google2 <- data.frame(Word = names(pros_google1),Number=pros_google1)

cons_google1 <- Corpus(VectorSource(cons_google))
cons_google1 <- tm_map(cons_google1, content_transformer(tolower))
cons_google1 <- tm_map(cons_google1, removeNumbers)
cons_google1 <- tm_map(cons_google1, removeWords, stopwords("english"))
cons_google1 <- tm_map(cons_google1, removeWords, c("google", "work", "get", "can", "will", "company","really","hard","much","many"))
cons_google1 <- tm_map(cons_google1, removePunctuation)
cons_google1 <- tm_map(cons_google1, stripWhitespace)
tdm8 <- TermDocumentMatrix(cons_google1)
cons_google1 <- as.matrix(tdm8)
cons_google1 <- sort(rowSums(cons_google1),decreasing=T)
cons_google2 <- data.frame(Word = names(cons_google1),Number=cons_google1)

c19 <- pros_google2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "forestgreen")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Google: Pros")+
  geom_text(aes(y = Number/2, label = Number))

c20 <- cons_google2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "orangered3")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Google: Cons")+
  geom_text(aes(y = Number/2, label = Number))

# Microsoft: important words in pros and cons
microsoft <- mydata %>% filter(company == "microsoft")
pros_microsoft <- microsoft$pros
cons_microsoft <- microsoft$cons

pros_microsoft1 <- Corpus(VectorSource(pros_microsoft))
pros_microsoft1 <- tm_map(pros_microsoft1, content_transformer(tolower))
pros_microsoft1 <- tm_map(pros_microsoft1, removeNumbers)
pros_microsoft1 <- tm_map(pros_microsoft1, removeWords, stopwords("english"))
pros_microsoft1 <- tm_map(pros_microsoft1, removeWords, c("microsoft", "work", "get", "can", "will", "company","good","great","perks","lot","amazing"))
pros_microsoft1 <- tm_map(pros_microsoft1, removePunctuation)
pros_microsoft1 <- tm_map(pros_microsoft1, stripWhitespace)
tdm9 <- TermDocumentMatrix(pros_microsoft1)
pros_microsoft1 <- as.matrix(tdm9)
pros_microsoft1 <- sort(rowSums(pros_microsoft1),decreasing=T)
pros_microsoft2 <- data.frame(Word = names(pros_microsoft1),Number=pros_microsoft1)

cons_microsoft1 <- Corpus(VectorSource(cons_microsoft))
cons_microsoft1 <- tm_map(cons_microsoft1, content_transformer(tolower))
cons_microsoft1 <- tm_map(cons_microsoft1, removeNumbers)
cons_microsoft1 <- tm_map(cons_microsoft1, removeWords, stopwords("english"))
cons_microsoft1 <- tm_map(cons_microsoft1, removeWords, c("microsoft", "work", "get", "can", "will", "company","really","hard","much","many"))
cons_microsoft1 <- tm_map(cons_microsoft1, removePunctuation)
cons_microsoft1 <- tm_map(cons_microsoft1, stripWhitespace)
tdm10 <- TermDocumentMatrix(cons_microsoft1)
cons_microsoft1 <- as.matrix(tdm10)
cons_microsoft1 <- sort(rowSums(cons_microsoft1),decreasing=T)
cons_microsoft2 <- data.frame(Word = names(cons_microsoft1),Number=cons_microsoft1)

c21 <- pros_microsoft2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "forestgreen")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Microsoft: Pros")+
  geom_text(aes(y = Number/2, label = Number))

c22 <- cons_microsoft2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "orangered3")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Microsoft: Cons")+
  geom_text(aes(y = Number/2, label = Number))

# Netflix: important words in pros and cons
netflix <- mydata %>% filter(company == "netflix")
pros_netflix <- netflix$pros
cons_netflix <- netflix$cons

pros_netflix1 <- Corpus(VectorSource(pros_netflix))
pros_netflix1 <- tm_map(pros_netflix1, content_transformer(tolower))
pros_netflix1 <- tm_map(pros_netflix1, removeNumbers)
pros_netflix1 <- tm_map(pros_netflix1, removeWords, stopwords("english"))
pros_netflix1 <- tm_map(pros_netflix1, removeWords, c("netflix", "work", "get", "can", "will", "company","good","great","perks","lot","amazing"))
pros_netflix1 <- tm_map(pros_netflix1, removePunctuation)
pros_netflix1 <- tm_map(pros_netflix1, stripWhitespace)
tdm11 <- TermDocumentMatrix(pros_netflix1)
pros_netflix1 <- as.matrix(tdm11)
pros_netflix1 <- sort(rowSums(pros_netflix1),decreasing=T)
pros_netflix2 <- data.frame(Word = names(pros_netflix1),Number=pros_netflix1)

cons_netflix1 <- Corpus(VectorSource(cons_netflix))
cons_netflix1 <- tm_map(cons_netflix1, content_transformer(tolower))
cons_netflix1 <- tm_map(cons_netflix1, removeNumbers)
cons_netflix1 <- tm_map(cons_netflix1, removeWords, stopwords("english"))
cons_netflix1 <- tm_map(cons_netflix1, removeWords, c("netflix", "work", "get", "can", "will", "company","really","hard","much","many"))
cons_netflix1 <- tm_map(cons_netflix1, removePunctuation)
cons_netflix1 <- tm_map(cons_netflix1, stripWhitespace)
tdm12 <- TermDocumentMatrix(cons_netflix1)
cons_netflix1 <- as.matrix(tdm12)
cons_netflix1 <- sort(rowSums(cons_netflix1),decreasing=T)
cons_netflix2 <- data.frame(Word = names(cons_netflix1),Number=cons_netflix1)

c23 <- pros_netflix2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "forestgreen")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Netflix: Pros")+
  geom_text(aes(y = Number/2, label = Number))

c24 <- cons_netflix2 %>%
  slice(1:7)%>%
  ggplot(., aes(reorder(Word, + Number), Number))+
  geom_bar(stat = "identity", colour = "black", fill = "orangered3")+
  coord_flip()+
  labs(x = "", y = "Frequancy", title = "Netflix: Cons")+
  geom_text(aes(y = Number/2, label = Number))

grid.arrange(c13, c14, c15, c16, c17, c18,ncol = 2)
grid.arrange(c19,c20, c21, c22, c23, c24,ncol = 2)

#5 Do the negative reviews tend to be longer than positive ones and is it a trend that people #usually complain more? 

cor(nchar(mydata_clean$pros),mydata_clean$`overall-ratings`)
cor(nchar(mydata_clean$cons),mydata_clean$`overall-ratings`)
#Yes, longer reviews tend to be associated with lower scores. People complain.

#Our longest review
library(stringr)
summary(nchar(mydata_clean$summary))
longest_review_index = which.max(str_count(string = mydata_clean$summary,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))
mydata_clean$summary[longest_review_index]
str_count(longest_review_index)

#6 What are people's sentiments in the summary of their reviews?
#words in each summary
mydata_clean%>%
  select(company,summary)%>%
  group_by(company)%>%
  unnest_tokens(output = word,input=summary)%>%
  count()
#Bing Lexicon total negative and positive summary
mydata_clean%>%
  select(company,summary)%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()
#pics
mydata_clean%>%
  select(company,summary)%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)

#Proportion of Positive words in Reviews
mydata_clean%>%
  select(company,summary)%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#Positive Review, if reviews that have a lot of positive words are rated as helpful?
mydata_clean %>%
  select(company,summary,`overall-ratings`)%>%
  group_by(company)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(`overall-ratings`,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))
#pics
mydata_clean %>%
  select(company,summary,`overall-ratings`)%>%
  group_by(company)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(`overall-ratings`,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=`overall-ratings`,y=proportion,fill=sentiment))+geom_col()+theme_economist()

#Fraction of Positive Words
mydata_clean %>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(company)%>%
  summarize(positivity = sum(sentiment=='positive')/n())
#Correlation between Positive Words and Review helpfulness
mydata_clean%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(company,`overall-ratings`)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,`overall-ratings`))

#emotions covered by this nrc lexicon.
get_sentiments('nrc')%>%
  group_by(sentiment)%>%
  count()
mydata_clean %>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()
#pics
mydata %>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()

#Ratings of each Review based on Emotions Expressed
mydata_clean%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(company,sentiment,`overall-ratings`)%>%
  count()  
#Ratings of all Reviews based on Emotion Expressed
mydata_clean%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(company,sentiment,`overall-ratings`)%>%
  count%>%
  group_by(sentiment, `overall-ratings`)%>%
  summarize(n = mean(n))%>%
  data.frame()
#pics
mydata_clean%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(company,sentiment,`overall-ratings`)%>%
  count%>%
  group_by(sentiment, `overall-ratings`)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=`overall-ratings`,y=n,fill=`overall-ratings`))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

#Correlation between emotion expressed and overall-ratings
mydata_clean%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(company,sentiment,`overall-ratings`)%>%
  count%>%
  ungroup%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,`overall-ratings`))
#Scatterplot of relationship
mydata_clean%>%
  group_by(company)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(company,sentiment,`overall-ratings`)%>%
  count%>%
  ungroup%>%
  group_by(sentiment)%>%
  ggplot(aes(x=`overall-ratings`,y=n))+geom_point()+facet_wrap(~sentiment)+geom_smooth(method='lm',se=F)

#split mydata_clean by commpany
company1 <- mydata_clean%>%
  filter(company == 'google')
company2 <- mydata_clean%>%
  filter(company == 'amazon')
company3 <- mydata_clean%>%
  filter(company == 'netflix')
company4 <- mydata_clean%>%
  filter(company == 'facebook')
company5 <- mydata_clean%>%
  filter(company == 'apple')
company6 <- mydata_clean%>%
  filter(company == 'microsoft')
#wordcloud
library(wordcloud)
#google
wordcloudData = 
  company1%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#amazon
wordcloudData = 
  company2%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#netflix
wordcloudData = 
  company3%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#facebook
wordcloudData = 
  company4%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#apple
wordcloudData = 
  company5%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#microsoft
wordcloudData = 
  company6%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))


#Comparison Cloud
#company1
wordcloudData = 
  company1%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)
warnings()

wordcloudData = 
  company2%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)
warnings()

wordcloudData = 
  company3%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)
warnings()

wordcloudData = 
  company4%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)
warnings()

wordcloudData = 
  company5%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)
warnings()

wordcloudData = 
  company6%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)
warnings()
