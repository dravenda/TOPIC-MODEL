#COURSE TOPIC MODELLING UNIVERSITAT DE BARCELONA FEB 2021

#TOPIC MODELLING APPLICATION 

#IMPORT AND PREPROCESSING USING QUANTEDA
#Install main needed packages:
install.packages(c("tidyverse", "quanteda", "topicmodels", "stm", "readxl"))

library(tidyverse)
library(quanteda)
library(topicmodels)
library(stm)
#To read Excel files:
library(readxl)

help(package = "quanteda") 
library(help = "tidyverse")
vignette("topicmodels",package="topicmodels")

#Set your working directory (update the path):
setwd("~/Doctorado UB/Congresos applications/APPLICATION TBS/BCN poste/CURSO TOPIC MODELING/LDA")

getwd()

#Import Scopus csv file and display it. 
#Several files including text of different formats (e.g. Word, pdf, etc.) can be simultaneously 
#imported in R using package "readtext": https://cran.r-project.org/web/packages/readtext/vignettes/readtext_vignette.html

#Import csv file including journal papers:
df_ar <- read_csv("scopus_ar.csv")

df_ar

View(df_ar)

str(df_ar)

glimpse(df_ar)

attributes(df_ar)

save(df_ar, file = "df_ar.RData") #save dataset in working directory

load("df_ar.RData") #load dataset from working directory

df_ar <- distinct(df_ar) #remove duplicates

df_ar <- filter(df_ar, Abstract != "[No abstract available]")  #filter observations with abstract

#df_ar <- filter(df_ar, nchar(Abstract) > 50) #alternative with nchar (n. characters)

#df_ar <- subset(df_ar, nchar(Abstract) > 50) #alternative with subset function in base R

#Create and adjust variables:

df_ar$numcit <- df_ar$`Cited by` #create numcit as a copy of `Cited by`

#df_ar <- mutate(df_ar, numcit = `Cited by`) #another way to create a variable with mutate {dplyr} (in 'tidyverse') )

df_ar$numcit[is.na(df_ar$numcit)] <- 0 #replace missing values (NA) with 0

anyNA(df_ar$numcit) #to check if there is any NA

df_ar$numpage <- df_ar$`Page end` - df_ar$`Page start`  #create variable for number of paper pages

anyNA(df_ar$numpage)

sum(is.na(df_ar$numpage))

df_ar$numpage[is.na(df_ar$numpage)] <- mean(df_ar$numpage, na.rm = TRUE) #replace missing with mean

df_ar$numauth <- str_count(df_ar$`Author(s) ID`, ";") #create variable number of authors

anyNA(df_ar$numauth)

#Display list of papers from dataset in R Markdown (open file topicM_MD.Rmd)
#Install from CRAN R Markdown
install.packages('rmarkdown')

#If you want to generate PDF output in R Markdown, you will need to install LaTeX:
install.packages('tinytex')
tinytex::install_tinytex()  # install TinyTeX

#use this function to create pdf from R Markdown (or button 'Knit to PDF'): render("file_name.Rmd", "pdf_document")

#Create corpora:

load("df_ar.RData")

library(quanteda)

#documentation: https://quanteda.io/index.html
help(package = "quanteda") 

#create corpus for paper titles:

corpustitle <- corpus(df_ar, text_field = "Title", docid_field = "DOI")

save(corpustitle, file = "corpustitle.RData")

#Examine "corpustitle" object: 
typeof(corpustitle) #named character vector with attributes

class(corpustitle) #S3 classes: "corpus" and ""character"

dim(corpustitle)

attributes(corpustitle)

names(corpustitle) #DOIs

head(corpustitle)

summary(corpustitle, n = 5)  #show 5 documents

docvars(corpustitle)

names(docvars(corpustitle)) #get the name of the corpus variables

getAnywhere(docvars.corpus) #get the method function code (you can ignore this line)
quanteda:::docvars.corpus  #get the method function code, alternative, (you can ignore this line)

#create corpus for paper abstracts:

corpusabstr <- corpus(df_ar, text_field = "Abstract", docid_field = "DOI")

save(corpusabstr, file = "corpusabstr.RData")

head(corpusabstr)

#create corpus for paper keywords:

corpuskeyw <- corpus(df_ar, text_field = "Author Keywords", docid_field = "DOI")

save(corpuskeyw, file = "corpuskeyw.RData")

head(corpuskeyw)

#remove accented characters (more relevant for some languages):

iconv("això és la solució a la qüestió", to='ASCII//TRANSLIT')

corpustitle <- iconv(corpustitle, to='ASCII//TRANSLIT')

corpusabstr <- iconv(corpusabstr, to='ASCII//TRANSLIT')

corpuskeyw <- iconv(corpuskeyw, to='ASCII//TRANSLIT')

#Other preprocessing and generation of dfm (document-feature/term matrix) 

dfm_title <- dfm(corpustitle, tolower = TRUE, stem = FALSE, remove = stopwords("english"),
                 remove_punct = TRUE, remove_symbols = TRUE,  remove_numbers = TRUE, remove_url = TRUE)

dfm_title <-dfm_keep(dfm_title, min_nchar = 3) #keep words with minimum 3 characters

save(dfm_title, file = "dfm_title.RData")

load("dfm_title.RData")

View(dfm_title)

head(dfm_title)

typeof(dfm_title) #S4 object

class(dfm_title)

attributes(dfm_title)

slotNames(dfm_title) #display the names of the slots (components of S4 object)

#Lemmatize dfm (optional), to be done for all dfm matrices:

dfm_replace(dfm_title, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)

#Generate dfm for corpora of abstracts and keywords

dfm_abstr <- dfm(corpusabstr, tolower = TRUE, stem = FALSE, remove = stopwords("english"),
                 remove_punct = TRUE, remove_symbols = TRUE,  remove_numbers = TRUE, remove_url = TRUE)

dfm_abstr <-dfm_keep(dfm_abstr, min_nchar = 3) #keep words with minimum 3 characters

save(dfm_abstr, file = "dfm_abstr.RData")

dfm_keyw <- dfm(corpuskeyw, tolower = TRUE, stem = FALSE, remove = stopwords("english"),
                remove_punct = TRUE, remove_symbols = TRUE,  remove_numbers = TRUE, remove_url = TRUE)

dfm_keyw <-dfm_keep(dfm_keyw, min_nchar = 3) #keep words with minimum 3 characters

save(dfm_keyw, file = "dfm_keyw.RData")

#Weigh dfm: 0.4 dfm_title, 0.4 dfm_keyw, 0.2 dfm_abstr (titles and keywords more weight than abstracts)

dfm_titlew <- dfm_title * 2
save(dfm_titlew, file = "dfm_titlew.RData")

dfm_keyww <- dfm_keyw * 2
save(dfm_keyww, file = "dfm_keyww.RData")

dfm_bound <- rbind(dfm_abstr, dfm_titlew, dfm_keyww) #bind dfm by rows, docvars are lost!

dfm_group <-dfm_group(dfm_bound) #group documents by DOI (frequencies are added)

docvars(dfm_group) <- docvars(dfm_abstr) #reassign docvars to combined dfm

save(dfm_group, file = "dfm_group.RData")

#LDA TOPIC MODEL
library(topicmodels)
#https://www.rdocumentation.org/packages/topicmodels/versions/0.2-11
help(package = "topicmodels")

#convert dfm to a format for topicmodels:

dtmtm <- convert(dfm_group, to = "topicmodels")

typeof(dtmtm) #list

save(dtmtm, file = "dtmtm.RData")

#Select the best number of topics for LDA model:
install.packages("ldatuning")
library(ldatuning)
#See documentation:
#https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
#https://rpubs.com/MNidhi/NumberoftopicsLDA

#The following function may take about 13 minutes to run:

topnugibbs <- FindTopicsNumber(
  dtmtm,
  topics = seq(from = 5, to = 100, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12345),
  mc.cores = 4L,
  verbose = TRUE
)

save(topnugibbs, file = "topnugibbs.RData")

FindTopicsNumber_plot(topnugibbs) # the best about 30 topics: [25-35]

#You can visualize the above plot in an R Markdown document

#A second run may be needed to infer the best number of topics out of a more restricted set
#The following function may take about 3 minutes:

topnugibbs2 <- FindTopicsNumber(
  dtmtm,
  topics = seq(from = 25, to = 35, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12345),
  mc.cores = 4L,
  verbose = TRUE
)

save(topnugibbs2, file = "topnugibbs2.RData")

FindTopicsNumber_plot(topnugibbs2)  #35 topics the best number

#Selection of the best topic based on Perplexity (lower perplexity -> better fit)
#https://rpubs.com/MNidhi/NumberoftopicsLDA

#Define dtm train (70%) and dtm test (30%):

dim(dtmtm) #display matrix dimension (number of rows)

sampling <- sample(1:742, replace = FALSE, size = nrow(dtmtm)*0.7) #choose random sample of size = 0.7 * 742

dtmtm_train <- dtmtm[sampling,]

dtmtm_test <- dtmtm[-sampling,]

perplexity_df <- data.frame(train=numeric(), test=numeric(), n_topics = numeric())
topics <- c(20:40)

#20 minutes to run:
for (i in topics){
  fitlda <- LDA(dtmtm_train, k = i, method = "Gibbs", control = list(seed = 12345))
  perplexity_df[i-19,1] <- perplexity(fitlda, newdata = dtmtm_train)
  perplexity_df[i-19,2] <- perplexity(fitlda, newdata = dtmtm_test)
  perplexity_df[i-19,3] <- i
}

save(perplexity_df, file="perplexity_df.RData")

#plotting the perplexity of both train and test
g <- ggplot(data=perplexity_df, aes(x= n_topics)) + labs(y="Perplexity",x="Number of topics") +
  ggtitle("Perplexity of hold out and training data")

g <- g + geom_line(aes(y = test), colour="red")
g <- g + geom_line(aes(y = train), colour="green")
g     #35 the best number of topics

#Display perplexity_df (dataframe) and its plot in an R Markdown document
#run the following function in the console: rmarkdown::render("topicM_MD.Rmd", "word_document")

#Estimate LDA with the best number of topics:

lda_best <- LDA(dtmtm, k = 35, method = "Gibbs", control = list(seed = 12345))

save(lda_best, file = "lda_best.RData")

typeof(lda_best)

str(lda_best)

likterms <- terms(lda_best, 40); likterms #matrix of most likely 40 terms for each topic

write_csv(as.data.frame(likterms), "likterms.csv") #Export to csv file

#Create a table of most likely 40 terms per topic in R Markdown

liktopics <- topics(lda_best, 10); liktopics  #most likely 10 topics for each document

postprob <- posterior(lda_best) #posterior probabilities of the topics for each document

save(postprob, file="postprob.RData")

typeof(postprob)

attributes(postprob)

str(postprob)

#topics are probability distributions over the entire vocabulary

beta <- postprob$terms  # get beta from results (topic-term matrix)

dim(beta)  #K distributions over ncol(DTM) terms

rowSums(beta) #rows in beta sum to 1

postprob_tt <- as_tibble(t(postprob$terms)) #dataset topic-term distributions with probabilities

View(postprob_tt)

#for every document we have a probability distribution of its contained topics
theta <- postprob$topics  #document-topic matrix

View(theta)

dim(theta)

class(theta)

rowSums(theta)[1:10] #rows in theta sum to 1

postprob_dt <- as_tibble(postprob$topics) #dataset documents-topics

names(postprob_dt) <- paste0("topic", 1:35) #rename columns to topic1-topic35

postprob_dt$DOI <- row.names(postprob$topics) #create variable DOI with row names (matrix attribute) 

View(postprob_dt)

#Compute average topic prevalence across the corpus:
topicproport <- map_dbl(postprob_dt, mean) #sapply function alternative to map_dbl

sort(topicproport, decreasing = TRUE)

#Add document variables to dataframe postprob_dt:
load("df_ar.RData")

postprob_dtt <- left_join(postprob_dt, df_ar, by = "DOI") #function "merge" can be used instead

View(postprob_dtt)

save(postprob_dtt, file="postprob_dtt.RData")

#order full dataset by decreasing "topic1" prevalence and display the 3 most related papers:

extract <- select(arrange(postprob_dtt, desc(topic1)), topic1, DOI, Title, Abstract, `Author Keywords`)[1:3,]  

as.character(extract[1,2:5]) #convert to character vector for better full display

write_csv(postprob_dtt, "postprob_dtt.csv") #Export to csv file

#Display table (extract) in R Markdown

#create dataset with mean topic prevalence by year:
meantopyr <- aggregate(select(postprob_dtt, topic1:topic35, Year), list(postprob_dtt$Year), mean, na.rm = TRUE)

View(meantopyr)

save(meantopyr, file="meantopyr.RData")

#VISUALIZATION PLOT CHARTS 

#Bar chart for topic trend (You can generate it in R Markdown)
ggplot(meantopyr, aes(x = as.factor(Year), y = topic1))+
  geom_col(colour = 'red', fill = "green")+
  labs(y = "topic prevalence", x= "publication year") +
  ggtitle("Trend of topic")+
  geom_text(aes(label = round(topic1, digits = 3)))

#Reshape long dataset mean topic prevalence by year:
meantopyr_l <- pivot_longer(meantopyr,cols = starts_with("topic"), names_to = 'topic', values_to = "prevalence")

View(meantopyr_l)

topics <- names(select(meantopyr, topic1:topic16)) #create vector of "subset" of topics (from 1 to 16)

#Plot bar charts for various topics at once:
filter(meantopyr_l, topic %in% topics) %>%
  ggplot(aes(factor(Year), prevalence, fill = factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 4)

install.packages("tidytext")
library(tidytext)
#https://www.tidytextmining.com/index.html 
#textbook on ggplot2: https://ggplot2-book.org/ 

#"tidy" function in tidytext converts to one-document-per-topic-per-row format including gamma
#gamma = the probability of that topic generated from that document

load("lda_best.RData")
lda_td0 <- tidy(lda_best, matrix = "gamma")

View(lda_td0)

#tidy function converts to one-topic-per-term-per-row format including beta
#beta = the probability of that term being generated from that topic:

lda_td1 <- tidy(lda_best, matrix = "beta") 

head(lda_td1)

top_terms <- lda_td1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

View(top_terms)

#Plot top 10 terms by topic:

filter(top_terms, topic < 17) %>%
  mutate(term = reorder_within(term, by = beta, within = topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip()

#visualize topics as word cloud:

install.packages("wordcloud2")
library(wordcloud2)

topic_wc = 1 
load("postprob.RData")

#select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(postprob$terms[topic_wc,], decreasing=TRUE)[1:40]

words <- names(top40terms)

#extract the probabilitIes of each of the 40 terms:
probabilities <- sort(postprob$terms[topic_wc,], decreasing=TRUE)[1:40]

#visualize the terms as word cloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8, shape = 'circle')

letterCloud(data.frame("word" = words, "freq" = probabilities), size = 1, word = "UB")

#The LDAvis allows you to interactively visualize an LDA topic model:
install.packages("LDAvis")
devtools::install_github("cpsievert/LDAvisData")

#Create the function for LDAvis visualization (run the following code):

topicmodels_json_ldavis <- function(fitted, doc_term){
  require(LDAvis)
  require(slam)
  
  # Find required quantities
  phi <- as.matrix(posterior(fitted)$terms)
  theta <- as.matrix(posterior(fitted)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(doc_term)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = as.vector(table(doc_term$i)),
                                 term.frequency = term_freq)
  
  return(json_lda)
}


load("dtmtm.RData")
load("lda_best.RData")

lda_json <-topicmodels_json_ldavis(fitted = lda_best, doc_term = dtmtm)

serVis(lda_json)  


#SRUCTURAL TOPIC MODEL
#https://www.rdocumentation.org/packages/stm/versions/1.3.5
#http://www.structuraltopicmodel.com/
#https://www.jstatsoft.org/article/view/v091i02 (MAIN PAPER 2019)

help(package = "stm")

library(stm)

load("dfm_group.RData")

dtmstm <- quanteda::convert(dfm_group, to = "stm")

typeof(dtmstm) #list

names(dtmstm) #documents (DTM), vocab, meta

str(dtmstm)

View(dtmstm$meta) #dataframe of meta variables

save(dtmstm, file = "dtmstm.RData")

#Search the best number of topics (variables: Year, numcit, numpage, numauth)

#It may take about 30 minutes for 20 k (topics), try with 10 k:

bestkfin <- searchK(dtmstm$documents, dtmstm$vocab, K = 20:40, prevalence =~ numcit +
                      numpage + numauth + factor(Year), data = dtmstm$meta)

save(bestkfin, file = "bestkfin.RData")

bestkfindf <- bestkfin$results

bestkfindf #list of metrics

#Plot metrics:
plot(bestkfin) #25 K the best; max(Held-Out Lik., Lower Bound, Exclus., Sem. Coh.), min(Res.)

#Plot semantic coherence and exclusivity (select k on the frontier)
ggplot(bestkfindf, aes(x = semcoh, y = exclus)) +
  geom_point(color = "grey93") + geom_text(aes(label = K)) +
  ggtitle("Fig. 1. Statistics to select the best number of topics") +
  xlab("Semantic Coherence") + ylab("Exclusivity") 

#stm estimation:
stmestim <- stm(documents = dtmstm$documents, vocab = dtmstm$vocab, K = 25, prevalence =~numcit +
                  numpage + numauth + factor(Year), data = dtmstm$meta, init.type = "Spectral")  

save(stmestim, file = "stmestim.RData")

typeof(stmestim)

str(stmestim)

#dataframe of documents topic-proportions:
tabletopics <- stmestim$theta

View(tabletopics)  #display the matrix

rowSums(tabletopics) #the sum of the rows equals one

#average topic prevalence across corpus
topicproport <- map_dbl(as_tibble(tabletopics), mean)

sort(topicproport, decreasing = TRUE)

#plot average topic prevalence
tibble(topics = factor(1:25), topicprop = topicproport) %>%
  ggplot(aes(x = reorder(topics, -topicprop), y = topicprop)) +
  geom_bar(stat ="identity") + geom_text(aes(label = round(topicproport, digits = 3))) +
  ggtitle("Expected Topic Proportions") + ylab("Topic proportions") 

#plot expected proportion of the corpus that belongs to each topic:
plot(stmestim, n=5, type = "summary")  #n = number of most likely words per topic

#plot Semantic Coherence And Exclusivity for each topic to assess its quality
topicQuality(stmestim, documents = dtmstm$documents, M = 10)

#Display most likely 30 words for each topic:

topiclabels <- labelTopics(stmestim, topics = 1:25, n = 30, frexweight = 0.5); topiclabels

typeof(topiclabels)

names(topiclabels)

probmatrix <- topiclabels[["prob"]]

frexmatrix <-topiclabels[["frex"]]

#Display Word Cloud by topic (separate windows)
windows()
cloud(stmestim, topic = 1)
dev.off()

#Display most likely words per topic
windows()
plot(stmestim, topics = 1:10, text.cex = 0.9, type = "labels")

#examine documents from corpus (e.g. abstracts) that are highly associated with topics:
load("corpustitle.RData")

textdocs <- texts(corpustitle); textdocs

#textdocs <- str_sub(textdocs, start = 1L, end = 200L) #extract substrings from a character vector 

thoughts1 <- findThoughts(stmestim, texts = textdocs, n = 2, topics = 1)$docs[[1]]  #2 docs most associated with topic 1

#par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts1, width = 30, main = "Topic 1")

thoughts2 <- findThoughts(stmestim, texts = textdocs, n = 2, topics = 2)$docs[[1]]  #2 docs most associated with topic 2

#Estimating metadata/topic relationships (regression) 

regstm <- estimateEffect(1:10 ~ numcit + numpage + numauth + factor(Year), stmestim,
                         metadata = dtmstm$meta)

summary(regstm)

summary(regstm, topics = 1) #display only topic 1 estimation

#dataset of regression estimation:
topic1est <- as_tibble(summary(regstm, topics = 1)$tables[[1]]) 

topic1est <- mutate(topic1est, variables = row.names(summary(regstm, topics = 1)$tables[[1]]), .before = 1) 

View(topic1est)

write_csv(topic1est,"topic1est.csv")

#Display table topic1est in R Markdown








