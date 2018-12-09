
install.packages("ngram")
install.packages("tm")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("textmineR")
install.packages("tokenizers")
#install.packages("RWeka")


library(tm)
library(SnowballC)
library(topicmodels)
library(textmineR)
library(ngram)
library(tokenizers)
#library(RWeka)

dm = read.delim("/Users/aviv/Documents/bound_99/descr_099.txt", sep = "|", quote = "")

ds = read.delim("/Users/aviv/Documents/bound_99/speeches_099.txt", sep = "|", quote = "")
z = dm$speech_id[dm$chamber=="S"]
ds = ds[ds$speech_id%in%z,]
ds[,2] = iconv(ds[,2], to='UTF-8-MAC', sub='byte')

#tm lets you store sparse matrices efficiently
corp = VCorpus(VectorSource(ds[,2]))
corp = tm_map(corp, content_transformer(tolower))
corp = tm_map(corp, removeWords, stopwords("english"))
corp = tm_map(corp, removePunctuation)
corp = tm_map(corp, removeNumbers)
corp = tm_map(corp, stemDocument)

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
dtm <- DocumentTermMatrix(corp, control = list(tokenize=NLP_tokenizer, weighting=weightTf))

ui = unique(dtm$i)
zdtm = dtm[ui,]
zdtm = zdtm[1:500,]

g = LDA(zdtm,10,method = 'VEM',control=NULL,model=NULL)
gi = posterior(g)

rev(sort(gi$terms[1,]))[1:10]
rev(sort(gi$terms[7,]))[1:10]

gi$topics[1,]

# ##old code
# 
# remove.packages("RWeka", "Library/Frameworks/R.framework/Versions/3.5/Resources/library/RWeka")
# 
# remove.packages("rJava", "Library/Frameworks/R.framework/Versions/3.5/Resources/library/")
# /Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjsig.dylib

#rtots <- apply(dtm , 1, sum) 
#dtm.new   <- dtm[rowTotals> 0, ]

# options('java.home')
# options("java.home"="/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/")
# Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home')
# Sys.setenv(LD_LIBRARY_PATH='$JAVA_HOME/lib/server')

z = dm$speech_id[dm$chamber=="S"]
ds = ds[ds$speech_id%in%z,]
ds[,2] = iconv(ds[,2], to='UTF-8-MAC', sub='byte')
ds[,2] = tolower(ds[,2])
ng = c()
for(i in 1:dim(ds)[1]){
	z = ds[3,2]
	z = preprocess(z, case = "lower", remove.punct=TRUE, fix.spacing = TRUE)
	ng[i] = ngram_asweka(z, 2)
	#print(i)
	}

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = "_"), use.names = FALSE)
}
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))


dtm <- DocumentTermMatrix(corp, control = list(tokenize=NLP_tokenizer,removePunctuation = T, stemming = T, tolower = T, stopwords= stopwords("english"), weighting=weightTfIdf))
