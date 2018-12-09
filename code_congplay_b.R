
install.packages("slam")
install.packages("tm")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("textmineR")
install.packages("ngram")
install.packages("tokenizers")

library(slam)
library(tm)
library(SnowballC)
library(topicmodels)
library(textmineR)
library(ngram)
library(tokenizers)

#dm = read.delim("/Users/js2758/OneDrive/Documents/Projects/transparency/data/senate/hein-bound/descrs/descr_099.txt", sep = "|", quote = "")
#dm = read.delim("C:/Users/Jedst/OneDrive/Documents/Projects/transparency/data/senate/hein-bound/descrs/descr_099.txt", sep = "|", quote = "")
dm = read.delim("/Users/aviv/Documents/bound_99/descr_099.txt", sep = "|", quote = "")

#ds = read.delim("/Users/js2758/OneDrive/Documents/Projects/transparency/data/senate/hein-bound/speeches/speeches_099.txt", sep = "|", quote = "")
#ds = read.delim("/Users/Jedst/OneDrive/Documents/Projects/transparency/data/senate/hein-bound/speeches/speeches_099.txt", sep = "|", quote = "")
ds = read.delim("/Users/aviv/Documents/bound_99/speeches_099.txt", sep = "|", quote = "")

z = dm$speech_id[dm$chamber=="S"]
ds = ds[ds$speech_id%in%z,]
ds[,2] = iconv(ds[,2], to='UTF-8-MAC', sub='byte')

corp = VCorpus(VectorSource(ds[,2]))
corp = tm_map(corp, content_transformer(tolower))
corp = tm_map(corp, removeWords, stopwords("english"))
corp = tm_map(corp, removePunctuation)
corp = tm_map(corp, removeNumbers)
corp = tm_map(corp, stemDocument)

dtm <- DocumentTermMatrix(corp, control = list(tokenize=words, weighting=weightTf))
#dtm$dimnames$Terms

tfdif = c()
for(i in 1:dim(dtm)[2]){
  z = log(sum(dtm[,i]))+1
  zz = log(dim(dtm)[1]/sum(dtm[,i]>0))
  tfdif[i] = z*zz
  print(100*i/dim(dtm)[2])}

z = cbind(dtm$dimnames$Terms,tfdif)
#write.csv(z, file = "/Users/js2758/OneDrive/Documents/Projects/transparency/data/tfdif.csv")
z = read.csv(z, file = "/Users/js2758/OneDrive/Documents/Projects/transparency/data/tfdif.csv")
tfdif = z[,3]

zdtm = dtm[,tfdif>15]
ui = unique(zdtm$i)
zdtm = zdtm[ui,]
zdtm = zdtm

g = LDA(zdtm,40,method = 'VEM',control=NULL,model=NULL)
gi = posterior(g)

rev(sort(gi$terms[1,]))[1:10]
rev(sort(gi$terms[7,]))[1:10]

gi$topics[1,]

dt = gi$topics
ind = seq(1, dim(dt)[1])

plot(dt[,1]~ind)



##old code

dtm <- DocumentTermMatrix(corp, control = list(tokenize=words, weighting=weightTfIdf))
z = dtm$dimnames$Terms
uj = unique(dtm$j)
m = match(uj, dtm$j)
zw = dtm$v[m]
zr = rank(zw)
plot(zw~zr)


remove.packages("RWeka", "Library/Frameworks/R.framework/Versions/3.5/Resources/library/RWeka")

remove.packages("rJava", "Library/Frameworks/R.framework/Versions/3.5/Resources/library/")
/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/lib/server/libjsig.dylib

#rtots <- apply(dtm , 1, sum) 
#dtm.new   <- dtm[rowTotals> 0, ]

options('java.home')
options("java.home"="/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home/")
Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk-11.0.1.jdk/Contents/Home')
Sys.setenv(LD_LIBRARY_PATH='$JAVA_HOME/lib/server')

z = dm$speech_id[dm$chamber=="S"]
ds = ds[ds$speech_id%in%z,]
ds[,2] = iconv(ds[,2], to='UTF-8-MAC', sub='byte')
ds[,2] = tolower(ds[,2])
ng = c()
for(i in 1:dim(ds)[1]){
	z = ds[3,2]
	z = preprocess(z, case = "lower", remove.punct=TRUE, fix.spacing = TRUE)
	ng[i] = ngram_asweka(z, 2)
	print(i)}

library(RWeka)

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = "_"), use.names = FALSE)
}
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
dtm <- DocumentTermMatrix(corp, control = list(tokenize=NLP_tokenizer, weighting=weightTf))


dtm <- DocumentTermMatrix(corp, control = list(tokenize=NLP_tokenizer,removePunctuation = T, stemming = T, tolower = T, stopwords= stopwords("english"), weighting=weightTfIdf))
