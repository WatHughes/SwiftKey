# TBD

# library(tm) # Not really helpful for this project
# install.packages('ngram') # Does not appear to be useful
# library(ngram)
# install.packages('quanteda')
library(quanteda)
require(RColorBrewer)

source('LoadTextFile.R')

DataDir = '../data'
EnUsDir = paste0(DataDir,'/en_US')

# Remove all documents that contain any of these words. The object is to avoid
# training with phrases that require profanity to convey a thought. That helps
# the final application of the model avoid 'going there'. Note that the user can
# still enter profanity in which case the app will have to fall back on its
# unknown word processing. Also note this list is somewhat arbitrary (I like it)
# and that this use of ProfaneWords is very much a simplification. For example,
# 'pussy cat' is not profane. But most commonly, I expect 'pussy' to be very
# profane so its here.

ProfaneWords = c( # Similar to https://en.wikipedia.org/wiki/Seven_dirty_words
     'asshole', 'cocksucker', 'cunt', 'fuck', 'motherfucker', 'piss',
     'pussy', 'shit', 'tits'
)

# Thoughts on punctuation. There's information content in the sentence ending
# period, question mark, and exclamation point. By that, I mean that n-grams
# shouldn't cross that boundary. Apostrophe is part of a word. <sigh> Other uses
# of punctuation include URLs, hastags, etc. and I think are best left alone as
# part of words because the resulting ngrams will be rare so won't influence the
# results.

# 'Words' like RT probably wont' be entered into the app so I wonder if I should
# remove tweets that contain them?

SampleSize = 100000
Verbose = F
if (hostname == 'VM-EP-3')
{
    SampleSize = -1
    Verbose = T
}
if (hostname == 'AJ')
{
    SampleSize = -1
    Verbose = T
}

# Notepad was used to add the encoding bytes to the 'U' version of these files.
invisible(LoadTextFile('en_US.blogsU.txt',EnUsDir,'Blogs',SampleSize,verbose=Verbose))
invisible(LoadTextFile('en_US.newsU.txt',EnUsDir,'News',SampleSize,verbose=Verbose))
invisible(LoadTextFile('en_US.twitterU.txt',EnUsDir,'Twitter',SampleSize,verbose=Verbose))

table(Encoding(Blogs))
table(Encoding(News))
table(Encoding(Twitter))
# unknown   UTF-8
#  636261  263027
# unknown   UTF-8
#  874277  135965
# unknown   UTF-8
# 2282716   77432


BlogsC=corpus(Blogs)
# BlogsT = tokenize(BlogsC)
tokenInfoB=summary(BlogsC)
BlogsD = dfm(BlogsC)
# proc.time()
# BlogsD = dfm(BlogsC,ngrams=1:3)
# proc.time()
NewsC=corpus(News)
# NewsT = tokenize(NewsC)
tokenInfoC=summary(NewsC)
NewsD = dfm(NewsC)
TwitterC=corpus(Twitter)
# TwitterT = tokenize(TwitterC)
tokenInfoT=summary(TwitterC)
TwitterD = dfm(TwitterC)

BasicStats = data.frame(Source=c('Blogs','News','Tweets'),
                        WordCount=c(sum(BlogsD),sum(NewsD),sum(TwitterD)),
                        DistinctWordCount=c(BlogsD@Dim[2],NewsD@Dim[2],TwitterD@Dim[2]),
                        LineCount=c(BlogsD@Dim[1],NewsD@Dim[1],TwitterD@Dim[1])
)
#   Source WordCount DistinctWordCount LineCount
# 1  Blogs  36911104            380886    899288
# 2   News  33487271            331769   1010242
# 3 Tweets  29562101            419436   2360148

par(mfrow=c(1,3))
hist(tokenInfoB$Tokens,breaks=c(0,10,20,30,40,50,100,300),xlab='Word Count',main='Blogs')
hist(tokenInfoC$Tokens,breaks=c(0,10,20,30,40,50,100,300),xlab='Word Count',main='News')
hist(tokenInfoT$Tokens,breaks=c(0,10,20,30,40,50,100,300),xlab='Word Count',main='Tweets')

layout(matrix(c(1,4,2,5,3,6),nrow=2),heights=c(lcm(1), lcm(6)))
par(mar=c(0,0,0,0))
plot.new()
text(x=0.5,y=0.5,cex=2,'Blogs')
frame()
text(x=0.5,y=0.5,cex=2,'News')
frame()
text(x=0.5,y=0.5,cex=2,'Tweets')
plot(BlogsD, max.words=100,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))
plot(NewsD, max.words=100,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))
plot(TwitterD, max.words=100,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))

