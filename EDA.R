# TBD

# library(tm) # Not really helpful for this project
# install.packages('ngram') # Does not appear to be useful
# library(ngram)
# install.packages('quanteda')
library(quanteda)
require(RColorBrewer)

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



# LoadTextFile uses readLines on the indicated file assuming that it is UTF-8
# and put the resulting vector in the global environment as ObjectName.
# For convenience, return it. Optionaly report the load time, return a sample,
# and/or limit the number of rows read.

LoadTextFile = function(FileName,DataDir='.',ObjectName=FileName,
                        SampleSize=-1,MaxRowsToRead=-1,verbose=F)
{
    StartTime = proc.time()
    FN = paste0(DataDir,'/',FileName)
    con = file(FN,open='rb') # So that Control-Z doesn't stop readLines since that is valid UTF-8
    ret = readLines(con,n=MaxRowsToRead,encoding='UTF-8');
    close(con)
    LoadTime = proc.time()
    LoadTime = LoadTime - StartTime
    if (verbose == T)
    {
        nrows = length(ret)
        print(paste0(format(nrows,big.mark=','),' rows loaded in:'))
        print(LoadTime)
    }
    if (SampleSize > 0)
    {
        ret = sample(ret, SampleSize)
    }
    assign(ObjectName,
           ret,
           envir=.GlobalEnv)
    ret
} # LoadTextFile

SampleSize = 100000
invisible(LoadTextFile('en_US.blogsU.txt',EnUsDir,'Blogs',SampleSize,verbose=T))
invisible(LoadTextFile('en_US.newsU.txt',EnUsDir,'News',SampleSize,verbose=T))
invisible(LoadTextFile('en_US.twitterU.txt',EnUsDir,'Twitter',SampleSize,verbose=T))

# fb = paste0(EnUsDir,'/en_US.blogs.txt')
# fn = paste0(EnUsDir,'/en_US.news.txt')
# ft = paste0(EnUsDir,'/en_US.twitter.txt')
# Notepad was used to add the encoding bytes to these files.
fb = paste0(EnUsDir,'/en_US.blogsU.txt')
fn = paste0(EnUsDir,'/en_US.newsU.txt')
ft = paste0(EnUsDir,'/en_US.twitterU.txt')
j=scan(fb,what=character(),encoding='UTF-8',nlines=3)
nchar(j[1])
nchar(j[16])
conb = file(fb,encoding='UTF-8')
lb = readLines(conb)
close(conb)
conn = file(fn,open='rb',encoding='UTF-8') # rb 'cause there's a control Z in the file
ln = readLines(conn)
close(conn)
cont = file(ft,encoding='UTF-8')
lt = readLines(cont)
close(cont)
# Warning messages: ### Fixed by adding the UTF-8 encoding bytes.
# 1: line 167155 appears to contain an embedded nul
# 2: line 268547 appears to contain an embedded nul
# 3: line 1274086 appears to contain an embedded nul
# 4: line 1759032 appears to contain an embedded nul
length(lb)
# [1] 899288
str(l)
# chr [1:899288] "In the years thereafter, most of the Oil fields and platforms were named after pagan “gods”.""| __truncated__ ...
nc=nchar(l)
str(nc)
# int [1:899288] 92 22 692 199 608 54 17 306 302 698 ...
max(nc)
# [1] 40833
length(ln)
# [1] 77259 # Hmmm, should be 1,010,242
head(Encoding(lb)) # [1] "UTF-8"   "unknown" "unknown" "unknown" "unknown" "unknown"
head(lb)

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
NewsC=corpus(News)
# NewsT = tokenize(NewsC)
tokenInfoC=summary(NewsC)
NewsD = dfm(NewsC)
TwitterC=corpus(Twitter)
# TwitterT = tokenize(TwitterC)
tokenInfoT=summary(TwitterC)
TwitterD = dfm(TwitterC)

par(mfrow=c(1,3))
hist(tokenInfoB$Tokens,xlab='Word Count',main='Blogs')
hist(tokenInfoC$Tokens,xlab='Word Count',main='News')
hist(tokenInfoT$Tokens,xlab='Word Count',main='Tweets')

layout(matrix(c(1,4,2,5,3,6),nrow=2),heights=c(lcm(1), lcm(6)))
par(mar=rep(0, 0))
plot.new()
text(x=0.5,y=0.5,cex=2,'Blogs')
frame()
text(x=0.5,y=0.5,cex=2,'News')
frame()
text(x=0.5,y=0.5,cex=2,'Tweets')
plot(BlogsD, max.words=100,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))
plot(NewsD, max.words=100,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))
plot(TwitterD, max.words=100,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))

