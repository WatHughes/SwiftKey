# TBD

# library(tm) # Not really helpful for this project
# install.packages('ngram') # Does not appear to be useful
# library(ngram)
# install.packages('quanteda')
require(quanteda)
require(RColorBrewer)
# install.packages('DT')
# install.packages('shinyBS')
# install.packages('digest')
# devtools::install_github("ebailey78/shinyBS") # Didn't work out for me.

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

hostname = system('hostname', intern=T)

SampleSize = 100000
Verbose = F
if (hostname == 'VM-EP-3')
{
    SampleSize = -1
    Verbose = T
}
if (hostname == 'AJ')
{
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

BlogsD4 = dfm(BlogsC,ngrams=4)
NewsD4 = dfm(NewsC,ngrams=4)
TwitterD4 = dfm(TwitterC,ngrams=4)

# TwitterT1=tokenize(TwitterC)
# TwitterD1234 = dfm(TwitterC,ngrams=1:4)
# str(TwitterD)
# str(TwitterD1234)
# TwitterD@Dimnames$features[3]
# TwitterD1234@Dimnames$features[3]
# TwitterD1234@Dimnames$features[2265891]
# TwitterD1234@p[2265891]
# TwitterD1234@p[2265892]
# TwitterD1234@x[4323226]
# TwitterD1234@x[4323227]
# TwitterD1234@i[3715]
# TwitterD1234@i[3716]
# TwitterD1234@i[3717]
# TwitterD1234@i[3718]
# TwitterD1234@i[3714]
# grep('^a_case_of',TwitterD1234@Dimnames$features)
# grep('^a_case_of',TwitterD1234@Dimnames$features,value=T)
# colSums(TwitterD1234[,grep('^a_case_of',TwitterD1234@Dimnames$features)])
tail(sort(colSums(BlogsD4[,grep('^a_case_of',BlogsD4@Dimnames$features)])),6)
tail(sort(colSums(NewsD4[,grep('^a_case_of',NewsD4@Dimnames$features)])),6)
tail(sort(colSums(TwitterD4[,grep('^a_case_of',TwitterD4@Dimnames$features)])),6)
Q2 = function(last3)
{
    pattern = paste0('^',last3,'_')
    print(tail(sort(colSums(BlogsD4[,grep(pattern,BlogsD4@Dimnames$features)])),6))
    print(tail(sort(colSums(NewsD4[,grep(pattern,NewsD4@Dimnames$features)])),6))
    tail(sort(colSums(TwitterD4[,grep(pattern,TwitterD4@Dimnames$features)])),6)
}
Q2('a_case_of') # beer
Q2('would_mean_the') # world
Q2('make_me_the') # happiest

Q2('struggling_but_the') # ** nada **; grep no help; Google and English suggest defense
Q2('but_the') # truth -- not a choice

Q2('date_at_the') # end/beverly ** Of the choices, grocery == 1? ** Wrong ** Try Movies based on English *** Wrong ***
Q2('be_on_my') # way
Q2('in_quite_some') # time

Q2('with_his_little') # brother/sister, not choices; grep/Google no help; Eng -> fingers
Q2('his_little') # Not choices
Q2('little') # Not choices
# FWIW unigrams suggest eyes:
colSums(NewsD[,c('eyes','toes','fingers','ears')])+colSums(TwitterD[,c('eyes','toes','fingers','ears')])+colSums(BlogsD[,c('eyes','toes','fingers','ears')])
#    eyes    toes fingers    ears
#   16387    1285    4324    2692

Q2('faith_during_the') # three/worship/last, not choices; grep no help; Google suggests bad
Q2('you_must_be') # a, not a choice; no hints from grep. Google suggests insane or asleep
Q2('') #

BasicStats = data.frame(Source=c('Blogs','News','Tweets'),
                        DocumentCount=c(BlogsD@Dim[1],NewsD@Dim[1],TwitterD@Dim[1]),
                        DistinctWordCount=c(BlogsD@Dim[2],NewsD@Dim[2],TwitterD@Dim[2]),
                        WordCount=c(sum(BlogsD),sum(NewsD),sum(TwitterD))
)
BasicStats$AverageWC = BasicStats$WordCount / BasicStats$DocumentCount
BasicStats$RatioDWC = BasicStats$DistinctWordCount / BasicStats$WordCount
save(BasicStats,file='BasicStats.rda')

#   Source   DocumentCount  DistinctWordCount   WordCount AverageWC    RatioDWC
# 1  Blogs         899,288            380,886  36,911,104  41.04481 0.010319009
# 2   News       1,010,242            331,769  33,487,271  33.14777 0.009907317
# 3 Tweets       2,360,148            419,436  29,562,101  12.52553 0.014188301

par(mfrow=c(1,3),mar=3*c(1,1,1,1))
hist(tokenInfoB$Tokens,breaks=c(0,10,20,30,40,50,100,300),xlab='Word Count',main='Blogs')
abline(v=BasicStats$AverageWC[1],col='green',lwd=2)
legend('right',legend=paste0('Avg. Word Count ',sprintf('%2.0f',BasicStats$AverageWC[1])),text.col='darkgreen')
hist(tokenInfoC$Tokens,breaks=c(0,10,20,30,40,50,100,300),xlab='Word Count',main='News')
abline(v=BasicStats$AverageWC[2],col='green',lwd=2)
legend('right',legend=paste0('Avg. Word Count ',sprintf('%2.0f',BasicStats$AverageWC[2])),text.col='darkgreen')
hist(tokenInfoT$Tokens,breaks=c(0,10,20,30,40,50,100,300),xlab='Word Count',main='Tweets')
abline(v=BasicStats$AverageWC[3],col='green',lwd=2)
legend('right',legend=paste0('Avg. Word Count ',sprintf('%2.0f',BasicStats$AverageWC[3])),text.col='darkgreen')

png(filename='ExploratoryHistograms.png',width=700,height=350,res=96)
par(mfrow=c(1,3),mar=3*c(1,1,1,1))
# breaks=c(0,25,50,100,300)
breaks=c(0,10,20,30,40,50,100,300)
hist(tokenInfoB$Tokens,breaks=breaks,xlab='Word Count',main='Blogs')
abline(v=BasicStats$AverageWC[1],col='green',lwd=2)
legend('right',legend=paste0('Avg. Word Count ',sprintf('%2.0f',BasicStats$AverageWC[1])),text.col='darkgreen')
hist(tokenInfoC$Tokens,breaks=breaks,xlab='Word Count',main='News')
abline(v=BasicStats$AverageWC[2],col='green',lwd=2)
legend('right',legend=paste0('Avg. Word Count ',sprintf('%2.0f',BasicStats$AverageWC[2])),text.col='darkgreen')
hist(tokenInfoT$Tokens,breaks=breaks,xlab='Word Count',main='Tweets')
abline(v=BasicStats$AverageWC[3],col='green',lwd=2)
legend('right',legend=paste0('Avg. Word Count ',sprintf('%2.0f',BasicStats$AverageWC[3])),text.col='darkgreen')
dev.off()

png(filename='ExploratoryWordClouds.png',width=700,height=350,res=96)
MaxWords = 50
layout(matrix(c(1,4,2,5,3,6),nrow=2),heights=c(lcm(1), lcm(8)))
par(mar=c(0,0,0,0))
plot.new()
text(x=0.5,y=0.5,cex=2,'Blogs')
frame()
text(x=0.5,y=0.5,cex=2,'News')
frame()
text(x=0.5,y=0.5,cex=2,'Tweets')
plot(BlogsD, max.words=MaxWords,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))
plot(NewsD, max.words=MaxWords,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))
plot(TwitterD, max.words=MaxWords,colors=brewer.pal(6,'Dark2'),scale=c(8,.5))
dev.off()

head(sort(table(sapply(tokenize(head(TwitterC$documents$texts,2500000)),function(x) x[1])),decreasing=T),20)
#      I      "    I'm     RT Thanks    The      i   Just    You     My     If     We  Happy     So
# 178194  48305  34945  34903  34864  34179  22268  22196  20899  19909  19798  17583  17562  16574
#    you   It's thanks   What  Thank   Good
#  16380  16168  15717  15428  13542  13248

head(sort(table(sapply(tokenize(head(NewsC$documents$texts,2500000)),function(x) x[1])),decreasing=T),20)
#      "    The     In    But      A     He     It    And     As    For      I   When     On   This
# 139601 115813  24066  19951  18586  11676   8301   7964   7504   7266   6859   6529   6422   6328
#     If     At  After   That   It's      -
#   6082   5956   5746   5279   5010   4855

head(sort(table(sapply(tokenize(head(BlogsC$documents$texts,2500000)),function(x) x[1])),decreasing=T),20)
#     I   The     "  This    So   And    In    We    It   But     A    As    My     -     1    If
# 68959 55493 32940 17410 16880 15183 14701 12275 11431 11022 10360  9221  9185  8959  8902  8426
#  When   I'm  What There
#  7794  7133  6858  6601
