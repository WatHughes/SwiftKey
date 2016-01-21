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
save(Blogs,file='../data/Rdata/Blogs.rda')
save(News,file='../data/Rdata/News.rda')
save(Twitter,file='../data/Rdata/Twitter.rda')

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

BNC = corpus(append(Blogs,News))
BNCD4 = dfm(BNC,ngrams=4)

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

save(TwitterD4,file='../data/Rdata/TwitterD4.rda')
str(TwitterD4)
TwitterU4=colSums(TwitterD4)
str(TwitterU4)
#  Named num [1:18242385] 5 1 1 12 3343 ...
#  - attr(*, "names")= chr [1:18242385] "how_are_you_btw" "are_you_btw_thanks" "you_btw_thanks_for" "btw_thanks_for_the" ..
save(TwitterU4,file='../data/Rdata/TwitterU4.rda')
TwitterU4Top5p=head(sort(TwitterU4,decreasing=T),length(TwitterU4)*.05)
head(TwitterU4Top5p)
tail(TwitterU4Top5p)
TwitterU4Top1p=head(sort(TwitterU4,decreasing=T),length(TwitterU4)*.01)
head(TwitterU4Top1p)
# thanks_for_the_follow     thanks_for_the_rt     thank_you_for_the     can't_wait_to_see
#                  6269                  3343                  3051                  2704
#     thank_you_so_much        is_going_to_be
#                  2308                  2282
tail(TwitterU4Top1p)
#        it_down_a_bit are_going_to_release   now_i_realize_that       out_i_might_be
#                    5                    5                    5                    5
#  forever_in_my_heart  better_or_for_worse
#                    5                    5
save(TwitterU4Top1p,file='../data/Rdata/TwitterU4Top1p.rda')

save(NewsD4,file='../data/Rdata/NewsD4.rda')
str(NewsD4)
NewsU4=colSums(NewsD4)
str(NewsU4)
#  Named num [1:25409131] 1 1 1 1 1 1 1 3 1 1 ...
#  - attr(*, "names")= chr [1:25409131] "he_wasn't_home_alone" "wasn't_home_alone_apparently" "the_st_louis_plant" "st_louis_plant_had" ...
save(NewsU4,file='../data/Rdata/NewsU4.rda')
NewsU4Top5p=head(sort(NewsU4,decreasing=T),length(NewsU4)*.05)
head(NewsU4Top5p)
tail(NewsU4Top5p)
NewsU4Top1p=head(sort(NewsU4,decreasing=T),length(NewsU4)*.01)
head(NewsU4Top1p)
#       the_end_of_the   for_the_first_time        at_the_end_of      the_rest_of_the
#                 2904                 2603                 2583                 2199
#  said_in_a_statement in_the_united_states
#                 1780                 1778
tail(NewsU4Top1p)
# there's_something_about_that               what_my_job_is    two_sources_familiar_with
#                            5                            5                            5
#        not_yet_available_but          bothered_by_much_of    citizens_united_v_federal
#                            5                            5                            5
save(NewsU4Top1p,file='../data/Rdata/NewsU4Top1p.rda')

save(BlogsD4,file='../data/Rdata/BlogsD4.rda')
str(BlogsD4)
BlogsU4=colSums(BlogsD4)
str(BlogsU4)
#  Named num [1:28394580] 1 1 1 1 4 1 1 1 1 1 ...
#  - attr(*, "names")= chr [1:28394580] "in_the_years_thereafter" "the_years_thereafter_most" "years_thereafter_most_of" "thereafter_most_of_the" ...
save(BlogsU4,file='../data/Rdata/BlogsU4.rda')
BlogsU4Top5p=head(sort(BlogsU4,decreasing=T),length(BlogsU4)*.05)
head(BlogsU4Top5p)
tail(BlogsU4Top5p)
BlogsU4Top1p=head(sort(BlogsU4,decreasing=T),length(BlogsU4)*.01)
head(BlogsU4Top1p)
#   the_end_of_the  the_rest_of_the    at_the_end_of at_the_same_time when_it_comes_to
#             3358             3054             2910             2257             2007
#    to_be_able_to
#             1887
tail(BlogsU4Top1p)
# will_be_sponsored_by    the_world_of_film   but_i_believe_it's    crack_in_the_wall
#                    5                    5                    5                    5
#     a_little_off_and     because_i_say_so
#                    5                    5
save(BlogsU4Top1p,file='../data/Rdata/BlogsU4Top1p.rda')

save(BNCD4,file='../data/Rdata/BNCD4.rda')
str(BNCD4)
BNCU4=colSums(BNCD4)
str(BNCU4)
#  Named num [1:52238160] 1 1 1 1 7 3 2 1 1 1 ...
#  - attr(*, "names")= chr [1:52238160] "in_the_years_thereafter" "the_years_thereafter_most" "years_thereafter_most_of" "thereafter_most_of_the" ...
save(BNCU4,file='../data/Rdata/BNCU4.rda')
BNCU4Top5p=head(sort(BNCU4,decreasing=T),length(BNCU4)*.05)
head(BNCU4Top5p)
tail(BNCU4Top5p)
BNCU4Top1p=head(sort(BNCU4,decreasing=T),length(BNCU4)*.01)
head(BNCU4Top1p)
#     the_end_of_the      at_the_end_of    the_rest_of_the for_the_first_time   at_the_same_time
#               6262               5493               5253               4472               3952
#    one_of_the_most
#               3622
tail(BNCU4Top1p)
#  because_i_find_the     him_he_can't_do praying_in_the_holy    the_mercy_of_our    well_in_both_the
#                   5                   5                   5                   5                   5
#  instead_of_all_the
#                   5
save(BNCU4Top1p,file='../data/Rdata/BNCU4Top1p.rda')

BNCU4TopHp=BNCU4Top1p[1:round(length(BNCU4Top1p)/2,0)]
head(BNCU4TopHp)
#     the_end_of_the      at_the_end_of    the_rest_of_the for_the_first_time   at_the_same_time
#               6262               5493               5253               4472               3952
#    one_of_the_most
#               3622
tail(BNCU4TopHp)
#    me_by_my_friend   i_was_heading_to     this_is_from_a more_about_what_it parking_lot_it_was
#                  8                  8                  8                  8                  8
#    bit_and_then_we
#                  8
save(BNCU4TopHp,file='../data/Rdata/BNCU4TopHp.rda')
