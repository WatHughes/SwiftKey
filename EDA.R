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
load('../data/Rdata/Twitter.rda')
load('../data/Rdata/Blogs.rda')
load('../data/Rdata/News.rda')

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

BNCD3 = dfm(BNC,ngrams=3)
BlogsD3 = dfm(BlogsC,ngrams=3)
NewsD3 = dfm(NewsC,ngrams=3)
TwitterD3 = dfm(TwitterC,ngrams=3)

BNCD2 = dfm(BNC,ngrams=2)
BlogsD2 = dfm(BlogsC,ngrams=2)
NewsD2 = dfm(NewsC,ngrams=2)
TwitterD2 = dfm(TwitterC,ngrams=2)

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
#  in_washington_d.c_before washington_d.c_before_the      is_entering_its_last
#                         2                         2                         2
#       swam_across_a_river      before_he_was_picked     he's_been_blind_since
#                         2                         2                         2
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

save(BNCD3,file='../data/Rdata/BNCD3.rda')
str(BNCD3)
BNCU3=colSums(BNCD3)
str(BNCU3)
save(BNCU3,file='../data/Rdata/BNCU3.rda')
BNCU3Top5p=head(sort(BNCU3,decreasing=T),length(BNCU3)*.05)
head(BNCU3Top5p)
tail(BNCU3Top5p)
BNCU3Top1p=head(sort(BNCU3,decreasing=T),length(BNCU3)*.01)
head(BNCU3Top1p)
tail(BNCU3Top1p)
save(BNCU3Top1p,file='../data/Rdata/BNCU3Top1p.rda')
BNCU3TopHp=BNCU3Top1p[1:round(length(BNCU3Top1p)/2,0)]
head(BNCU3TopHp)
tail(BNCU3TopHp)
save(BNCU3TopHp,file='../data/Rdata/BNCU3TopHp.rda')

save(BNCD2,file='../data/Rdata/BNCD2.rda')
str(BNCD2)
BNCU2=colSums(BNCD2)
str(BNCU2)
save(BNCU2,file='../data/Rdata/BNCU2.rda')
BNCU2Top5p=head(sort(BNCU2,decreasing=T),length(BNCU2)*.05)
head(BNCU2Top5p)
tail(BNCU2Top5p)
BNCU2Top1p=head(sort(BNCU2,decreasing=T),length(BNCU2)*.01)
head(BNCU2Top1p)
tail(BNCU2Top1p)
save(BNCU2Top1p,file='../data/Rdata/BNCU2Top1p.rda')
BNCU2TopHp=BNCU2Top1p[1:round(length(BNCU2Top1p)/2,0)]
head(BNCU2TopHp)
tail(BNCU2TopHp)
save(BNCU2TopHp,file='../data/Rdata/BNCU2TopHp.rda')

save(TwitterD3,file='../data/Rdata/TwitterD3.rda')
str(TwitterD3)
TwitterU3=colSums(TwitterD3)
str(TwitterU3)
save(TwitterU3,file='../data/Rdata/TwitterU3.rda')
TwitterU3Top5p=head(sort(TwitterU3,decreasing=T),length(TwitterU3)*.05)
head(TwitterU3Top5p)
tail(TwitterU3Top5p)
TwitterU3Top1p=head(sort(TwitterU3,decreasing=T),length(TwitterU3)*.01)
head(TwitterU3Top1p)
tail(TwitterU3Top1p)
save(TwitterU3Top1p,file='../data/Rdata/TwitterU3Top1p.rda')

save(TwitterD2,file='../data/Rdata/TwitterD2.rda')
str(TwitterD2)
TwitterU2=colSums(TwitterD2)
str(TwitterU2)
save(TwitterU2,file='../data/Rdata/TwitterU2.rda')
TwitterU2Top5p=head(sort(TwitterU2,decreasing=T),length(TwitterU2)*.05)
head(TwitterU2Top5p)
#  in_the for_the  of_the  on_the   to_be  to_the
#   78326   73948   56868   48457   46986   43397
tail(TwitterU2Top5p)
#  but_nothin  as_interim and_premium    the_tarp  haha_crazy come_kidnap
#           9           9           9           9           9           9
save(TwitterU2Top5p,file='../data/Rdata/TwitterU2Top5p.rda')
TwitterU2Top1p=head(sort(TwitterU2,decreasing=T),length(TwitterU2)*.01)
head(TwitterU2Top1p)
tail(TwitterU2Top1p)
#      then_try    it_bothers   lakers_lose    making_any           a_v tomorrow_that
#            50            50            50            50            50            50
save(TwitterU2Top1p,file='../data/Rdata/TwitterU2Top1p.rda')

save(TwitterU2Top5p,file='../data/Rdata/TwitterU2Top5p.rda')
save(BNCU2Top5p,file='../data/Rdata/BNCU2Top5p.rda')
save(BNCU4Top5p,file='../data/Rdata/BNCU4Top5p.rda')

# Material for the evaluations:

set.seed(17)
TS = sample(TwitterC,10)
set.seed(17)
BS = sample(BlogsC,10)
set.seed(17)
NS = sample(NewsC,10)

> sapply(list(TS,BS,NS),function(S){S$documents})
#      [,1]  Tweets
#  [1,] "Wow! Amazing. What will you bring? We need a pic of your suitcase!"
# What will you <bring>
#  [2,] "If you're in town shopping on Black Friday and need a place to stay check us out online @ www.stayinportland.com."
# If you're in town shopping on Black Friday and <need>
#  [3,] "Bynum sure behaves like someone who could have used a year or two in college to grow up a little...."
# someone who could have used a year or <two>
#  [4,] "Yea, I know. It's too bad that we aren't in Idaho at the correct time! What a shame! :("
# It's too bad that we aren't <in>
#  [5,] "are you kidding me?? X games- where shaun white makes gravity non existant"
#  [6,] "There's always that one person that no matter what they've done to you, you just still can't let them go."
# There's always that one person <that> no <matter> <what> <they/they've>
#  [7,] "\"We're going to cut away from Newt Gingrich because we have important news to report.\""
# We're going to cut away <from>
#  [8,] "if you don't want to be around someoneorpendtime with them be real about it from the jump don't make plans and stand them up"
# If you don't <want> <to> <be> <around> <someone>[not someonor]
#  [9,] "Thanks, Noah. Once a die-hard, always a die-hard."
# [10,] "Hello KIWW, I would luv to know if you all do business in Asia and if you still engage in sponsoring sports properties?"
# Hello KIWW, I would luv to <know> <if> <you> <all> <do> <business> <in>
#       [,2] Blogs
#  [1,] "\"You're breathing fast. Your cheeks are flushed. Your pupils are dilated.\""
# You're breathing <fast>. Your cheeks are <flushed>. Your pupils <are> <dilated>.
#  [2,] "Typically, we will run the tests, provide feedback to candidates, and then brief the selection panel on our findings. Where we have effective person specifications, we can advise on the likelihood of each candidate providing the school with the particular characteristics which the post demands."
# Typically, we will run <the> <tests>, provide feedback <to> <candidates>, <and>
#  [3,] "This sort of thing could never happen even in your worst nightmares."
# This sort <of> <thing> <could> <never> <happen> <even> <in> <your> <worst> <nightmare/s>
#  [4,] "Specific to the dance front, this supposed prowess often comes as a result of having studied with such-and-such and so-and-so. Or with the sheer amount of time one has spent with tango. Neither of which necessarily means anything. One question that I'm sick of hearing when meeting someone new is, \"How long have you been dancing?\" Though it may appear innocuous, it's a question with loaded expectations, and I often get the sense that there is an evaluation being made depending on the response. But everyone knows people who have been dancing for a good amount of time but who still, to put it mildly, kind of suck. Of course, some of these people feel that they've put in the hours and have earned their degree, and become \"teachers\" in some form. Either by actually holding classes, or more informally by instructing people they meet in the milongas and/or prácticas."
# Speciic to the dance <front>, <this> supposed <prowess> <often> <comes> <as> <a> <result> <of> <having> <studied> <with>
#  [5,] "\"Santa Claus\" by Throwing Muses (from, Just Can't Get Enough: New Wave Xmas, 1996)"
#  [6,] "This diversity and openness to different genres is reflected in Aldrei's line-up. Punk band Morðingjarnir perform an adrenaline-fuelled set, which is cut short when the drunk bassist is carried off the stage for exhorting the audience to throw things at him."
# This diversity and <openness> <to> <different> <genres> <is> <reflected> <in>
#  [7,] "our new book for the month of may is"
# our new book for the <month> <of>
#  [8,] "Upper heaven; grace, now"
#  [9,] "I enjoyed another great Mother's Day on Sunday! Actually, the entire weekend was great!"
# I enjoyed another <great> Mother's <Day> <on> <Sunday>!
# [10,] "We are home now. We arrived 10 days ago from our 4-month-long exchange to India. I think now we are all feeling much better our bodies adjusting slowly to the new time with the 13 1/2 hours difference between home and our last home in Gwalior."
# We are home <now>. We arrived 10 <days> <ago> <from> <our>
#       [,3] News
#  [1,] "1/2 cup no-salt, no-sugar, fat-free marinara sauce (such as Pomi)"
#  [2,] "But for most members of The Church of Jesus Christ of Latter-day Saints, they are the best 20 hours of the year."
# But for most <members> <of> <The> <Church>
#  [3,] "The supporting role was an unfamiliar one for Cannon, who was used to leading her team during a four-year varsity basketball career that began when she was a freshman at North Carroll. Cannon did everything for the Mavericks this season, ranking among the top six players in four major categories."
# The supporting <role> <was> <an> <unfamiliar> <one> <for>
#  [4,] "When he was 3, the family fled again, this time to New York, after Arab riots and the massacre of Jews in the old Palestine."
# When he <was> 3, <the> <family> <fled> <again>, <this> <time> <to>
#  [5,] "\"1906 Earthquake,\" despite its title, teaches us that the fire that followed the quake wreaked as much or more havoc as the seismic event itself. Views that show the city after the smoke finally cleared bring to mind the photo-journalistic record of events such as the destruction of Hiroshima or the carpet-bombing of German cities at the end of World War II."
#  [6,] "\"Ultimately, because I'm a word-driven songwriter, I need support from people who love language,\" she said."
# Ultimately, because I'm <a> word-driven <songwriter>, <I> <need> <support> <from> <people> <who> <love> <language>,
#  [7,] "Thirty-five drop-off locations around the county will be open to the public from 10 a.m. to 2 p.m. People can dispose of all types of medication, no questions asked. Needles will not be accepted."
# Thirty-five drop-off <locations> <around> <the>
#  [8,] "Also, the owners are betting the team's fortunes on the field will improve under the leadership of General Manager Sandy Alderson, resulting in increased attendance, sponsorships and luxury-suite sales."
# Also, the owners <are> <betting> <the> <team's> <fortunes> <on> <the> <field> <will> <improve> <under> <the> <leadership> <of>
#  [9,] "The colorful turbans are partly inspired by the Fela Kuti Queens — the 27 women in African musician Fela Kuti's entourage to whom he was married at one time. Knowles counts them as style inspirations and credits them with her introduction to African prints. She also name-checks Icelandic singer Björk and Diana Ross as her major fashion influences. Ross \"has been the iconic image of so many decades,\" Knowles says. \"And she's always been able to transform in a non-costumey way. It's effortless and easy.\""
# The colorful turbans <are> partly <inspired> <by> <the>
# [10,] "Dr. Paul W. Nelson named program director for the General Surgery Residency Program at St. Vincent Medical Group at St. Vincent Carmel Hospital."
# Dr. Paul W. Nelson named program director <for> <the>
