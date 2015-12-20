# TBD

library(tm)
install.packages('ngram')
library(ngram)

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

# LoadTextFile uses readLines on the indicated file assuming that it is UTF-8
# and put the resulting vector in the global environment as ObjectName.
# For convenience, return it. Optionaly report the load time.

LoadTextFile = function(FileName,DataDir='.',ObjectName=FileName,
                        MaxRowsToRead=-1,verbose=F)
{
    StartTime = proc.time()
    FN = paste0(DataDir,'/',FileName)
    con = file(FN,open='rb') # So that Control-Z doesn't stop readLines since that is valid UTF-8
    assign(ObjectName,
           readLines(con,n=MaxRowsToRead,encoding='UTF-8'),
           envir=.GlobalEnv)
    close(con)
    LoadTime = proc.time()
    LoadTime = LoadTime - StartTime
    ret = get(ObjectName)
    if (verbose == T)
    {
        nrows = length(ret)
        print(paste0(format(nrows,big.mark=','),' rows loaded in:'))
        print(LoadTime)
    }
    ret
} # LoadTextFile

invisible(LoadTextFile('en_US.blogsU.txt',EnUsDir,'Blogs',verbose=T))
invisible(LoadTextFile('en_US.newsU.txt',EnUsDir,'News',verbose=T))
invisible(LoadTextFile('en_US.twitterU.txt',EnUsDir,'Twitter',verbose=T))

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

