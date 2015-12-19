# TBD

library(tm)

DataDir = '../data'
EnUsDir = paste0(DataDir,'/en_US')
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

