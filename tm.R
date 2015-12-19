library(tm)
install.packages('RWeka') # P 6
library(RWeka)
install.packages('Snowball') # package ‘Snowball’ is not available (for R version 3.2.2)
install.packages('SnowballC')
library(SnowballC)
install.packages('openNLP')
library(openNLP)
install.packages('filehash')
library(filehash)
install.packages('tm.corpus.Reuters21578') # package ‘tm.corpus.Reuters21578’ is not available (for R version 3.2.2)
library(tm.corpus.Reuters21578)

new('PlainTextDocument',.Data='Some text. Wat was here.',Cached=T,
    Author='me',DateTimeStamp=Sys.time(),Description='Example Description.',
    ID='ID01',Origin='Custom',Heading='Chap. 1',Language='en_US',URI='xx')
# Error in getClass(Class, where = topenv(parent.frame())) :
#   “PlainTextDocument” is not a defined class

PlainTextDocument
ptd = PlainTextDocument("A simple plain text document",
                          heading = "Plain text document",
                          id = basename(tempfile()),
                          language = "en")
meta(ptd)
str(ptd)

txt = system.file('texts','txt',package='tm')
str(txt) # chr "E:/Apps/R/R-3.2.2/library/tm/texts/txt"
# This turns into VCorpus under the covers.
ovid = Corpus(DirSource(txt),readerControl=list(reader=readPlain,language='la',load=T))
str(ovid)
# List of 5
#  $ ovid_1.txt:List of 2
#   ..$ content: chr [1:16] "    Si quis in hoc artem populo non novit amandi," "         hoc legat et lecto carmine doctus amet." "    arte citae veloque rates remoque moventur," "         arte leves currus: arte regendus amor." ...
#   ..$ meta   :List of 7
#   .. ..$ author       : chr(0)
#   .. ..$ datetimestamp: POSIXlt[1:1], format: "2015-12-16 02:48:42"
#   .. ..$ description  : chr(0)
#   .. ..$ heading      : chr(0)
#   .. ..$ id           : chr "ovid_1.txt"
#   .. ..$ language     : chr "la"
#   .. ..$ origin       : chr(0)
#   .. ..- attr(*, "class")= chr "TextDocumentMeta"
#   ..- attr(*, "class")= chr [1:2] "PlainTextDocument" "TextDocument"
#  $ ovid_2.txt:List of 2
#   ..$ content: chr [1:17] "    quas Hector sensurus erat, poscente magistro" "         verberibus iussas praebuit ille manus." "    Aeacidae Chiron, ego sum praeceptor Amoris:" "         saevus uterque puer, natus uterque dea." ...
#   ..$ meta   :List of 7
#   .. ..$ author       : chr(0)
#   .. ..$ datetimestamp: POSIXlt[1:1], format: "2015-12-16 02:48:42"
#   .. ..$ description  : chr(0)
#   .. ..$ heading      : chr(0)
#   .. ..$ id           : chr "ovid_2.txt"
#   .. ..$ language     : chr "la"
#   .. ..$ origin       : chr(0)
#   .. ..- attr(*, "class")= chr "TextDocumentMeta"
#   ..- attr(*, "class")= chr [1:2] "PlainTextDocument" "TextDocument"
#  $ ovid_3.txt:List of 2
#   ..$ content: chr [1:17] "    vera canam: coeptis, mater Amoris, ades!" "    este procul, vittae tenues, insigne pudoris," "         quaeque tegis medios, instita longa, pedes." "    nos venerem tutam concessaque furta canemus," ...
#   ..$ meta   :List of 7
#   .. ..$ author       : chr(0)
#   .. ..$ datetimestamp: POSIXlt[1:1], format: "2015-12-16 02:48:42"
#   .. ..$ description  : chr(0)
#   .. ..$ heading      : chr(0)
#   .. ..$ id           : chr "ovid_3.txt"
#   .. ..$ language     : chr "la"
#   .. ..$ origin       : chr(0)
#   .. ..- attr(*, "class")= chr "TextDocumentMeta"
#   ..- attr(*, "class")= chr [1:2] "PlainTextDocument" "TextDocument"
#  $ ovid_4.txt:List of 2
#   ..$ content: chr [1:17] "    scit bene venator, cervis ubi retia tendat," "         scit bene, qua frendens valle moretur aper;" "    aucupibus noti frutices; qui sustinet hamos," "         novit quae multo pisce natentur aquae:" ...
#   ..$ meta   :List of 7
#   .. ..$ author       : chr(0)
#   .. ..$ datetimestamp: POSIXlt[1:1], format: "2015-12-16 02:48:42"
#   .. ..$ description  : chr(0)
#   .. ..$ heading      : chr(0)
#   .. ..$ id           : chr "ovid_4.txt"
#   .. ..$ language     : chr "la"
#   .. ..$ origin       : chr(0)
#   .. ..- attr(*, "class")= chr "TextDocumentMeta"
#   ..- attr(*, "class")= chr [1:2] "PlainTextDocument" "TextDocument"
#  $ ovid_5.txt:List of 2
#   ..$ content: chr [1:18] "    mater in Aeneae constitit urbe sui." "    seu caperis primis et adhuc crescentibus annis," "         ante oculos veniet vera puella tuos:" "    sive cupis iuvenem, iuvenes tibi mille placebunt." ...
#   ..$ meta   :List of 7
#   .. ..$ author       : chr(0)
#   .. ..$ datetimestamp: POSIXlt[1:1], format: "2015-12-16 02:48:42"
#   .. ..$ description  : chr(0)
#   .. ..$ heading      : chr(0)
#   .. ..$ id           : chr "ovid_5.txt"
#   .. ..$ language     : chr "la"
#   .. ..$ origin       : chr(0)
#   .. ..- attr(*, "class")= chr "TextDocumentMeta"
#   ..- attr(*, "class")= chr [1:2] "PlainTextDocument" "TextDocument"
#  - attr(*, "class")= chr [1:2] "VCorpus" "Corpus"

# Doc is wrong on P 12. PCorpus rather than Corpus.
ovidDB = PCorpus(DirSource(txt),
                readerControl=list(reader=readPlain,language='la',load=T),
                dbControl=list(useDb=T,dbName='E:/wat/misc/Coursera/C10_Cap/ovidDB',dbType='DB1'))
str(ovidDB) # Similar to ovid, cept PCorpus instead of VCorpus
ID(ovid[[5]]) # Error: could not find function
Author(ovid[[5]]) # Error: could not find function
meta(ovid[[5]])
ovid[[5]]$content
show(ovid)
show(ovidDB)
# <<PCorpus>>
# Metadata:  corpus specific: 0, document level (indexed): 0
# Content:  documents: 5
summary(ovidDB)
#            Length Class             Mode
# ovid_1.txt 2      PlainTextDocument list
# ovid_2.txt 2      PlainTextDocument list
# ovid_3.txt 2      PlainTextDocument list
# ovid_4.txt 2      PlainTextDocument list
# ovid_5.txt 2      PlainTextDocument list
inspect(ovid)

reut21578XMLgz = system.file('texts','reut21578.xml.gz',package='tm')
reut21578XMLgz = system.file('texts','reuters-21578.xml',package='tm')
Reut = VCorpus(ReutersSource()
Reut = Corpus(DirSource(reut21578),
    readerControl = list(reader = readReut21578XML))

reuters21578 <- system.file("texts", "reuters-21578.xml", package = "tm")
rs <- ReutersSource(reuters21578)
inspect(Corpus(rs)[1:2])

f = system.file('texts','rcv1_2330.xml',package='tm')
rcv1 = readRCV1(elem=list(content = readLines(f)),
                 language='en', id='id1')
meta(rcv1)
#   author       :
#   datetimestamp: 1996-08-20
#   description  :
#   heading      : USA: Tylan stock jumps; weighs sale of company.
#   id           : 2330
#   language     : en
#   origin       : Reuters Corpus Volume 1
#   publisher    : Reuters Holdings Plc
#   topics       : c("C15", "C152", "C18", "C181", "CCAT")
#   industries   : I34420
#   countries    : USA

data(iris)
op <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
with(iris, plot(Sepal.Length, Species))
with(iris, plot(Species, Sepal.Length)) # Huh? The plot type changes
par(op)
