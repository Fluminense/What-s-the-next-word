###
##Using data.table##
#
library(data.table)
library(stringr)

#set the data.tables
unigramDT <- data.table(W1=names(freq), counts=freq)
bigramDT <- data.table(words=names(freqBigram), counts=freqBigram)
trigramDT <- data.table(words=names(freqTrigram),counts=freqTrigram)
quadrigramDT <- data.table(words=names(freqQuadrigram),counts=freqQuadrigram)

# Transforming the freqBigram  into a data.table with two columns
split2 <- str_split(bigramDT$words," ",n=2)
bigramW1<-sapply(split2, "[", 1)
bigramW2<-sapply(split2,"[",2)
bigramDT[,c("W1","W2"):=list(bigramW1,bigramW2)]
rm(bigramW1,bigramW2,split2)

setcolorder(bigramDT, c("W1", "W2", "counts","words"))
bigramDT[,words:=NULL]

# Same to trigrams
split3 <- str_split(trigramDT$words," ",n=3)
trigramW1<-sapply(split3, "[", 1)
trigramW2<-sapply(split3,"[",2)
trigramW3<-sapply(split3,"[",3)
trigramDT[,c("W1","W2","W3"):=list(trigramW1,trigramW2,trigramW3)]
rm(trigramW1,trigramW2,trigramW3,split3)

setcolorder(trigramDT, c("W1", "W2","W3", "counts","words"))
trigramDT[,words:=NULL]

#last, quadrigrams
split4 <- str_split(quadrigramDT$words," ",n=4)
quadrigramW1<-sapply(split4, "[", 1)
quadrigramW2<-sapply(split4,"[",2)
quadrigramW3<-sapply(split4,"[",3)
quadrigramW4<-sapply(split4,"[",4)
quadrigramDT[,c("W1","W2","W3","W4"):=list(quadrigramW1,quadrigramW2,quadrigramW3,quadrigramW4)]
rm(quadrigramW1,quadrigramW2,quadrigramW3,quadrigramW4,split4)

setcolorder(quadrigramDT, c("W1","W2","W3","W4", "counts","words"))
quadrigramDT[,words:=NULL]


save(quadrigramDT,
     file = "C://Users/rodrigoap/Documents/Rodrigo Backup/Coursera/Capstone Project/CapstoneShiny/data/quadrigramDT.rdata")

save(trigramDT,
     file = "C://Users/rodrigoap/Documents/Rodrigo Backup/Coursera/Capstone Project/CapstoneShiny/data/trigramDT.rdata")

save(bigramDT,
     file = "C://Users/rodrigoap/Documents/Rodrigo Backup/Coursera/Capstone Project/CapstoneShiny/data/bigramDT.rdata")

save(unigramDT,
     file = "C://Users/rodrigoap/Documents/Rodrigo Backup/Coursera/Capstone Project/CapstoneShiny/data/unigramDT.rdata")
##


format(object.size(list(unigramDT,bigramDT,trigramDT,quadrigramDT)),"Mb")
