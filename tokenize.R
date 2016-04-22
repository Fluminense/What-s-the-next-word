tokenize<-function(x){
    x <- tolower(x)                    #transform all the word to lowercase
    x <- gsub("[^a-z\n\']", " ", x)    #keep only the alphabetic characters
    x <- unlist(strsplit(x," "))       #separate string into words 
    x <- grep("\\S",x,value = T)       #remove any white spaces
    x <- removePunctuation(x)
    return(x)}