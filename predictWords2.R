## Ideas to construct search function with data.table

predictWord2<-function(input){
    input<-tokenize(input) #Clean the input
    #input<-wordStem(input, language ="english")
    len<-length(input)
    
    if (len > 3){
        i<-1
        while(len-i!=3)i<-i+1
        input<-input[-(1:i)]
        pred<-quadrigramDT[W1==input[1]][W2==input[2]][W3==input[3]][1:5]$W4
        if( length(pred)<3 | is.na(pred)[1] ){
            input<-input[2:3]
            pred<- trigramDT[W1==input[1]][W2==input[2]][1:5]$W3
            if( length(pred)<3 | is.na(pred)[1] ){
                input<-input[3]
                pred<-bigramDT[W1==input[1]][1:5]$W2
                if( length(pred)<3 | is.na(pred)[1] ) {
                    pred<-unigramDT[1:5]$W1}
            }}
    } else if (len==3){
        pred<- pred<-quadrigramDT[W1==input[1]][W2==input[2]][W3==input[3]][1:5]$W4
        if( length(pred)<3 | is.na(pred)[1] ){
            input<-input[2:3]
            pred<- trigramDT[W1==input[1]][W2==input[2]][1:5]$W3
            if( length(pred)<3 | is.na(pred)[1] ){
                input<-input[3]
                pred<-bigramDT[W1==input[1]][1:5]$W2
                if( length(pred)<3 | is.na(pred)[1] ) {
                    pred<-unigramDT[1:5]$W1}
            }}
    } else if (len==2){
        pred<- trigramDT[W1==input[1]][W2==input[2]][1:5]$W3
        if( length(pred)<3 | is.na(pred)[1] ){
            input<-input[2]
            pred<-bigramDT[W1==input[1]][1:5]$W2
            if( length(pred)<3 | is.na(pred)[1] ) {
                pred<-unigramDT[1:5]$W1}
        }
    } else if (len==1){
        pred<-bigramDT[W1==input[1]][1:5]$W2
        if( length(pred)<3 | is.na(pred)[1] ) {
            pred<-unigramDT[1:5]$W1}
    }
    predictions<-c(pred[1],pred[2],pred[3],pred[4])

}
