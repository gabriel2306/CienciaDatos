cuantil <- function(data, cuant) {

    data<-sort(data)
    
    np<-length(data)*cuant
    
    if(np%%1==0){
        xp<-(data[np]+data[np+1])/2
    } else {
        xp<-data[(np-np%%1)+1]
    }

    return(xp)
}