anomalosKVecinos <- function (data, muestra, k, grado) {
    tmp<-""

    for(i in 1:length(data[1,])){
        data[,i] = sort(data[,i])
        if(data[k+1,i] > grado) {
            valor<-paste(muestra[k+1,1],muestra[k+1,2])
            tmp<-paste(tmp,"La muestra ", i, 
                " con valor (", valor, ") es outlier - ",sep="")
        }
    }

    return(tmp)
}