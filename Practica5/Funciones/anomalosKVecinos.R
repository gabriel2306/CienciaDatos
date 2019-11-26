anomalosKVecinos <- function (data, muestra, k, grado) {
    tmp<-""

    for(i in 1:5){
        data[,i] = sort(data[,i])
        if(data[k+1,i] > grado) {
            valor<-paste(muestra[4,1],muestra[4,2])
            tmp<-paste(tmp,"La muestra ", i, " con valor (", valor, ") es outlier - ",sep="")
        }
    }

    return(tmp)
}