anomalosKVecinos <- function (data, k, grado) {
    tmp<-""

    for(i in 1:5){
        data[,i] = sort(data[,i])
        if(data[k+1,i] > grado) {
            tmp<-paste(tmp,"La muestra ", i, " es outlier - ",sep="")
        }
    }

    return(tmp)
}