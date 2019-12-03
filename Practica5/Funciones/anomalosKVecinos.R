anomalosKVecinos <- function (distancias, muestra, k, grado,dimensiones) {
    tmp<-""

    for(i in 1:(length(muestra)/dimensiones)){
        distancias[,i] = sort(distancias[,i])
        if(distancias[k+1,i] > grado) {
            valor<-""
            for (j in 1:dimensiones){
                valor<-paste(valor,muestra[k+1,j],sep=" ")
            }
            tmp<-paste(tmp,"La muestra ", i, 
                " con valor (", valor, " ) es outlier - ",sep="")
        }
    }

    return(tmp)
}