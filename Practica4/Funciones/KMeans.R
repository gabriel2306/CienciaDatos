KMeans <- function(matrizCentroides, matrizMuestras) {
    if (class(matrizCentroides)=="matrix" && class(matrizMuestras)=="matrix") {

    } else {

    }
}

calcularMatrizDistancias <- function (matrizCentroides, matrizMuestras,dimensiones) {
    sizeCentroides <- length(matrizCentroides)/dimensiones
    sizeMuestras <- length(matrizMuestras)/dimensiones
    x<-c()

    for (i in 1:sizeCentroides) {
        for (j in 1:sizeMuestras)
            x<-c(x,distanciaEuclidea(matrizCentroides[i,],matrizMuestras[j,]))
    }

    return (matrix(x,nrow=sizeCentroides,ncol=sizeMuestras,byrow=T))
}

distanciaEuclidea <- function (a,b) {
    dimensiones <- length(a)
    sum <- 0
    for(i in 1:dimensiones){
        sum<-sum+(a[i]-b[i])^2
    }

    return(sqrt(sum))
}