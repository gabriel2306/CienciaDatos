KMeans <- function(matrizCentroides, matrizMuestras) {

    dimensiones <- length(matrizMuestras[1,])

    if (class(matrizCentroides)=="matrix" && class(matrizMuestras)=="matrix") {

    } else {

    }
}

calcularMatrizDistancias <- function (matrizMuestras, matrizCentroides,dimensiones) {
    sizeCentroides <- length(matrizCentroides)/dimensiones
    sizeMuestras <- length(matrizMuestras)/dimensiones
    x<-c()

    for (i in 1:sizeCentroides) {
        for (j in 1:sizeMuestras)
            x<-c(x,distanciaEuclidea(matrizCentroides[i,],matrizMuestras[j,]))
    }

    m<-matrix(x,nrow=sizeCentroides,ncol=sizeMuestras,byrow=T)

    return(m)
}

calcularMatrizPertenencia <- function (matrizDistancias, nCentroides) {
    sizeDistancias <- length(matrizDistancias)/nCentroides
    x<-c()

    for (i in 1:sizeDistancias) {
        posMinimo <- filaMinimo(matrizDistancias[,i])
        for (j in 1:nCentroides)
            if (j==posMinimo){
                x<-c(x,1)
            } else {
                x<-c(x,0)
            }
    }

    m<-matrix(x,nrow=nCentroides,ncol=sizeDistancias)

    return(m)
}

comprobarMatrizPertenencia <- function (matriz1, matriz2) {
    iguales<-TRUE
    i<-1

    while(iguales && i<=length(matriz1)){
        if (matriz1[i]==matriz2[i]){
            i<-i+1
        } else {
            iguales<-FALSE
        }
    }

    return(iguales)
}

filaMinimo <- function (columnaDistancias) {
    sizeFilas <- length(columnaDistancias)

    pos<-1
    menor<-columnaDistancias[1]

    for (i in 1:sizeFilas) {
        if (columnaDistancias[i] < menor) {
            menor<-columnaDistancias[i]
            pos<-i
        }
    }

    return(pos)
}

muestrasPorCluster <- function (matrizPertenencia, nCentroides) {
    sizeDistancias <- length(matrizPertenencia)/nCentroides
    finalList<-c()
    for (i in 1:nCentroides){
        finalList<-c(finalList, list(muestrasPorFila(matrizPertenencia[i,])))
    }
    
    m<-matrix(finalList,nrow=nCentroides,ncol=sizeDistancias)
    return(finalList)
}

muestrasPorFila <- function (filaPertenencia) {
    sizeFila <- length(filaPertenencia)
    muestras<-c()
    
    for (i in 1:sizeFila){
        if (filaPertenencia[i]==1){
            muestras<-c(muestras,i)
        }
    }

    return(muestras)
}

distanciaEuclidea <- function (a,b) {
    dimensiones <- length(a)
    sum <- 0
    for(i in 1:dimensiones){
        sum<-sum+(a[i]-b[i])^2
    }

    return(sqrt(sum))
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

datosPorCluster <- function (matrizMuestras, muestrasPorCluster, dimensiones) {
    sizeMuestras <- length(matrizMuestras)/dimensiones
    colocacion<-c()

    for (i in 1:dimensiones) {
        mismoCluster<-c()
        for (j in 1:sizeMuestras) {
            if (j %in% muestrasPorCluster[i]) {
                mismoCluster <- c(mismoCluster, list(matrizMuestras[j,]))
            }
        }
        colocacion<-c(colocacion,list(mismoCluster))
    }

    return(colocacion)
}