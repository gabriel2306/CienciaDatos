KMeans <- function(matrizMuestras, matrizCentroides) {
    centroidesFinales<-c()

    if (class(matrizCentroides)=="matrix" && class(matrizMuestras)=="matrix") {
        dimensiones <- length(matrizMuestras[1,])
        centroidesFinales<-ejecutarKMeans(matrizMuestras, matrizCentroides, dimensiones)
    }

    return(centroidesFinales)
}

ejecutarKMeans <- function (matrizMuestras,matrizCentroides,dimensiones) {
    iguales <- TRUE
    centros <- matrizCentroides
    matrizP1 <- c()
    i<-1
    nCentroides<-length(matrizCentroides)/dimensiones

    while(iguales){
        matrizD<-calcularMatrizDistancias(matrizMuestras, centros, dimensiones)
        matrizP2<-calcularMatrizPertenencia(matrizD,nCentroides)
        muestrasEnCluster<-muestrasPorCluster(matrizP2,nCentroides)
        muestrasSeparadas<-obtenerMuestrasSeparadas(matrizMuestras,muestrasEnCluster,dimensiones)
        centros<-obtenerNuevosCentroides(muestrasSeparadas,dimensiones) 
        if (i==1){
            matrizP1<-matrizP2
        }
        iguales<-comprobarMatrizPertenencia(matrizP1,matrizP2)
        matrizP1<-matrizP2
    }

    return(centros)
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

obtenerMuestrasSeparadas <- function (matrizMuestras, muestrasPorCluster, dimensiones) {
    muestras<-t(matrizMuestras)
    sizeMuestras <- length(matrizMuestras)/dimensiones
    separadas<-list()

    for (i in 1:length(muestrasPorCluster)){
        nMuestrasCluster<-length(muestrasPorCluster[[i]])
        temp<-c()
        for (j in 1:sizeMuestras) {
            if (j %in% muestrasPorCluster[[i]]) {
                temp<-c(temp, muestras[,j])
            }
        }
        separadas[[i]]<-matrix(temp,nrow=dimensiones,ncol=nMuestrasCluster)
    }

    return(separadas)
}

obtenerNuevosCentroides <- function (muestrasSeparadas, dimensiones) {
    sizeMuestras <- length(muestrasSeparadas)
    centros<-c()

    for (i in 1:sizeMuestras){
        matriz<-muestrasSeparadas[[i]]
        centros<-c(centros,nuevoCentroide(t(matriz),dimensiones))
    }

    return(matrix(centros,nrow=sizeMuestras,ncol=dimensiones,byrow=T))
}

nuevoCentroide <- function (matrizMuestras, dimensiones) {
    centro<-c()

    for (i in 1:dimensiones){
        suma<-0
        columna <- matrizMuestras[,i]
        media <- mean(columna)
        centro<-c(centro, media)
    }

    return(matrix(centro,nrow=1,ncol=dimensiones))
}