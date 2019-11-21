KMeans <- function(matrizMuestras, matrizCentroides) {
    centroidesFinales<-c()

    if (class(matrizCentroides)=="matrix" && class(matrizMuestras)=="matrix") {
        dimensiones <- length(matrizMuestras[1,])
        allData<-ejecutarKMeans(matrizMuestras, matrizCentroides, dimensiones)
        lista<-allData[[length(allData)]]
        centroidesFinales<-lista[[2]]

        if (dimensiones==2) {
            ejecutarRepresentar(matrizMuestras,matrizCentroides,allData)
        }
    }

    return(centroidesFinales)
}

ejecutarKMeans <- function (matrizMuestras,matrizCentroides,dimensiones) {
    iguales <- FALSE
    centros <- matrizCentroides
    nCentroides<-length(matrizCentroides)/dimensiones
    matrizP1 <- matrix(0,nrow=nCentroides,ncol=length(matrizMuestras)/dimensiones)

    data<-list()
    i<-1
    while(!iguales){
        matrizD<-calcularMatrizDistancias(matrizMuestras, centros, dimensiones)
        matrizP2<-calcularMatrizPertenencia(matrizD,nCentroides)
        muestrasEnCluster<-muestrasPorCluster(matrizP2,nCentroides)
        muestrasSeparadas<-obtenerMuestrasSeparadas(matrizMuestras,muestrasEnCluster,dimensiones)
        centros<-obtenerNuevosCentroides(muestrasSeparadas,dimensiones)
        iguales<-comprobarMatrizPertenencia(matrizP1,matrizP2)
        matrizP1<-matrizP2

        data[[i]]<-list(muestrasSeparadas,centros)
        i<-i+1
    }

    return(data)
}

ejecutarRepresentar <- function (matrizMuestras, matrizCentroides, data) {
    png(paste("./tmp/resultadoKMeans.png"))

    size<-length(data)
    
    if (size%%2 == 0) {
        div <- size/2 
    } else {
        div <- floor(size/2) + 1
    }

    par(mfrow = c(div, 2))

    representarInicial(matrizMuestras, matrizCentroides)

    for (i in 1:(size-1)) {
        info<-data[[i]]
        representar(info[[1]],matrizMuestras,info[[2]],i)
    }

    dev.off()
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

representarInicial <- function (matrizMuestras,matrizCentroides){
    colores <- c("red","blue","green","black","purple")

    titulo<-"Estado inicial"

    limites <- obtenerLimites(matrizMuestras)
    plot(matrizMuestras[,1],matrizMuestras[,2],pch=1,col="orange",
        xlim=limites$x,ylim=limites$y,main=titulo,xlab="X",ylab="Y")

    indiceColor <- 1

    nClusters<-length(matrizCentroides)/2
    for (i in 1:nClusters) {
        m<- matrizCentroides[i,]
        points(m[1],m[2],pch=8,col=colores[indiceColor])
        indiceColor <- indiceColor + 1
        if (indiceColor==5) {
            indiceColor <- 1
        }
    }
}

representar <- function(muestrasSeparadas,matrizMuestras,matrizCentroides,i){
    colores <- c("red","blue","green","black","purple")

    titulo<-paste("Iteracion ",i)

    m<-muestrasSeparadas[[1]]
    limites <- obtenerLimites(matrizMuestras)
    plot(m[1,],m[2,],pch=1,col=colores[1],xlim=limites$x,ylim=limites$y,
        main=titulo,xlab="X",ylab="Y")

    centroide<-matrizCentroides[1,]
    points(centroide[1],centroide[2],pch=8,col=colores[1])

    indiceColor <- 2

    nClusters<-length(muestrasSeparadas)
    for (i in 2:nClusters) {
        m<- muestrasSeparadas[[i]]
        points(m[1,],m[2,],pch=1,col=colores[i])

        centroide<-matrizCentroides[i,]
        points(centroide[1],centroide[2],pch=8,col=colores[indiceColor])

        indiceColor <- indiceColor + 1
        if (indiceColor==5) {
            indiceColor <- 1
        }
    }
}

obtenerLimites <- function(matrizMuestras){
    maximos <- c()
    minimos <- c()

    for(i in 1:2){
        maximos<-c(maximos,max(matrizMuestras[,i]))
        minimos<-c(minimos,min(matrizMuestras[,i]))
    }
    l<-list("x"=c(minimos[1],maximos[1]),"y"=c(minimos[2],maximos[2]))
    return(l)
}