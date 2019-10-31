plotDisp <- function(data, n, m, regresion, xl, yl, ruta) {

    png(paste("./tmp/",ruta,sep=""))

    plot(data[,n], data[,m], xlab=xl, ylab=yl,main="Diagrama de Dispersion")
    abline(regresion, col="blue")

    dev.off()
}