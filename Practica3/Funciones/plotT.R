plotT <- function(t, ruta) {

    png(paste("./tmp/",ruta,sep=""))

    plot(t, y = NULL, type = c("proportional", "uniform"))
    text(t, all=TRUE, cex=.8)

    dev.off()
}