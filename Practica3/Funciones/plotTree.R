plotTree <- function(tree, ruta) {

    png(paste("./tmp/",ruta,sep=""))

    rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

    dev.off()
}