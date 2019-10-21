graph <- function(rules, ruta) {

    png(paste("./tmp/",ruta,sep=""))

    plot(rules,method="graph",control=list(type="items"))

    dev.off()
}