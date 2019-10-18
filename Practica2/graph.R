graph <- function(rules, ruta) {

    install.packages("arulesViz")
    library(arulesViz)

    png(paste("./tmp/",ruta,sep=""))

    plot(rules,method="graph",control=list(type="items"))

    dev.off()
}