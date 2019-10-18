graph2 <- function(rules, ruta) {

    install.packages("arulesViz")
    library(arulesViz)

    png(paste("./tmp/",ruta,sep=""))

    plot(rules,method="paracoord",control=list(reorder=TRUE))

    dev.off()
}