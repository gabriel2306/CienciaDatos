leerM <- function(ruta) {

    data<-read.table(ruta,header=TRUE)
    mat<-as.matrix(data)
    sz<-dim(mat)
    l<-c()

    for (i in 1:sz[1]) {
        for (j in 1:sz[2]){
            l<-c(l,mat[i,j])
        }
    }

    return(l)
}