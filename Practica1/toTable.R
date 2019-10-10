toTable <- function(data1,data2) {

    v<-c(data1,data2)

    m<-matrix(v,2,length(data1),byrow=T)

    return(m)
}