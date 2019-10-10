tche <- function(des,media,k) {
    
    rango<-des*k
    inter<-list(as.integer(media-rango),as.integer(media+rango))

    return(inter)
}