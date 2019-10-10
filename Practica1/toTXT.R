toTxt <- function(save,name) {
    write.table(save, paste("./",name), sep="\t", row.names=F) 
}