#' Genes in Cluster
#'
#' This function takes in the 1. cluster number you are interested 
#' 2. data.val2 and 3. annotation file and tells you how many genes are 
#' in the cluster of a SOM cluster.
#' 
#' @param clustNum The number cluster you want to investigate
#' @export

genesInClust <- function(clustNum, data.val2, annotation) {
  sub_cluster <- subset(data.val2, som$unit.classif == clustNum)
  sub_data <- as.data.frame(sub_cluster[,1])
  colnames(sub_data) <- "ITAG"
  resultsTable <- merge(sub_data,annotation,by = "ITAG", all.x  = TRUE)
  print(nrow(unique(resultsTable)))
  return(unique(resultsTable))
}

#' Genes in Cluster Super SOM 
#'
#' This function takes in the 1. cluster number you are interested 
#' 2. data.val2 and 3. annotation file and tells you how many genes are 
#' in the cluster of a SOM cluster.
#' 
#' @param clustNum The number cluster you want to investigate
#' @export

genesInClust_ssom <- function(clustNum) {
  sub_cluster <- subset(data.val2, ssom.unit.classif == clustNum)
  sub_data <- as.data.frame(sub_cluster[,2])
  colnames(sub_data) <- "ITAG"
  resultsTable <- merge(sub_data, annotation, by = "ITAG", all.x = TRUE)
  print(nrow(unique(resultsTable)))
  return(unique(resultsTable))
}


