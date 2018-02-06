#' Genes in Cluster
#'
#' This function takes in the 1. cluster number you are interested 
#' 2. data.val2 and 3. annotation file and tells you how many genes are 
#' in the cluster of a SOM cluster.
#' 
#' @param clustNum infile Path to the input file
#' @export

clusterGO <- function(clustNum){
  ##Sets up plot
  dev.off()
  plot.new()
  
  #sub_cluster
  sub_cluster <- subset(data.val2, som$unit.classif == clustNum)
  
  itag.sc <- as.data.frame(sub_cluster$gene) 
  colnames(itag.sc)[1] <- "itag"
  itag.sc$sc <- 1    
  
  #Since each orthologue between tf2 and wt are represented twice in this set, we have to keep only the unique ITAGs.
  
  itag.sc <- unique(itag.sc) #Check. Should cut the list in half. # dim(itag.sc) before and after
  
  #Merge all by itag
  matrixGO <- merge(itag.sc, geneLength, by = "itag", all = TRUE)
  matrixGO[is.na(matrixGO)] <- 0
  pat <- matrixGO
  
  #Now that we have the data in the right format, we can proceed with GO enrichment.
  
  genes = as.integer(pat[,"sc"])
  names(genes) = pat$itag
  table(genes)
  length(genes)
  
  pwf = nullp(genes,bias.data = pat$length)
  
  GO.wall = goseq(pwf,gene2cat = cate)
  head(GO.wall)
  
  #This is going to correct for multiple testing.  You can specify the p-value cut-off of GO categories you are interested.
  
  enriched.GO = GO.wall$category[p.adjust(GO.wall$over_represented_pvalue, method = "BH") < 0.05]
  
  enriched.GO
  
  my.GO <- as.character(enriched.GO)
  my.GO.table <- Term(my.GO)
  my.GO.table
  t <- as.matrix(my.GO.table)
  
  print(t) #prints only GO categories that are significant
}
