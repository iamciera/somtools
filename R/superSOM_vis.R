## For superSOMs

#' Cluster Visualization by region for superSOM
#' 
#' Seperated by regions, tiussue and colored by genotype. 
#' @export

clusterVis_region_ssom <- function(clustNum){
  sub_cluster <- subset(data.val2, ssom.unit.classif == clustNum)
  sub_data <- sub_cluster[,c(1, 9:14)] # just the sample types
  m.data <- melt(sub_data)
  m.data$region <- ifelse(grepl("A", m.data$variable), "tip", 
                          ifelse(grepl("B", m.data$variable), "middle", "base"))
  
  m.data$tissue <- ifelse(grepl("other", m.data$variable, ignore.case = T), "rachis", 
                          ifelse(grepl("mbr", m.data$variable, ignore.case = T), "margin", "NA"))
  
  p <- ggplot(m.data, aes(y = value, x = region, color = genotype))
  p + geom_point(alpha = 0.5,
                 position = "jitter", 
                 size = 1) + 
    theme_bw() + 
    scale_colour_manual(values = c( "#d8b365","#5ab4ac")) +
    geom_boxplot(alpha = 0.70, outlier.size = 0) +
    theme(legend.text = element_text(size = 20), 
          text = element_text(size = 20)) + 
    facet_grid(tissue~.)
}

#' Cluster Visualization Line for superSOM
#' 
#' Line plot for superSOMs. Not really appropriate, but looks cool. 
#' 
#' @export

clusterVis_line_ssom <- function(clustNum) {
  sub_cluster <- subset(data.val2, ssom.unit.classif == clustNum)
  sub_data <- sub_cluster[,c(1,2,9:14)] # just the sample types
  sub_data <- melt(sub_data)
  sub_data <- within(sub_data, lineGroup <- paste(gene, genotype, sep = '.'))
  ggplot(sub_data, aes(variable, value, group = lineGroup, color = genotype)) + 
    geom_line(alpha = .1) + 
    geom_point(alpha = .0) +
    theme_bw() +
    scale_color_manual(values = c("#d8b365", "#5ab4ac"
    )) +
    theme(text = element_text(size = 20))
}

#' Cluster Visualization by Genotype
#' 
#' @param clustNum cluster number you are interested in
#' @export

clusterVis_geno <- function(clustNum){
  
  sub_cluster <- subset(plot.data, som.unit.classif == clustNum)
  
  sub_data <- sub_cluster[,c(1,9:14)] # just the sample types
  names(sub_data)
  m.data <- melt(sub_data) 
  
  m.data$genotype <- as.factor(m.data$genotype)
  
  m.data$region <- ifelse(grepl("A", m.data$variable, ignore.case = T), "A.tip", 
                          ifelse(grepl("B", m.data$variable, ignore.case = T), "B.middle", "C.base"))
  #m.data$tissue <- ifelse(grepl("other", m.data$variable, ignore.case = T), "rachis", 
  #ifelse(grepl("mbr", m.data$variable, ignore.case = T), "margin", "base"))
  
  p <- ggplot(m.data, aes(x = variable, y=value, color = genotype))
  p + geom_point(alpha = 0.5, position = "jitter", size = 1) + 
    geom_boxplot(alpha = 0.75, outlier.size = 0) + 
    theme_bw() + 
    scale_colour_manual(values = c("#ef8a62", "#67a9cf"))  + 
    facet_grid(region ~ .)
}