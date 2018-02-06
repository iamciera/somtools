#' Cluster Visualization
#' 
#' This function makes a ggpot point plot with boxplots overlayed
#'  based on all six tissue types of that cluster.
#'  
#' @param clustNum cluster number you are interested in
#' @export

clusterVis <- function(clustNum){
  sub_cluster <- subset(data.val2, som$unit.classif == clustNum)
  sub_data <- sub_cluster[,c(1, 8:13)] # just the sample types
  m.data <- melt(sub_data) 
  p <- ggplot(m.data, aes(x = variable, y = value))
  p + geom_point(alpha = 0.5,position = "jitter", size = 1) + 
    geom_boxplot(alpha = 0.75, outlier.size = 0) + 
    theme_bw() + 
    theme(text = element_text(size = 30),
          axis.text.x = element_text(angle = 90, 
                                     vjust = 1)) +
    xlab("Tissue Region") +
    ylab("Scaled Gene Expression")
}

#' Cluster Visualization Color
#' 
#' This function makes a ggplot point plot with boxplots overlayed
#'  based on all six tissue types of that cluster.
#'  
#'  Default palette colors are 
#'  lcmPaletteColors <- c( "#b3a2ce", "#4753a4","#bf9e71", "#956025","#b2d9a6", "#0f7c3e")
#'  to match manuscript.
#'  
#' @param clustNum cluster number you are interested in
#' @export

clusterVis_color <- function(clustNum){
  sub_cluster <- subset(data.val2, som$unit.classif == clustNum)
  sub_data <- sub_cluster[,c(1, 8:13)] # just the sample types
  m.data <- melt(sub_data) 
  p <- ggplot(m.data, aes(x = variable, y = value, color = variable))
  p + geom_point(alpha = 0.5,position = "jitter", size = 1) + 
    geom_boxplot(alpha = 0.75, outlier.size = 0) + 
    theme_bw(base_size = 28) + 
    scale_color_manual(values = lcmPaletteColors) + ## specified in colors.R
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 20),
          axis.ticks.x = element_blank(),
          legend.position = "none") +
    scale_y_continuous(breaks = c(0)) +
    xlab("") +
    ylab("")
}

#' Cluster Visualization by Region
#' 
#' This function first splits the libraries into tissue types (margin and rachis) 
#' and logitudinal axis (top, middle, base).  Then, it makes a box plot with 
#' all points seperated by regions and colored by tissue.
#'
#'  
#' @param clustNum cluster number you are interested in
#' @export

clusterVis_region <- function(clustNum){
  sub_cluster <- subset(data.val2, som$unit.classif == clustNum)
  sub_data <- sub_cluster[,c(1, 8:13)] # just the sample types
  m.data <- melt(sub_data)
  m.data$region <- ifelse(grepl("wta", m.data$variable, ignore.case = T), "top", 
                          ifelse(grepl("wtb", m.data$variable, ignore.case = T), "mid", "base"))
  
  m.data$tissue <- ifelse(grepl("other", m.data$variable, ignore.case = T), "rachis", 
                          ifelse(grepl("mbr", m.data$variable, ignore.case = T), "margin", "NA"))
  
  p <- ggplot(m.data, aes(y = value, x = tissue, color = tissue))
  p + geom_point(alpha = 0.5,position = "jitter", size = 1) + 
    geom_boxplot(alpha = 0.70, outlier.size = 0) +
    scale_colour_manual(values = tissueColors) +
    theme_bw(base_size = 25) + 
    facet_grid(region~.) +
    labs(x = "tissue", y = "scaled value") 
}


#' Cluster Visualization by Genotype
#' 
#' This function first splits the libraries into tissue types (margin and rachis) 
#' and logitudinal axis (top, middle, base).  Then, it makes a box plot with 
#' all points seperated by regions and colored by tissue.
#'
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

#' clusterVis_PCA
#' Highlights the cluster in the PCA map 
#' @export

clusterVis_PCA <- function(clustNum) {
  
  #make dataset for visualization
  data.val3 <- data.val2
  data.val3$cluster[data.val3[,20] == clustNum] <- "subcluster"
  data.val3$cluster[data.val3[,20] != clustNum] <- "other"
  
  #plot
  
  p <- ggplot(data.val3, aes(PC1, PC2, color = cluster)) 
  p + geom_point(size = I(2), alpha = 0.6) +
    scale_colour_manual(values = c("#cccccc", "#000000")) + 
    theme_bw() + 
    theme(legend.text = element_text(
      size = 30, 
      face = "bold"), 
      text = element_text(size = 30), 
      legend.position = "none")
}

#' clusterVis_line
#' @export 

clusterVis_line <- function(clustNum) {
  sub_cluster <- subset(data.val2, som$unit.classif == clustNum)
  sub_data <- sub_cluster[,c(1,8:13,22)] # just the sample types
  sub_data <- melt(sub_data)
  head(sub_data)
  sub_data <- within(sub_data, lineGroup <- paste(gene,sep = '.'))
  ggplot(sub_data, aes(variable, value, group = lineGroup)) + 
    geom_line(alpha = .1) + 
    geom_point(alpha = .0) +
    theme_bw() 
}
