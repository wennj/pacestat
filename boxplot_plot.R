#boxplot for AK


time_by_AK_boxplot <- function(data, 
                               #minutes, 
                               #mytime = NULL, 
                               gender,
                               #seperateG,
                               AK){
  
  #selected age class (AK)
  #AK <- c(30,80)
  if(length(AK) == 1){
    ageclass <- c(paste("W", AK, sep = ""), paste("M", AK, sep = ""))
  }else{
    agerange <- seq(AK[1], AK[2], by = 5)
    Wages <- paste("W", agerange, sep = "")
    Mages <- paste("M", agerange, sep = "")
    agerange <- c(Wages, Mages)
  }
  agerange <- gsub("20", "", agerange)
  agerange <- gsub("25", "", agerange)
  agerange <- unique(agerange)
  data <- data[data$AK %in% agerange,]
  
  #filter by selected gender
  data <- data[data$G %in% gender,]
  
  #colors for genders
  Gcolors <- c("#8795E8", "#FF6AD5", "lightblue")
  Gcolors <- setNames(Gcolors, c("M", "F", "D"))
  
  ggplot(data, aes(x = AK, y = as_hms(Zeit.Netto), fill = G)) + 
    geom_boxplot(outlier.colour="red", 
                 outlier.shape = 1,
                 outlier.size = 2,
                 alpha = 0.5) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 65, hjust = 1),
          legend.position = "none"
    ) +
    scale_fill_manual(values = Gcolors)
  
  
}