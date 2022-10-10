#histogram for time in hh:mm:ss ----
hist_timeDistribution <- function(data, 
                           minutes, 
                           mytime = NULL, 
                           gender,
                           seperateG,
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
  
  #handle the mytime input ---
  #color the bar with the included "my time"
  #but check whether my time is provided and
  #if the value is valid
  res <- try(as_hms(mytime), silent = TRUE)
  if(any(class(res) == "try-error")){
    markmycenter <- NULL
  }else{
    if(!is.null(mytime)){
      mytime <- as_hms(mytime)
      #minutes_hms <- as_hms(minutes*60)
      #center <- round_hms(mytime, secs = minutes*60)
      #center_low <-  as_hms(center-as_hms(minutes_hms/2))
      #center_high <- as_hms(center+as_hms(minutes_hms/2))
      #mycenter <- data[data$Zeit.Netto > center_low & data$Zeit.Netto < center_high,]
      #markmycenter <- geom_histogram(data = mycenter, aes(x = Zeit.Netto), 
      #                               binwidth = minutes*60, 
      #                               color = "black", 
      #                               fill = "red")
      markmycenter <- geom_vline(xintercept = mytime, linetype ="dashed", 
                                 color = "black", size = 0.5)
      
    }else{
      markmycenter <- NULL
    }
  }
  
  #set the breaks for the x axis ---
  smartx <- c(1,5,seq(15,60, by = 15))
  if(minutes %in% smartx){
    scaleX <- scale_x_time(breaks = breaks_width("15 min"))
  }else{
    scaleX <- scale_x_time()
  }
  
  if(seperateG == FALSE){
    
    ggplot(data, aes(x = Zeit.Netto)) + 
      geom_histogram(fill="lightblue", 
                     binwidth = minutes*60,  # 15 min *60 sec/min
                     color="black") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 65, hjust = 1),
            legend.position = "none"
      ) +
      scale_y_continuous(expand = c(0,0)) +
      scaleX +
      markmycenter
    
    
  }else{
    
    data$G <- factor(data$G, levels = c("M", "F", "D"))
    
    Gcolors <- c("#8795E8", "#FF6AD5", "lightblue")
    Gcolors <- setNames(Gcolors, c("M", "F", "D"))
    
    ggplot(data, aes(x = Zeit.Netto, fill = G)) + 
      geom_histogram(binwidth = minutes*60,
                     color="black",
                     alpha = 0.5,
                     position = "identity") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 65, hjust = 1),
            legend.position = "none"
      ) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(values = Gcolors) +
      scaleX +
      markmycenter #draws the bar that includes the 'my time'
    
  }
}


#histogram for pace ----
hist_paceDistribution <- function(data, 
                                 minutes, 
                                 mytime = NULL, 
                                 gender,
                                 seperateG,
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
  
  #handle the mytime input ---
  #color the bar with the included "my time"
  #but check whether my time is provided and
  #if the value is valid
  res <- try(as_hms(mytime), silent = TRUE)
  if(any(class(res) == "try-error")){
    markmycenter <- NULL
  }else{
    if(!is.null(mytime)){
      mytime <- as_hms(mytime)
      #minutes_hms <- as_hms(minutes*60)
      #center <- round_hms(mytime, secs = minutes*60)
      #center_low <-  as_hms(center-as_hms(minutes_hms/2))
      #center_high <- as_hms(center+as_hms(minutes_hms/2))
      #mycenter <- data[data$Zeit.Netto > center_low & data$Zeit.Netto < center_high,]
      #markmycenter <- geom_histogram(data = mycenter, aes(x = Zeit.Netto), 
      #                               binwidth = minutes*60, 
      #                               color = "black", 
      #                               fill = "red")
      markmycenter <- geom_vline(xintercept = paceCalculator(42.195, mytime, "km"), linetype ="dashed", 
                                 color = "black", size = 0.5)
      
    }else{
      markmycenter <- NULL
    }
  }
  
  #set the breaks for the x axis ---
  smartx <- c(1,5,seq(15,60, by = 15))
  if(minutes %in% smartx){
    scaleX <- scale_x_time(breaks = breaks_width("15 sec"))
  }else{
    scaleX <- scale_x_time()
  }
  
  if(seperateG == FALSE){
    
    ggplot(data, aes(x = Pace)) + 
      geom_histogram(fill="lightblue", 
                     binwidth = minutes,  # 15 min *60 sec/min
                     color="black") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 65, hjust = 1),
            legend.position = "none"
      ) +
      scale_y_continuous(expand = c(0,0)) +
      scaleX +
      markmycenter
    
    
  }else{
    
    data$G <- factor(data$G, levels = c("M", "F", "D"))
    
    Gcolors <- c("#8795E8", "#FF6AD5", "lightblue")
    Gcolors <- setNames(Gcolors, c("M", "F", "D"))
    
    ggplot(data, aes(x = Pace, fill = G)) + 
      geom_histogram(binwidth = minutes,
                     color="black",
                     alpha = 0.5,
                     position = "identity") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 65, hjust = 1),
            legend.position = "none"
      ) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(values = Gcolors) +
      scaleX +
      markmycenter #draws the bar that includes the 'my time'
    
  }
}