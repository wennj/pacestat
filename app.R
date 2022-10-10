#https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/

#advanced applciation layout guide
#https://shiny.rstudio.com/articles/layout-guide.html

#Customize your UI with HTML
#https://shiny.rstudio.com/articles/html-tags.html

#Shiny HTML Tags Glossary
#https://shiny.rstudio.com/articles/tag-glossary.html

#Shiny Widgets Gallery
#https://shiny.rstudio.com/gallery/widget-gallery.html

#leaflet #https://rstudio.github.io/leaflet/basemaps.html

library(scales)
library(shiny)
library(leaflet)
library(shinyTime)
library(ggplot2)
library(hms)
source("histogram_plot.R")
source("boxplot_plot.R")
source("pace_calculator.R")

df <- read.csv(file = "data/VMM2022.csv", sep = ";")
df$Zeit.Netto <- as_hms(df$Zeit.Netto)
df$Pace <- paceCalculator(42.195, df$Zeit.Netto, "km")

#Define UI ----
ui <- fluidPage(
  titlePanel("Analyse der Nettozielzeit"),
  
  sidebarLayout(
    sidebarPanel(
      #position = "right",
      helpText("Analyse der Nettozeiten basierend auf den
               Ergebnissen des ausgewählten Marathons."),
      
      selectInput("select", 
                  label = "Auswahl der Stadt", 
                  choices = list("Berlin" = 1, 
                                 "Frankfurt" = 2,
                                 "Köln" = 3,
                                 "München" = 4,
                                 "Münster" = 5
                                 ), 
                  selected = 5),
      
      selectInput("select", 
                  label = "Auswahl des Jahres", 
                  choices = list("2019" = 1, 
                                 "2020" = 2,
                                 "2021" = 3,
                                 "2022" = 4
                                 ), 
                  selected = 4),
      
      textInput("mytime", 
                label = "Meine Zeit (hh:mm:ss)", 
                value = "04:10:44"),
      
      sliderInput("range",
                  label = "Intervall in Minuten:",
                  min = 1, max = 60, value = 5),
      
      radioButtons("setTime", label = "Angabe der Zeit",
                   choices = list("Nettozeit" = 1, "Pace" = 2), 
                   selected = 1),
      
      checkboxGroupInput("checkGender", label = "Geschlecht", 
      choices = list("Frauen" = "F", 
                     "Männer" = "M"),
                     #"Divers" = "D"),
      selected = c("F", "M", "D")),
      
      checkboxInput("checkSeperateG", 
                    label = "Getrennt nach Geschlecht",
                    value = TRUE),
      
      sliderInput("ageClassRange", label = "Wahl der Altersklasse(n)", 
                  min = 20, 
                  max = 80,
                  step = 5,
                  value = c(20, 40))
      
      
  
      ),

    mainPanel(plotOutput("histTime"),
              plotOutput("boxAK")
              )
  )
)
  


# Define server logic ----
server <- function(input, output){
  
  #output$map <- renderLeaflet({
  #  leaflet() %>% addTiles()
  #})
  
  output$histTime <- renderPlot({
    
    if(req(input$setTime) == 1){
      hist_timeDistribution(df, 
                            minutes = input$range, 
                            mytime = input$mytime,
                            gender = input$checkGender,
                            seperateG = input$checkSeperateG,
                            AK <- input$ageClassRange)
    }else{
      
      hist_paceDistribution(df, 
                            minutes = input$range, 
                            mytime = input$mytime,
                            gender = input$checkGender,
                            seperateG = input$checkSeperateG,
                            AK <- input$ageClassRange)
      
      
    }
  })
  
  
  
  output$boxAK <- renderPlot({
      time_by_AK_boxplot(df,
                         AK <- input$ageClassRange,
                         gender = input$checkGender)
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
