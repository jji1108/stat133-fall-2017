
library(shiny)
library(ggvis)
library(dplyr)

source("../code/functions.R")
rawscore <- read.csv("../data/cleandata/cleanscores.csv")

#Convert variables into factors, set order of levels
rawscore$Grade <- factor(rawscore$Grade, levels = c("A+", "A", "A-",
                                                    "B+", "B", "B-",
                                                    "C+", "C", "C-",
                                                    "D", "F"))
#Creating a vector that allows for selection of different variables in the conditional panels
continuous <- c("HW1",
               "HW2",
               "HW3",
               "HW4",
               "HW5",
               "HW6",
               "HW7",
               "Hw8",
               "HW9",
               "ATT",
               "QZ1",
               "QZ2",
               "QZ3",
               "QZ4",
               "EX1",
               "EX2",
               "Homework",
               "Quiz",
               "Lab",
               "Test1",
               "Test2",
               "Overall")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Grade Visualizer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         conditionalPanel(condition = "input.tabselected == 1",
                          h3("Grade Distribution"),
                          tableOutput("freq_table")),
         conditionalPanel(condition = "input.tabselected == 2",
                          h3("Panel1"),
                          selectInput("var2", "X-axis Variable", continuous,
                                      selected = "HW1"),
                          sliderInput("bin","Bin Width",
                                      min = 1, max = 10, value = 1)),
         conditionalPanel(condition = "input.tabselected == 3",
                          selectInput("var3","X-axis Variable",continuous,
                                      selected = "Test1"),
                          selectInput("var4","Y-axis Variable",continuous,
                                      selected = "Overall"),
                          sliderInput("opacity", "Opacity",
                                      min = 0, max = 1, value = 0.1),
                          radioButtons("var5",label = h3("Show line"), 
                                       choices = list("none" = 1, 
                                                      "lm" = 2, 
                                                      "loess" = 3),
                                       selected = 1)
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Barchart", value = 1, 
                           ggvisOutput("barchart")),
                  tabPanel("Histogram", value = 2, 
                           ggvisOutput("histogram"),
                           h4("Summary statistics"),
                           verbatimTextOutput("summary_stats")),
                  tabPanel("Scatterplot", value = 3,
                           ggvisOutput("scatterplot"),
                           verbatimTextOutput("correlation")),
                  id = "tabselected")
    )
  )
)
      # Define server logic required to draw a histogram
 server <- function(input, output) {
  
  #make dataframe of grade distributions and render it onto the output
  Grade <- levels(rawscore$Grade)
  Freq <- as.vector(unname(table(rawscore$Grade)))
  Prop <- round(Freq/nrow(rawscore), digits = 2)
  freq_table <- data.frame(Grade, Freq, Prop)
  
  #attaching the grade distribution to the output 
  output$freq_table <- renderTable(freq_table)
  
  #creating the barchart element for display in the tab panel
   vis_barchart <- reactive ({
     var1 <- rawscore$Grade
     
     rawscore%>%
       ggvis(x = ~var1, fill = "blue")%>%
       layer_bars(fillOpacity := 0.8, fillOpacity.hover := 1)%>%
       add_axis("y",title = "frequency")
     
   })
   vis_barchart %>% bind_shiny("barchart")
   
  #creating the histogram for display in the tab panel 
  vis_histogram <- reactive ({
    var2 <- prop("x", as.symbol(input$var2))
      
    rawscore%>%
      ggvis(x = var2, fill := "red")%>%
      layer_histograms(stroke := 'white',
                       width = input$bin)
  })
   vis_histogram %>% bind_shiny("histogram")
   
   #attaching the printed output of `print_stats` to the output object
   output$summary_stats <- renderPrint(print_stats(rawscore[, input$var2]))
   
  #creating the scatterplot for display in the tab panel 
  vis_scatterplot <- reactive({
    var3 <- prop("x", as.symbol(input$var3))
    var4 <- prop("y", as.symbol(input$var4))

  if (input$var5 == 1){
    rawscore%>%
      ggvis(x = var3, y = var4, fill := "green")%>%
      layer_points(opacity:= input$opacity)
  }else if (input$var5 == 2){
    rawscore%>%
      ggvis(x = var3, y = var4, fill := "green")%>% 
      layer_points(opacity:= input$opacity)%>%
      layer_model_predictions(model = "lm")
    } else {
     rawscore%>%
      ggvis(x = var3, y = var4, fill := "green")%>% 
      layer_points(opacity:= input$opacity)%>%
      layer_smooths()
    }
})
  vis_scatterplot %>% bind_shiny("scatterplot")
  
  #Creating a coorelation output for displaying in the tab panel
  output$correlation <- renderPrint(cor((rawscore[, input$var3]), rawscore[ ,input$var4]))
 }
 
# Run the application 
shinyApp(ui = ui, server = server)
























