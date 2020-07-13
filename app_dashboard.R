library(tidyverse)
library(caret)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(DT)

col <- c("light-blue", "blue", "purple")
#c("#FAEFD1", "#DC863B", "#C93312")

ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(
    title = 'Accuracy Report'
  ),
  dashboardSidebar(
    sliderInput('threshhold', 'Cutoff Threshold', min = 0, max = 1, value = 0.5),
    
    #checkboxInput("localChek", "Upload my evaluation results", value = FALSE),
    materialSwitch(
      inputId = "localChek",
      label = "Upload my evaluation results",
      value = FALSE,
      status = "default"
    ),
    conditionalPanel(
      "input.localChek == true",
      fileInput(
        "file1",
        "Upload my file",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      )
    )
    
    
  ),
  dashboardBody(
    # valueBoxes
    fluidRow(
      valueBox(
        uiOutput("sensitivity"), 
        subtitle = tags$p("Sensitivity", style = "font-size: 150%;"),
        icon = icon("users"), color = col[1] 
      ),
      valueBox(
        uiOutput("specificity"), 
        subtitle = tags$p("Specificity", style = "font-size: 150%;"),
        icon = icon("line-chart"), color = col[2]
      ),
      valueBox(
        uiOutput("accuracy"), 
        subtitle = tags$p("Accuracy", style = "font-size: 150%;"),
        icon = icon("gear"), color = col[3] 
      )
    ),
    
    # Boxes
    fluidRow(
      
      column(
        width = 6,
        box(title = 'Confusion Matrix',
            status = 'info',
            width = 12,
            verbatimTextOutput('c_matrix')
        ),
        
        box(title = 'View',
            status = 'info',
            width = 12,
            DT::dataTableOutput("tbl1")
        )
      ),
      column(
        width = 6,
        box(title = 'References',
            status = 'info',
            width = 12,
            height = '1000px',
            a(href="https://en.wikipedia.org/wiki/Sensitivity_and_specificity",
              "Confusion Matrix on Wikipedia"),
            imageOutput("myImage")
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    
    dat = read_csv(file.path('data', 'results.csv'))
    
    if(input$localChek & !is.null(input$file1)) {
      dat = read_csv(input$file1$datapath)
    } 
    dat
  })

  
  output$tbl1 <- DT::renderDT(
    rename(data(), 'Customer ID' = X1)
  )
  
  data_prep <- reactive({
    # turn probs into classes
    data() %>% 
      mutate(p_class = ifelse(pred > input$threshhold, 1, 0)) %>% 
      mutate(actual = as.factor(actual)) %>% 
      mutate(p_class = as.factor(p_class))
  })
  
  data_c_matrix <- reactive({
    dat = data_prep()
    # confusion matrix
    confusionMatrix(dat$p_class, dat$actual,positive = '1')
  })
  
  output$c_matrix <- renderPrint({
    data_c_matrix()
  })
  
  output$c_matrix_ys <- renderTable({
    # raw confusion matrix 
    knitr::kable(data_c_matrix()$table)
  })
  
  
  output$sensitivity <- renderText({
    prettyNum(as.numeric(data_c_matrix()$byClass[1]), big.mark=",")
  })
  
  output$specificity <- renderText({
    prettyNum(as.numeric(data_c_matrix()$byClass[2]), big.mark=",")
  })
  
  output$accuracy <- renderText({
    prettyNum(as.numeric(data_c_matrix()$overall[1]), big.mark=",")
  })
  
  output$myImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('figs', 'tp-fp.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  
}

shinyApp(ui, server)
