library(shinythemes)
library(shiny)

# deprecated layout
ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    'Accuracy Reporter',
    tabPanel(
      'Main',
      sidebarPanel(
        width = 3,
        fileInput(
          "file1",
          "Upload my file",
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        actionButton("go", "Go!"),
        hr(),
        sliderInput('threshhold', 'Threshold', min = 0, max = 1, value = 0.5),
        hr(),
        a(href="https://en.wikipedia.org/wiki/Sensitivity_and_specificity", "Confusion Matrix")
      ),
      
      mainPanel(
        fluidRow(
          #column(6,
            column(4, h2(uiOutput("sensitivity"))),
            column(4, h2(uiOutput("specificity"))),
            column(4, h2(uiOutput('accuracy')))
            #verbatimTextOutput('c_matrix')
          #)#,
          #column(6, DT::DTOutput("tbl1"))
        )
      )
    ),
    tabPanel(
      'View',
      fluidRow(
        column(4, DT::DTOutput("tbl1"))
      )
      
      # uiOutput("sensitivity"),
      # uiOutput("specificity"),
      # verbatimTextOutput('c_matrix')
    )
  )
  
)

server <- function(input, output, session) {
  
  data <- eventReactive(input$go, {
    read_csv(input$file1$datapath)    
  })
  
  # output$tbl1 <- renderDataTable(
  #   data()
  # )
  
  output$tbl1 <- DT::renderDT(
    data(),
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      searchHighlight = TRUE
    )
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
    paste('Sensitivity:', prettyNum(as.numeric(data_c_matrix()$byClass[1]), big.mark=","))
  })
  
  output$specificity <- renderText({
    paste('Specificity:', prettyNum(as.numeric(data_c_matrix()$byClass[2]), big.mark=",")) 
  })
  
  output$accuracy <- renderText({
    paste('Accuracy:', prettyNum(as.numeric(data_c_matrix()$overall[1]), big.mark=","))
  })
  
}

shinyApp(ui, server)