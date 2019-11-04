library(shiny)
library(tidyverse)
library(visNetwork)
library(DT)
library(stringi)
library(shinyalert)
library(rsconnect)

# global

books <- list("dataset.csv")

instructorsName = 'Instructors'
facultyName = 'Faculty'

columnNamesDone = 1

# Define UI for data upload app ----install
ui <- fluidPage(
  
  # App title ----
  titlePanel("Visualizing Interaction Data"),
  
  # Info.md
  includeMarkdown("info.md"),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a test file --
      
      selectInput("selection", "Choose a dataset:",
                  choices = books),
      
      actionButton("update", "Change"),
      downloadButton("report", "Generate report"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
      # Horizontal line ----
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: table and network ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput("contents")),
                  tabPanel("Network", visNetworkOutput("network"))
      ),
      
      verbatimTextOutput("shiny_return"),
      
      # Button
      useShinyalert(),  # Set up shinyalert
      actionButton("preview", "Preview")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  rawData <- read.csv("dataset.csv")
  rv <- reactiveValues(rawData = rawData)

  observeEvent(input$selection, {
    if(input$selection != "") {
    updateSelectInput(session = session, inputId = "instructorsname", choices = colnames(read.csv(input$selection)))
    updateSelectInput(session = session, inputId = "facultyname", choices = colnames(read.csv(input$selection)))
    rv$rawData <<- read.csv(input$selection)
    }
    
  })
  
  observeEvent(input$file1, {
    if(input$file1$datapath != "") {
      updateSelectInput(session = session, inputId = "instructorsname", choices = colnames(read.csv(input$file1$datapath)))
      updateSelectInput(session = session, inputId = "facultyname", choices = colnames(read.csv(input$file1$datapath)))
      rv$rawData <<- read.csv(input$file1$datapath)
    }
    
  })
  
  instructorsName <- eventReactive(input$instructorsname, {
    return(input$instructorsname)
  })
  
  facultyName <- eventReactive(input$facultyname, {
    return(input$facultyname)
  })
  
  #rawData <- eventReactive(input$file1, {
  #  read.csv(input$file1$datapath)
  #})
  
  #rawData <- eventReactive(input$selection, {
  #  read.csv(input$selection)
  #})
  
  
   
  output$contents <- DT::renderDataTable({
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- rv$rawData
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(head(df))
    
  }, options = list( paging = 0, searching = 0, info = 0, ordering = 0 ))
  
  # network render 
  
  output$network <- renderVisNetwork({
    
    if(columnNamesDone == 1) {
    instructors <- rv$rawData %>%
      select(instructorsName()) %>%
      distinct() %>%
      rename(labelName = instructorsName())%>%
      mutate(label2 = "(L)")%>%
      unite("label", labelName:label2, sep=" ")
    
    instructors <- mutate(instructors, group = "Librarian")
    
    faculty <- rv$rawData %>%
      select(facultyName()) %>%
      distinct() %>%
      rename(labelName = facultyName())%>%
      mutate(label2 = "(F)")%>%
      unite("label", labelName:label2, sep=" ")
    
    faculty <- mutate(faculty, group = "Faculty")
    
    nodes <- full_join(instructors, faculty)
    
    nodes <- nodes %>%
      rowid_to_column("id")
    
    nodes <- mutate(nodes, title = label)
    
    per_act <- full_join(instructors,faculty)
    
    per_act <- rv$rawData %>%
      group_by_( instructorsName(), facultyName() ) %>%
      summarise(weight = n()) %>%
      rename( ins = instructorsName()) %>%
      rename( fact = facultyName()) %>%
      mutate( inslabel = "(L)") %>%
      mutate( factlabel = "(F)") %>%
      unite("uniteins", c("ins","inslabel"), sep=" ") %>%
      unite("unitefact", c("fact","factlabel"), sep=" ") %>%
      ungroup()
        
    edges <- per_act %>% 
      left_join(nodes, by = c( uniteins = "label")) %>% 
      rename(from = id)
    
    
    edges <- edges %>% 
      left_join(nodes, by = c( unitefact = "label")) %>% 
      rename(to = id)
    
    edges <- select(edges, from, to, weight)
    
    edges <- mutate(edges, title = "Instruction")
    
    
    network <- visNetwork(nodes, edges) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visInteraction(hover = TRUE) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") %>%
      visEvents(hoverNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
    
    return(network)
    }
  })
  
  output$shiny_return <- renderPrint({
    input$current_node_id
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Untitled.Rmd")
      file.copy("Untitled.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(rawData = rv$rawData, instructorsName = instructorsName(), facultyName = facultyName())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert("Want to export?", "html export is in the works. Just screenshot for now!", type = "info")
  })
}

# Create app

shinyApp(ui = htmlTemplate("www/index.html"), server)
#shinyApp(ui, server)
