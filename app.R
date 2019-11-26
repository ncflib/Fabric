library(shiny)
library(tidyverse)
library(visNetwork)
library(DT)
library(stringi)
library(digest)


# global
decideColor <- function(x,y) {
  if(x >= y) {
    "red";
  } else {
    "blue";
  }
}
anon <- function(x) {
  rl <- rle(x)$lengths
  ans<- paste("Person", rep(seq_along(rl), rl))
  return(ans)
}
books <- list("dataset.csv")

instructorsName = 'Instructors'
facultyName = 'Faculty'
columnNamesDone = 0

# Define server logic to read selected file ----
server <- function(input, output, session) {
  rawData <- read.csv("dataset.csv")
  facultyList <- read.csv("facultylist.csv")
  rv <- reactiveValues()
  rv$facultyList = facultyList;
  rv$facultCheck = FALSE;
  rv$anonCheck = FALSE;
  ns <- session$ns

  observeEvent(input$selection, {
    if(input$selection != "") {
    updateSelectInput(session = session, inputId = "instructorsname", choices = colnames(read.csv(input$selection)))
    updateSelectInput(session = session, inputId = "facultyname", choices = colnames(read.csv(input$selection)))
    rv$rawData <- read.csv(input$selection)
    }
  })
  
  observeEvent(input$file1, {
    if(input$file1$datapath != "") {
      updateSelectInput(session = session, inputId = "instructorsname", choices = colnames(read.csv(input$file1$datapath)))
      updateSelectInput(session = session, inputId = "facultyname", choices = colnames(read.csv(input$file1$datapath)))
      rv$rawData <- read.csv(input$file1$datapath)
    }
  })
  
  observeEvent(input$file2, {
    if(input$file1$datapath != "") {
      updateSelectInput(session = session, inputId = "facultyListName", choices = colnames(read.csv(input$file2$datapath)))
      updateSelectInput(session = session, inputId = "facultyListDivision", choices = colnames(read.csv(input$file2$datapath)))
      rv$facultyList <- read.csv(input$file2$datapath)
    }
  })
  
  observeEvent(input$selection2, {
    if(input$selection2 != "") {
      updateSelectInput(session = session, inputId = "facultyListName", choices = colnames(read.csv(input$selection2)))
      updateSelectInput(session = session, inputId = "facultyListDivision", choices = colnames(read.csv(input$selection2)))
      rv$facultyList <- read.csv(input$selection2)
    }
  })
  
  observeEvent(input$anonCheck, {
    rv$anonCheck = input$anonCheck;
  })
  
  observeEvent(input$facultyCheck, {
    rv$facultyCheck = input$facultyCheck;
  })
  
  instructorsName <- eventReactive(input$instructorsname, {
    return(input$instructorsname)
  })
  
  facultyName <- eventReactive(input$facultyname, {
    return(input$facultyname)
  })
  
  
   
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
  
  output$facultyList <- DT::renderDataTable({
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- rv$facultyList
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
    
    if(instructorsName() != "" && facultyName() != "") {

      instructors <<- rv$rawData %>%
      select(instructorsName()) %>%
      distinct() %>%
      rename(labelName = instructorsName())%>%
      mutate(label2 = "(L)")%>%
      unite("label", labelName:label2, sep=" ")
    
    instructors <<- mutate(instructors, group = "Librarian")
    faculty <<- rv$rawData

    if(rv$facultyCheck == TRUE) {
      divisionList <<- merge(x = rv$rawData, y = rv$facultyList, by.y = "Faculty", all = TRUE)
      faculty <<- divisionList %>%
      select(facultyName(),Division) %>%
      distinct() %>%
      rename(labelName = facultyName())%>%
      rename(group = Division)%>%
      mutate(label2 = "(F)") %>%
      unite("label", c(labelName,label2), sep=" ")
    } else {
      faculty <<- rv$rawData %>%
        select(facultyName()) %>%
        distinct() %>%
        rename(labelName = facultyName())%>%
        mutate(label2 = "(F)") %>%
        unite("label", c(labelName,label2), sep=" ")
      
    }
    nodes <<- full_join(instructors, faculty)
    
    nodes <<- nodes %>%
      rowid_to_column("id")
    
    nodes <<- mutate(nodes, title = label)
    
    per_act <<- full_join(instructors,faculty)
    
    per_act <<- rv$rawData %>%
      group_by_( instructorsName(), facultyName() ) %>%
      summarise(weight = n()) %>%
      rename( ins = instructorsName()) %>%
      rename( fact = facultyName()) %>%
      mutate( inslabel = "(L)") %>%
      mutate( factlabel = "(F)") %>%
      unite("uniteins", c("ins","inslabel"), sep=" ") %>%
      unite("unitefact", c("fact","factlabel"), sep=" ") %>%
      ungroup()
        
    edges <<- per_act %>% 
      left_join(nodes, by = c( uniteins = "label")) %>% 
      rename(from = id)
    
    
    edges <<- edges %>% 
      left_join(nodes, by = c( unitefact = "label")) %>% 
      rename(to = id)
    
    edges <<- select(edges, from, to, weight)
    
    edges <<- mutate(edges, title = "Instruction")
    

    meanweight <<- mean(edges$weight)
    
    edges$width <<- edges$weight
    edges$color <<- ifelse(edges$weight>quantile(edges$weight, 0.75) + IQR(edges$weight)*1.5,"red","blue")
    
    if(rv$anonCheck == TRUE) {
    nodes$label <<- anon(nodes$label)
    nodes$title <<- anon(nodes$title);
    }
    
    network <<- visNetwork(nodes, edges) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visInteraction(hover = TRUE ) %>%
      visEdges(smooth = FALSE ) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
    
    return(network)
    }
  })
  
  
  output$pieDiv <- renderPlot({
    if(!is.null(input$current_node_id)){
    targetNodes = nodes[which(nodes$id == input$current_node_id),]
    targetInteractions = edges[which( (edges$from == input$current_node_id) | (edges$to == input$current_node_id)),]
    if(nrow(targetInteractions)>0) {
      if(targetNodes["group"] != "Librarian" || is.na(targetNodes["group"])) { 
        targetDivisions = merge( x = targetInteractions, y = nodes, by.x = 'from', by.y = 'id', all.x= TRUE); 
      } else {
        targetDivisions = merge( x = targetInteractions, y = nodes, by.x = 'to', by.y = 'id', all.x= TRUE);
      }
    targetDivisions = targetDivisions$group;
    targetDivisions[is.na(targetDivisions)] = "NA"
    pie(table(targetDivisions, useNA = "always"));
    }
    }
    
    
  })
  
  output$top5div <- DT::renderDataTable({
    if(!is.null(input$current_node_id)){
      targetNodes = nodes[which(nodes$id == input$current_node_id),]
      targetInteractions = edges[which( (edges$from == input$current_node_id) | (edges$to == input$current_node_id)),]
      if(nrow(targetInteractions)>0) {
      if(targetNodes["group"] != "Librarian" || is.na(targetNodes["group"])) { 
        targetDivisions = merge( x = targetInteractions, y = nodes, by.x = 'from', by.y = 'id', all.x= TRUE); 
      } else {
        targetDivisions = merge( x = targetInteractions, y = nodes, by.x = 'to', by.y = 'id', all.x= TRUE);
      }
      targetTable = targetDivisions[order(targetDivisions$weight, decreasing = TRUE),]
      dataFrameTop5 = data.frame(targetTable$title.y,targetTable$weight);
      colnames(dataFrameTop5)[1] = "Faculty"
      colnames(dataFrameTop5)[2] = "# of Interactions"
      return(dataFrameTop5);
      }
    }
  }, options = list( paging = 0, searching = 0, info = 0, ordering = 0 ))
  
  
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
}

# Create app

shinyApp(ui = htmlTemplate("www/index.html"), server)
#shinyApp(ui, server)
