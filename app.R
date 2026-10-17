My_UI <- fluidPage (
  
  titlePanel("R Shiny App"),
  
  
  sidebarLayout(
    sidebarPanel(
      p("Public School Data from data.gov"),
      
      
      selectInput(
        inputId = "school_level",
        label = "School_Level",
        choices = c("All", unique(school_data$SCHOOL_LEVEL))
        
        
        ),
      
      
      
      textInput(
        inputId = "city",
        label = "City",
        value = ""
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("FTE Enrollment", plotlyOutput("scatter_plot")),
        tabPanel("Student Faculty Ratio", plotlyOutput("box_plot")),
        tabPanel("School Types", plotlyOutput("bar_chart"))
      
      )
      
    )
    
  )
  
)


server <- function(input, output, session) {
  
  sc_filter <- reactive({
    
    data <- sc_data
    if (input$school_level != "All") {
      data <- subset(data, SCHOOL_LEVEL == input$school_level)
    }
    
    if (input$city != "") {
      data <- subset(data, grepl(input$city, LCITY, ignore.case = TRUE))
    }
    
    return(data)
  })
  
  
  
  output$scatter_plot <- renderPlotly({
    
    p1 <- ggplot(sc_filter(), aes(
      
      x = TOTAL,
      y = FTE,
      fill = SCHOOL_LEVEL,
      text = paste("School:", SCH_NAME, "<br>Total: ", TOTAL, "<br>FTE:", FTE)
    )) + 
      geom_point() + 
      labs(
        x = "Students",
        y = "FTE",
        fill = "School Level"
      )
    
    ggplotly(p1, tooltip = "text")
  })
  
  output$box_plot <- renderPlotly({
    
    p2 <- ggplot(sc_filter(), aes(
      
      x = SCHOOL_TYPE_TEXT,
      y = STUTERATIO
    )) + 
      geom_boxplot() +
      labs(
        x = "School Type",
        y = "Student Faculty Ratio"
      )
    
    ggplotly(p2)
  })
  
  output$bar_chart <- renderPlotly({
    
    p3 <- ggplot(sc_filter(), aes(
      
      x = TOTAL,
      y = VIRTUAL,
      fill = SCHOOL_TYPE_TEXT,
      text = paste(SCH_NAME)
    )) +
      geom_col() +
      labs(
        x = "",
        y = "Students",
        fill = "Type"
      )
    ggplotly(p3, tooltip = "text")
      
  })
  
}


shinyApp(ui = My_UI, server = server)