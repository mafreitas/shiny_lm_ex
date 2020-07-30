
library(shiny)

ui <- fluidPage(

    titlePanel("Linear Modelling App"),

    sidebarLayout(
        sidebarPanel(
 
            fileInput("data", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            actionButton("lm_run", "Run Regression"),
            
        ),


        mainPanel(
            tableOutput('table'),
            plotOutput('scatterplot')
            
        )
    )
)


server <- function(input, output) {
    
    model = reactiveValues()
    
    
    dataInput = reactive({
        
        inFile <- input$data
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath,
                 header = T,
                 #header = input$header,
                 stringsAsFactors = FALSE)
        
    })
    
    observeEvent(input$lm_run, {
        
        model$linear_model = lm(isolate(dataInput()$Y) ~ isolate(dataInput()$X) )
        
    })
    
    output$table <- renderTable({
        dataInput()
    })
    
    output$scatterplot <- renderPlot({
        plot(dataInput()$X,dataInput()$Y)
        abline(model$linear_model)
    })

}


shinyApp(ui = ui, server = server)
