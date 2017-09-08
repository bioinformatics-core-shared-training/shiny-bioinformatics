library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Interrogating the NKI breast cancer dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("thegene","Gene to Analyse",
                  choices=c("ESR1","ERBB2","PTEN"),
                  selected  = "ESR1"),
      radioButtons("colour","Colour of histogram",choices=c("red","green","blue"),selected="red")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("boxplot"),
      verbatimTextOutput("testResult")
    )
  )
)

server <- function(input, output) {
  
  library(breastCancerNKI)
  
  data(nki)
  expression.values <- exprs(nki)
  features <- fData(nki)
  er.status <- pData(nki)$er
  
  filterByExpression <- reactive({
    gene <- input$thegene
    probe.id <- as.character(features$probe[match(gene, features$HUGO.gene.symbol)])
    Sys.sleep(10)
    expression.values[probe.id,]
  })
  
  output$boxplot <- renderPlot({
    
    values <- filterByExpression()
    
    boxplot(values ~ er.status,col=input$colour)
  })
  
  output$testResult <- renderPrint(
    {
      values <- filterByExpression()
      t.test(values ~ er.status)
    }
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)