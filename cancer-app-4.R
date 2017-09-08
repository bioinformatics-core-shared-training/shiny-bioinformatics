library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Interrogating the NKI breast cancer dataset"),
  
  sidebarLayout(
    sidebarPanel(
      DT::dataTableOutput("genes")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

server <- function(input, output) {
  
  library(breastCancerNKI)
  library(Biobase)
  
  data(nki)
  expression.values <- exprs(nki)
  features <- fData(nki)
  er.status <- pData(nki)$er
  
  output$genes <- DT::renderDataTable(features[,c("probe","EntrezGene.ID","HUGO.gene.symbol")],selection="single")
  filterByExpression <- reactive({
    selectedRow <- input$genes_rows_selected
    probe.id <- as.character(features$probe[selectedRow])
    expression.values[probe.id,]
  })
  
  geneName <- reactive({
    selectedRow <- input$genes_rows_selected
    features$HUGO.gene.symbol[selectedRow]    
    
  })
  
  output$boxplot <- renderPlot({
    
    values <- filterByExpression()
    boxplot(values ~ er.status,col=input$colour,main=geneName())
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