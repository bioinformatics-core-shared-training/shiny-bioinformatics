library(shiny)
library(Biobase)

ui <- fluidPage(
  
  # Application title
  titlePanel("Interrogating the NKI breast cancer dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("thegene","Gene to Analyse",
                  choices=c("ESR1","ERBB2","PTEN"),
                  selected  = "ESR1")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

server <- function(input, output) {
  
  library(breastCancerNKI)
  
  data(nki)
  expression.values <- exprs(nki)
  features <- fData(nki)
  er.status <- pData(nki)$er
  
  output$boxplot <- renderPlot({
    
    gene <- input$thegene
    probe.id <- as.character(features$probe[match(gene, features$HUGO.gene.symbol)])
    
    values <- expression.values[probe.id,]
    boxplot(values ~ er.status)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)