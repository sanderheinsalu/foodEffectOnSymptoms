library(shiny)
library(DT)

# Define UI ----
ui <- fluidPage(
  titlePanel(h1("Title")
  ),
  sidebarLayout(
    sidebarPanel(
      column(12, 
             fileInput("file", h3("Upload CSV." ),
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain", ".csv")))),
     mainPanel(
      tabsetPanel(type="tabs", 
tabPanel("Tab1",
        h3("Tab1title"),
        p("Tab1 text"),
        p( DT::dataTableOutput("results1")
        )
) #end tabPanel "Tab1"
,
tabPanel("Tab2",
         h3("Tab2title"),
         p("Tab2 text"),
         p( DT::dataTableOutput("results2")
         )
) #end tabPanel "Tab2"
)# end tabsetPanel
    ) # end mainPanel
  )
)

# Define server logic ----
server <- function(input, output) {
output$results <- DT::renderDataTable({
    req(input$file)
data <- data.frame(read.csv(input$file$datapath,
                            header = TRUE
                            )
                   )
DT::datatable(data)
}) # end output$results

output$results2 <- DT::renderDataTable({
  req(input$file)
  data <- data.frame(read.csv(input$file$datapath,
                              header = TRUE
                              )
                    )
  DT::datatable(data)
}) # end output$results2
} # end server

# Run the app ----
shinyApp(ui = ui, server = server)