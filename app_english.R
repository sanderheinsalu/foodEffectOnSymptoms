library(shiny)
library(MTS)
library(readODS)
library(DT)

# Define UI ----
ui <- fluidPage(
  titlePanel(h1("The effect of food on symptoms, or the effect of other inputs on some outputs.")
  ),
  sidebarLayout(
    sidebarPanel(#fluidrow(
      column(12, 
             numericInput("ynum", 
                          h3("Number of output columns in file."), 
                          value = 2)
      ),
      column(12, 
             numericInput("xnum", 
                          h3("Number of input columns in file."), 
                          value = 36)
      ),    
      column(12, # Column width out of 12. 
             fileInput("file", h3("Upload CSV file."  ),multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                       )
            ),
      column(12, 
             helpText("The first columns in the file are the outputs, the next four columns the time of day dummy variables, the last columns the inputs. This application evaluates the effect of the inputs on the outputs. Currently only 4 time of day intervals are supported. For example, 06:00-12:00, 12:01-16:00, 16:01-20:00, 20:01-06:00. 
                      If a time interval column has 1 in it, this means the row's inputs and outputs occurred in this time interval. In each row, exactly one of the time columns should be 1, the others 0. See", a(href="https://docs.google.com/spreadsheets/d/14rU8q4YUpjsmfS1D6n0TdVGvX0-RzCzVyfZGOOnwo_c","the example"),
                      ". Each row of the example has outputs (symptoms) and inputs (foods eaten). The inputs and outputs must be numeric. For example, 1 means the input was consumed, 0 that it was not. Or 0,1,2,... units of input consumed. 
                      
                      The header row may consist of any text without spaces. The file should have at least twice more rows (observations) than the total number of input and output columns. ")
      ),
      # Input: Select separator ----
      radioButtons("sep", "CSV column separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "CSV comment separator",
                   choices = c(None = "",
                               "Double quote" = '"',
                               "Single quote" = "'"),
                   selected = '"')
      #)
    ),
    mainPanel(
      h3("How much the output changes when the input increases by one unit."),
      #        p("Outputs", textOutput("valjundid"), 
      #        "Inputs", textOutput("sisendid"),
      p(textOutput("kontroll")),
      p(""),
      p("The first row of the table shows the average output and its standard deviation. The rows after that show the effect of the input on the output and the standard deviation of this effect. If the effect is more than twice the standard deviation, then it is statistically significant. The output in the previous time interval (t-1) is one of the inputs for the output in the current time interval (t). Method: seasonality (the effect of the time interval) is removed with linear regression. The residuals of this are vector autoregressed with exogenous variables (VARX, also known as autoregressive distributed lag, ADL), where the inputs from the CSV file are the exogenous variables. The influence of both current and past (lagged) inputs is studied. "),
      p( DT::dataTableOutput("tulemused")
         #tableOutput("tulemused")
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$kontroll <- renderText({
    req(input$file)
    data <- data.frame(read.csv(input$file$datapath,
                                header = TRUE,
                                sep = input$sep,
                                quote = input$quote
    )
    )
    xcolumns <- input$xnum
    ycolumns <- input$ynum
    totalcolumns <- length(colnames(data))
    if(xcolumns+ycolumns+4 == totalcolumns){
      return(paste("Columns in file ",totalcolumns,", outputs ",ycolumns,", inputs", xcolumns,", time intervals 4. The sum matches."))
    }
    else{
      return(paste("Columns in file ",totalcolumns,", outputs ",ycolumns,", inputs", xcolumns,", time intervals 4. Error: the number of input and output columns plus four must equal the total number of columns."))
    }
  })
  
  output$sisendid <- renderText({ 
    req(input$file)
    data <- data.frame(read.csv(input$file$datapath,
                                header = TRUE,
                                sep = input$sep,
                                quote = input$quote
    )
    )
    xcolstart <- input$ynum +4 +1
    xcolend <- input$ynum +4 +input$xnum
    return(colnames(data)[xcolstart:xcolend])
  }) 
  
  output$valjundid <- renderText({
    req(input$file)
    data <- data.frame(read.csv(input$file$datapath,
                                header = TRUE,
                                sep = input$sep,
                                quote = input$quote
    )
    )
    ycolumns <- input$ynum  
    return(colnames(data)[1:ycolumns])
  })
  
  output$tulemused <- DT::renderDataTable({
    req(input$file)
    data <- data.frame(read.csv(input$file$datapath,
                                header = TRUE,
                                sep = input$sep,
                                quote = input$quote
    )
    )
    for (col in colnames(data)){
      data[,col] <- as.numeric(data[,col]) #NAs introduced by coercion
    }
    data <- data[!is.na(rowSums(data)), ]  # truncate to nonempty rows
    
    xcolstart <- input$ynum +4 +1
    xcolend <- input$ynum +4 +input$xnum
    xcolumns <- xcolend-xcolstart+1
    ycolumns <- input$ynum
    timecolstartp1 <- input$ynum +1 +1
    timecolend <- input$ynum +4
    xdf <- data[, xcolstart:xcolend,drop=F]
    ydf <- data[,1:ycolumns,drop=F] 
    #timedummies <- data[,(ycolumns+1):(ycolumns+4),drop=F]
    timedummiesdrop1 <- data[,timecolstartp1:timecolend,drop=F]
    
    y <- ydf
    # Remove time of day effects (seasonality). 
    for (col in colnames(ydf)){
      #  y[,col] <- residuals(lm(ydf[,col] ~ timedummiesdrop1[[1:3]])) # invalid type 'list'. Another error Recursive indexing failed
      y[,col] <- residuals(lm(ydf[,col] ~ timedummiesdrop1[[1]] +timedummiesdrop1[[2]] +timedummiesdrop1[[3]]))
      # This is why have to fix the number of time dummies, not user-selected.
    }
    # How many lags of the inputs x to use in predicting the output y. 
    lags <- min(8, floor(length(y[,1])/xcolumns/1.5))
    lagnames <- "viit0"
    for (i in 1:lags){
      lagnames <- c(lagnames, paste("viit",i))
    }
    # Run the regression. 
    fit <- VARX(y, 1, xt = xdf, m=4) 
    ncolumns <- 2*ycolumns
    # Arrange the regression results in a table and change the column names accordingly.
    fitvaljund <- cbind(fit[["coef"]], fit[["se.coef"]])
    clnms <- cbind(colnames(fit[["coef"]]), colnames(fit[["se.coef"]]))
    clnms2 <- cbind(colnames(fit[["coef"]]), colnames(fit[["se.coef"]]))
    clnms2[seq(1, ncolumns, 2)] <- clnms[1:ycolumns]
    clnms2[seq(2, ncolumns, 2)] <- paste0("st.err(", clnms[1:ycolumns], ")")
    fitvaljund[,seq(1, ncolumns, 2)] <- round(fit[["coef"]],3)
    fitvaljund[,seq(2, ncolumns, 2)] <- paste0("(",round(fit[["se.coef"]],3),")")
    colnames(fitvaljund) <- clnms2
# Change the row names of the regression results table to reflect the regression intercept, lagged dependent and independent variables (outputs and inputs). 
    rownames(fitvaljund)[1] <- "keskmine"
    for (i in seq_along(colnames(y))){
      rownames(fitvaljund)[1+i] <- paste0(rownames(fitvaljund)[1+i],"_t-1")
    }
    for (j in 1:lags){
      for (i in 1:xcolumns){
        rownames(fitvaljund)[1+ycolumns+j*xcolumns+i] <- paste0(rownames(fitvaljund)[1+ycolumns+j*xcolumns+i],"_t-",j)
      }
    } # end for

    DT::datatable(fitvaljund, options = list(lengthMenu = c(30, 60, 100, 200, 300), pageLength = 200))
    
  }) # end output$tulemused
} # end server()

# Run the app ----
shinyApp(ui = ui, server = server)