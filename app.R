#https://community.rstudio.com/t/shinyapps-io-cant-deploy-apps-built-with-r-4-0-0/63143/7
#Error: Unhandled Exception: Child Task 876104247 failed: Error building image: Error building withr (2.4.1). R version 4.0.4 currently unavailable
library(shiny)
library(MTS)
library(readODS)
library(DT)

# Define UI ----
ui <- fluidPage(
  titlePanel(h1("Toidu mõju sümptomitele või muude sisendite mõju muudele väljunditele.")
  ),
  sidebarLayout(
    sidebarPanel(#fluidrow(
      column(12, 
             numericInput("ynum", 
                          h3("Vali väljunditulpade arv failis"), 
                          value = 2)
      ),
      column(12, 
             numericInput("xnum", 
                          h3("Vali sisenditulpade arv failis"), 
                          value = 36)
      ),    
      column(12, # Column width out of 12. 
             fileInput("file", h3("CSV tabelarvutusfaili üleslaadimine."  ),multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"))),
      column(12, 
             helpText("Esimesed tulbad failis on väljundid,",
                      "järgmised ajavahemiku tähised, viimased",
                      "sisendid, mille mõju väljundile hinnata.", 
                      "Praegu lubatud täpselt 4 ajavahemikku päevas.",
                      "Ajavahemikud nt kell 6-12, 12-16, 16-20, 20-6.",
                      "Ajavahemiku tulbas 1 tähendab, et selle rea",
                      "sisendid ja väljundid on selles ajavahemikus.",
                      "Vt ", a(href="https://docs.google.com/spreadsheets/d/1lC56rtvZYnd1CZChsuK_6Tbif8xfn5pSxvyNP1n8QZs","näidis"),
                      ". Näidise igas reas on sümptomid (väljundid), aja tähised (1 või 0) ja söödud toidud.",
                      "Sisendid ja väljundid peavad numbrilised olema.",
                      "Pealkirjareas ükskõik mis tühikuteta tekstid.",
                      "Ridu (vaatlusi) peaks failis olema üle kahe korra rohkem kui sisendi- ja väljunditulpasid kokku.")
      ),      
      # Input: Select separator ----
      radioButtons("sep", "CSV tulpade eraldaja",
                   choices = c(Koma = ",",
                               Semikoolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "CSV kommentaari eraldaja",
                   choices = c(None = "",
                               "Jutumärk" = '"',
                               "Ülakoma" = "'"),
                   selected = '"')
    #)
      ),
    mainPanel(
      tabsetPanel(type="tabs", tabPanel("Viitajadridades",
      h3("Kui palju sisendi suurenemine ühe ühiku võrra väljundit muudab
        # murdosana väljundi algsest väärtusest
         ."),
#        p("Väljundid", textOutput("valjundid"), 
#        "Sisendid", textOutput("sisendid"),
p(textOutput("kontroll")),
p(""),
     p("Tabeli esimene rida näitab väljundi keskmist suurust ja selle standardhälvet. Edasised read näitavad sisendi mõju väljundile ja mõju standardhälvet. Kui mõju on suurem kui kahekordne st.hälve, siis on mõju statistiliselt oluline. Väljund eelmisel ajavahemikul (t-1) on ka sisend praeguse ajavahemiku (t) väljundile. 
       Meetod: sesoonsus (kellaaja mõju) eemaldatakse väljunditest lineaarse regressiooniga. 
#       Väljundist võetakse logaritm. 
       Seejärel tehakse väljundite peal 1. järku vektor-autoregressioon väliste muutujatega (VARX, tuntud ka kui jagatud viitaegadega autoregressioon ADL). Välised muutujad on CSV-st võetud sisendid. Uuritakse nii samaaegse välise muutuja mõju kui ka viitajaga mõjusid."),
        p( DT::dataTableOutput("tulemused")
      )
      ) #end tabPanel "Viitajad ridades"
,
tabPanel("Viitajadveergudes",
         h3("Kui palju sisendi suurenemine ühe ühiku võrra väljundit muudab
          #  murdosana väljundi algsest väärtusest
            ."),
         p(""),
         p("Tabeli iga rida näitab sisendi mõju väljundile samal ajavahemikul ja viitaegadega. Sulgudes on mõju standardhälbed - kui mõju on suurem kui kahekordne st.hälve, siis on mõju statistiliselt oluline. Väljund eelmisel ajavahemikul (t-1) on ka sisend praeguse ajavahemiku (t) väljundile, aga seda mõju tabelis pole.
       Meetod: sesoonsus (kellaaja mõju) eemaldatakse väljunditest lineaarse regressiooniga. 
#           Väljundist võetakse logaritm. 
           Seejärel tehakse väljundite peal 1. järku vektor-autoregressioon väliste muutujatega (VARX, tuntud ka kui jagatud viitaegadega autoregressioon ADL). Välised muutujad on CSV-st võetud sisendid. Uuritakse nii samaaegse välise muutuja mõju kui ka viitajaga mõjusid."),
         p( DT::dataTableOutput("tulemused2")
         )
) #end tabPanel "Viitajad veergudes"
,
tabPanel("Viitaeg veerus, ainult olulised, log.",
         h3("Kui palju sisendi suurenemine ühe ühiku võrra väljundit protsentides muudab. Standardhälve sulgudes. Väljundid peavad olema positiivsed, muidu tekib veateade, sest ei saa logaritmi võtta."),
         p( DT::dataTableOutput("tulemusedOlulised")
         )
) #end tabPanel "Viitaeg veerus, ainult olulised, log."
,
tabPanel("Viitaeg veerus, ainult olulised, lin.",
         h3("Kui palju sisendi suurenemine ühe ühiku võrra väljundit muudab. Standardhälve sulgudes."),
         p( DT::dataTableOutput("tulemusedOlulisedLin")
         )
) #end tabPanel "Viitaeg veerus, ainult olulised, lin."
      )# end tabsetPanel
    ) # end mainPanel
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
      return(paste("Tulpade arv failis ",totalcolumns,", väljundeid ",ycolumns,", sisendeid", xcolumns,", ajavahemikke 4. Summa klapib."))
    }
    else{
      return(paste("Tulpade arv failis ",totalcolumns,", väljundeid ",ycolumns,", sisendeid", xcolumns,", ajavahemikke 4. Viga. Sisendi- ja väljunditulpade summa peab olema tulpade arv miinus 4."))
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
#ydf <- log(ydf) # Error NaN, Inf
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
fit <- VARX(y, 1, xt = xdf, m=lags) 
rounding <- 2
ncolumns <- 2*ycolumns
fitvaljund <- cbind(fit[["coef"]], fit[["se.coef"]])
clnms <- cbind(colnames(fit[["coef"]]), colnames(fit[["se.coef"]]))
clnms2 <- cbind(colnames(fit[["coef"]]), colnames(fit[["se.coef"]]))
clnms2[seq(1, ncolumns, 2)] <- clnms[1:ycolumns]
clnms2[seq(2, ncolumns, 2)] <- paste0("st.hälve(", clnms[1:ycolumns], ")")
fitvaljund[,seq(1, ncolumns, 2)] <- round(fit[["coef"]],rounding)
fitvaljund[,seq(2, ncolumns, 2)] <- paste0("(",round(fit[["se.coef"]],rounding),")")
colnames(fitvaljund) <- clnms2
#fitvaljund[,seq(2, ncolumns, 2)] <- round(fit[["se.coef"]],3)
#fitvaljund <- cbind(round(fit[["coef"]],rounding), paste0("(",round(fit[["se.coef"]],rounding),")")) # combine
# fitvaljund <- cbind(round(fit[["coef"]],rounding), round(fit[["se.coef"]],rounding)) # combine
# fitvaljund <- fitvaljund[, c(matrix(1:ncol(fitvaljund), nrow = 2), byrow = T))] # and reorder. 
#fitvaljund <- matrix(rbind(round(fit[["coef"]],2), round(fit[["se.coef"]],2)), nrow = nrow(fit[["coef"]]))
#colnames(fitvaljund)[seq(2, ncolumns, 2)] <- paste0("st.hälve(",colnames(fit[["coef"]])[seq(1, ncolumns, 2)],")")

rownames(fitvaljund)[1] <- "keskmine"
for (i in seq_along(colnames(y))){
  rownames(fitvaljund)[1+i] <- paste0(rownames(fitvaljund)[1+i],"_t-1")
}
for (j in 1:lags){
  for (i in 1:xcolumns){
    rownames(fitvaljund)[1+ycolumns+j*xcolumns+i] <- paste0(rownames(fitvaljund)[1+ycolumns+j*xcolumns+i],"_t-",j)
  }
}
# fitvaljund1 <- matrix(round(fit[["coef"]][4:147,1]/(2*fit[["se.coef"]][4:147,1]),1), ncol=4)

DT::datatable(fitvaljund, options = list(lengthMenu = c(30, 60, 100, 200, 300), pageLength = 200)) # , filter='top' # useful for numeric, not when parentheses around st.err. 

}) # end output$tulemused

output$tulemused2 <- DT::renderDataTable({
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
  timedummiesdrop1 <- data[,timecolstartp1:timecolend,drop=F]
  
  y <- ydf
  # Remove time of day effects (seasonality). 
  for (col in colnames(ydf)){
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
  fit <- VARX(y, 1, xt = xdf, m=lags) 
  rounding <- 3
  # fitvaljund <- cbind(fit[["coef"]], fit[["se.coef"]])
  # clnms <- cbind(colnames(fit[["coef"]]), colnames(fit[["se.coef"]]))
  # clnms2 <- cbind(colnames(fit[["coef"]]), colnames(fit[["se.coef"]]))
  # clnms2[seq(1, ncolumns, 2)] <- clnms[1:ycolumns]
  # clnms2[seq(2, ncolumns, 2)] <- paste0("st.hälve(", clnms[1:ycolumns], ")")
  # fitvaljund[,seq(1, ncolumns, 2)] <- round(fit[["coef"]],3)
  # fitvaljund[,seq(2, ncolumns, 2)] <- paste0("(",round(fit[["se.coef"]],3),")")
  # colnames(fitvaljund) <- clnms2
  # 
  # rownames(fitvaljund)[1] <- "keskmine"
  # for (i in 1:length(colnames(y))){
  #   rownames(fitvaljund)[1+i] <- paste0(rownames(fitvaljund)[1+i],"_t-1")
  # }
  # for (j in 1:lags){
  #   for (i in 1:xcolumns){
  #     rownames(fitvaljund)[1+ycolumns+j*xcolumns+i] <- paste0(rownames(fitvaljund)[1+ycolumns+j*xcolumns+i],"_t-",j)
  #   }
  # }
  
  fitvaljund3 <-matrix(0,nrow=xcolumns*ycolumns, ncol=lags+1)
  fitvaljund3std <-matrix(0,nrow=xcolumns*ycolumns, ncol=lags+1)
  fitvaljund3text <-matrix("",nrow=xcolumns*ycolumns, ncol=lags+1)
  coefnumber <- length(fit[["coef"]][,1])
  colnames(fitvaljund3) <- lagnames
  colnames(fitvaljund3text) <- lagnames
  rwnms <- rep(colnames(xdf),ycolumns)
  for(i in 1:ycolumns){
    rwnms[(1+(i-1)*xcolumns):(i*xcolumns)] <- paste0(colnames(xdf),"->",colnames(y)[i])
  }
  rownames(fitvaljund3) <- rwnms
  rownames(fitvaljund3text) <- rwnms
  # Rearrange coefficients so that the effects of xdf on the first output ydf[,1] are the top rows of the matrix, the effects of xdf on ydf[,2] the second block of rows, etc.
  for (i in 1:ycolumns){
    fitvaljund3[(1+(i-1)*xcolumns):(i*xcolumns),] <- matrix(round(fit[["coef"]][(1+ycolumns+1):coefnumber,i], rounding), ncol=lags+1)
    fitvaljund3std[(1+(i-1)*xcolumns):(i*xcolumns),] <- matrix(round(fit[["se.coef"]][(1+ycolumns+1):coefnumber,i], rounding), ncol=lags+1)
  }
  for (i in 1:(xcolumns*ycolumns)){
    for (j in 1:(lags+1)){
      fitvaljund3text[i,j] <- paste0(fitvaljund3[i,j]," (",fitvaljund3std[i,j],")")
    }
  }
  
  DT::datatable(fitvaljund3text, options = list(lengthMenu = c(30, 60, 100), pageLength = xcolumns*ycolumns)) # , filter='top' # useful for numeric, not when parentheses around st.err. 
  
}) # end output$tulemused2

output$tulemusedOlulised <- DT::renderDataTable({
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
  timedummiesdrop1 <- data[,timecolstartp1:timecolend,drop=F]
  
  y <- ydf
  ydf <- log(ydf+0.001)
  # Remove time of day effects (seasonality). 
  for (col in colnames(ydf)){
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
  fit <- VARX(y, 1, xt = xdf, m=lags) 
  rounding <- 3
  
  fitvaljund3 <-matrix(0,nrow=xcolumns*ycolumns, ncol=lags+1)
  fitvaljund3std <-matrix(0,nrow=xcolumns*ycolumns, ncol=lags+1)
  fitvaljund3text <-matrix("",nrow=xcolumns*ycolumns, ncol=lags+1)
  coefnumber <- length(fit[["coef"]][,1])
  colnames(fitvaljund3) <- lagnames
  colnames(fitvaljund3text) <- lagnames
  rwnms <- rep(colnames(xdf),ycolumns)
  for(i in 1:ycolumns){
    rwnms[(1+(i-1)*xcolumns):(i*xcolumns)] <- paste0(colnames(xdf),"->",colnames(y)[i])
  }
  rownames(fitvaljund3) <- rwnms
  rownames(fitvaljund3text) <- rwnms
  # Rearrange coefficients so that the effects of xdf on the first output ydf[,1] are the top rows of the matrix, the effects of xdf on ydf[,2] the second block of rows, etc.
  for (i in 1:ycolumns){
    fitvaljund3[(1+(i-1)*xcolumns):(i*xcolumns),] <- matrix(round(fit[["coef"]][(1+ycolumns+1):coefnumber,i], rounding), ncol=lags+1)
    fitvaljund3std[(1+(i-1)*xcolumns):(i*xcolumns),] <- matrix(round(fit[["se.coef"]][(1+ycolumns+1):coefnumber,i], rounding), ncol=lags+1)
  }
  for (i in 1:(xcolumns*ycolumns)){
    for (j in 1:(lags+1)){
      if (abs(fitvaljund3[i,j]) >1.96*fitvaljund3std[i,j]){
      fitvaljund3text[i,j] <- paste0(100*fitvaljund3[i,j]," (",100*fitvaljund3std[i,j],")")
      }
      else 
        fitvaljund3text[i,j] <- ""
    }
  }
  
  DT::datatable(fitvaljund3text, options = list(lengthMenu = c(30, 60, 100), pageLength = xcolumns*ycolumns)) # , filter='top' # useful for numeric, not when parentheses around st.err. 
  #rm(list = c('ydf','xdf','y','fit')) # empty tab in app. 
  
}) # end output$tulemusedOlulised

output$tulemusedOlulisedLin <- DT::renderDataTable({
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
  timedummiesdrop1 <- data[,timecolstartp1:timecolend,drop=F]
  
  y <- ydf # initialise y, later overwritten.
  # Remove time of day effects (seasonality). 
  for (col in colnames(ydf)){
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
  fit <- VARX(y, 1, xt = xdf, m=lags) 
  rounding <- 3
  
  fitvaljund <-matrix(0,nrow=xcolumns*ycolumns, ncol=lags+1)
  fitvaljundstd <-matrix(0,nrow=xcolumns*ycolumns, ncol=lags+1)
  fitvaljundtext <-matrix("",nrow=xcolumns*ycolumns, ncol=lags+1)
  coefnumber <- length(fit[["coef"]][,1])
  colnames(fitvaljund) <- lagnames
  colnames(fitvaljundtext) <- lagnames
  rwnms <- rep(colnames(xdf),ycolumns)
  for(i in 1:ycolumns){
    rwnms[(1+(i-1)*xcolumns):(i*xcolumns)] <- paste0(colnames(xdf),"->",colnames(y)[i])
  }
  rownames(fitvaljund) <- rwnms
  rownames(fitvaljundtext) <- rwnms
  # Rearrange coefficients so that the effects of xdf on the first output ydf[,1] are the top rows of the matrix, the effects of xdf on ydf[,2] the second block of rows, etc.
  for (i in 1:ycolumns){
    fitvaljund[(1+(i-1)*xcolumns):(i*xcolumns),] <- matrix(round(fit[["coef"]][(1+ycolumns+1):coefnumber,i], rounding), ncol=lags+1)
    fitvaljundstd[(1+(i-1)*xcolumns):(i*xcolumns),] <- matrix(round(fit[["se.coef"]][(1+ycolumns+1):coefnumber,i], rounding), ncol=lags+1)
  }
  for (i in 1:(xcolumns*ycolumns)){
    for (j in 1:(lags+1)){
      if (abs(fitvaljund[i,j]) >1.96*fitvaljundstd[i,j]){
        fitvaljundtext[i,j] <- paste0(fitvaljund[i,j]," (",fitvaljundstd[i,j],")")
      }
      else 
        fitvaljundtext[i,j] <- ""
    }
  }
  
  DT::datatable(fitvaljundtext, options = list(lengthMenu = c(30, 60, 100), pageLength = xcolumns*ycolumns)) 
  
}) # end output$tulemusedOlulisedLin

} # end server

# Run the app ----
shinyApp(ui = ui, server = server)