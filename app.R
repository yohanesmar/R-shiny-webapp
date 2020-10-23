############################################
# Data Professor                           #
# http://youtube.com/dataprofessor         #
# http://github.com/dataprofessor          #
# http://facebook.com/dataprofessor        #
# https://www.instagram.com/data.professor #
############################################

# Import libraries
library(rsconnect)   #untuk publish/deploy web app
rsconnect::setAccountInfo(name='yohanesmar', token='E6EA2FBA5AC9175D0BCBD9947C375F1D', secret='qLjO2PjfD+W9m+rkcOp0F3HLr39Jd6CmtZhGThGg')

library(shiny)
library(shinythemes)
library(nnet)

# Read in the RF model
modelku <- readRDS("modelmultinom.rds")


####################################
# UI Input                 #
####################################

ui <- fluidPage( theme = shinytheme("flatly"),
    #navigation bar             
    navbarPage("Program Predictor",
    
    #tabpanel
    tabPanel("Home",
    # Page header
    #headerPanel('Iris Predictor'),
    
    # Input values
    sidebarPanel(
        HTML("<h3>Input parameters</h3>"),
        selectInput(inputId = "a", 
                    label = "School Type", 
                    choices = list("Private" = "private", "Public" = "public"), 
                    selected = "Private"),
        numericInput(inputId = "b", 
                     label = "Read Score", 
                     value = 0,
                     max = 100,
                     min = 0),
        numericInput(inputId = "c", 
                     label = "Write Score", 
                     value = 0,
                     max = 100,
                     min = 0),
        numericInput(inputId = "d", 
                     label = "Math Score", 
                     value = 0,
                     max = 100,
                     min = 0),
        numericInput(inputId = "e", 
                     label = "Social Study Score", 
                     value = 0,
                     max = 100,
                     min = 0),
        numericInput(inputId = "f", 
                     label = "Awards", 
                     value = 0,
                     max = 100,
                     min = 0),
        sliderInput(inputId = "g", 
                    label = "Class ID",
                    min = 1, 
                    max = 20,
                    value = 1),
        
        actionButton(inputId = "submitbutton", label = "Submit", 
                     class = "btn btn-primary")), #close sideBar Panel, tapi masih lanjut 
    
    mainPanel(
        HTML("<h3>Status/Output</h3>"), # Status/Output Text Box
        verbatimTextOutput('contents'),
        tableOutput('tabledata'),
        tableOutput('tabledata2')
) # close mainPanel


), # close close TabPanel 1 (home). Jangan lupa dikasih koma apabila mau nambah tabPanel
tabPanel("About", 
         sidebarPanel(fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),


    
tabPanel("Contact", "This panel is intentionally left blank")


) # close NavbarPage
# close FluidPage

####################################
# Server                           #
####################################

server<- function(input, output, session) {
    
    # Input Data
    datasetInput <- reactive({  
        
        dframe <- data.frame(Name = c("schtyp","read","write","math","socst","awards","cid"), #name harus sesuai model yang dibuat
                         Value = as.character(c(input$a,input$b,input$c,input$d,input$e,input$f,input$g)), #value mengisi baris di kolom 2                    
                         stringsAsFactors = FALSE)
        
        #Species <- "isi apa aja yang penting kolomnya kebuat buat di integrasikan ke model"
        #df <- rbind(df, Species)
        transpose <- t(dframe)              # untuk transpose row to column, vice versa
        write.table(transpose,"inputs.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)  #disave dalam bentuk csv
        
        testing <- read.csv(paste("inputs.csv"), header = TRUE)          #dipanggil lagi dalam bentuk data.frame untuk modeling
        testing$schtyp <- factor(testing$schtyp, levels = c("private", "public"))
        
        
        
        predictit <- predict(modelku,testing,type = "prob")
        Prediction <- predict(modelku,testing)
        
        Output <- data.frame(Prediction)
        print(Output)
        
    }) #close Reactive datasetinput1
    
    
    datasetInput2 <- reactive({  
        
        dframe <- data.frame(Name = c("schtyp","read","write","math","socst","awards","cid"), #name harus sesuai model yang dibuat
                             Value = as.character(c(input$a,input$b,input$c,input$d,input$e,input$f,input$g)), #value mengisi baris di kolom 2                    
                             stringsAsFactors = FALSE)
        
        #Species <- "isi apa aja yang penting kolomnya kebuat buat di integrasikan ke model"
        #df <- rbind(df, Species)
        transpose <- t(dframe)              # untuk transpose row to column, vice versa
        write.table(transpose,"inputs.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)  #disave dalam bentuk csv
        
        testing <- read.csv(paste("inputs.csv"), header = TRUE)          #dipanggil lagi dalam bentuk data.frame untuk modeling
        testing$schtyp <- factor(testing$schtyp, levels = c("private", "public"))
        
        
        
        predictit <- predict(modelku,testing,type = "prob")
        Prediction <- predict(modelku,testing)
        
        Output <- data.frame(t(predictit))
        print(Output)
        
    }) #close reactive datasetinput2
    
    ####################################
    # UI Output (mainPanel)       #
    ####################################
    
    
    # Status/Output Text Box
    output$contents <- renderText({if (input$submitbutton>0) { isolate("Calculation complete.") } else {"Server is ready for calculation."}})
    
    # Prediction results table
    output$tabledata <- renderTable({if (input$submitbutton>0) { isolate(datasetInput())}})
    # Prediction results table 2
    output$tabledata2 <- renderTable({if (input$submitbutton>0) { isolate(datasetInput2())}})
    
    } # close function

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
