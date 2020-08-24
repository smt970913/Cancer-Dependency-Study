library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(GGally)
library(plotly)

# Load data
resp_data <- read.csv("resp.csv")
exp_data <- read.csv("exp.csv")
info_data <- read.csv("info.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    
    # Application title
    titlePanel("Cancer Dependency Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Select the time frequency
            selectInput(inputId = "Gene", label = strong("Gene"), 
                        choices = c("GATA3", "ZNF148", "TUBB"),
                        selected = "Elastic-Net"),
            selectInput(inputId = "Protein", label = strong("Protein"), 
                        choices = c("GATA3", "AR", "MEK1", "PDK1", "Notch1", "p90RSK_Caution")
            ), 
            actionButton("go", "Submit")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("PointPlot"),
            tableOutput("view")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Data set selection
    sub_data <- eventReactive(input$go, {
        if(input$Gene == "GATA3"){
            genes <- resp_data$GATA3
            if(input$Protein == "GATA3"){
                protein <- exp_data$GATA3
                index <- rep(1, length(genes))
            }
            if(input$Protein == "AR"){
                protein <- exp_data$AR
                index <- rep(2, length(genes))
            }
            if(input$Protein == "MEK1"){
                protein <- exp_data$MEK1
                index <- rep(3, length(genes))
            }
            if(input$Protein == "PDK1"){
                protein <- exp_data$MEK1
                index <- rep(4, length(genes))
            }
            if(input$Protein == "Notch1"){
                protein <- exp_data$Notch1
                index <- rep(5, length(genes))
            }
            if(input$Protein == "p90RSK_Caution"){
                protein <- exp_data$p90RSK_Caution
                index <- rep(6, length(genes))
            }
        }
        if(input$Gene == "ZNF148"){
            genes <- resp_data$ZNF148
            if(input$Protein == "GATA3"){
                protein <- exp_data$GATA3
                index <- rep(1, length(genes))
            }
            if(input$Protein == "AR"){
                protein <- exp_data$AR
                index <- rep(2, length(genes))
            }
            if(input$Protein == "MEK1"){
                protein <- exp_data$MEK1
                index <- rep(3, length(genes))
            }
            if(input$Protein == "PDK1"){
                protein <- exp_data$MEK1
                index <- rep(4, length(genes))
            }
            if(input$Protein == "Notch1"){
                protein <- exp_data$Notch1
                index <- rep(5, length(genes))
            }
            if(input$Protein == "p90RSK_Caution"){
                protein <- exp_data$p90RSK_Caution
                index <- rep(6, length(genes))
            }
        }
        if(input$Gene == "TUBB"){
            genes <- resp_data$TUBB
            if(input$Protein == "GATA3"){
                protein <- exp_data$GATA3
                index <- rep(1, length(genes))
            }
            if(input$Protein == "AR"){
                protein <- exp_data$AR
                index <- rep(2, length(genes))
            }
            if(input$Protein == "MEK1"){
                protein <- exp_data$MEK1
                index <- rep(3, length(genes))
            }
            if(input$Protein == "PDK1"){
                protein <- exp_data$MEK1
                index <- rep(4, length(genes))
            }
            if(input$Protein == "Notch1"){
                protein <- exp_data$Notch1
                index <- rep(5, length(genes))
            }
            if(input$Protein == "p90RSK_Caution"){
                protein <- exp_data$p90RSK_Caution
                index <- rep(6, length(genes))
            }
        }
        
        return(data.frame(cbind(genes, protein, index)))
        
    })
    
    output$PointPlot <- renderPlot({
        ggplot(data = sub_data(), aes(genes, protein, col = "red")) + 
            geom_point(col = "black", size = 1.3) +
            theme_economist()
    })
    
    output$view <- renderTable({
        info_data[sub_data()$index[1], 2:3]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)