###
#
###

# Load libraries
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(reshape2)
library(tidyr)
library(rmarkdown)
library(plotly)

# Load data
spendPerPersonData <- read_rds("data/grossCurrentExpenditureWithPop.rds")

# Set the options for the primary support reason checkboxes
primarySupportReasons <- unique(spendPerPersonData$PrimarySupportReason_Key)

# Set the options for the x-axis, facet values and color values
facetChoices <- xAxisChoices <- colValues <-  c("Support Setting" = "supportOrCareType", "Region" = "GEOGRAPHY_NAME", "Primary Support Reason" = "PrimarySupportReason_Key", "Care Type (short/long term)" = "CareType_Key", "Age Band" = "AgeBand_Key")


# Set out UI
ui <- fluidPage(
    
    title = "Adult Social Care Spending Data 2017/18",
    
    titlePanel("Dashboard for Adult Social Care Finance Report 2017/18 (England)"), 
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select the x axis
            selectInput(inputId = "xAxisChoice", label = "Choose x-axis (horizontal for table)", choices = xAxisChoices),
            
            # Select the facets
            selectInput(inputId = "facetValue", label = "Choose panels (or vertical for table)", choices = facetChoices, selected = "PrimarySupportReason_Key"),
            
            # Select plot type
            selectInput(inputId = "plotType", label = "Plot/Table type", choices = c("Total spending (bar graph)" = "bar", "Spending per 100k population (each point is a local authority)" = "scatter"), selected = "scatter" )  ,
            
            # Select colours
            selectInput(inputId = "colorValue", label = "Choose colour variable", choices = colValues, selected = "supportOrCareType"),
            
            # Filter by primary support reason
            checkboxGroupInput(inputId = "PrimarySupportReason",
                               label = "Filter plot by Primary Support Reason",
                               choices = primarySupportReasons,
                               selected = primarySupportReasons[1:5][-4]),
            
            # br() element to introduce extra vertical spacing ----
            br(),
            
            # Maybe shove a y-axis slider here
            
            
            # Download full data set button
            downloadButton(outputId = "downloadDataSet", label = "Download full data set"),
            
            width = 3
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        
                        # Main plot
                        tabPanel("Plot", 
                                 plotlyOutput(outputId = "ggplotly"),
                                 uiOutput(outputId = "noteText")), 
                        
                        # Table
                        tabPanel("Table", 
                                 titlePanel(textOutput(outputId = "tableNote")),
                                 fluidRow(
                                     # column(12, offset = 0, textOutput(outputId = "tableNote")),
                                     column(3, selectInput(inputId = "aggregateFormula", label = "Choose summary statistic for table", choices = c("mean", "median", "sum"), selected = "mean")),
                                     column(9, offset = 0, textOutput(outputId = "suppNote")),   
                                     tableOutput(outputId = "summaryTable"),
                                     
                                     downloadButton(outputId = "downloadSummaryTable", label = "Download summary table")
                                 )
                        ),
                        # About
                        tabPanel("About", uiOutput(outputId = "outputUI"))
                        
                        
            ),
            width = 9
        )
    )
)




# Define server logic 
server <- function(input, output) {
    
    selectData <- reactive({
        
        # quo() basically says, do not evaluate this now, simply put it in an envelope and pass it to the next bit
        # of the function, which will tell you when to evaluate it with the use of !!.
        function(plotType, x_axis_selection="supportOrCareType", facet_selection="PrimarySupportReason_Key"){
            
            # Filter the data on the relevant primary support reasons
            relevantData <- filter(spendPerPersonData, PrimarySupportReason_Key %in% input$PrimarySupportReason)        
            
                        
            # Remove the outliers from the spendPerPerson data if you are going to be looking at it
            # on a per person level (they squash the rest of the graph)
            if(plotType=="scatter"){
                # Take out the extremely high values
                relevantData <- relevantData %>% group_by_("DH_GEOGRAPHY_NAME", facet_selection, x_axis_selection) %>% 
                    summarise(total_spend = sum(ITEMVALUE))
                
                # Take out the 143 (out of 15k) < 0 values which must be errors
                #relevantData <- filter(relevantData, SpendingPerPerson>=0)
                
            }
            else {
                relevantData <- spendPerPersonData
                
                
            }
            
            
            return(relevantData)
            
        }
    })
    # Use the above function to create the appropriate data for the plot                
    
    
    
    plotToRender <- reactive({
        
        showNotification(input$facetValue)
        # Select the data
        #### Note the weird syntax here - selectData() returns the actual function
        # and then whatever is in brackets after that is the argument passed to the function
        # as per: https://github.com/rstudio/shiny/issues/858
        
        plotData <- selectData()(plotType = input$plotType, x_axis_selection = input$xAxisChoice, facet_selection = input$facetValue)
        
        
        # Create the plot
        p <- plot_ly(plotData, x = ~get(input$xAxisChoice), y = ~total_spend, 
                     color = ~get(input$facetValue), 
                     text = ~DH_GEOGRAPHY_NAME, 
                     hoverinfo = 'text',
                     type = "box") %>% 
            layout(boxmode = "group")#,
                   #hoverlabel = 'text') # the alternative is hoverinfo then it's neater but no value
        
        # Return the plot
        return(p)
        
        
        
    }) #plotToRender
    
    output$ggplotly <- renderPlotly({
        plotToRender()
    })
    

    }

# Run the application 
shinyApp(ui = ui, server = server)

