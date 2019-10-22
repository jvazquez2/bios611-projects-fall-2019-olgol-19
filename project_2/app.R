library(shiny)
library(tidyverse)
library(shinythemes)


# Load and clean the data 
UMD_data <- read_tsv("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project1_2019/UMD_Services_Provided_20190719.tsv") %>%
  mutate(N_Date = as.Date(Date, "%m/%d/%Y")) %>%
  select_if(~!is.character(.)) %>%
  select_if(~!is.logical(.)) %>%
  arrange(N_Date)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Urban Ministry of Durham Data Report"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of data to plot
                    selectInput(inputId = "type", 
                                label = strong("Please select the data you want to display:"),
                                choices = c("Client file number" = "Number of Clients",
                                            "Food Pounds" = "Food Pounds",
                                            "Diapers" = "Diapers",
                                            "Clothing Items"= "Clothing Items",
                                            "School Kits"="School Kits",
                                            "Hygiene Kits"="Hygiene Kits"),
                                selected = "Number of Clients"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"),start = "2000-01-01", end = "2019-06-30",
                                   min = "1983-01-01", max = "2019-06-30")

                    ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    
                    plotOutput(outputId = "myplot", height = "300px")#,
                    #textOutput(outputId = "desc"),
                    #tags$a(href = "https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project1_2019/UMD_Services_Provided_20190719.tsv", "Data Source: Urban Ministry of Durham", target = "_blank")
                            )
                  
                         )
       
                )


# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_range <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    UMD_cld <- UMD_data %>%
      filter(between(N_Date,as.POSIXct(input$date[1]), date < as.POSIXct(input$date[2]))) %>%
      mutate(Year = as.numeric(format(N_Date, "%Y")))
  })
  
  
  # Create the plotOutput function is expecting
  output$myplot <- renderPlot({
    df=UMD_cld %>%
      group_by(input$type, Year) %>%
      summarize(input$type=n())

    ggplot(df, aes(Year, input$type)) + 
      geom_col(alpha = 0.4) +
      geom_smooth(color='red')+
      theme_classic()+
      xlab("Year of Service Provided") +
      ylab("Number of Clients") +
      ggtitle("Increase in number of clients by year")

  })
  
  # Pull in description of trend
  output$desc <- renderText({
    trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)