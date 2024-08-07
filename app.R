# Load required libraries
library(shiny)
library(tidyverse)
library(scales)
library(readxl)
library(shinythemes)
library(DT)

# Define UI for the Shiny app
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Blood Pressure Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File", 
                accept = c(".xlsx")),
      textInput("patientName", "Name of the Patient", ""),
      downloadButton("downloadPlot", "Download Plot"),
      hr(),
      verbatimTextOutput("instructions")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Data Table", dataTableOutput("dataTable")),
        tabPanel("Summary Table", dataTableOutput("summaryTable"))
      )
    )
  )
)

# Define server logic required to read the Excel file and plot the data
server <- function(input, output) {
  
  # Reactive expression to read the uploaded Excel file
  data <- reactive({
    req(input$file1)
    df <- read_xlsx(input$file1$datapath)
    
    df$Date <- as.Date(df$Date, format="%d/%m/%y")
    df$Time <- format(df$Time, "%H:%M")
    df$Sys <- as.numeric(df$Sys)
    df$Dia <- as.numeric(df$Dia)
    
    df %>%  
      pivot_longer(-c(Date, Time, BPM, AM_PM), names_to = "pressure", values_to = "values")
  })
  
  # Render the plot
  output$plot <- renderPlot({
    df <- data()
    patient_name <- input$patientName
    
    df %>% 
      ggplot(aes(x=Date, y=values, color=pressure)) +
      geom_point(aes(shape=AM_PM), size=4) +
      geom_line(aes(group=pressure), color="black", linewidth= .7) +
      geom_hline(yintercept = 90, color="red2", linetype="twodash", alpha=.5) +
      geom_hline(yintercept = 70, color="navy", linetype="twodash", alpha=.5) +
      geom_hline(yintercept= 120, color="navy", linetype="twodash", alpha=.5) +
      geom_hline(yintercept = 140, color="red2", linetype="twodash", alpha=.5) +
      facet_wrap(~ AM_PM, ncol=1) +
      scale_x_date(breaks = "1 day", date_labels = "%b \n%d") +
      scale_y_continuous(breaks = c(70,80,90,100, 110, 120, 130, 140)) +
      scale_color_manual(name = "Measurement", values = c("Sys" = "maroon", "Dia" = "chartreuse3")) +
      scale_shape_manual(name = "Part of the day", values = c("AM" = 15, "PM" = 16)) +
      theme_bw() +
      theme(axis.text = element_text(size=14),
            axis.title = element_text(size=16),
            title = element_text(size=20, face="bold"),
            legend.position = "left",
            legend.text = element_text(size= 14),
            legend.title = element_text(size = 16),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "white"),
            strip.text = element_text(size = "12", face = "italic")) +
      labs(
        title = "Blood Pressure Measurements",
        subtitle = patient_name
      )
  })
  
  # Render the data table
  output$dataTable <- renderDataTable({
    df <- data()
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Generate the summary table
  summaryData <- reactive({
    df <- data() %>% 
      pivot_wider(names_from = pressure, values_from = values)
    
    # Remove non-numeric columns for summary
    df <- df %>% select(-Date, -Time, -AM_PM)
    
    b <- as.data.frame(do.call(cbind, 
                               lapply(df, summary)))
    
    b$stat <- row.names(b)
    row.names(b) <- NULL
    
    b <- b[, c(ncol(b), 1:(ncol(b)-1))]
    
    row_to_round <- "Mean"
    row_index <- which(b$stat == row_to_round)
    b[row_index, 2:ncol(b)] <- round(b[row_index, 2:ncol(b)], 0)
    
    
    b
  })
  
  # Render the summary table
  output$summaryTable <- renderDataTable({
    b <- summaryData()
    datatable(b, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Define the download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("blood_pressure_plot_", input$patientName, ".png", sep = "")
    },
    content = function(file) {
      df <- data()
      patient_name <- input$patientName
      
      p <- df %>% 
        ggplot(aes(x=Date, y=values, color=pressure)) +
        geom_point(aes(shape=AM_PM), size=4) +
        geom_line(aes(group=pressure), color="black", linewidth= .7) +
        geom_hline(yintercept = 90, color="red2", linetype="twodash", alpha=.5) +
        geom_hline(yintercept = 70, color="navy", linetype="twodash", alpha=.5) +
        geom_hline(yintercept= 120, color="navy", linetype="twodash", alpha=.5) +
        geom_hline(yintercept = 140, color="red2", linetype="twodash", alpha=.5) +
        facet_wrap(~ AM_PM, ncol=1) +
        scale_x_date(breaks = "1 day", date_labels = "%b \n%d") +
        scale_y_continuous(breaks = c(70,80,90,100, 110, 120, 130, 140)) +
        scale_color_manual(name = "Measurement", values = c("Sys" = "maroon", "Dia" = "chartreuse3")) +
        scale_shape_manual(name = "Part of the day", values = c("AM" = 15, "PM" = 16)) +
        theme_bw() +
        theme(axis.text = element_text(size=14),
              axis.title = element_text(size=16),
              title = element_text(size=20, face="bold"),
              legend.position = "left",
              legend.text = element_text(size= 14),
              legend.title = element_text(size = 16),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill = "white"),
              strip.text = element_text(size = "12", face = "italic")) +
        labs(
          title = "Blood Pressure Measurements",
          subtitle = patient_name
        )
      ggsave(file, plot = p, width = 10, height = 5, dpi = 300)
    }
  )
  
  # Render the instructions
  output$instructions <- renderText({
    "Please upload an Excel file with the following columns:\n\nDate | Time | AM_PM | Sys | Dia | BPM\n\nFormat of Date should be: DD/MM/YY (eg: 09/07/24)\nFormat of Time: HH:MM (eg: 09:00)\nAM_PM: AM for antimeridian hours, PM for postmeridian hours\n Template available at: \nhttps://github.com/AndreaC1978/blood_pressure"
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
