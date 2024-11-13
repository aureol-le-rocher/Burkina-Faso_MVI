# Load necessary libraries
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)
library(plotly)
library(ggplot2)
library(geojsonio)

# Load the Burkina Faso shapefile from an online GeoJSON source
burkina_shape <- geojson_read("https://raw.githubusercontent.com/holtzy/D3-graph-gallery/master/DATA/world.geojson", what = "sp")

# Filter to keep only Burkina Faso
burkina_shape <- burkina_shape[burkina_shape$admin == "Burkina Faso", ]

# Load the data
cibles_data <- read_excel("Cibles RTSS 2024 Burkina Faso.xlsx", sheet = "Feuil1", skip = 2)
colnames(cibles_data) <- c("Région", "District", "Cibles_annuelles", "Cibles_Mensuelles")

vaccination_data <- read_excel("Enfants vaccinés RTSS.xlsx", sheet = "RTSS", skip = 1)
colnames(vaccination_data) <- c("Région", "District", "RTS_S1", "RTS_S2", "RTS_S3")

# Data preprocessing
cibles_data <- cibles_data %>% 
  filter(!is.na(Cibles_annuelles)) %>% 
  mutate(Cibles_annuelles = as.numeric(Cibles_annuelles), Cibles_Mensuelles = as.numeric(Cibles_Mensuelles))

vaccination_data <- vaccination_data %>%
  mutate(RTS_S1 = as.numeric(RTS_S1), RTS_S2 = as.numeric(RTS_S2), RTS_S3 = as.numeric(RTS_S3))

# Join the data
merged_data <- left_join(cibles_data, vaccination_data, by = c("Région", "District"))

# Merge with shapefile
burkina_map_data <- st_as_sf(burkina_shape) %>%
  left_join(merged_data %>% group_by(Région) %>%
              summarise(Total_Target = sum(Cibles_annuelles, na.rm = TRUE),
                        Total_Vaccinated = sum(RTS_S1, RTS_S2, RTS_S3, na.rm = TRUE),
                        Coverage = round(100 * Total_Vaccinated / Total_Target, 1)),
            by = c("name" = "Région"))

# UI with bslib
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  titlePanel("RTSS Vaccination Dashboard - Burkina Faso"),
  navbarPage(
    title = "Dashboard",
    
    tabPanel("Overview",
             fluidRow(
               valueBoxOutput("total_targets", width = 3),
               valueBoxOutput("total_vaccinated", width = 3),
               valueBoxOutput("coverage_s1", width = 2),
               valueBoxOutput("coverage_s2", width = 2),
               valueBoxOutput("coverage_s3", width = 2)
             )
    ),
    
    tabPanel("Map",
             leafletOutput("coverage_map", height = 600)
    ),
    
    tabPanel("Monthly Trend",
             plotlyOutput("monthly_trend", height = 400)
    )
  )
)

# Server
server <- function(input, output, session) {
  # Summary calculations
  output$total_targets <- renderValueBox({
    total_targets <- sum(merged_data$Cibles_annuelles, na.rm = TRUE)
    valueBox(format(total_targets, big.mark = ","), "Total Annual Target", icon = icon("flag"), color = "blue")
  })
  
  output$total_vaccinated <- renderValueBox({
    total_vaccinated <- sum(merged_data$RTS_S1, merged_data$RTS_S2, merged_data$RTS_S3, na.rm = TRUE)
    valueBox(format(total_vaccinated, big.mark = ","), "Total Vaccinated", icon = icon("users"), color = "green")
  })
  
  output$coverage_s1 <- renderValueBox({
    coverage_s1 <- round(100 * sum(merged_data$RTS_S1, na.rm = TRUE) / sum(merged_data$Cibles_annuelles, na.rm = TRUE), 1)
    valueBox(paste0(coverage_s1, "%"), "Coverage - Dose 1", icon = icon("syringe"), color = "purple")
  })
  
  output$coverage_s2 <- renderValueBox({
    coverage_s2 <- round(100 * sum(merged_data$RTS_S2, na.rm = TRUE) / sum(merged_data$Cibles_annuelles, na.rm = TRUE), 1)
    valueBox(paste0(coverage_s2, "%"), "Coverage - Dose 2", icon = icon("syringe"), color = "orange")
  })
  
  output$coverage_s3 <- renderValueBox({
    coverage_s3 <- round(100 * sum(merged_data$RTS_S3, na.rm = TRUE) / sum(merged_data$Cibles_annuelles, na.rm = TRUE), 1)
    valueBox(paste0(coverage_s3, "%"), "Coverage - Dose 3", icon = icon("syringe"), color = "red")
  })
  
  # Map of coverage
  output$coverage_map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = burkina_map_data$Coverage)
    
    leaflet(burkina_map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(Coverage), color = "#BDBDC3", weight = 1,
        fillOpacity = 0.7, label = ~paste(name, ":", Coverage, "% Coverage"),
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Coverage, title = "Coverage (%)", opacity = 0.7)
  })
  
  # Monthly Trend Line Chart
  output$monthly_trend <- renderPlotly({
    monthly_data <- merged_data %>%
      mutate(Month = rep(seq(as.Date('2024-01-01'), by = 'month', length.out = 9), length.out = nrow(merged_data))) %>%
      group_by(Month) %>%
      summarise(
        RTS_S1 = sum(RTS_S1, na.rm = TRUE),
        RTS_S2 = sum(RTS_S2, na.rm = TRUE),
        RTS_S3 = sum(RTS_S3, na.rm = TRUE)
      )
    
    p <- ggplot(monthly_data, aes(x = Month)) +
      geom_line(aes(y = RTS_S1, color = "Dose 1")) +
      geom_line(aes(y = RTS_S2, color = "Dose 2")) +
      geom_line(aes(y = RTS_S3, color = "Dose 3")) +
      theme_minimal() +
      labs(title = "Monthly Vaccination Progress by Dose", x = "Month", y = "Vaccinations") +
      scale_color_manual(values = c("Dose 1" = "purple", "Dose 2" = "orange", "Dose 3" = "red"))
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
