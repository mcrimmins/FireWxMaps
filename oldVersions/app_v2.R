# update of app_v1.R; added seasonal and fire cause filters

library(shiny)
library(terra)
library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)

# Load data
load("./Data/FOD.RData")
fc <- sf::st_drop_geometry(fc)
us_states <- map_data("state")
us_states$region <- tools::toTitleCase(us_states$region)

gh500 <- terra::rast("./Data/R2_hgt_500mb_1979_2023_CONUS.tif")
gh700 <- terra::rast("./Data/R2_hgt_700mb_1979_2023_CONUS.tif")

# Adjust fire dates
fc$DISCOVERY_DATE <- as.Date(fc$DISCOVERY_DATE, "%m/%d/%Y")
fc$CONT_DATE <- as.Date(fc$CONT_DATE, "%m/%d/%Y")
fc$OPERATION_DAYS <- as.numeric(fc$CONT_DATE - fc$DISCOVERY_DATE)+1

ui <- fluidPage(
  tags$head(tags$style("#loading-content { position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-size: 24px; text-align: center; }")),
  div(id = "loading-content", "Loading data..."),
  titlePanel("Fire Occurrence and Geopotential Height Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("states", "Select States:", choices = sort(unique(fc$STATE)), multiple = TRUE, selected = "AZ"),
      selectInput("height_level", "Select Geopotential Height Level:", choices = c("500mb" = "gh500", "700mb" = "gh700")),
      sliderInput("operation_days", "Filter by Operation Days:", min = min(fc$OPERATION_DAYS, na.rm = TRUE), max = max(fc$OPERATION_DAYS, na.rm = TRUE), value = c(min(fc$OPERATION_DAYS, na.rm = TRUE), max(fc$OPERATION_DAYS, na.rm = TRUE)), step = 1),
      sliderInput("month_range", "Select Month Range:", min = 1, max = 12, value = c(1, 12), step = 1, ticks = TRUE),
      selectInput("cause_classification", "Select Cause Classification:", choices = c("All", sort(unique(fc$NWCG_CAUSE_CLASSIFICATION)))),
      selectInput("sort_metric", "Sort by:", choices = c("Fire Count" = "count", "Total Area Burned" = "area")),
      numericInput("top_days", "Number of Top Days to Display:", 12, min = 1, max = 30),
      sliderInput("expd", "Area of extent padding (in degrees):", min = 0.5, max = 10, value = 2, step = 0.5),
      p("This tool uses the Spatial wildfire occurrence database for the United States, 1992-2020 (FOD, Short 2022) and the NOAA-NCEP/DOE Reanalysis II for daily 18Z geopotential height data.")
    ),
    mainPanel(
      div(class = "left-align", plotOutput("fireMap", width = "850px", height = "650px")),
      DT::dataTableOutput("fireDaysTable")
    )
  )
)

server <- function(input, output, session) {
  # Hide loading message when session is ready
  observe({
    removeUI(selector = "#loading-content")
  })
  output$fireMap <- renderPlot({
    req(input$states, input$height_level, input$sort_metric, input$top_days)
    
    fireData <- subset(fc, STATE %in% input$states & OPERATION_DAYS >= input$operation_days[1] & OPERATION_DAYS <= input$operation_days[2])
    fireData <- fireData %>% filter(format(DISCOVERY_DATE, "%m") %in% sprintf("%02d", input$month_range[1]:input$month_range[2]))
    if (input$cause_classification != "All") {
      fireData <- fireData %>% filter(NWCG_CAUSE_CLASSIFICATION == input$cause_classification)
    }
    
    
    fireDays <- fireData %>%
      group_by(DISCOVERY_DATE) %>%
      summarise(FireCount = n(), TotalArea = sum(FIRE_SIZE, na.rm = TRUE)) %>%
      arrange(desc(FireCount))
    
    colnames(fireDays)[1] <- "Date"
    
    sortCol <- input$sort_metric
    fireDays <- if (sortCol == "count") {
      fireDays[order(fireDays$FireCount, decreasing = TRUE),]
    } else {
      fireDays[order(fireDays$TotalArea, decreasing = TRUE),]
    }
    
    selectDays <- unique(na.omit(fireDays$Date))[1:input$top_days]
    fireData <- fireData %>% filter(DISCOVERY_DATE %in% selectDays)
    fireData$Date <- factor(fireData$DISCOVERY_DATE, levels = selectDays)
    
    ghData <- if (input$height_level == "gh500") gh500 else gh700
    ghSub <- ghData[[which(time(ghData) %in% selectDays)]]
    names(ghSub) <- time(ghSub)
    gh_df <- as.data.frame(ghSub, xy = TRUE) %>%
      pivot_longer(cols = -c(x, y), names_to = "Date", values_to = "Geopotential_Height") %>%
      mutate(Date = as.Date(Date))
    
    expd <- input$expd
    xmin <- min(fireData$LONGITUDE, na.rm = TRUE) - expd
    xmax <- max(fireData$LONGITUDE, na.rm = TRUE) + expd
    ymin <- min(fireData$LATITUDE, na.rm = TRUE) - expd
    ymax <- max(fireData$LATITUDE, na.rm = TRUE) + expd
    
    fireStats <- fireData %>%
      group_by(Date) %>%
      summarise(FireCount = n(), TotalArea = sum(FIRE_SIZE, na.rm = TRUE))
    fireStats$label <- paste0(fireStats$TotalArea, " / ", fireStats$FireCount)
    
    ggplot() +
      geom_point(data = fireData, aes(x = LONGITUDE, y = LATITUDE, size = FIRE_SIZE_CLASS), shape = 21, fill = "lightgrey", color = "black", stroke = 1, alpha = 0.8) +
      geom_contour(data = gh_df, aes(x = x, y = y, z = Geopotential_Height, color = after_stat(level)), binwidth = 10) +
      geom_path(data = us_states, aes(x = long, y = lat, group = group), color = "black", linewidth = 0.5) +
      geom_text(data = fireStats, aes(x = xmin + 0.2, y = ymax - 0.2, label = label), inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3, color = "black") +
      facet_wrap(~Date) +
      scale_color_gradientn(colors = c("blue", "cyan", "yellow", "orange", "red")) +
      scale_size_manual(values = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6, "G" = 7)) +
      coord_fixed(ratio = 1, xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
      labs(title = paste(input$height_level, "Geopotential Height & Fire Locations"), x = "Longitude", y = "Latitude", color = "Geopotential Height (m)", size = "Fire Size Class") +
      theme_minimal() + theme(legend.position = "bottom")
  })
  output$fireDaysTable <- DT::renderDataTable({
    req(input$states, input$sort_metric, input$top_days)
    fireData <- subset(fc, STATE %in% input$states & OPERATION_DAYS >= input$operation_days[1] & OPERATION_DAYS <= input$operation_days[2])
    fireData <- fireData %>% filter(format(DISCOVERY_DATE, "%m") %in% sprintf("%02d", input$month_range[1]:input$month_range[2]))
    if (input$cause_classification != "All") {
      fireData <- fireData %>% filter(NWCG_CAUSE_CLASSIFICATION == input$cause_classification)
    }
    fireDays <- fireData %>%
      group_by(DISCOVERY_DATE) %>%
      summarise(FireCount = n(), TotalArea = sum(FIRE_SIZE, na.rm = TRUE)) %>%
      arrange(desc(FireCount))
    colnames(fireDays)[1] <- "Date"
    sortCol <- input$sort_metric
    fireDays <- if (sortCol == "count") {
      fireDays[order(fireDays$FireCount, decreasing = TRUE),]
    } else {
      fireDays[order(fireDays$TotalArea, decreasing = TRUE),]
    }
    fireDays[1:input$top_days, ]
  })
}

shinyApp(ui, server)
