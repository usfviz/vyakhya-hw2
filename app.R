rm(list=ls())
cat("\014")

library(shiny)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(plotly)

ui <- fluidPage(
  headerPanel('Life Expectancy vs Fertility Rate'),

    mainPanel(
    selectInput("region_selected", "Region", choices = c("All" = "All",
                    "East Asia & Pacific" = "East Asia & Pacific",
                    "Europe & Central Asia" = "Europe & Central Asia",
                    "Latin America & Caribbean" = "Latin America & Caribbean",
                    "Middle East & North Africa" = "Middle East & North Africa",
                    'North America' = "North America",
                    "South Asia" = "South Asia",
                    "Sub-Saharan Africa" = "Sub-Saharan Africa"), selected = NULL), 
    plotOutput("plot1",
               click = "plot_click",
               hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce")),
    uiOutput("hover_info"),
    sliderInput("year", "Year", 1960, 2014, 2014, sep = "", animate = animationOptions(interval=600, loop = F), width = 1000))
    # sliderInput("pop", "Population", 10, 30, 20, step = 10, ticks=FALSE))
)

server = function(input, output) {
  
  data_fr=read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')
  data_le=read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
  data_pop=read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv')
  data_regions <- read.csv("Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv")
  
  data_le <- melt(data_le, na.rm = T, value.name = "LifeExpectancy", variable.name = "Year")
  data_fr <- melt(data_fr, na.rm = T, value.name = "FertilityRate", variable.name = "Year")
  data_pop <- melt(data_pop, na.rm = T, value.name = "Population", variable.name = "Year")
  
  data_le <- data_le[,c("Country.Name", "Country.Code", "Year", "LifeExpectancy")]
  data_fr <- data_fr[,c("Country.Name", "Country.Code", "Year", "FertilityRate")]
  data_pop <- data_pop[,c("Country.Name", "Country.Code", "Year", "Population")]
  
  data <- merge(x = data_le, y = data_fr, by = c("Country.Name", "Country.Code", "Year"), all = TRUE)
  data <- merge(x = data, y = data_pop, by = c("Country.Name", "Country.Code", "Year"), all = TRUE)
  data <- merge(x = data, y = data_regions[,c("Country.Code", "Region")], by = c("Country.Code"), all = TRUE)
  
  data$Year <- substr(data$Year, 2, 5)
  data$Year <- as.numeric(data$Year)
  
  data <- data[data$Region != "", ] # Removing Aggregates
  data <- na.omit(data)
  
  p1 <- reactive({
    data_sub <- data[data$Year==input$year,]
    assign("data_sub", data_sub, envir = .GlobalEnv)
    region_selected <- input$region_selected
    if (is.null(region_selected) | region_selected == "All"){
      g1 <- ggplot(data_sub, aes(x = LifeExpectancy, y = FertilityRate, size = Population, fill = Region)) +
        geom_point(shape = 21)
    } else {
      region_data <- data_sub[data_sub$Region == input$region_selected, ]
      non_region_data <- data_sub[data_sub$Region != input$region_selected, ]
      g1 <- ggplot(data_sub, aes(x = LifeExpectancy, y = FertilityRate, size = Population, fill = Region)) +
            geom_point(data = non_region_data, alpha = 0.2, shape = 21) + geom_point(data = region_data, shape = 21)
    }
    g1 + scale_size(range = c(0, 30), guide = FALSE) + 
      scale_color_brewer(name = "Region") +
      theme_bw() + xlab("Life Expectancy") + ylab("Fertility Rate") +
      xlim(10, 90) + ylim(1,9) + 
      # scale_size_area(max_size = input$pop)) +
      guides(fill=guide_legend(order = 2, override.aes = list(shape=22, size = 8)), shape = guide_legend(order = 1)) + theme(legend.key=element_blank()) 
    
    })
  
  # data <- subset(data, Year==var())
  # data <- data[data$Year == year,]
  
  output$plot1 <- renderPlot({plot
    
    p1()
    
  }, height = 400, width = 900)
  
  # output$vals <- renderPrint({
  #   hover <- input$plot_hover 
  #   # print(str(hover)) # list
  #   y <- nearPoints(data_sub, input$plot_hover)[input$var_y]
  #   req(nrow(y) != 0)
  #   y
  # })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(data_sub, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0(point[,c('Country.Name')])))
    )
  })
  
  
}

shinyApp(ui = ui, server = server)