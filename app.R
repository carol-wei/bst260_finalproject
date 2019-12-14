# Read in data and cut outliers
dat <- read.csv("ems_incident_dispatch_data_v2.csv", header=T)
dat <- dat[which(dat$dispatch_response_seconds_qy > 0),]
dat <- dat[which(dat$initial_severity_level_code < 8),]

# Use google maps to get nyc map
register_google(key = 'AIzaSyCRpZB738AZsCoBkgMOKkGT6IORiP10dc0')
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)

# Find zip codes in our dataset
zips <- unique(dat$zipcode)
zips <- as.vector(zips)

options(tigris_use_cache = TRUE)
nyc <- zctas(cb = TRUE, starts_with = zips)

# Sort Data
v <- dat %>%
  group_by(zipcode, initial_severity_level_code) %>%
  summarize(avg = mean(incident_response_seconds_qy, na.rm = TRUE), income=mean(income, na.rm=TRUE))

v1 <- dat %>%
  group_by(zipcode) %>%
  summarize(avg = mean(incident_response_seconds_qy, na.rm = TRUE), income=mean(income, na.rm=TRUE))

# Color scheme of average response time. v1 used for consistency
rbPal <- colorRampPalette(c('white','red3'))
v$Col <- rbPal(6)[as.numeric(cut(v$avg,breaks = c(0,as.vector(as.numeric(quantile(v1$avg,probs = c(1/6,2/6,3/6,4/6,5/6)))),100000)))]

# Sorting Data
t <- getSpPPolygonsIDSlots(nyc)
zipcodes <- nyc$GEOID10

values <- data.frame("id" = t, "ZIPCODE" = zipcodes, stringsAsFactors = FALSE) 
values <- merge(x=v, y=values, by.x='zipcode', by.y='ZIPCODE')

options(warn=-1)
tidy_nyc = tidy(nyc)
plotData = left_join(tidy_nyc, values, by='id')
plotData$x <- plotData$long
plotData$y <- plotData$lat
plotData$minutes <- round(plotData$avg/60, digits = 1)

# Redo for overall average data
rbPal <- colorRampPalette(c('white','red3'))
v1$Col <- rbPal(6)[as.numeric(quantileCut(v1$avg,6),breaks = 6)]

t1 <- getSpPPolygonsIDSlots(nyc)
zipcodes <- nyc$GEOID10

values1 <- data.frame("id" = t1, "ZIPCODE" = zipcodes, stringsAsFactors = FALSE) 
values1 <- merge(x=v1, y=values1, by.x='zipcode', by.y='ZIPCODE')

tidy_nyc = tidy(nyc)
plotData1 = left_join(tidy_nyc,values1,by='id')
plotData1$x <- plotData1$long
plotData1$y <- plotData1$lat
plotData1$minutes <- round(plotData1$avg/60, digits = 1)

# Used to create legend
cuts <- round(as.numeric(quantile(v1$avg,probs = c(1/6,2/6,3/6,4/6,5/6)))/60, digits = 1)
cuts0 <- paste('0 - ' , cuts[1], sep = '')
cuts1 <- paste(cuts[1], ' - ' , cuts[2], sep = '')
cuts2 <- paste(cuts[2], ' - ' , cuts[3], sep = '')
cuts3 <- paste(cuts[3], ' - ' , cuts[4], sep = '')
cuts4 <- paste(cuts[4], ' - ' , cuts[5], sep = '')
cuts5 <- paste(cuts[5], '+', sep = '')
cuts <- c(cuts0,cuts1,cuts2,cuts3,cuts4,cuts5)

# Shiny App UI/Server
ui = fluidPage(
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("New York City EMS Response Time Heat Map"),
  
  tabsetPanel(
    
    tabPanel("Average Response Time",
             sidebarLayout(
               sidebarPanel(
                 p(em("Response time"), "mapped by average within zipcode"),
                 br(),
                 p(em("Click map"), "to see average income and average EMS response time for a certain zip code."),
                 br(),
                 plotOutput("Legend", height = '150px')
                 
               ),
               
               mainPanel(
                 plotOutput("Map1",        
                            click = "plot2_click")
               )
             ),
             fluidRow(
               column(width = 12,
                      h4("Points near click"),
                      verbatimTextOutput("click_info1")
               )
             )
    ),
    
    tabPanel("Severity Selection",
             sidebarLayout(
               sidebarPanel(
                 p(em("Response Time"), "mapped by severity code assessed at call. More emergent situations are given severity scores of 1, and thus generally have faster response times"),
                 
                 br(),
                 
                 p(em("Click map"), "to see average income and average EMS response time for a certain zip code and severity code."),
                 
                 selectInput("severity", label = "Select call severity:",
                             choices = (as.list(sort(unique(dat$initial_severity_level_code))))),
                 br(),
                 plotOutput("Legend1", height = '150px')
                 
               ),
               
               mainPanel(
                 plotOutput("Map",        
                            click = "plot1_click")
               )
             ),
             
             fluidRow(
               column(width = 12,
                      h4("Points near click"),
                      verbatimTextOutput("click_info")
               )
             )
    )
  )
)

server = function(input, output) {
  
  output$Map = renderPlot({
    
    gg <- ggmap(nyc_map)
    gg + 
      geom_polygon(data=plotData %>% filter(initial_severity_level_code %in% input$severity), aes(x=long, y=lat, group=group), color="blue", fill = plotData %>% filter(initial_severity_level_code %in% input$severity) %>% .$Col, alpha=0.5) +
      ggthemes::theme_map()
  }
  )
  
  output$click_info <- renderPrint({
    nearPoints(plotData %>% filter(initial_severity_level_code %in% input$severity) %>% select(x, y, zipcode, income, minutes), input$plot1_click, 'x', 'y', maxpoints = 1, threshold = 20, addDist = FALSE)
  })
  
  output$Map1 = renderPlot({
    
    gg <- ggmap(nyc_map)
    gg + 
      geom_polygon(data=plotData1, aes(x=long, y=lat, group=group), color="blue", fill = plotData1$Col, alpha=0.5) +
      ggthemes::theme_map()
  }
  )
  
  output$click_info1 <- renderPrint({
    nearPoints(plotData1 %>% select(x, y, zipcode, income, minutes), input$plot2_click, 'x', 'y', maxpoints = 1, threshold = 20, addDist = FALSE)
  })
  
  output$Legend <- renderPlot({
    par(bg = 'light grey')
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("topleft", legend = cuts, title = 'Mean Response Minutes', pch=16, xpd = TRUE, inset = c(-.2,-3.1), pt.cex=3, cex=1.2, bty='n',
           col=rbPal(6))
  })
  
  output$Legend1 <- renderPlot({
    par(bg = 'light grey')
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("topleft", legend = cuts, title = 'Mean Response Minutes', pch=16, xpd = TRUE, inset = c(-.2,-3.1), pt.cex=3, cex=1.2, bty='n',
           col=rbPal(6))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)