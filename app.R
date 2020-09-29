# DEPENDENCIES -----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(plotly)
library(xts)

# LOAD DATA --------------------------------------------------------------------

source('data_downloader.R')

# SETTINGS ---------------------------------------------------------------------

covid_col = '#cc4c02'

# FUNCTIONS --------------------------------------------------------------------

# function to plot cumulative COVID cases by date
cumulative_plot <- function(plot_date) {
    dt <- as.Date(covid19_schools_summary[ , 'Collected_Date' ])
    d1 <- covid19_schools_summary[ , 'Cumulative_School_Related_Cases' ]
    plot_df <- data.frame(date = dt, cases = d1)
    plot_df = subset(plot_df, date <= plot_date)
    g1 = ggplot(plot_df, aes(x = date, y = cases)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        ylab('cumulative cases') + theme_bw() + 
        scale_colour_manual(values = c(covid_col)) +
        scale_y_continuous(labels = function(l) { as.integer(l) }) +
        theme(legend.title = element_blank(), legend.position = '', plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

# INITIALIZATION ---------------------------------------------------------------

cv_min_date = min(covid19_schools_summary$Collected_Date)
current_date = max(covid19_schools_summary$Collected_Date)
cv_max_date_clean = format(as.POSIXct(current_date),'%d %B %Y')

# CREATE BASEMAP ---------------------------------------------------------------

basemap <- leaflet() %>% setView(lng = -85.3232, lat = 49, zoom = 6)
basemap <- basemap %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    addCircleMarkers(data = cases_per_school, 
                     lng = ~lon, 
                     lat = ~lat, 
                     radius = ~(cases_per_school), # ~(cases_per_school)^(1/5), 
                     weight = 1, 
                     color = covid_col,
                     fillOpacity = 0.2, 
                     label = sprintf('<strong>%s</strong><br/>City: %s<br/>Confirmed cases (cumulative): %s<br/>', 
                                     cases_per_school$school_name, 
                                     cases_per_school$city, 
                                     cases_per_school$cases_per_school) %>% lapply(htmltools::HTML), 
                     labelOptions = labelOptions(
                         style = list('font-weight' = 'normal', padding = '3px 8px', 'color' = covid_col),
                         textsize = '15px', direction = 'auto'))

# SHINY UI ---------------------------------------------------------------------
ui <- bootstrapPage(
    tags$head(includeHTML('gtag.html')),
    navbarPage(theme = shinytheme('flatly'), collapsible = TRUE,
               'Ontario Schools COVID-19 Tracker', id='nav',
               
               # tab: COVID-19 mapper ------------------------------------------
               tabPanel('COVID-19 mapper',
                        div(class='outer',
                            tags$head(includeCSS('styles.css')),
                            leafletOutput('mymap', width='100%', height='100%'),
                            
                            absolutePanel(id = 'controls', 
                                          class = 'panel panel-default',
                                          top = 75, 
                                          left = 55, 
                                          width = 250, 
                                          fixed=TRUE,
                                          draggable = TRUE, 
                                          height = 'auto',
                                          # span(tags$i(h6('Reported cases are subject to significant variation in testing policy and capacity between countries.')), style='color:#045a8d'),
                                          h3(textOutput('reactive_case_count'), align = 'right'),
                                          # h4(textOutput('reactive_death_count'), align = 'right'),
                                          h6(textOutput('clean_date_reactive'), align = 'right'),
                                          # plotOutput('epi_curve', height='130px', width='100%'),
                                          plotOutput('cumulative_plot', height='130px', width='100%'),
                                          
                                          sliderInput('plot_date',
                                                      label = h5('Select mapping date'),
                                                      min = as.Date(cv_min_date,'%Y-%m-%d'),
                                                      max = as.Date(current_date,'%Y-%m-%d'),
                                                      value = as.Date(current_date),
                                                      timeFormat = '%d %b',
                                                      animate=animationOptions(interval = 3000, loop = FALSE))
                            )
                            
                            
                        )
               ),
               
               # tab: plots ----------------------------------------------------
               tabPanel('Plots',
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                # span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                # span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
                                # 
                                # pickerInput("level_select", "Level:",
                                #             choices = c("Global", "Continent", "Country", "US state"),
                                #             selected = c("Country"),
                                #             multiple = FALSE),
                                # 
                                # pickerInput("region_select", "Country/Region:",
                                #             choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country),
                                #             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                #             selected = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country)[1:10],
                                #             multiple = TRUE),
                                # 
                                # pickerInput("outcome_select", "Outcome:",
                                #             choices = c("Deaths per 100,000", "Cases per 100,000", "Cases (total)", "Deaths (total)"),
                                #             selected = c("Deaths per 100,000"),
                                #             multiple = FALSE),
                                # 
                                # pickerInput("start_date", "Plotting start date:",
                                #             choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"),
                                #             options = list(`actions-box` = TRUE),
                                #             selected = "Date",
                                #             multiple = FALSE),
                                
                                sliderInput("minimum_date",
                                            "Minimum date:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=as.Date(cv_min_date),
                                            timeFormat="%d %b")
                                
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    # tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                                    # tabPanel("New", plotlyOutput("country_plot")),
                                    # tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log")),
                                    tabPanel('School related cases', br(), plotlyOutput('school_related_cases_details_plot')),
                                    tabPanel('Schools with cases', br(), plotlyOutput('schools_with_cases_plot')),
                                    tabPanel('Active school cases by municipality', br(), plotlyOutput('active_cases_by_municipality_plot'))
                                )
                            )
                        )
               ),
               
               # tab: Data -----------------------------------------------------
               tabPanel('Data',
                        h3('Summary of cases in schools'),
                        numericInput('maxrows_1', 'Rows to show', 10),
                        verbatimTextOutput('rawtable_1'),
                        downloadButton('downloadCsv_1', 'Download as CSV'),tags$br(),tags$br(),
                        h3('Schools with active COVID-19 cases'),
                        numericInput('maxrows_2', 'Rows to show', 10),
                        verbatimTextOutput('rawtable_2'),
                        downloadButton('downloadCsv_2', 'Download as CSV'),tags$br(),tags$br(),
                        'Adapted from data published by ', tags$a(href='https://data.ontario.ca/dataset/summary-of-cases-in-schools', 
                                                                  'Government of Ontario.')
               ),
               
               # tab: About this site --------------------------------------------
               tabPanel('About this site',
                        tags$div()
               )
               
    )          
)

# SHINY SERVER -----------------------------------------------------------------

server <- function(input, output) {
    
    # school_related_cases_details_plot ----------------------------------------
    output$school_related_cases_details_plot <- renderPlotly({
        df <- covid19_schools_summary
        idx <- which(df$Collected_Date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~Collected_Date, y = ~Cumulative_School_Related_Cases, name = 'Cumulative School Related Cases', type = 'scatter', mode = 'lines+markers')
        # fig <- fig %>% add_trace(y = ~New_Total_School_Related_Cases, name = 'New Total School Related Cases', mode = 'lines+markers') 
        # fig <- fig %>% add_trace(y = ~New_School_Related_Student_Cases, name = 'New School Related Student Cases', mode = 'lines+markers') 
        # fig <- fig %>% add_trace(y = ~New_School_Related_Staff_Cases, name = 'New School Related Staff Cases', mode = 'lines+markers') 
        # fig <- fig %>% add_trace(y = ~New_School_Related_Unspecified_Cases, name = 'New School Related Unspecified Cases', mode = 'lines+markers') 
        # fig <- fig %>% add_trace(y = ~Cumulative_School_Related_Cases, name = 'Cumulative School Related Cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~Cumulative_School_Related_Student_Cases, name = 'Cumulative School Related Student Cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~Cumulative_School_Related_Staff_Cases, name = 'Cumulative School Related Staff Cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~Cumulative_School_Related_Unspecified_Cases, name = 'Cumulative School Related Unspecified Cases', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'School related cases',
                              xaxis = list(title = 'collected date'),
                              yaxis = list (title = 'cases'))
        fig
    })
    
    # schools_with_cases_plot --------------------------------------------------
    output$schools_with_cases_plot <- renderPlotly({
        df <- covid19_schools_summary[ , c('Collected_Date', 'Current_Schools_w_Cases') ]
        idx <- which(df$Collected_Date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~Collected_Date, y = ~Current_Schools_w_Cases, name = 'Current schools with cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Schools with cases',
                              xaxis = list(title = 'collected date'),
                              yaxis = list (title = 'schools'))
        fig
    })
    
    # active_cases_by_municipality_plot ----------------------------------------
    output$active_cases_by_municipality_plot <- renderPlotly({
        active_cases_by_municipality <- tapply(covid19_schools_active$Municipality,
                                               list(covid19_schools_active$Collected_Date,
                                                    covid19_schools_active$Municipality),
                                               length)
        active_cases_by_municipality <- na.locf(active_cases_by_municipality)
        colidx <- order(active_cases_by_municipality[ nrow(active_cases_by_municipality), ], decreasing = TRUE)
        active_cases_by_municipality <- active_cases_by_municipality[ , colidx ] 
        active_cases_by_municipality <- data.frame(active_cases_by_municipality)
        active_cases_by_municipality <- data.frame(Collected_Date = as.Date(rownames(active_cases_by_municipality)), active_cases_by_municipality)
        rownames(active_cases_by_municipality) <- NULL
        df <- active_cases_by_municipality[ , 1:11 ]
        idx <- which(df$Collected_Date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        nms <- colnames(df)[ -1 ]
        nms2 <- colnames(df)[ -1 ] %>% str_replace_all(., '\\.', ' ') %>% str_replace_all(., '\\s+', ' ') %>% str_trim
        code_str <- sprintf('
                            fig <- plot_ly(df, x = ~Collected_Date, y = ~%s, name = \'%s\', type = \'scatter\', mode = \'lines+markers\')
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig
                            ', 
                            nms[ 1 ], nms2[ 1 ],
                            nms[ 2 ], nms2[ 2 ],
                            nms[ 3 ], nms2[ 3 ],
                            nms[ 4 ], nms2[ 4 ],
                            nms[ 5 ], nms2[ 5 ],
                            nms[ 6 ], nms2[ 6 ],
                            nms[ 7 ], nms2[ 7 ],
                            nms[ 8 ], nms2[ 8 ],
                            nms[ 9 ], nms2[ 9 ],
                            nms[ 10 ], nms2[ 10 ])
        fig <- parse(text = code_str) %>% eval
        fig <- fig %>% layout(title = 'Active school cases by municipality (top 10)',
                              xaxis = list(title = 'collected date'),
                              yaxis = list (title = 'cases'))
        fig
    })
    
    # cumulative_plot ----------------------------------------------------------
    output$cumulative_plot <- renderPlot({
        cumulative_plot(input$plot_date)
    })
    
    # clean_date_reactive ------------------------------------------------------
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(input$plot_date), '%d %B %Y')
    })
    
    # reactive_case_count ------------------------------------------------------
    output$reactive_case_count <- renderText({
        idx <- max(which(covid19_schools_summary$Collected_Date <= as.Date(input$plot_date)))
        count <- last(covid19_schools_summary[ idx, 'Cumulative_School_Related_Cases' ])
        paste0(prettyNum(count, big.mark = ','), ' cases')
    })
    
    # mymap --------------------------------------------------------------------
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    # downloadCsv_1 ------------------------------------------------------------
    output$downloadCsv_1 <- downloadHandler(
        filename = function() {
            paste('schoolcovidsummary_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_summary, file)
        }
    )
    
    # rawtable_1 ---------------------------------------------------------------
    output$rawtable_1 <- renderPrint({
        orig <- options(width = 1000)
        print(tail(covid19_schools_summary, input$maxrows_1), row.names = FALSE)
        options(orig)
    })
    
    # downloadCsv_2 ------------------------------------------------------------
    output$downloadCsv_2 <- downloadHandler(
        filename = function() {
            paste('schoolsactivecovid_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_active, file)
        }
    )
    
    # rawtable_2 ---------------------------------------------------------------
    output$rawtable_2 <- renderPrint({
        orig <- options(width = 1000)
        print(tail(covid19_schools_active, input$maxrows_2), row.names = FALSE)
        options(orig)
    })
}

# RUN THE APPLICATION ----------------------------------------------------------
shinyApp(ui = ui, server = server)
