# DEPENDENCIES -----------------------------------------------------------------

library(DT)
library(reshape2)
library(rgdal)
library(shiny)
library(shinythemes)
library(sp)
library(plotly)
library(xts)

# LOAD DATA --------------------------------------------------------------------

source('data_downloader.R')

# SETTINGS ---------------------------------------------------------------------

covid_col = '#d62728'

basemap_cache_file <- 'data/basemap.rdata'

use_cached_basemap <- FALSE

# FUNCTIONS --------------------------------------------------------------------

# INITIALIZATION ---------------------------------------------------------------

cv_min_date = min(covid19_schools_summary$collected_date)
current_date = max(covid19_schools_summary$collected_date)
cv_max_date_clean = format(as.POSIXct(current_date),'%d %B %Y')

# SHINY UI ---------------------------------------------------------------------
ui <- bootstrapPage(
    tags$head(includeHTML('gtag.html')),
    navbarPage(theme = shinytheme('flatly'), collapsible = TRUE,
               'Ontario Schools COVID-19 Tracker', id='nav',
               
               # tab: COVID-19 Mapper ------------------------------------------
               tabPanel('COVID-19 Mapper',
                        div(class='outer',
                            
                            # tag: stylesheet ----------------------------------
                            tags$head(includeCSS('styles.css')),
                            
                            # leaflet: basemap  --------------------------------
                            leafletOutput('basemap_leaflet', width='100%', height='100%'),
                            
                            # panel: controls ----------------------------------
                            absolutePanel(id = 'controls', 
                                          class = 'panel panel-default',
                                          top = 75, 
                                          left = 55, 
                                          width = 350, 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          height = 'auto',
                                          h3(textOutput('reactive_case_count'), align = 'right'),
                                          h6(textOutput('clean_date_reactive'), align = 'right'),
                                          plotOutput('cumulative_plot', height = '130px', width = '100%'),
                                          sliderInput('plot_date',
                                                      label = h5('Select mapping date'),
                                                      min = as.Date(cv_min_date,'%Y-%m-%d'),
                                                      max = as.Date(current_date,'%Y-%m-%d'),
                                                      value = as.Date(current_date),
                                                      timeFormat = '%d %b',
                                                      animate = animationOptions(interval = 3000, loop = FALSE)),
                                          h3('daily summary', align = 'right'),
                                          div(tableOutput('daily_summary'), style = 'font-size: small; width: 100%')
                            )
                            
                        )
                        
               ),
               
               # tab: Plots ----------------------------------------------------
               tabPanel('Plots',
                        
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("minimum_date",
                                            "Minimum date:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=as.Date(cv_min_date),
                                            timeFormat="%d %b")
                                
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    
                                    tabPanel('Cumulative school-related cases', 
                                             br(), 
                                             plotlyOutput('school_related_cases_details_plot')),
                                    
                                    tabPanel('Schools with cases', 
                                             br(), 
                                             plotlyOutput('schools_with_cases_plot')),
                                    
                                    tabPanel('Active school cases by municipality', 
                                             br(), 
                                             plotlyOutput('active_cases_by_municipality_plot')),
                                    
                                    tabPanel('Active school cases by school board', 
                                             br(), 
                                             plotlyOutput('active_cases_by_board_plot'))
                                    
                                )
                            )
                        )
               ),
               
               # tab: Data -----------------------------------------------------
               tabPanel('Data',
                        # h3('Summary of cases in schools'),
                        # # numericInput('maxrows_1', 'Rows to show', 10),
                        # DTOutput('school_summary_data_dt'),
                        # downloadButton('download_csv_button_1', 'Download as CSV'),tags$br(),tags$br(),
                        # h3('Schools with active COVID-19 cases'),
                        # # numericInput('maxrows_2', 'Rows to show', 10),
                        # DTOutput('school_cases_demo_data_dt'),
                        # downloadButton('download_csv_button_2', 'Download as CSV'),tags$br(),tags$br(),
                        # 'Adapted from data published by ', tags$a(href='https://data.ontario.ca/dataset/summary-of-cases-in-schools', 
                        #                                           'Government of Ontario.')
                        
                        tabsetPanel(
                            tabPanel('Summary of cases in schools', 
                                     br(), 
                                     downloadButton('download_csv_button_1', 'Download as CSV'),
                                     br(),
                                     DTOutput('school_summary_data_dt')),
                            tabPanel('Schools with active COVID-19 cases with demographics', 
                                     br(), 
                                     downloadButton('download_csv_button_2', 'Download as CSV'),
                                     br(),
                                     DTOutput('school_cases_demo_data_dt'))
                        )
               ),
               
               # tab: About this site --------------------------------------------
               tabPanel('About this site',
                        tags$div()
               )
               
    )          
)

# SHINY SERVER -----------------------------------------------------------------

server <- function(input, output) {
    
    # basemap_leaflet ----------------------------------------------------------
    output$basemap_leaflet <- renderLeaflet({
        if (use_cached_basemap) progress_max <- 2 else progress_max <- 7
        withProgress(max = progress_max, 
                     value = 0, 
                     message = 'please wait...', 
                     expr = {
                         if (use_cached_basemap) {
                             incProgress(1, 'loading map')
                             # use cached version of basemap
                             load(file = basemap_cache_file, envir = .GlobalEnv)    
                         } else {
                             incProgress(1, 'loading shapes')
                             # regenerate the basemap
                             # https://www12.statcan.gc.ca/census-recensement/alternative_alternatif.cfm?archived=1&l=eng&dispext=zip&teng=gpr_000b11a_e.zip&k=%20%20%20%2040968&loc=http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip
                             # https://stackoverflow.com/questions/43093712/draw-a-map-of-a-specific-country-with-leaflet
                             canada <- readOGR(dsn = 'data', layer = 'gpr_000b11a_e')
                             ontario <- subset(canada, PRNAME == 'Ontario')
                             incProgress(1, 'generating map')
                             basemap <- leaflet(ontario)
                             incProgress(1, 'setting view')
                             basemap <- setView(basemap, lng = -85.3232, lat = 49, zoom = 6) 
                             incProgress(1, 'adding polygons')
                             basemap <- addPolygons(basemap, weight = 3)
                             incProgress(1, 'adding tiles')
                             basemap <- addProviderTiles(basemap, providers$Esri.NatGeoWorldMap)
                             incProgress(1, 'caching map')
                             save('basemap', file = basemap_cache_file)
                         }
                         
                         # add case data markers
                         incProgress(1, 'adding markers')
                         basemap <- addCircleMarkers(basemap,
                                                     data = cases_per_school, 
                                                     lng = ~lon, 
                                                     lat = ~lat, 
                                                     radius = ~(cases_per_school) * 2, # ~(cases_per_school)^(1/5), 
                                                     weight = 1, 
                                                     color = ~covid_col,
                                                     fillOpacity = 0.3, 
                                                     label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>Confirmed cases (cumulative): %s<br/></div>', 
                                                                     cases_per_school$school_name, 
                                                                     cases_per_school$city, 
                                                                     cases_per_school$school_level, 
                                                                     cases_per_school$school_board, 
                                                                     cases_per_school$school_language, 
                                                                     cases_per_school$school_enrolment, 
                                                                     cases_per_school$low_income, 
                                                                     cases_per_school$cases_per_school) %>% lapply(htmltools::HTML), 
                                                     labelOptions = labelOptions(
                                                         style = list('font-weight' = 'normal', padding = '3px 8px', 'color' = covid_col),
                                                         textsize = '15px', direction = 'auto'))
                         
                         basemap
                     })
        
    })
    
    # cumulative_plot ----------------------------------------------------------
    output$cumulative_plot <- renderPlot({
        # function to plot cumulative COVID cases by date
        plot_date <- input$plot_date
        dt <- as.Date(covid19_schools_summary[ , 'collected_date' ])
        d1 <- covid19_schools_summary[ , 'cumulative_school_related_cases' ]
        plot_df <- data.frame(date = dt, cases = d1)
        plot_df = subset(plot_df, date <= plot_date)
        g1 = ggplot(plot_df, aes(x = date, y = cases)) + 
            geom_line() + 
            geom_point(size = 1, alpha = 0.8) +
            ylab('cumulative cases') + 
            theme_bw() + 
            scale_colour_manual(values = c(covid_col)) +
            scale_y_continuous(labels = function(l) { as.integer(l) }) +
            theme(legend.title = element_blank(), 
                  legend.position = '', 
                  plot.title = element_text(size = 10), 
                  plot.margin = margin(5, 12, 5, 5))
        g1
    })
    
    # daily_summary ------------------------------------------------------------
    output$daily_summary <- renderTable({
        df <- covid19_schools_summary
        idx <- order(df$collected_date)
        df <- df[ idx, ]
        cn <- c(
            'collected_date', 
            # 'reported_date', 
            'current_schools_w_cases', 
            'current_schools_closed', 
            # 'current_total_number_schools',
            'new_total_school_related_cases', 
            # 'new_school_related_student_cases', 
            # 'new_school_related_staff_cases', 
            # 'new_school_related_unspecified_cases',
            # 'recent_total_school_related_cases', 
            # 'recent_school_related_student_cases',
            # 'recent_school_related_staff_cases', 
            # 'recent_school_related_unspecified_cases', 
            # 'past_total_school_related_cases', 
            # 'past_school_related_student_cases', 
            # 'past_school_related_staff_cases', 
            # 'past_school_related_unspecified_cases', 
            'cumulative_school_related_cases' 
            # 'cumulative_school_related_student_cases', 
            # 'cumulative_school_related_staff_cases', 
            # 'cumulative_school_related_unspecified_cases'
        )
        df <- df[ , cn ]
        idx <- which(df$collected_date <= as.Date(input$plot_date))
        idx <- max(idx)
        df <- df[ (idx - 1):idx, ]
        colnames(df) <- str_replace_all(colnames(df), '_', ' ')
        df1 <- reshape2::melt(apply(df[ , -1 ], 2, diff))
        df1$variable <- rownames(df1)
        colnames(df1) <- c('change', 'variable')
        df2 <- reshape2::melt(df[ 2, -1 ])
        df <- merge(df2, df1, on = 'variable', all = TRUE)
        df
    })
    
    # clean_date_reactive ------------------------------------------------------
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(input$plot_date), '%d %B %Y')
    })
    
    # reactive_case_count ------------------------------------------------------
    output$reactive_case_count <- renderText({
        idx <- max(which(covid19_schools_summary$collected_date <= as.Date(input$plot_date)))
        count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
        paste0(prettyNum(count, big.mark = ','), ' cases')
    })
    
    # school_related_cases_details_plot ----------------------------------------
    output$school_related_cases_details_plot <- renderPlotly({
        df <- covid19_schools_summary
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative School Related Cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative School Related Student Cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative School Related Staff Cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_unspecified_cases, name = 'Cumulative School Related Unspecified Cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_total_school_related_cases, name = 'New Total School Related Cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_student_cases, name = 'New School Related Student Cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_staff_cases, name = 'New School Related Staff Cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_unspecified_cases, name = 'New School Related Unspecified Cases', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Cumulative school-related cases',
                              xaxis = list(title = 'collected date'),
                              yaxis = list (title = 'cases'))
        fig
    })
    
    # schools_with_cases_plot --------------------------------------------------
    output$schools_with_cases_plot <- renderPlotly({
        df <- covid19_schools_summary[ , c('collected_date', 'current_schools_w_cases') ]
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~collected_date, y = ~current_schools_w_cases, name = 'Current schools with cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Schools with cases',
                              xaxis = list(title = 'collected date'),
                              yaxis = list (title = 'schools'))
        fig
    })
    
    # active_cases_by_municipality_plot ----------------------------------------
    output$active_cases_by_municipality_plot <- renderPlotly({
        active_cases_by_municipality <- tapply(covid19_schools_active$municipality,
                                               list(covid19_schools_active$collected_date,
                                                    covid19_schools_active$municipality),
                                               length)
        active_cases_by_municipality <- na.locf(active_cases_by_municipality)
        colidx <- order(active_cases_by_municipality[ nrow(active_cases_by_municipality), ], decreasing = TRUE)
        active_cases_by_municipality <- active_cases_by_municipality[ , colidx ] 
        active_cases_by_municipality <- data.frame(active_cases_by_municipality)
        active_cases_by_municipality <- data.frame(collected_date = as.Date(rownames(active_cases_by_municipality)), active_cases_by_municipality)
        rownames(active_cases_by_municipality) <- NULL
        df <- active_cases_by_municipality[ , 1:11 ]
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        nms <- colnames(df)[ -1 ]
        nms2 <- colnames(df)[ -1 ] %>% str_replace_all(., '\\.', ' ') %>% str_replace_all(., '\\s+', ' ') %>% str_trim
        code_str <- sprintf('
                            fig <- plot_ly(df, x = ~collected_date, y = ~%s, name = \'%s\', type = \'scatter\', mode = \'lines+markers\')
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
    
    # active_cases_by_board_plot -----------------------------------------------
    output$active_cases_by_board_plot <- renderPlotly({
        active_cases_by_board <- tapply(covid19_schools_active$school_board,
                                        list(covid19_schools_active$collected_date,
                                             covid19_schools_active$school_board),
                                        length)
        active_cases_by_board <- na.locf(active_cases_by_board)
        colidx <- order(active_cases_by_board[ nrow(active_cases_by_board), ], decreasing = TRUE)
        active_cases_by_board <- active_cases_by_board[ , colidx ] 
        active_cases_by_board <- data.frame(active_cases_by_board)
        active_cases_by_board <- data.frame(collected_date = as.Date(rownames(active_cases_by_board)), active_cases_by_board)
        rownames(active_cases_by_board) <- NULL
        df <- active_cases_by_board[ , 1:11 ]
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        nms <- colnames(df)[ -1 ]
        nms2 <- colnames(df)[ -1 ] %>% str_replace_all(., '\\.', ' ') %>% str_replace_all(., '\\s+', ' ') %>% str_trim
        code_str <- sprintf('
                            fig <- plot_ly(df, x = ~collected_date, y = ~%s, name = \'%s\', type = \'scatter\', mode = \'lines+markers\')
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
        fig <- fig %>% layout(title = 'Active school cases by school board (top 10)',
                              xaxis = list(title = 'collected date'),
                              yaxis = list (title = 'cases'))
        fig
    })
    
    # download_csv_button_1 ----------------------------------------------------
    output$download_csv_button_1 <- downloadHandler(
        filename = function() {
            paste('schoolcovidsummary_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_summary, file)
        }
    )
    
    # school_summary_data_dt ---------------------------------------------------
    output$school_summary_data_dt <- renderDT({
        df <- covid19_schools_summary
        colnames(df) <- str_replace_all(colnames(df), '_', ' ')
        datatable(
            df,
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip'
            ),
            rownames = FALSE,
            class = 'display'
        )
    })
    
    # download_csv_button_2 ----------------------------------------------------
    output$download_csv_button_2 <- downloadHandler(
        filename = function() {
            paste('schoolsactivecovidwithdemographics_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_active_with_demographics, file)
        }
    )
    
    # school_cases_demo_data_dt ------------------------------------------------
    output$school_cases_demo_data_dt <- renderDT({
        df <- covid19_schools_active_with_demographics
        df$board.name <- NULL
        df$school.name <- NULL
        colnames(df) <- str_replace_all(colnames(df), '_|\\.', ' ')
        datatable(
            df,
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip'
            ),
            rownames = FALSE,
            class = 'display'
        )
    })
}

# RUN THE APPLICATION ----------------------------------------------------------
shinyApp(ui = ui, server = server)
