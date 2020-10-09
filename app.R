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
    navbarPage(theme = shinytheme('flatly'), 
               collapsible = TRUE,
               'Ontario Schools COVID-19 Tracker', 
               id = 'nav',
               
               # tab: COVID-19 Mapper ------------------------------------------
               tabPanel('COVID-19 Mapper',
                        div(class='outer',
                            
                            # tag: stylesheet ----------------------------------
                            tags$head(includeCSS('styles.css')),
                            
                            # leaflet: basemap  --------------------------------
                            leafletOutput('basemap_leaflet', width = '100%', height = '100%'),
                            
                            # panel: controls ----------------------------------
                            absolutePanel(id = 'controls', 
                                          class = 'panel panel-default',
                                          top = 75, 
                                          left = 55, 
                                          width = 350, 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          height = 'auto',
                                          
                                          # cumulative_case_count_text ---------
                                          h3(textOutput('cumulative_case_count_text'), align = 'right'),
                                          
                                          # clean_date_reactive_text -----------
                                          h6(textOutput('clean_date_reactive_text'), align = 'right'),
                                          
                                          # cumulative_plot --------------------
                                          plotOutput('cumulative_plot', height = '130px', width = '100%'),
                                          
                                          # plot_date --------------------------
                                          sliderInput('plot_date',
                                                      label = h5('Select mapping date'),
                                                      min = as.Date(cv_min_date,'%Y-%m-%d'),
                                                      max = as.Date(current_date,'%Y-%m-%d'),
                                                      value = as.Date(current_date),
                                                      timeFormat = '%d %b',
                                                      animate = animationOptions(interval = 3000, loop = FALSE)),
                                          
                                          # daily_summary ----------------------
                                          h3('Daily Summary', align = 'right'),
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
                                    
                                    tabPanel('New school-related cases', 
                                             br(), 
                                             plotlyOutput('school_related_new_cases_details_plot')),
                                    
                                    tabPanel('Active school-related cases by municipality', 
                                             br(), 
                                             plotlyOutput('active_cases_by_municipality_plot')),
                                    
                                    tabPanel('Active school-related cases by school board', 
                                             br(), 
                                             plotlyOutput('active_cases_by_board_plot'))
                                    
                                    # tabPanel('Schools with cases', 
                                    #          br(), 
                                    #          plotlyOutput('schools_with_cases_plot'))
                                    
                                )
                            )
                        )
               ),
               
               # tab: Data -----------------------------------------------------
               tabPanel('Data',
                        tabsetPanel(
                            tabPanel('Summary of cases in schools', 
                                     h3('Summary of cases in schools'),
                                     br(), 
                                     downloadButton('download_csv_button_1', 'Download as CSV'),
                                     br(),
                                     DTOutput('school_summary_data_dt'),
                                     br(),
                                     'Adapted from data published by Government of Ontario: ', 
                                     a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', 'Summary of cases in schools')
                            ),
                            tabPanel('Schools with active cases and school demographic data', 
                                     h3('Schools with active cases and school demographic data'),
                                     br(), 
                                     downloadButton('download_csv_button_2', 'Download as CSV'),
                                     br(),
                                     DTOutput('school_cases_demo_data_dt'),
                                     br(),
                                     'Adapted from data published by Government of Ontario: ', 
                                     a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', 'Schools with active COVID-19 cases'),
                                     ', ',
                                     a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics', 'School information and student demographics')
                            )
                        )
               ),
               
               # tab: Data Dictionary and Data Sources -------------------------
               tabPanel('Data Dictionary and Data Sources',
                        tabsetPanel(
                            tabPanel('Summary of cases in schools', 
                                     h3('Summary of cases in schools'),
                                     br(), 
                                     DTOutput('school_summary_data_dictionary_dt'),
                                     br(),
                                     'Adapted from data published by Government of Ontario: ', 
                                     a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/7fbdbb48-d074-45d9-93cb-f7de58950418', 'Summary of cases in schools')
                            ),
                            tabPanel('Schools with active cases and school demographic data', 
                                     h3('Schools with active cases and school demographic data'),
                                     br(), 
                                     DTOutput('school_cases_demo_data_dictionary_dt'),
                                     br(),
                                     'Adapted from data published by Government of Ontario: ', 
                                     a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/8b6d22e2-7065-4b0f-966f-02640be366f2', 'Schools with active COVID-19 cases'),
                                     ', ',
                                     a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a', 'School information and student demographics')
                            )
                        )
               ),
               
               # tab: About this site ------------------------------------------
               tabPanel('About this site',
                        tags$div(),
                        h3('COVID-19 SCHOOL DASHBOARD KEY AIMS & INFORMATION'),
                        a(href = 'COVID19SchoolDashboard.com', 'COVID19SchoolDashboard.com'), ' reports and maps confirmed school-related cases of COVID-19 in publicly funded elementary and secondary schools in Ontario, Canada, and connects this to data on school social background characteristics (school-level demographic data).',
                        p('The main aim of this site is to provide real-time data visualization of affected schools for broad dissemination. This will help to increase transparency and understanding of the education scenario as it evolves. It will help school communities (parents, students, teachers and staff, leaders and administrators), community members and neighbours, education and health professionals, officials, researchers, media, and the general public.'),
                        h4('Why is this important?'),
                        p('The effects of COVID-19 are more severe on high-risk communities, populations, and schools. There are strong equity concerns. Visualizing COVID-19 case data with data on school social background characteristics will give us a better understanding of the composition of affected schools. In short, we will get closer to understanding the human dimension of COVID-19 on school populations.'),
                        h4('Update frequency'),
                        p('This site is automatically updated every weekday (excluding public holidays) following the release of school-related COVID-19 case data by the Ontario Ministry of Education. Cumulative totals represent all total cases reported to the Ministry of Education as of 5 September 2020, including resolved cases. The first school-related cases appeared in the dataset on 10 September 2020.'),
                        p('This site uses the latest publicly available data on school information and student demographics released by the Ontario Ministry of Education for school background characteristics. This dataset is updated by the Ministry monthly.'),
                        p('Click ', a(href = '', 'here'), ' for more information on data sources and definitions of terms.'),
                        h3('CONTEXT'),
                        p('Pandemic-related school closures in Ontario affected over 2 million elementary and secondary school students. The province’s first school closure announcement was issued on 12 March 2020 for an initial period from 14 March to 4 April 2020. This compelled all publicly funded elementary and secondary schools to close. Public school closures were extended another three times – first until 4 May, then 31 May, and finally until the end of June 2020.'),
                        p('Phased reopening of publicly funded schools in the province began on 8 September 2020 and continued until 21 September 2020, by which time all schools should have opened.'),
                        p('When interpreting the data, it is important to keep in mind that data on COVID-19 school-related cases prior to 21 September 2020 will reflect partial education system reopening. The first school-related cases appeared in the dataset on 10 September 2020.'),
                        h3('HOW TO NAVIGATE THE SITE'),
                        h4('COVID-19 School Mapper Tab'),
                        p('The first school-related cases appeared in the dataset on 10 September 2020. Affected schools are plotted by geocode on the map.'), 
                        h5('Bubbles'),
                        p('The size of the bubbles indicates the magnitude of cumulative cases at specific schools relative to others. The bigger the bubble, the more cumulative cases at that school – that is, the more it has been affected relative to other schools.'),
                        p('Hovering on a bubble reveals school-specific COVID-19 case data and school social background information. Currently, the bubbles show: cumulative cases, city, level, board, main language of instruction, enrolment, proportion of students from low-income households, ***** '),
                        p('View the Data Dictionary for definitions of these indicators.'),
                        h5('Quick view summary pane'),
                        tags$ul(
                            tags$li(tags$i('Cumulative Case Chart:'), ' Shows the number of cumulative school-related cases in Ontario. Use the slider to view the evolution since the first day of cases in the dataset. Selecting a specific date on the slider will adjust data in the Daily Summary accordingly.'),
                            tags$li(tags$i('Daily Summary:'), ' Summarizes cumulative school-related cases, new total school-related cases, current schools with cases, and current schools closed. It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.')
                        ),
                        h4('Plots Tab'),
                        p('These graphs provide an indication of the evolution of school-related cases over time. The first school-related cases appeared in the dataset on 10 September 2020. Currently, there are four graphs.'),
                        h5('Cumulative school-related cases'),
                        p('Shows all cumulative school-related cases, including resolved cases in Ontario and the breakdown of student cases, staff, and unspecified individual cases.'),
                        h5('New school-related cases'),
                        p('Shows all new school-related cases in Ontario, and the breakdown of student, staff, and unspecified cases.'),
                        h5('Active school-related cases by municipality'),
                        p('Shows the top 10 municipalities with active school-related cases.'),
                        h5('Active school-related cases by school board'),
                        p('Shows the top 10 school boards with active school-related cases.'),
                        h5('Slider'),
                        p('Keep the slider to the minimum date (10 Sept) to see the full evolution of cases up to the most current date for every graph.'),
                        h5('Tools for added functionality'),
                        p('Hover over the legend of each graph to access tools for added functionality: download graph as .PNG image file, zoom, pan, box select, lasso select, zoom in, zoom out, autoscale, reseat axes, toggle spike lines, show closest data on hover, compare data on hover.'),
                        p('"Compare data on hover" is especially useful to see and compare the number of cases on all lines in the graph on a specific date.'),
                        h4('Data Tab'),
                        h5('Summary of cases in schools'),
                        p('Presents raw data of cases in schools. Data table can be manipulated in ascending or descending order by variable of interest. Table can be downloaded as a .CSV file for independent analysis.'),
                        p('Variables included: collected date; reported date; current schools with cases; current schools closed; current total number of schools; new (total school-related cases; student; staff; unspecified); recent (total school-related cases; student; staff; unspecified); past (total school-related cases; student; staff; unspecified); cumulative (total school-related cases; student; staff; unspecified).'),
                        p('Recent and past case data available as from 1 October 2020. See ', a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/7fbdbb48-d074-45d9-93cb-f7de58950418', 'Summary of cases in schools')),
                        p('Schools with active cases and school demographic data'),
                        p('Use the search function to see if a specific school, board, or municipality has been affected.'),
                        h4('Data Dictionary and Data Sources Tab'),
                        h5('Data Dictionary'),
                        p('Lists all definitions of variables and terms used. Extracted from data dictionaries of original datasets sourced.'),
                        h5('Data Sources'),
                        p('Lists all publicly available data sources used to generate the COVID-19 School Dashboard.'),
                        h5('Source Code'),
                        p('Source code for this site can be found ', a(href = 'https://gitlab.com/br00t/ontario-covid19-dashboard', 'here')),
                        h3('AUTHORSHIP, ATTRIBUTIONS, CITATION'),
                        tags$ul(
                            tags$li(a(href = 'https://www.edu.uwo.ca/faculty-profiles/prachi-srivastava.html', 'Dr. Prachi Srivastava'), ', Associate Professor, Faculty of Education, University of Western Ontario, Canada.'),
                            tags$li(a(href = 'mailto:prachi.srivastava@uwo.ca', 'Prachi.srivastava@uwo.ca')),
                            tags$li(a(href = 'https://twitter.com/PrachiSrivas', '@PrachiSrivas')),
                            tags$li(a(href = 'https://orcid.org/0000-0003-4865-8963', 'ORCID iD: 0000-0003-4865-8963'))
                        ),
                        p('The technical development and design of the COVID-19 School Dashboard was done by an independent developer.'),
                        h4('Preliminary site structure based on:'),
                        tags$cite('Parker, E., & Leclerc, Q. (2020). COVID-19 tracker. [Web application]. ',  a(href = 'https://vac-lshtm.shinyapps.io/ncov_tracker/', 'https://vac-lshtm.shinyapps.io/ncov_tracker/')),
                        h4('Cite the COVID-19 School Dashboard as:'),
                        tags$cite('Srivastava, P. (2020). COVID-19 school dashboard (1.0 Oct 2020). [Web application]. ', a(href = 'http://covid19schooldashboard.com/', 'http://covid19schooldashboard.com/')),
                        br(), br(),
                        a(href = 'https://www.edu.uwo.ca', tags$img(src = 'uwo_logo.png'))
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
                             basemap <- addPolygons(basemap, weight = 3, fillColor = '#696969', opacity = 0.5)
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
                                                     label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>Students receiving special education services: %s%%<br/>First language not english: %s%%<br/>Immigrant from non-english country: %s%%<br/>Confirmed cases (cumulative): %s<br/></div>', 
                                                                     cases_per_school$school_name, 
                                                                     cases_per_school$city, 
                                                                     cases_per_school$school_level, 
                                                                     cases_per_school$school_board, 
                                                                     cases_per_school$school_language, 
                                                                     cases_per_school$school_enrolment, 
                                                                     cases_per_school$low_income, 
                                                                     cases_per_school$special_education, 
                                                                     cases_per_school$non_english, 
                                                                     cases_per_school$from_non_english, 
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
        plot_df <- data.frame(Date = dt, cases = d1)
        plot_df = subset(plot_df, Date <= plot_date)
        g1 = ggplot(plot_df, aes(x = Date, y = cases)) + 
            geom_line() + 
            geom_point(size = 1, alpha = 0.8) +
            ylab('Cumulative cases') + 
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
        colnames(df) <- c('Variable', 'Count', 'Change')
        idx <- which(df$Change > 0)
        df[ idx, 'Change' ] <- sprintf('+%s', df[ idx, 'Change' ])
        df
    }, align = 'r', striped = TRUE)
    
    # clean_date_reactive_text -------------------------------------------------
    output$clean_date_reactive_text <- renderText({
        format(as.POSIXct(input$plot_date), '%d %B %Y')
    })
    
    # cumulative_case_count_text -----------------------------------------------
    output$cumulative_case_count_text <- renderText({
        idx <- max(which(covid19_schools_summary$collected_date <= as.Date(input$plot_date)))
        count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
        paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
    })
    
    # school_related_cases_details_plot ----------------------------------------
    output$school_related_cases_details_plot <- renderPlotly({
        df <- covid19_schools_summary
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative school-related cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative school-related student cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative school-related staff cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_unspecified_cases, name = 'Cumulative school-related unspecified cases', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Cumulative school-related cases',
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'Cumulative cases'))
        fig
    })
    
    # school_related_new_cases_details_plot ----------------------------------------
    output$school_related_new_cases_details_plot <- renderPlotly({
        df <- covid19_schools_summary
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~collected_date, y = ~new_total_school_related_cases, name = 'New total school-related cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_student_cases, name = 'New school-related student cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_staff_cases, name = 'New school-related staff cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_unspecified_cases, name = 'New school-related unspecified cases', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'New school-related cases',
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'New cases'))
        fig
    })
    
    # schools_with_cases_plot --------------------------------------------------
    output$schools_with_cases_plot <- renderPlotly({
        df <- covid19_schools_summary[ , c('collected_date', 'current_schools_w_cases') ]
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~collected_date, y = ~current_schools_w_cases, name = 'Current schools with cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Schools with cases',
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'Schools'))
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
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'Active cases'))
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
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'Active cases'))
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
        colnames(df) <- str_to_title(colnames(df))
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
        colnames(df) <- str_to_title(colnames(df))
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
    
    # school_summary_data_dictionary_dt ----------------------------------------
    output$school_summary_data_dictionary_dt <- renderDT({
        field <- c('Collected Date',
                   'Reported Date',
                   'Current Schools W Cases',
                   'Current Schools Closed',
                   'Current Total Number Schools', 
                   'New Total School Related Cases',
                   'New School Related Student Cases',
                   'New School Related Staff Cases', 
                   'New School Related Unspecified Cases', 
                   'Recent Total School Related Cases', 
                   'Recent School Related Student Cases',
                   'Recent School Related Staff Cases',
                   'Recent School Related Unspecified Cases', 
                   'Past Total School Related Cases', 
                   'Past School Related Student Cases',
                   'Past School Related Staff Cases', 
                   'Past School Related Unspecified Cases', 
                   'Cumulative School Related Cases',
                   'Cumulative School Related Student Cases',
                   'Cumulative School Related Staff Cases',
                   'Cumulative School Related Unspecified Cases')
        description <- c('date results collected',
                         'date results reported',
                         'count of schools with active cases currently',
                         'count of schools closed',
                         'total number of schools in province',
                         'total new school-related cases of all types since last reporting date',
                         'new school-related student cases since last reporting date',
                         'new school-related staff cases since last reporting date',
                         'new school-related unspecified cases since last reporting date',
                         'total recent school-related cases',
                         'recent school-related student cases',
                         'recent school-related staff cases',
                         'recent school-related unspecified cases',
                         'total past school-related cases',
                         'past school-related student cases',
                         'past school-related staff cases',
                         'past school-related unspecified cases',
                         'cumulative total school-related cases',
                         'cumulative school-related student cases',
                         'cumulative school-related staff cases',
                         'cumulative school-related unspecified cases')
        df <- data.frame(field, description)
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
    
    # school_cases_demo_data_dictionary_dt -------------------------------------
    output$school_cases_demo_data_dictionary_dt <- renderDT({
        field <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
                   'Municipality', 'Confirmed Student Cases', 'Confirmed Staff Cases',
                   'Confirmed Unspecified Cases', 'Total Confirmed Cases',
                   'Board Number', 'Board Name', 'Board Type', 'School Number', 
                   'School Name', 'School Type', 'School Special Condition Code', 
                   'School Level', 'School Language', 'Grade Range', 'Street', 'City',
                   'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
                   'Percentage Of Students Whose First Language Is Not English', 
                   'Percentage Of Students Whose First Language Is Not French', 
                   'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
                   'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
                   'Percentage Of Students Identified As Gifted', 
                   'Percentage Of Students Receiving Special Education Services', 
                   'Percentage Of School Aged Children Who Live In Low Income Households', 
                   'Percentage Of Students Whose Parents Have Some University Education')
        description <- character(34)
        df <- data.frame(field, description)
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
