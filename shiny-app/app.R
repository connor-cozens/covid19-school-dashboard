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

# FUNCTIONS --------------------------------------------------------------------

# get_summary_table
#
# generate Daily summary table
get_summary_table <- function(givenDate) {
    df <- covid19_schools_summary
    idx <- order(df$collected_date)
    df <- df[ idx, ]
    cn <- c(
        'collected_date', 
        'cumulative_school_related_cases', 
        'new_total_school_related_cases', 
        'current_schools_with_cases', 
        'current_schools_closed'
    )
    #Take only the columns outlined in cn from covid19_schools_summary
    df <- df[ , cn ]
    if (givenDate == 0){
        idx <- which(df$collected_date <= as.Date(now()))
    }
    else{
        idx <- which(df$collected_date <= givenDate)
    }
    idx <- max(idx)
    #Take only the 2 most recently dated rows from df, the dataframe
    df <- df[ (idx - 1):idx, ]
    colnames(df) <- str_replace_all(colnames(df), '_', ' ')
    #Rearrange df as df1 such that the columns become rows (with values that are the difference between the two original rows)
    df1 <- reshape2::melt(apply(df[ , -1 ], 2, diff))
    df1$variable <- rownames(df1)
    colnames(df1) <- c('change', 'variable')
    #Rearrange df as df2 such that the columns become rows (values are the more recent of the two entries)
    df2 <- reshape2::melt(df[ 2, -1 ])
    #Merge df1 and df2 together (row names, most recent value, change between the today and last)
    df <- merge(df2, df1, on = 'variable', all = TRUE)
    colnames(df) <- c('Variable', 'Count', 'Change')
    idx <- which(df$Change > 0)
    df[ idx, 'Change' ] <- sprintf('+%s', df[ idx, 'Change' ])
    df$Variable <- str_to_sentence(df$Variable)
    df$Variable <- str_replace_all(df$Variable, ' w ', ' with ')
    df$Variable <- str_replace_all(df$Variable, 'school related', 'school\\-related')
    #Add in %'s for Current Schools with Cases and Current Schools Closed
    schools_count <- max(covid19_schools_summary$current_total_number_schools)
    df$Percentage <- NA
    df[ 2, 'Percentage' ] <- round(df[ 2, 'Count' ] / schools_count, 4) * 1e2
    df[ 3, 'Percentage' ] <- round(df[ 3, 'Count' ] / schools_count, 4) * 1e2
    df <- df[ c(1, 4, 3, 2), ]
    
    #Add in schools with >5 cases
    df2 <- data.frame("Current schools with >5 cases", sum(cases_per_school$cases_per_school >5), "NA", round(sum(cases_per_school$cases_per_school > 5) / schools_count, 4) * 1e2)
    names(df2) <- c("Variable", "Count", "Change", "Percentage")
    df <- rbind(df,df2)
    #Add in schools with >1 cases
    df3 <- data.frame("Current schools with >1 cases", sum(cases_per_school$cases_per_school > 1), "NA", round(sum(cases_per_school$cases_per_school > 1) / schools_count, 4) * 1e2)
    names(df3) <- c("Variable", "Count", "Change", "Percentage")
    df <- rbind(df,df3)
    df <- df[ c(1, 2, 3, 5, 6, 4), ]
    #Return the table
    df
}

# last_week_obtain
#
# Obtains the dates (Year-Month-Day) for the previous weeks Monday and Friday
last_week_obtain <- function(givenDate) {
    if (givenDate == 0){
        theDate <- as.Date(max(covid19_schools_active$reported_date)) - 7
        while(weekdays(theDate) != "Friday"){
            theDate <- theDate + 1
        }
        earlyDate <- theDate - 4
        df <- list(earlyDate, theDate)
    }
    else{
        while(weekdays(givenDate) != "Friday"){
            givenDate <- givenDate + 1
        }
        earlyDate <- givenDate - 4
        df <- list(earlyDate, givenDate)
    }
    return(df)
}

# last_two_weeks_obtain
# 
# Obtains the dates (Year-Month-Day) for 2 weeks ago Monday and last weeks Friday
last_two_weeks_obtain <- function(givenDate) {
    if (givenDate == 0){
        theDate <- as.Date(max(covid19_schools_active$reported_date)) - 7
        while(weekdays(theDate) != "Friday"){
            theDate <- theDate + 1
        }
        earlyDate <- theDate - 11
        df <- list(earlyDate, theDate)
    }
    else{
        while(weekdays(givenDate) != "Friday"){
            givenDate <- givenDate + 1
        }
        earlyDate <- givenDate - 11
        df <- list(earlyDate, givenDate)
    }
    return(df)
}

# get_weekly_summary_table
# 
# generate Weekly summary table
# similar in function to the daily summary table, but with differences calculated over 7 or 14 days instead of 2
get_weekly_summary_table <- function(timeFrame, givenDate) {
    df <- covid19_schools_summary
    idx <- order(df$collected_date)
    df <- df[ idx, ]
    cn <- c(
        'collected_date', 
        'cumulative_school_related_cases', 
        'current_schools_with_cases', 
        'current_schools_closed'
    )
    df <- df[ , cn ]
    #if timeFrame == TRUE, 7 days view, otherwise 14 days view
    if (timeFrame == TRUE) {
        dates <- last_week_obtain(givenDate)
    }
    if (timeFrame == FALSE) {
        dates <- last_two_weeks_obtain(givenDate)
    }
    
    idx1 <- match(dates[[1]], df[,1])
    idx2 <- match(dates[[2]], df[,1])
    df <- rbind(df[idx1,], df[idx2,])
    
    colnames(df) <- str_replace_all(colnames(df), '_', ' ')
    
    df1 <- reshape2::melt(apply(df[ , -1 ], 2, diff))
    df1$variable <- rownames(df1)
    colnames(df1) <- c('change', 'variable')
    df2 <- reshape2::melt(df[ 2, -1 ])
    df <- merge(df2, df1, on = 'variable', all = TRUE)
    colnames(df) <- c('Variable', 'Count', 'Change')
    idx <- which(df$Change > 0)
    df[ idx, 'Change' ] <- sprintf('+%s', df[ idx, 'Change' ])
    df$Variable <- str_to_sentence(df$Variable)
    df$Variable <- str_replace_all(df$Variable, ' w ', ' with ')
    df$Variable <- str_replace_all(df$Variable, 'school related', 'school\\-related')
    schools_count <- max(covid19_schools_summary$current_total_number_schools)
    df$Percentage <- NA
    df[ 2, 'Percentage' ] <- round(df[ 2, 'Count' ] / schools_count, 4) * 1e2
    df[ 3, 'Percentage' ] <- round(df[ 3, 'Count' ] / schools_count, 4) * 1e2
    df <- df[ c(1, 3, 2), ]
    df
}

# get_schools_no_cases
#
# return dataframe from school_geocodes with schools that have cases removed from it.
get_schools_no_cases <- function() {
    df <- cases_per_school
    cn <- c(
        'school_name',
        'city'
    )
    df <- df[ , cn]
    df2 <- school_demographics
    
    df2 <- df2[!(df2$'school name' %in% df$school_name), , drop = FALSE]
    return (df2)
}

# get_schools_no_cases_20_21
# 
# return dataframe from school_geocodes with schools that have cases removed from it. Data from 2020/2021
get_schools_no_cases_20_21 <- function() {
    df <- cases_per_school_20_21
    cn <- c(
        'school_name',
        'city'
    )
    df <- df[ , cn]
    df2 <- school_demographics_20_21
    
    df2 <- df2[!(df2$'school name' %in% df$school_name), , drop = FALSE]
    return (df2)
}

# OVERRIDES --------------------------------------------------------------------
#CSS style override for navbar_js
navbar_js <- "@media (max-width: 1325px) {
    .navbar-header {
        float: none;
    }
    .navbar-left,.navbar-right {
        float: none !important;
    }
    .navbar-toggle {
        display: block;
    }
    .navbar-collapse {
        border-top: 1px solid transparent;
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.1);
    }
    .navbar-fixed-top {
        top: 0;
        border-width: 0 0 1px;
    }
    .navbar-collapse.collapse {
        display: none!important;
    }
    .navbar-nav {
        float: none!important;
        margin-top: 7.5px;
    }
    .navbar-nav>li {
        float: none;
    }
    .navbar-nav>li>a {
        padding-top: 10px;
        padding-bottom: 10px;
    }
    .collapse.in{
        display:block !important;
    }
}"

# INITIALIZATION ---------------------------------------------------------------

# SHINY UI ---------------------------------------------------------------------
ui <- bootstrapPage(
    tags$head(includeHTML('gtag.html'),
              tags$style(HTML("
                              @import url('https://fonts.googleapis.com/css2?family=Nunito+Sans&display=swap');
                              p { font-family: 'Nunito Sans';},
                              h1 { font-family: 'Nunito Sans';},
                              h2 { font-family: 'Nunito Sans';},
                              h3 { font-family: 'Nunito Sans';},
                              h4 { font-family: 'Nunito Sans';},
                              "))),
    navbarPage(theme = shinytheme('united'), 
               collapsible = TRUE,
               'COVID-19 School Dashboard', 
               id = 'nav',
               
               # TAB: COVID-19 Mapper 2021-22 ----------------------------------
               tabPanel('Map - 2021-22',
                        div(class='outer',
                            
                            # tag: stylesheet ----------------------------------
                            tags$head(includeCSS('styles.css')),
                            
                            # leaflet: basemap  --------------------------------
                            leafletOutput('basemap_leaflet', width = '100%', height = '100%'),
                            
                            # panel: button: viewOptions
                            absolutePanel(id = 'viewOptions',
                                          class = 'panel panel-default',
                                          top = "0%", 
                                          right = "0%", 
                                          width = 'auto', 
                                          #fixed = TRUE,
                                          draggable = FALSE, 
                                          height = 'auto',
                                          actionButton("getOptions", "Viewing Options", icon("cog"))
                            ),
                            
                            uiOutput('mapperViewOptions'),
                            
                            
                            
                            
                            # panel: controls ----------------------------------
                            absolutePanel(id = 'controls', 
                                          class = 'panel panel-default',
                                          top = "5%", 
                                          left = 55, 
                                          width = 500, 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          height = 'auto',
                                          
                                          tags$style(HTML(".tabbable > .nav > li[class=active] > a {color:#e95420;}")),
                                          tags$style(HTML(".tabbable > .nav > li > a {color:#777777;}")),
                                          
                                          tabsetPanel(
                                              tabPanel(id = "daily",
                                                       
                                                       h2('Daily Summary', align = 'right', style="font-size:150%;"),
                                                       
                                                       # cumulative_case_count_text ---------
                                                       h3(textOutput('cumulative_case_count_text'), align = 'right'),
                                                       
                                                       # clean_date_reactive_text -----------
                                                       h6(div('Data reported on'), textOutput('clean_date_reactive_text'), align = 'right'),
                                                       
                                                       # daily_summary_1_dt -----------------
                                                       div(tableOutput('daily_summary_1_dt'), style = 'font-size: small; width: 100%'),
                                                       
                                                       h6('Drag this box to move it', align = 'right')
                                              ),
                                              tabPanel(id = "weekly",
                                                       
                                                       h2('Weekly Summary', align = 'right', style="font-size:150%;"),
                                                       
                                                       # weeklyRadio -----------
                                                       div(
                                                           radioButtons(
                                                               inputId = "weeklyRadio",
                                                               label = strong("Select a timeframe:"),
                                                               choices = list("7-day view", "14-day view"),
                                                               inline = TRUE
                                                           ), align = "right"),
                                                       
                                                       #whichWeekView ----------
                                                       uiOutput("whichWeekView")
                                                       
                                              )
                                          )
                                          
                                          
                            ),
                            
                            
                            
                        ),
                        
                        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: teal}")),
                        # TIMESLIDER -------------------------------------------
                        uiOutput('timesliderViewer')
                        
               ),
               # TAB: COVID-19 Mapper 2020-21 ----------------------------------
               tabPanel('Map - 2020-21',
                        div(class='outer',
                            
                            # tag: stylesheet ----------------------------------
                            tags$head(includeCSS('styles.css')),
                            
                            # leaflet: map20_21  ---------------------------------
                            leafletOutput('map_leaflet20_21', width = '100%', height = '100%'),
                            
                            # panel: button: viewOptions20_21 --------------------
                            absolutePanel(id = 'viewOptions20_21',
                                          class = 'panel panel-default',
                                          top = "0%", 
                                          right = "0%", 
                                          width = 'auto',
                                          draggable = FALSE, 
                                          height = 'auto',
                                          actionButton("getOptions20_21", "Viewing Options", icon("cog"))
                            ),
                            
                            uiOutput('mapperViewOptions20_21'),
                            
                            # panel: controls ----------------------------------
                            absolutePanel(id = 'controls', 
                                          class = 'panel panel-default',
                                          top = "5%", 
                                          left = 55, 
                                          width = 300, 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          height = 'auto',
                                          
                                          tags$style(HTML(".tabbable > .nav > li[class=active] > a {color:#e95420;}")),
                                          tags$style(HTML(".tabbable > .nav > li > a {color:#777777;}")),
                                          h2('Year Summary', align = 'center', style="font-size:200%;"),
                                          
                                          # cumulative_case_count_text_20_21 ---
                                          h3(textOutput('cumulative_case_count_text_20_21'), align = 'right'),
                                          
                                          # clean_date_reactive_text -----------
                                          h6(div('Data last reported on'), textOutput('clean_date_reactive_text_20_21'), align = 'right'),
                                          
                                          h6('Drag this box to move it', align = 'right')
                            )
                        ),
                        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: teal}")),
                        # TIMESLIDER -------------------------------------------
                        uiOutput('timesliderViewer20_21')
               ),
               # TAB: Overview and Search --------------------------------------
               tabPanel('Overview & Search',
                        tabsetPanel(
                            tabPanel('2021-2022',
                                     # cumulative_plot -------------------------
                                     h3('Cumulative Case Chart'),
                                     plotlyOutput('cumulative_plot', width = '100%'),
                                     hr(),
                                     # daily_summary_2_dt ----------------------
                                     h3('Daily Summary', align = 'left'),
                                     div(tableOutput('daily_summary_2_dt'), style = 'font-size: small; width: 100%'),
                                     hr(),
                                     # weeklyRadio2 ----------------------------
                                     h3('Weekly Summary', align = 'left'),
                                     div(
                                         radioButtons(
                                             inputId = "weeklyRadio2",
                                             label = strong("Select a timeframe:"),
                                             choices = list("7-day view", "14-day view"),
                                             inline = TRUE
                                         ), align = "right"),
                                     
                                     #whichWeekView2 ---------------------------
                                     uiOutput("whichWeekView2"),
                                     hr(),
                                     # school_details_dt -----------------------
                                     h3('Search Function and Table', align = 'left'),
                                     div('Search schools, boards, municipalities for confirmed cases of COVID-19.', width = '100%', align = 'left'),
                                     br(),
                                     div(DTOutput('school_details_dt'), style = 'font-size: small; width: 100%')
                            ),
                            tabPanel('2020-2021',
                                     # cumulative_plot_20_21 -------------------
                                     h3('Cumulative Case Chart'),
                                     plotlyOutput('cumulative_plot_20_21', width = '100%'),
                                     hr(),
                                     # school_details_dt_20_21 -----------------
                                     h3('Search Function and Table', align = 'left'),
                                     div('Search schools, boards, municipalities for confirmed cases of COVID-19.', width = '100%', align = 'left'),
                                     br(),
                                     div(DTOutput('school_details_dt_20_21'), style = 'font-size: small; width: 100%')
                            )
                            
                        )
                        
               ),
               
               # TAB: Data Tables & Data Dictionary ----------------------------
               tabPanel('Data Tables & Data Dictionary',
                        tabsetPanel(
                            tabPanel('2021-2022',
                                     br(),
                                     tabsetPanel(
                                         tabPanel('Summary of cases in schools', 
                                                  h3('Summary of cases in schools'),
                                                  br(), 
                                                  downloadButton('download_csv_button_1', 'Download as CSV'),
                                                  br(),
                                                  br(),
                                                  p('Scroll down to see data dictionary of terms for this table.'),
                                                  DTOutput('school_summary_data_dt'),
                                                  br(),
                                                  'Adapted from data published by Government of Ontario: ', 
                                                  a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', target = '_blank', 'Summary of cases in schools'),
                                                  br(), 
                                                  hr(),
                                                  h3('Data dictionary'),
                                                  DTOutput('school_summary_data_dictionary_dt')
                                         ),
                                         tabPanel('Schools with active cases and school demographic data', 
                                                  h3('Schools with active cases and school demographic data'),
                                                  br(), 
                                                  downloadButton('download_csv_button_2', 'Download as CSV'),
                                                  br(),
                                                  br(),
                                                  p('Scroll down to see data dictionary of terms for this table.'),
                                                  DTOutput('school_cases_demo_data_dt'),
                                                  br(),
                                                  'Adapted from data published by Government of Ontario: ', 
                                                  a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', target = '_blank', 'Schools with active COVID-19 cases'),
                                                  ', ',
                                                  a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics', target = '_blank', 'School information and student demographics'),
                                                  br(), 
                                                  hr(),
                                                  h3('Data dictionary'),
                                                  DTOutput('school_cases_demo_data_dictionary_dt')
                                         )
                                     )
                            ),
                            tabPanel('2020-2021',
                                     br(),
                                     tabsetPanel(
                                         tabPanel('Summary of cases in schools', 
                                                  h3('Summary of cases in schools'),
                                                  br(), 
                                                  downloadButton('download_csv_button_1_20_21', 'Download as CSV'),
                                                  br(),
                                                  br(),
                                                  p('Scroll down to see data dictionary of terms for this table.'),
                                                  DTOutput('school_summary_data_dt_20_21'),
                                                  br(),
                                                  'Adapted from data published by Government of Ontario during the 2020-2021 academic year: ', 
                                                  a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', target = '_blank', 'Summary of cases in schools'),
                                                  br(), 
                                                  hr(),
                                                  h3('Data dictionary'),
                                                  DTOutput('school_summary_data_dictionary_dt_20_21')
                                         ),
                                         tabPanel('Schools with active cases and school demographic data', 
                                                  h3('Schools with active cases and school demographic data'),
                                                  br(), 
                                                  downloadButton('download_csv_button_2_20_21', 'Download as CSV'),
                                                  br(),
                                                  br(),
                                                  p('Scroll down to see data dictionary of terms for this table.'),
                                                  DTOutput('school_cases_demo_data_dt_20_21'),
                                                  br(),
                                                  'Adapted from data published by Government of Ontario during the 2020-2021 academic year: ', 
                                                  a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', target = '_blank', 'Schools with active COVID-19 cases'),
                                                  ', ',
                                                  a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics', target = '_blank', 'School information and student demographics'),
                                                  br(), 
                                                  hr(),
                                                  h3('Data dictionary'),
                                                  DTOutput('school_cases_demo_data_dictionary_dt_20_21')
                                         )
                                     )
                            )
                        )
                        
               ),
               
               # TAB: Data Sources and Source Code -----------------------------
               tabPanel('Data Sources & Code',
                        h3('Data Sources 2021-22'),
                        tags$ul(
                            tags$li(a(href = 'https://data.ontario.ca/dataset?keywords_en=COVID-19', 'All COVID-19 datasets', target = '_blank')),
                            tags$li(a(href = 'https://www.ontario.ca/page/covid-19-cases-schools-and-child-care-centres', 'COVID-19 cases in schools and child care centres', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', 'Schools COVID-19 data overview', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/new_sif_data_table_2018_2019prelim_en_august.xlsx', ' School information and student demographics dataset (.xlsx)', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics', ' School information and student demographics overview', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv', 'Schools with active COVID-19 cases dataset (.csv)', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/7fbdbb48-d074-45d9-93cb-f7de58950418/download/schoolcovidsummary.csv', 'Summary of cases in schools dataset (.csv)', target = '_blank'))
                        ),
                        h3('Data 2020-21'),
                        downloadButton('download_csv_button_3_20_21', 'schoolcovidsummary20_21.csv'),
                        h3('Source Code'),
                        p('Source code for this site can be found ', a(href = 'https://github.com/connor-cozens/covid19-school-dashboard', 'here', target = '_blank')),
                        h3('Archive'),
                        p('The archival material, code for the site can be found ', a(href = 'https://doi.org/10.5683/SP3/Z9SNP0', 'here', target = '_blank'), 'and the integrated dataset can be found ', a(href = 'https://doi.org/10.5683/SP3/D0QXGQ', 'here.', target = '_blank'))
               ),
               
               # TAB: About this site ------------------------------------------
               tabPanel('About This Site',
                        absolutePanel(id = 'contents', 
                                      class = 'panel panel-default',
                                      top = '20%', 
                                      left = '75%', 
                                      width = 200, 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      height = 'auto',
                                      tags$style("#contents {background-color: #eeeeee;padding: 5px 5px 15px 5px;border: 1px solid black;}"),
                                      
                                      div(align = "center",
                                          h3(tags$u('Navigation')),
                                          a(href = "#Top of Page", 'Top of Page'),
                                          br(),
                                          a(href = "#Policy Context", ' Policy Context'),
                                          br(),
                                          a(href = "#2021-2022", ' 2021-2022 School Year'),
                                          br(),
                                          a(href = "#2020-2021", ' 2020-2021 School Year'),
                                          br(),
                                          a(href = "#2019-2020", ' 2019-2020 School Year'),
                                          br(),
                                          a(href = "#Site Navigation", ' Site Navigation'),
                                          br(),
                                          a(href = "#Future Developments", ' Future Developments'),
                                          br(),
                                          a(href = "#Authorship", ' Authorship'),
                                          br()
                                      )
                        ),
                        tags$div(),
                        # Overview ---------------------------------------------
                        div(
                            h3(id = "Top of Page", 'COVID-19 SCHOOL DASHBOARD KEY AIMS & INFORMATION'),
                            p(a(href = 'http://covid19schooldashboard.com', 'covid19schooldashboard.com', target = '_blank'), ' reports and maps confirmed school-related cases of COVID-19 in publicly funded elementary and secondary schools in Ontario, Canada, and connects this to data on school social background characteristics (school-level demographic data). The site covers the period September 2020 to June 2021 and September 2021 to December 2021, the last date for which school infection data for Ontario are publicly available.'),
                            p('The main aim of this site is to provide real-time data visualization of affected schools for broad dissemination. This will help to increase transparency and understanding of the education scenario as it evolves. It will help school communities (e.g., parents, students, teachers and staff, leaders and administrators), community members and neighbours, education and health professionals, officials, researchers, media, and the general public.'),
                            p('The site is best viewed on a desktop or tablet.'),
                            br(),
                            h4('Why is this important?'),
                            p('The effects of COVID-19 are more severe on high-risk communities, populations, and schools. There are strong equity concerns. Visualizing COVID-19 case data with data on school social background characteristics will give us a better understanding of the composition of affected schools.'),
                            p('In short, we will get closer to understanding the human dimension of COVID-19 on school populations.'),
                            br(),
                            h4('Update frequency'),
                            p('This site is no longer automatically updated as the ', a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', 'Ministry of Education ceased reporting data on school infections as of January 2022.', target = '_blank'), ' From September 2020 to December 2021, this site was automatically updated every weekday (excluding public holidays) following the release of school-related COVID-19 case data by the Ontario Ministry of Education. This site also used the latest publicly available data on school information and student demographics released by the Ontario Ministry of Education for school background characteristics.'),
                            br(),
                            h4('Archive of the COVID-19 School Dashboard'),
                            p('The archival material, code for the site can be found ', a(href = 'https://www.google.com/url?q=https://doi.org/10.5683/SP3/Z9SNP0&sa=D&source=docs&ust=1665177381441979&usg=AOvVaw1BTM0Gh6Z4wNMXF5sPbP3y', 'here', target = '_blank'), ', and the integrated dataset can be found ', a(href = 'https://www.google.com/url?q=https://doi.org/10.5683/SP3/D0QXGQ&sa=D&source=docs&ust=1665177381442561&usg=AOvVaw0P4TWDPfxDqe56k1Dzj9vT', 'here.', target = '_blank') ),
                            p('See also "Data Sources and Source Code" tab for more information on data sources used.'),
                            br(),
                            p(tags$b('2021-22 School Year')),
                            p('Cumulative totals represent all total cases reported as of 23 December 2021. This report provides a summary of COVID-19 activity in Ontario schools. Cumulative totals represent all total cases reported to the Ministry of Education as of 23 December 2021, including resolved cases. The data were available for public access and download as on 14 September 2021. The first reported date of school-level cases was 26 August 2021. The last reported date of school-level cases was 23 December 2021. No public data were made available by the Ministry as of January 2022.'),
                            p(tags$b('2020-21 School Year')),
                            p('Cumulative totals represent all total cases reported to the Ministry of Education as of 5 September 2020, including resolved cases. The first school-related cases appeared in the dataset on 10 September 2020.'),
                            p('The date shown in the Daily/Weekly Summary pane on the Mapper tab reports the last day on which official data were released by the Ontario Ministry of Education. This was 27 April 2021 for the 2020-21 school year.'),
                            br(),
                            h4('Caveats'),
                            p('The main aim of the COVID-19 School Dashboard is to show which schools are affected by confirmed cases as reported in the official data, visually plot where the schools are, and show relevant school background characteristics of affected schools. This site should not be used to draw inferences on the broader COVID-19 situation in Ontario, or on case numbers generally. A number of complementary metrics are useful in that regard.'),
                            p('The numbers of cases are extracted from official data sources. A number of contextual factors will affect data changes. The following is an informational list of potential relevant factors. It is not exhaustive. For example, changing testing scenarios can mean that as the frequency of testing increases or decreases, threshold of symptoms is widened or restricted, and backlog of results clears or increases, the number of new cases may show spikes or dips. As the situation evolves, vaccination rates and mass and partial school closures and reopening, amongst other factors, affect changes in data. '),
                            p('There are known lags in data reported in the Ministry of Education dataset, which may result in real-time discrepancies.'),
                            p('There may be some discrepancies in school demographic data if they are in the official dataset.'),
                            hr()
                        ),
                        # Policy -----------------------------------------------
                        div(
                            h3(id = "Policy Context", 'POLICY CONTEXT'),
                            p('Pandemic-related school closures in Ontario affected over 2 million elementary and secondary school students. The situation for students and schools evolved rapidly.'),
                            p('The following provides a brief policy context of provincial policy responses on school closures and reopening. It does not outline decisions of individual school boards or regional public health units (PHUs), unless they were named in provincial announcements.'),
                            p('The figure below shows the main Ontario-level school closure and re-opening periods from September 2020 – April 2021. Schools did not reopen for face-to-face instruction for the remainder of the school year (30 June 2021). There were exceptions for schools and programs serving children with special needs.'),
                            img(src='timeline.png', width='100%'),
                            tags$style("#subnote"),
                            p(id = "subnote", tags$b('Figure 1 Ontario-level school closures and reopening policy tracing (March 2020 – April 2021)')),
                            p(id = "subnote", 'Cite as: Srivastava, P., Taylor, P.J. (2021). COVID-19 school dashboard (1.1 May 2021). [Web application]. http://covid19schooldashboard.com/'),
                            p(id = "subnote", 'Notes. *: School closures: defined here as the suspension of in-school, face-to-face instruction. Public schools only. Special provisions for face-to-face instruction were made for special education needs services. These are not presented here. Only provincial-level decisions are presented. Individual school boards and PHUs may have additionally instituted localised school closures. These are not presented here.'),
                            p(id = "subnote", tags$sup("1"), ': All schools in PHUs of Algoma, North Bay Parry Sound, Northwestern, Porcupine, Sudbury, Thunder Bay, Timiskaming. ', tags$sup("2"), ': All schools in PHUs of Grey Bruce, Haliburton, Kawartha, Pine Ridge, Hastings and Prince Edward Counties, Kingston, Frontenac and Lennox & Addington, Leeds, Grenville and Lanark, Peterborough, Renfrew County. ', tags$sup("3"), ': All schools in PHUs of Eastern Ontario, Middlesex-London, Ottawa, Southwestern. ', tags$sup("4"),': All schools in PHUs of: Brant County, Chatham-Kent, Durham, Haldimand-Norfolk, Halton, Hamilton, Huron Perth, Lambton, Niagara, Simcoe-Muskoka, Waterloo, Wellington-Dufferin-Guelph, Windsor-Essex. ', tags$sup("5"),': All schools in PHUs of: Peel, Toronto, York.'),
                            p(id = "subnote", 'Source. Data extracted from official provincial government announcements  and verified on ICES COVID-19 Dashboard.'),
                            p(id = "subnote", tags$br(), tags$sup("i"), 'https://news.ontario.ca/en/release/59790/ontario-announces-provincewide-shutdown-to-stop-spread-of-covid-19-and-save-lives'),
                            p(id = "subnote", 'https://news.ontario.ca/en/statement/60033/over-100000-ontario-students-return-to-class-beginning-next-week'),
                            p(id = "subnote", 'https://news.ontario.ca/en/statement/60154/280000-more-ontario-students-to-return-to-class'),
                            p(id = "subnote", 'https://news.ontario.ca/en/release/60228/enhanced-safety-measures-in-place-as-in-person-learning-resumes-across-ontario'),
                            p(id = "subnote", 'https://news.ontario.ca/en/release/60228/enhanced-safety-measures-in-place-as-in-person-learning-resumes-across-ontario'),
                            p(id = "subnote", 'https://news.ontario.ca/en/release/61106/ontario-moves-schools-to-remote-learning-following-spring-break'),
                            br()
                        ),
                        # News by Year -----------------------------------------
                        div(
                            h4(id = "2021-2022", '2021-22 School Year'),
                            p('Schools operating on a modified/balanced calendar opened as early as 4 August 2021. The majority of schools opened according to regular board-level conventions from 7 to 10 September 2021. All schools should have been opened as on 13 September 2021 for the regular school year.. On 3 January 2022, it was announced schools would not reopen for in-person instruction until 17 January 2022. This was the only systems-wide closure in the 2021-22 school year.'),
                            br(),
                            h4(id = "2020-2021", '2020-21 School Year'),
                            p('Phased reopening of publicly funded schools in Ontario began on 8 September 2020 and continued until 21 September 2020, by which time all schools should have opened. This followed a period of province-wide and localised school closures. Schools operated virtually as of 19 April 2021 for the remainder of the school year (end June 2021), with special in-person provisions for special education needs services.'),
                            p('At the provincial level, the winter break commenced as on 21 December 2020 for an initial planned return to in-person instruction on 4 January 2021 for elementary and secondary schools.'),
                            p('On 21 December 2020, the provincial government announced a ', a(href = 'https://news.ontario.ca/en/release/59790/ontario-announces-provincewide-shutdown-to-stop-spread-of-covid-19-and-save-lives', target = '_blank', 'province-wide shutdown'), '. Virtual learning was announced for all schools for the period 4-8 January 2021.'),
                            p('Return to in-person instruction was planned for elementary and secondary schools on 11 January 2021 in the following public health units: Algoma, North Bay Parry Sound, Northwestern, Porcupine, Sudbury and District, Thunder Bay, Timiskaming.'),
                            p('For the rest of the province, return to in-person instruction was planned for 11 January for elementary schools and 25 January 2021 for secondary schools.'),
                            p('On 7 January 2020, the provincial government ', a(href = 'https://news.ontario.ca/en/release/59890/ontario-extends-teacher-led-online-learning-until-january-25-to-keep-students-and-staff-safe-in-sout', target = '_blank', 'extended the province-wide shutdown'), '. This extended virtual instruction for all elementary schools in Ontario until 25 January 2021. It  extended the shutdown in Northern Ontario, aligning with the shutdown in Southern Ontario.'),
                            p('On 12 January 2021, the provincial government issued a ', a('second state of emergency', href = 'https://news.ontario.ca/en/release/59922/ontario-declares-second-provincial-emergency-to-address-covid-19-crisis-and-save-lives', target = '_blank'), '. This included a further extension for virtual learning for all elementary and secondary schools in four regions: Hamilton, Peel, Toronto, Windsor-Essex, and York, until 10 February 2021.'),
                            p('On 20 January 2021, the Government announced that ', a(href = 'https://news.ontario.ca/en/statement/60033/over-100000-ontario-students-return-to-class-beginning-next-week', target='_blank', 'elementary and secondary schools in the following PHUs would be permitted to resume in-person learning on 25 January, 2021'), ': Grey Bruce Health Unit; Haliburton, Kawartha, Pine Ridge District Health Unit; Hastings and Prince Edward Counties Health Unit; Kingston, Frontenac and Lennox & Addington Health Unit;  Leeds, Grenville and Lanark District Health Unit; Peterborough Public Health; Renfrew County and District Health Unit.'),
                            p('All schools in the following 7 schools boards could resume in-person learning on January 25: Limestone District School Board; Renfrew County District School Board; Hastings and Prince Edward District School Board; Bruce-Grey Catholic District School Board; Renfrew County Catholic District School Board; Algonquin and Lakeshore Catholic District School Board; and Bluewater District School Board.'),
                            p('Some additional school boards (9) that span multiple PHUs could have some schools that resume in-person: Kawartha Pine Ridge District School Board; Trillium Lakelands District School Board; Upper Canada District School Board; Catholic District School Board of Eastern Ontario; Peterborough Victoria Northumberland and Clarington Catholic DSB; Conseil des écoles publiques de l`Est de l`Ontario; Conseil scolaire catholique Providence; Conseil scolaire catholique Mon Avenir; Conseil scolaire de district catholique du Centre-Est de l\'Ontario.'),
                            p('Northern PHUs that were permitted to return to in-person learning on 11 January 2021 would continue in-person learning unless otherwise directed by local PHUs.'),
                            p('On 28 January 2021 it was announced that ', a(href='https://news.ontario.ca/en/statement/60154/280000-more-ontario-students-to-return-to-class', target = '_blank', 'elementary and secondary schools in the following 4 additional PHUs can return to in-person learning on 1 February 2021'), ': Eastern Ontario Health Unit; Middlesex-London Health Unit; Southwestern Public Health; Ottawa Public Health. '),
                            p('All schools in the following 9 schools boards could resume in-person learning: Catholic District School Board of Eastern Ontario; Conseil des écoles publiques de l\'Est de l\'Ontario; Conseil scolaire de district catholique de l\'Est ontarien; Conseil scolaire de district catholique du Centre-Est de l\'Ontario; London District Catholic School Board; Ottawa Catholic District School Board; Ottawa-Carleton District School Board; Thames Valley District School Board; Upper Canada District School Board'),
                            p('Schools in 2 additional school boards that span multiple PHUs could have schools in the appropriate PHU resume in-person learning: Conseil scolaire catholique Providence; Conseil scolaire Viamonde.'),
                            p('On 3 February 2021, it was announced ', a(href='https://news.ontario.ca/en/release/60228/enhanced-safety-measures-in-place-as-in-person-learning-resumes-across-ontario', target = '_blank', 'all elementary and secondary schools in the following 13 PHUs could return to in-person learning on 8 February 2021'), ': Brant County Health Unit; Chatham-Kent Public Health; Durham Region Health Department; Haldimand-Norfolk Health Unit; Halton Region Public Health; City of Hamilton Public Health Services; Huron Perth Public Health; Lambton Public Health; Niagara Region Public Health; Simcoe-Muskoka District Health Unit; Region of Waterloo Public Health and Emergency Services; Wellington-Dufferin-Guelph Public Health; Windsor-Essex County Health Unit.'),
                            p('And, all elementary and secondary schools in the following 3 PHUs to in-person learning on 16 February 2021: Peel Public Health, Toronto Public Health, and York Region Public Health.'),
                            p('On 12 April 2021, it was announced that ', a(href = 'https://news.ontario.ca/en/release/61106/ontario-moves-schools-to-remote-learning-following-spring-break', target='_blank', 'all schools would revert to virtual schooling for an indeterminate period as of 19 April 2021'), 'following the re-scheduled spring break of 12-16 April 2021 during which time schools were closed. Schools did not reopen for face-to-face instruction for the remainder of the school year (i.e., until 30 June 2021).'),
                            br(),
                            h4(id = "2019-2020", '2019-20 School Year'),
                            p('The first school closure announcement in Ontario was issued on 12 March 2020 for an initial period from 14 March to 4 April 2020. This compelled all publicly funded elementary and secondary schools to close. Public school closures were extended another three times – first until 4 May, then 31 May, and finally until the end of June 2020.'),
                            hr()
                        ),
                        # Site Navigation --------------------------------------
                        div(
                            h3(id = "Site Navigation", 'HOW TO NAVIGATE THE SITE'),
                            h4('Map 2021-22 - Affected Ontario Schools Tab'),
                            p('Shows daily updates to cumulative school-related cases. All schools are plotted by geocode on the map. The default view shows all affected schools. Hovering on a school bubble shows school-specific data on case numbers and breakdown per school, administrative school-level data on school characteristics, and demographic data of the affected school population. The default view shows the final cumulative cases in all schools.'),
                            h4('Map 2020-21 - Affected Ontario Schools Tab'),
                            p('Shows the final cumulative school-related cases as last reported on the update of 27 April 2021. The first school-related cases appeared in the dataset on 10 September 2020. All schools are plotted by geocode on the map. The default view shows the final cumulative cases in all affected schools.'), 
                            br(),
                            h5(tags$b('Bubbles')),
                            p('The size of the bubbles indicates the magnitude of cumulative cases (student, staff, unidentified) at specific schools relative to others. ', tags$b('The bigger the bubble, the more cumulative cases at that school – that is, the more it has been affected relative to other schools.')),
                            p(tags$b('Hovering on a bubble reveals school-specific COVID-19 case data and school social background information. '), 'Currently, the bubbles show: '),
                            tags$ul(
                                tags$li('Name of school'),
                                tags$li('Confirmed cases (cumulative);'),
                                tags$li('Confirmed staff cases (cumulative);'),
                                tags$li('Confirmed student cases (cumulative);'),
                                tags$li('Confirmed unidentified cases (cumulative);'),
                                tags$li('city;'), 
                                tags$li('level;'), 
                                tags$li('board;'), 
                                tags$li('main language of instruction;'), 
                                tags$li('enrolment;'), 
                                tags$li('proportion of students from low-income households;'), 
                                tags$li('proportion of students whose first language is not English;'),
                                tags$li('proportion of students whose first language is not French;'), 
                                tags$li('proportion of students who are immigrants from a non-English country;'),
                                tags$li('proportion of students who are immigrants from a non-French country;'),
                                tags$li('proportion of students who are receiving special education services (for 2021-2022 school year only);'),
                                tags$li('parents have no university education (for 2020-21 school year only)'),
                            ),
                            br(),
                            h5(tags$b('Viewing Options')),
                            p('Click on \'View Options\' to customize which schools you see (with cases, without cases, or both). Affected schools are visualized by red bubbles. Schools without cases are visualized in blue. ‘View timeslider, case over time’ shows the evolution of cumulative cases in schools over time.'),
                            br(),
                            h5(tags$b('Timeslider')),
                            p('The timeslider date at the bottom of the screen will appear by clicking on ‘View timeslider, case over time’. Drag the circle across the select dates to see cumulative cases. Data in the quick view summary pane and visual data on the map change accordingly.'),
                            br(),
                            h5(('Quick view summary pane')),
                            tags$ul(
                                tags$li(em('Daily Summary:'), ' Summarizes cumulative school-related cases, new total school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario). It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.'),
                                tags$li(em('Weekly Summary:'), 'Summarizes cumulative school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario) for 7- or 14-day period from last known data reporting date in the Ministry of Education dataset.')
                            ),
                            br(),
                            h4('Overview & Search Tab'),
                            h5(('Cumulative Case Chart')),
                            p('Shows the total number of cumulative school-related cases in Ontario, and disaggregated to show cumulative school-related student cases, cumulative school-related staff cases, and unidentified cases. "Unidentified cases" is used by the Ministry of Education to refer to the following: "In some instances, the type of case has not been identified as either student/child or staff/provider/partner due to privacy considerations. These "individuals" only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases will be tracked as "individuals" but not included in the "student/child" or "staff/provider" columns.'),
                            br(),
                            h5(('Tools for added functionality')),
                            p('Hover over the legend to access tools for added functionality: download graph as .PNG image file, zoom, pan, box select, lasso select, zoom in, zoom out, autoscale, reseat axes, toggle spike lines, show closest data on hover, compare data on hover.'),
                            p('"Compare data on hover" is especially useful to see and compare the number of cases on all lines in the graph on a specific date.'),
                            br(),
                            h5(('Daily Summary')),
                            p('Summarizes cumulative school-related cases, new total school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario). It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.'),
                            br(),
                            h5(('Weekly Summary')),
                            p('Summarizes cumulative school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario) for 7- or 14-day period from last known data reporting date in the Ministry of Education dataset.'),
                            br(),
                            h5(('Search Function and Table')),
                            p('Use this to search schools, boards, municipalities for data on confirmed cases of COVID-19.'),
                            br(),
                            h4('Data Tables & Data Dictionary Tab'),
                            h5(('Summary of cases in schools')),
                            p('Presents raw data of cases in schools. Data table can be manipulated in ascending or descending order by variable of interest. Table can be downloaded as a .CSV file for independent analysis.'),
                            p('Variables included: collected date; reported date; current schools with cases; current schools closed; current total number of schools; new (total school-related cases; student; staff; unidentified); recent (total school-related cases; student; staff; unidentified); past (total school-related cases; student; staff; unidentified); cumulative (total school-related cases; student; staff; unidentified).'),
                            p('For 2020-21: Recent and past case data available as from 1 October 2020. See ', a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/7fbdbb48-d074-45d9-93cb-f7de58950418', target = '_blank', 'Summary of cases in schools')),
                            br(),
                            h5('Schools with active cases and school demographic data'),
                            p('Presents raw data of cases in schools combined with demographic data. Data table can be manipulated in ascending or descending order by variable of interest. Table can be downloaded as a .CSV file for independent analysis.'),
                            p('Use the search function to see if a specific school, board, or municipality has been affected.'),
                            br(),
                            h5('Data Dictionary'),
                            p('Lists definitions of terms and variables as defined in the dataset and on COVID-19 cases in schools and child care centres Ontario Ministry of Education website. '),
                            br(),
                            h5('Data Sources and Source Code Tab'),
                            p('Lists all publicly available data sources used to generate the COVID-19 School Dashboard.'),
                            br(),
                            h5(('Source code')),
                            p('Source code for this site can be found on GitHub ', a(href = 'https://github.com/connor-cozens/covid19-school-dashboard', 'here', target = "_blank"), ' or on the archive ', a(href = 'https://doi.org/10.5683/SP3/Z9SNP0', 'here', target = "_blank")),
                            br(),
                            h5('Research and Media Tab'),
                            p('This tab lists research-related applications and media coverage of the COVID-19 School Dashboard.'),
                            br(),
                            hr()
                        ),
                        # Authorship -------------------------------------------
                        div(
                            h3(id = "Authorship", 'AUTHORSHIP, ATTRIBUTIONS, CITATION'),
                            h4('Cite the COVID-19 School Dashboard as:'),
                            p('Srivastava, P., Marshall, J., Cozens, C., & Taylor, P.J. (2022). ', tags$em('COVID-19 school dashboard (2.0 March 2022). '), '[Web application]. ', a(href = 'http://covid19schooldashboard.com/', 'http://covid19schooldashboard.com/')),
                            br(),
                            p(a(href = 'https://www.edu.uwo.ca/faculty-profiles/prachi-srivastava.html', target = '_blank', 'Dr. Prachi Srivastava'), ', Associate Professor, Faculty of Education, University of Western Ontario, Canada.'),
                            p(a(href = 'mailto:prachi.srivastava@uwo.ca', 'prachi.srivastava@uwo.ca')),
                            p(a(href = 'https://twitter.com/PrachiSrivas', target = '_blank', '@PrachiSrivas')),
                            p(a(href = 'https://orcid.org/0000-0003-4865-8963', target = '_blank', 'ORCID iD: 0000-0003-4865-8963')),
                            br(),
                            p('Development: '),
                            p('Technical lead development and design: Peter J. Taylor'),
                            p('Further development:'),
                            p('Justin Marshall', a(href = 'mailto:powtatow@gmail.com', 'powtatow@gmail.com')),
                            p('Connor Cozens', a(href = 'mailto:cozcon@gmail.com', 'cozcon@gmail.com')),
                            br(),
                            h5('Preliminary site structure based on:'),
                            p('Parker, E., & Leclerc, Q. (2020). ', tags$em('COVID-19 tracker. '), '[Web application]. ',  a(href = 'https://vac-lshtm.shinyapps.io/ncov_tracker/', target = '_blank', 'https://vac-lshtm.shinyapps.io/ncov_tracker/')),
                            br(),
                            br(),
                            a(href = 'https://www.edu.uwo.ca', target = '_blank', tags$img(src = 'uwo_logo.png', height = '58', width = '243')),
                            br(),
                            br()
                        )
               ),
               
               # TAB: Media Section --------------------------------------------
               tabPanel
               ('Media & Research',
                   tags$div(),
                   h3('Covid-19 School Dashboard: Research applications and media coverage'),
                   p('Some research-related applications and media coverage of the COVID-19 School Dashboard.'),
                   br(),
                   h4(tags$b(tags$u('Media'))),
                   p(a (href='https://www.google.com/url?q=https://www.cbc.ca/listen/live-radio/1-55/clip/15938443&sa=D&source=docs&ust=1665177381444370&usg=AOvVaw0K4P8eNPw2zv3RH9FZJVIu', target = '_blank', 'CBC Spark with Nora Young - Pandemic Lessons in Education Tech,'), ' 25 September 2022'),
                   br(),
                   p(a (href='https://www.cbc.ca/radio/spark/tik-tok-teacher-1.6593395', target = '_blank', 'CBC Radio Spark - How this Teacher uses TikTok to Education and Entertain Students,'), ' 25 September 2022'),
                   br(),
                   p(a (href='https://lfpress.com/news/local-news/elementary-school-locations-drive-inequitable-covid-19-infection-rates-study', target = '_blank', 'The London Free Press - Elementary School Locations Drive Inequitable COVID-19 Infection Rates: Study,'), ' 9 February 2022'),
                   br(),
                   p(a (href='https://www.thestandard.com.hk/section-news/fc/4/234392/Helping-kids-face-new-normal', target = '_blank', 'The Standard, Hong Kong - Helping Kids Face New Normal, '), '29 September 2021'),
                   br(),
                   p(a('What London Can Be Podcast - Episode 11: Dr. Prachi Srivastava, ', href='https://www.lcf.on.ca/whatlondoncanbe/2021/9/7/episode-11-dr-prachi-srivastava', target = '_blank'), '19 September 2021'),
                   br(),
                   p(a('Toronto Star - It\'s back to class for students at a handful of schools across the GTA this week, ', href='https://www.thestar.com/news/gta/2021/08/04/its-back-to-class-for-students-at-a-handful-of-schools-across-the-gta-this-week.html', target = '_blank'), '4 August 2021'),
                   br(),
                   p(a('Western News - School opening is everyone\'s responsibility, says global expert, ', href='https://news.westernu.ca/2021/08/school-opening-is-everyones-responsibility-says-global-expert/', target = '_blank'), '3 August 2021'),
                   br(),
                   p(a('640 The Morning Show with Greg Brady - Science Table Releases Recommendations for Reopening Schools in Ontario, ', href='https://omny.fm/shows/am640-the-morning-show/science-table-releases-recommendations-for-reopeni', target = '_blank'), '21 July 2021'),
                   br(),
                   p(a('Let\'s Talk London with Jess Brady - Return to in-person lessons essential to student wellbeing: Ontario COVID-19 Science Advisory Table, ', href='https://omny.fm/shows/let-s-talk-london/return-to-in-person-lessons-essential-to-student-w', target = '_blank'), '21 July 2021'),
                   br(),
                   p(a('The London Free Press - London academic: We can\'t let COVID shut down Ontario schools again, ', href='https://lfpress.com/news/local-news/london-academic-we-cant-let-covid-shut-down-ontario-schools-again', target = '_blank'), '20 July 2021'),
                   br(),
                   p(a('What She Said! with Candace Sampson - Let That Shit Go, Lady Luck, Education Disruption and Unforked (28:40), ', href='https://soundcloud.com/whatshesaidtalk/let-that-shit-go-lady-luck-education-disruption-and-unforked', target = '_blank'), '19 July 2021'),
                   br(),
                   p(a('980 CFPL with Mike Stubbs - Ontario needs a \'real education recovery plan\' to avoid the harms of long-term school closures imposed due to the COVID-19 pandemic, ', href='https://omny.fm/shows/london-live-with-mike-stubbs/ontario-needs-a-real-education-recovery-plan-to-av', target = '_blank'), '15 July 2021'),
                   br(),
                   p(a('COVID-19 Research at Western - Prachi Srivastava, ', href='https://www.google.com/url?q=https://youtu.be/1rSXh-5gqys&sa=D&source=docs&ust=1665177381443150&usg=AOvVaw1gCxHrji8581nfRc0xO5lM', target = '_blank'), '9 July 2021'),
                   br(),
                   p(a('TVO, The Agenda - Ontario’s Education Disruption, ', href='https://www.tvo.org/video/ontarios-education-disruption', target = '_blank'), '28 June 2021'),
                   br(),
                   p(a('Toronto Star \'How will we catch up?\' With the learning loss by Ontario students this year, what can be done in September to get them back on track, ', href='https://www.thestar.com/news/gta/2021/06/27/how-will-we-catch-up-with-the-learning-loss-by-ontario-students-this-year-what-can-be-done-in-september-to-get-them-back-on-track.html', target = '_blank'), '27 June 2021'),
                   br(),
                   p(a('CBC Ontario Morning - Wednesday June 9, Part 3, ', href='https://www.cbc.ca/player/play/1906968643507', target = '_blank'), '9 June 2021'),
                   br(),
                   p(a('CTV News - Ottawa parents worry about long-term impacts of school closures amid new analysis on the subject, ', href='https://ottawa.ctvnews.ca/ottawa-parents-worry-about-long-term-impacts-of-school-closures-amid-new-analysis-on-the-subject-1.5461894', target = '_blank'), '8 June 2021'),
                   br(),
                   p(a('Ottawa Citizen - Ontario leads the country in COVID-19 school closures — and kids have paid a price, study finds, ', href='https://ottawacitizen.com/news/local-news/study', target = '_blank'), '8 June 2021'),
                   br(),
                   p(a('London Free Press - \'A real loss\': Local expert urges plan to address students\' learning gaps, ', href='https://lfpress.com/news/local-news/this-is-a-real-loss-plan-needed-to-address-students-learning-gaps-expert', target = '_blank'), '7 June 2021'),
                   br(),
                   p(a('The Globe and Mail - Children in Manitoba First Nations community to repeat school year disrupted by COVID-19, ', href='https://www.theglobeandmail.com/canada/article-children-in-manitoba-first-nations-community-to-repeat-school-year/', target = '_blank'), '7 June 2021'),
                   br(),
                   p(a('The Conversation - End of topsy-turvy school year: 5 education issues exposed by the COVID-19 pandemic, ', href='https://theconversation.com/end-of-topsy-turvy-school-year-5-education-issues-exposed-by-the-covid-19-pandemic-161145', target = '_blank'), '6 June 2021'),
                   br(),
                   p(a('CFRA Live - With in-person learning in Ontario out of the question until September, how do we address the current learning gaps before the new school year begins? ', href='https://omny.fm/shows/580-cfra/cfra-live-with-in-person-learning-in-ontario-out-o', target = '_blank'), '5 June 2021'),
                   br(),
                   p(a('TVO - Ontario is taking \'unnecessary risks\' with our kids\' education, ', href='https://www.tvo.org/article/ontario-is-taking-unnecessary-risks-with-our-kids-education', target = '_blank'), '4 June 2021'),
                   br(),
                   p(a('CTV News - What Happens When Kids Go Back To School? ', href='https://ottawa.ctvnews.ca/video?clipId=2215910', target = '_blank'), '4 June 2021'),
                   br(),
                   p(a('Toronto Star - \'Nope, it’s not worth it anymore.\' Attendance sliding at Ontario\'s virtual schools as parents and kids burned out, ', href='https://www.thestar.com/news/gta/2021/06/04/nope-its-not-worth-it-anymore-attendance[…]-ontarios-virtual-schools-as-parents-and-kids-burned-out.html', target = '_blank'), '4 June 2021'),
                   br(),
                   p(a('Maclean\'s - The shadow pandemic: How a year of disrupted education could seriously impact Canadian children, ', href='https://www.macleans.ca/longforms/covid-19-pandemic-disrupted-schooling-impact/', target = '_blank'), '4 June 2021'),
                   br(),
                   p(a('Let\'s Talk London with Jess Brady - Ontario schools to remain closed to in-person learning for rest of academic year, ', href='https://omny.fm/shows/let-s-talk-london/ontario-schools-to-remain-closed-to-in-person-lear', target = '_blank'), '2 June 2021'),
                   br(),
                   p(a('Western University COVID Next Research Campaign, ', href='https://uwo.ca/research/excellence/covidnext/#view/2a', target = '_blank'), '3 May 2021'),
                   br(),
                   p(a('The Standard Hong Kong - \'Help in dash to reopen schools\', ', href='https://www.thestandard.com.hk/section-news/fc/4/226080/Help-in-dash-to-reopen-schools', target = '_blank'), '29 December 2020'),
                   br(),
                   p(a("The London Free Press - \'Western professor's tool makes school COVID-19 data easier to find, grasp\', ", href='https://lfpress.com/news/local-news/western-professors-tool-makes-school-covid-19-data-easier-to-find-grasp', target = '_blank'), '30 November 2020'), 
                   br(), 
                   p(a('CityNews Toronto - \'Covid-19 school data base to assist parents\', ', href='https://toronto.citynews.ca/video/2020/11/29/covid-19-school-data-base-to-assist-parents/', target = '_blank'), '29 November 2020'), 
                   br(), 
                   p(a('CBC News \'Ontario News with Rita Celli\' - \'Are schools safe enough?\', ', href='https://www.cbc.ca/listen/live-radio/1-45-ontario-today/clip/15811055-are-schools-safe-enough', target = '_blank'), '26 November 2020'),
                   br(),
                   p(a("980AM Radio Show - \'Mapping COVID-19 in Ontario schools to better understand the virus' impacts\', ", href='https://omny.fm/shows/am980/mapping-covid-19-in-ontario-schools-to-better-unde', target = '_blank'), '25 November 2020'),
                   br(),
                   p(a("DailyHive News Toronto - \'There's a map showing COVID-19 cases in Ontario schools\', ", href='https://dailyhive.com/toronto/covid-19-map-ontario-schools', target = '_blank'), '25 November 2020'),
                   br(),
                   p(a("Global News - \'Coronavirus: expert in global education launches interactive map of Ontario school cases\', ", href='https://globalnews.ca/news/7481210/coronavirus-interactive-map-ontario-school-cases-covid-19/', target = '_blank'), '25 November 2020'),
                   br(),
                   p(a('Western University News - \'New interactive dashboard tracks COVID-19 cases in Ontario schools\', ', href='https://news.westernu.ca/2020/11/new-interactive-dashboard-tracks-covid-19-cases-in-ontario-schools/', target = '_blank'), '24 November 2020'),
                   br(),
                   p(a("CTV London, Ontario - \'New website helps simplify and track school COVID-19 case data\', ", href='https://london.ctvnews.ca/new-website-helps-simplify-and-track-school-covid-19-case-data-1.5201172', target = '_blank'), '23 November 2020'),
                   br(),
                   h4(tags$b(tags$u('Research and Related Knowledge Mobilization'))),
                   p(a("Srivastava, P., Expert presentation, ‘Trends in education and COVID-19: ", href='https://www.google.com/url?q=https://www.un.org/development/desa/pd/events/cpd56-egm&sa=D&source=docs&ust=1665333933558489&usg=AOvVaw15ymnbzYZ2G6cr_u9PPUX7', target = '_blank'), 'Rebuilding education systems for recovery.’ Expert group meeting on Population, Education and Sustainable Development, Population Division of the United Nations Department of Economic and Social Affairs (UNDESA), 6 September 2022. '),
                   br(),
                   p('Srivastava, P., Expert presentation, ‘Education recovery: Context and issues.’ Thames Valley District School Board Administrative Council Meeting, London, ON, 13 June 2022.'),
                   br(),
                   p('Srivastava, P., Lau, T., Ansari, D., & Thampi, N., Effects of socio-economic factors on elementary school student COVID-19 infections in Ontario, Canada. medRxiv. (22 February 2022).', a('https://doi.org/10.1101/2022.02.04.22270413', href='https://doi.org/10.1101/2022.02.04.22270413', target = '_blank')),
                   br(),
                   p('Lau, N.T.T., Ansari, D., Thampi, N., & Srivastava, P. Final dataset for analysis of student COVID-19 infections in schools (Ontario 2020-21), Version 1. Borealis, February 2022.', a('https://doi.org/10.5683/SP3/VKK7BR', href='https://doi.org/10.5683/SP3/VKK7BR', target = '_blank')),
                   br(),
                   p('Lau, N.T.T, Ansari, D., Thampi, N., & Srivastava, P. Mplus Files for analysis of student COVID-19 school infections (Ontario 2020-21), Version 1. Borealis, February 2022.', a('https://doi.org/10.5683/SP3/IJOWDY', href='https://doi.org/10.5683/SP3/IJOWDY', target = '_blank')),
                   br(),
                   p('Srivastava, P. & Gagon, A.A., Using geospatial data for educational planning: Education systems and COVID-19. Paper presented at the 2021 United Nations World Data Forum, Bern, Switzerland, 3 – 6 October 2021.'),
                   br(),
                   p('Srivastava, P., ‘How did the pandemic affect equity in education in Ontario schools?’, Pandemic Recovery - Creating Equity in Education Panel Discussion, Western Homecoming, Western University, 25 September 2021.'),
                   br(),
                   p('Gallagher-Mackay, K., Srivastava, P., Underwood, K., Dhuey, E., McCready, L., Born, K.B., Maltsev, A., Perkhun, A., Steiner, R., Barrett, K., & Sander, B., COVID-19 and education disruption in Ontario: Emerging evidence on impacts, Version 1.1. On behalf of the Ontario COVID-19 Science Advisory Table, Toronto, June 2021. ', a('https://doi.org/10.47326/ocsat.2021.02.34.1.0', href='https://doi.org/10.47326/ocsat.2021.02.34.1.0', target = '_blank')),
                   br(),
                   p('Srivastava, P., Invited panellist, for ‘Education continuity, school operations, and the COVID-19 pandemic: lessons from the United States and Canada’, Consortium of Universities for Global Health, 8 June 2021.'),
                   br(),
                   p('Srivastava, P., Invited panellist, Arts, humanities and social science response panel discussion, Innovation Ambassadors Showcase: COVID-19’s Impact on the Innovation Ecosystem. WorlDiscoveries, Western Morrisette Institute of Entrepreneurship, 20 May 2021.'),
                   br(),
                   p('Srivastava, P., Invited speaker, What the pandemic has taught us about education. Speaker’s Corner with David Crombie, The Enoch Turner Schoolhouse, 21 April 2021.'),
                   br(),
                   
               ),
               
               # TAB: Our Team -------------------------------------------------
               tabPanel('Our Team',
                        tags$div(),
                        h3('THE TEAM AND CONTACT'),
                        hr(),
                        h4('Dr. Prachi Srivastava'),
                        p(em('Principal Investigator and Project Lead, Western University')),
                        p('Dr. Prachi Srivastava is a tenured Associate Professor, Western University, specialising in education and global development. She is also Member, World Bank Expert Advisory Council on Citizen Engagement, and Senior Research Fellow, NORRAG. Previously, she served with the United Nations Mission in Kosovo and the International Rescue Committee. She holds a doctorate from the University of Oxford.'),
                        p('For inquiries, contact Dr. Srivastava at: ', a(href = 'mailto:prachi.srivastava@uwo.ca', 'prachi.srivastava@uwo.ca')),
                        p(a(href = 'https://twitter.com/PrachiSrivas', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28')), ' - ', a(href = 'https://www.linkedin.com/in/prachi-srivastava-9ab6122/', target = "_blank", tags$img(src = 'linkedin_logo.png', height = '24', width = '24')), ' - ', a(href = 'https://www.edu.uwo.ca/faculty-profiles/prachi-srivastava.html', target = "_blank", tags$img(src = 'uwo_logo.png', height = '24', width = '120'))),
                        hr(),
                        splitLayout(
                            verticalLayout(
                                h4('Justin Marshall'),
                                p(em('Developer')),
                                p('Justin graduated from Western University with an Honors BSc in Computer Science and a Minor in Software Engineering. He is currently open to full-time roles and opportunities. In addition to data visualization, Justin is interested in fields including game development, and is currently working on creating his own games and building a portfolio.')
                            ),
                            verticalLayout(
                                h4('Connor Cozens'),
                                p(em('Developer')),
                                p('Connor is a recent graduate from Western University with an Honors BSc in Computer Science and a Minor in Software Engineering. He is currently joining the Tech & Operations team at RBC in Toronto. Connor is passionate about artificial intelligence and data science. He is always looking for projects in these areas to get involved with, to learn, and contribute to growing research fields in these areas.')
                            )
                        , cellArgs = list(style='white-space: normal; overflow: hidden;')),
                        splitLayout(
                            verticalLayout(
                                p('Contact Justin  at: ', a(href = 'mailto:powtatow@gmail.com', 'powtatow@gmail.com')),
                                p(a(href = 'https://twitter.com/JuiceMarsh', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28')), ' - ', a(href = 'https://www.linkedin.com/in/JustinMarshall1998/', target = "_blank", tags$img(src = 'linkedin_logo.png', height = '24', width = '24')), ' - ', a(href = 'https://github.com/JustinMarshall98', target = "_blank", tags$img(src = 'github_logo.png', height = '32', width = '32')))
                            ),
                            verticalLayout(
                                p('Contact Connor at: ', a(href = 'mailto:cozcon@gmail.com', 'cozcon@gmail.com')),
                                p(a(href = 'https://twitter.com/ConnorCozens', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28')), ' - ', a(href = 'https://www.linkedin.com/in/connorcozens/', target = "_blank", tags$img(src = 'linkedin_logo.png', height = '24', width = '24')), ' - ', a(href = 'https://github.com/connor-cozens', target = "_blank", tags$img(src = 'github_logo.png', height = '32', width = '32')))
                            )
                            , cellArgs = list(style='white-space: normal; overflow: hidden;')),
                        hr(),
                        h4('Nathan Lau'),
                        p(em('Research Engineer')),
                        p('Nathan is a Post-Doctoral Fellow with a Doctoral Degree in Psychology from Western University.'),
                        p('Contact Nathan at: ', a(href = 'mailto:tlau97@uwo.ca', 'tlau97@uwo.ca')),
                        p(a(href = 'https://twitter.com/nathanlau16', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28'))),
                        hr(),
                        h4('Peter J. Taylor'),
                        p(em('Technical Advisor')),
                        p(a(href = 'https://twitter.com/br00t4c', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28')), ' - ', a(href = 'https://gitlab.com/br00t', target = "_blank", tags$img(src = 'gitlab_logo.png', height = '32', width = '32'))),
                        hr(),
                        h4('Kelly Bairos'),
                        p(em('Project Coordinator')),
                        p('Kelly has a Master’s in Education with a focus on Education Policy, and has been a project manager on several education research projects since 2010. Her professional work not only includes oversight of administrative, organizational, and financial project matters, but also planning and executing knowledge mobilization strategies.'),
                        p('Contact Kelly at: ', a(href = 'mailto:kbairos@uwo.ca', 'kbairos@uwo.ca')),
                        hr(),
                        p('For general inquiries:'),
                        p('Contact: ', a(href = 'mailto:covid19schooldashboard@gmail.com', 'covid19schooldashboard@gmail.com')),
               )
               
               
    ),
    tags$head(
        tags$style(HTML(navbar_js))
    )
    
    
    
)

# SHINY SERVER -----------------------------------------------------------------

server <- function(input, output, session) {
    
    # panel: button: viewOptions -----------------------------------------------
    
    # Variables for the 2022-2021 map view tab
    
    #Is the viewing options menu open?
    viewOptionsOpen <- FALSE
    #Are we currently viewing schools with cases? (If timeslider is closed)
    schoolsWithCases <- TRUE
    #Are we currently viewing schools without cases? (If timeslider is closed)
    schoolsWithoutCases <- FALSE
    #Is the timeslider currently open for this tab
    vTimeSlider <- FALSE
    #Suppress the menu's opening for the first time counting as 'ticking' a checkbox in the menu
    suppressFirstResponse1 <- TRUE
    suppressFirstResponse2 <- TRUE
    suppressFirstResponse3 <- TRUE
    
    # Variables for the 2021-2020 map view tab
    
    #Is the viewing options menu open?
    viewOptionsOpen20_21 <- FALSE
    #Are we currently viewing schools with cases? (If timeslider is closed)
    schoolsWithCases20_21 <- TRUE
    #Are we currently viewing schools without cases? (If timeslider is closed)
    schoolsWithoutCases20_21 <- FALSE
    #Is the timeslider currently open for this tab
    vTimeSlider20_21 <- FALSE
    #Suppress the menu's opening for the first time counting as 'ticking' a checkbox in the menu
    suppressFirstResponse1_20_21 <- TRUE
    suppressFirstResponse2_20_21 <- TRUE
    suppressFirstResponse3_20_21 <- TRUE
    
    # Observes the button for getting the viewing options menu (2022-2021)
    observeEvent(input$getOptions, {
        viewOptionsOpen <<- !viewOptionsOpen #flip when button is pressed
        if (!viewOptionsOpen){
            output$mapperViewOptions <- renderUI({
                #Render nothing in this spot
            })
        }
        else{
            #viewOptionsOpen <<-  FALSE #flip when button is pressed
            output$mapperViewOptions <- renderUI({
                absolutePanel(id = 'options',
                              class = 'panel panel-default',
                              top = "5%", 
                              right = "0%", 
                              width = 'auto', 
                              #fixed = TRUE,
                              draggable = FALSE, 
                              height = 'auto',
                              style = "padding-left: 1%;
                              border-radius: 25px;",
                              
                              checkboxInput("visOp1", "Schools with Cases", value = schoolsWithCases),
                              checkboxInput("visOp2", "Schools without Cases", value = schoolsWithoutCases),
                              checkboxInput("visTS", "View Timeslider, Case over time", value = vTimeSlider)
                )
            })
        }
    })
    
    #Update Map Markers for the 2022-2021 map
    updateMarkers <- function () {
        leafletProxy('basemap_leaflet') %>%
            clearMarkers()
        
        if (input$visOp2){
            leafletProxy(mapId = 'basemap_leaflet', session = session) %>%
                addCircleMarkers( 
                    data = get_schools_no_cases(), 
                    lng = ~longitude, 
                    lat = ~latitude, 
                    radius = 3, 
                    weight = 1, 
                    color = '#0000B0',
                    fillOpacity = 1, 
                    label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/><strong>Zero Confirmed Cases</strong></div>', 
                                    get_schools_no_cases()$`school name`, 
                                    get_schools_no_cases()$city, 
                                    get_schools_no_cases()$`school level`, 
                                    get_schools_no_cases()$`board name`, 
                                    get_schools_no_cases()$`school language`, 
                                    get_schools_no_cases()$enrolment, 
                                    get_schools_no_cases()$`percentage of school-aged children who live in low-income households`, 
                                    get_schools_no_cases()$`percentage of students whose first language is not english`, 
                                    get_schools_no_cases()$`percentage of students who are new to canada from a non-english speaking country`, 
                                    get_schools_no_cases()$`percentage of students whose first language is not french`, 
                                    get_schools_no_cases()$`percentage of students who are new to canada from a non-french speaking country`,
                                    get_schools_no_cases()$`percentage of students receiving special education services`) %>% 
                        lapply(htmltools::HTML), 
                    labelOptions = labelOptions(
                        style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                        textsize = '15px', direction = 'auto'))
        }
        if (input$visOp1){
            leafletProxy('basemap_leaflet') %>%
                addCircleMarkers(
                    data = cases_per_school, 
                    lng = ~lon, 
                    lat = ~lat, 
                    radius = 2,
                    weight = 1, 
                    color = '#b00000',
                    fillOpacity = 1)
            leafletProxy('basemap_leaflet') %>%
                addCircleMarkers( 
                    data = cases_per_school, 
                    lng = ~lon, 
                    lat = ~lat, 
                    radius = ~(cases_per_school) * 2,
                    weight = 1, 
                    color = '#d62728',
                    fillOpacity = 0.3, 
                    label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                                    cases_per_school$school_name, 
                                    cases_per_school$city, 
                                    cases_per_school$school_level, 
                                    cases_per_school$school_board, 
                                    cases_per_school$school_language, 
                                    cases_per_school$school_enrolment, 
                                    cases_per_school$low_income, 
                                    cases_per_school$non_english, 
                                    cases_per_school$from_non_english, 
                                    cases_per_school$non_french, 
                                    cases_per_school$from_non_french, 
                                    cases_per_school$special_education,
                                    cases_per_school$cases_per_school,
                                    cases_per_school$cases_per_school_staff,
                                    cases_per_school$cases_per_school_student,
                                    cases_per_school$cases_per_school_unidentified) %>% lapply(htmltools::HTML), 
                    labelOptions = labelOptions(
                        style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                        textsize = '15px', direction = 'auto'))
        }
    }
    
    #Observes the activity for Mapper2022-2021 "Schools With Cases" option
    observeEvent(input$visOp1,{
        if (!suppressFirstResponse1 && !input$visTS){
            if (!input$visOp1){
                schoolsWithCases <<- FALSE
                updateMarkers()
            }
            else {
                schoolsWithCases <<- TRUE
                updateMarkers()
            }
        }
        else {
            suppressFirstResponse1 <<- FALSE
        }
    }, ignoreInit = TRUE)
    
    #Observes the activity for Mapper2022-2021 "Schools Without Cases" option
    observeEvent(input$visOp2,{
        if (!suppressFirstResponse2 && !input$visTS){
            if (!input$visOp2){
                schoolsWithoutCases <<- FALSE
                updateMarkers()
            }
            else {
                schoolsWithoutCases <<- TRUE
                updateMarkers()
            }
        }
        else {
            suppressFirstResponse2 <<- FALSE
        }
    }, ignoreInit = TRUE)
    
    
    # Observes the button for getting the viewing options menu (2021-2020)
    observeEvent(input$getOptions20_21, {
        viewOptionsOpen20_21 <<- !viewOptionsOpen20_21 #flip when button is pressed
        if (!viewOptionsOpen20_21){
            output$mapperViewOptions20_21 <- renderUI({
                #Render nothing in this spot
            })
        }
        else{
            output$mapperViewOptions20_21 <- renderUI({
                absolutePanel(id = 'options20_21',
                              class = 'panel panel-default',
                              top = "5%", 
                              right = "0%", 
                              width = 'auto', 
                              draggable = FALSE, 
                              height = 'auto',
                              style = "padding-left: 1%;
                              border-radius: 25px;",
                              
                              checkboxInput("visOp1_20_21", "Schools with Cases", value = schoolsWithCases20_21),
                              checkboxInput("visOp2_20_21", "Schools without Cases", value = schoolsWithoutCases20_21),
                              checkboxInput("visTS20_21", "View Timeslider, Case over time", value = vTimeSlider20_21)
                )
            })
        }
    })
    
    #Update Map Markers for the 2021-2020 map
    updateMarkers20_21 <- function () {
        leafletProxy('map_leaflet20_21') %>%
            clearMarkers()
        
        if (input$visOp2_20_21){
            leafletProxy(mapId = 'map_leaflet20_21') %>%
                addCircleMarkers( 
                    data = get_schools_no_cases_20_21(), 
                    lng = ~longitude, 
                    lat = ~latitude, 
                    radius = 3, 
                    weight = 1, 
                    color = '#0000B0',
                    fillOpacity = 1, 
                    label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/><strong>Zero Confirmed Cases</strong></div>', 
                                    get_schools_no_cases_20_21()$`school name`, 
                                    get_schools_no_cases_20_21()$city, 
                                    get_schools_no_cases_20_21()$`school level`, 
                                    get_schools_no_cases_20_21()$`board name`, 
                                    get_schools_no_cases_20_21()$`school language`, 
                                    get_schools_no_cases_20_21()$enrolment, 
                                    get_schools_no_cases_20_21()$`percentage of school-aged children who live in low-income households`, 
                                    get_schools_no_cases_20_21()$`percentage of students whose first language is not english`, 
                                    get_schools_no_cases_20_21()$`percentage of students who are new to canada from a non-english speaking country`, 
                                    get_schools_no_cases_20_21()$`percentage of students whose first language is not french`, 
                                    get_schools_no_cases_20_21()$`percentage of students who are new to canada from a non-french speaking country`) %>% 
                        lapply(htmltools::HTML), 
                    labelOptions = labelOptions(
                        style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                        textsize = '15px', direction = 'auto'))
        }
        if (input$visOp1_20_21){
            leafletProxy('map_leaflet20_21') %>%
                addCircleMarkers(
                    data = cases_per_school_20_21, 
                    lng = ~lon, 
                    lat = ~lat, 
                    radius = 2,
                    weight = 1, 
                    color = '#b00000',
                    fillOpacity = 1)
            leafletProxy('map_leaflet20_21') %>%
                addCircleMarkers(
                    data = cases_per_school_20_21, 
                    lng = ~lon, 
                    lat = ~lat, 
                    radius = ~(cases_per_school_20_21$cases_per_school) * 2,
                    weight = 1, 
                    color = '#d62728',
                    fillOpacity = 0.3, 
                    label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Parents have no university education: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                                    cases_per_school_20_21$school_name, 
                                    cases_per_school_20_21$city, 
                                    cases_per_school_20_21$school_level, 
                                    cases_per_school_20_21$school_board, 
                                    cases_per_school_20_21$school_language, 
                                    cases_per_school_20_21$school_enrolment, 
                                    cases_per_school_20_21$low_income, 
                                    cases_per_school_20_21$non_english, 
                                    cases_per_school_20_21$from_non_english, 
                                    cases_per_school_20_21$non_french, 
                                    cases_per_school_20_21$from_non_french, 
                                    cases_per_school_20_21$some_university, 
                                    cases_per_school_20_21$cases_per_school,
                                    cases_per_school_20_21$cases_per_school_staff,
                                    cases_per_school_20_21$cases_per_school_student,
                                    cases_per_school_20_21$cases_per_school_unidentified) %>% lapply(htmltools::HTML), 
                    labelOptions = labelOptions(
                        style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                        textsize = '15px', direction = 'auto'))
        }
    }
    
    #Observes the activity for Mapper2021-2020 "Schools With Cases" option
    observeEvent(input$visOp1_20_21,{
        if (!suppressFirstResponse1_20_21 && !input$visTS20_21){
            if (!input$visOp1_20_21){
                schoolsWithCases20_21 <<- FALSE
                updateMarkers20_21()
            }
            else {
                schoolsWithCases20_21 <<- TRUE
                updateMarkers20_21()
            }
        }
        else {
            suppressFirstResponse1_20_21 <<- FALSE
        }
    }, ignoreInit = TRUE)
    
    #Observes the activity for Mapper2021-2020 "Schools Without Cases" option
    observeEvent(input$visOp2_20_21,{
        if (!suppressFirstResponse2_20_21 && !input$visTS20_21){
            if (!input$visOp2_20_21){
                schoolsWithoutCases20_21 <<- FALSE
                updateMarkers20_21()
            }
            else {
                schoolsWithoutCases20_21 <<- TRUE
                updateMarkers20_21()
            }
        }
        else {
            suppressFirstResponse2_20_21 <<- FALSE
        }
    }, ignoreInit = TRUE)
    
    #Observes the activity for Mapper2022-2021 "View Timeslider" option
    observeEvent(input$visTS,{
        if (!suppressFirstResponse3){
            if (!input$visTS){
                vTimeSlider <<- FALSE
                #Remove timeslider
                output$timesliderViewer <- renderUI({
                    #Render nothing here
                })
                updateMarkers()
                
                #Update daily summary tab when timeslider input changes
                # cumulative_case_count_text -----------------------------------------------
                output$cumulative_case_count_text <- renderText({
                    idx <- max(which(covid19_schools_summary$collected_date <= as.Date(now())))
                    count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
                    paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
                })
                # daily_summary_1_dt -------------------------------------------------------
                output$daily_summary_1_dt <- renderTable({
                    get_summary_table(0)
                }, align = 'r', striped = TRUE, width = '100%')
                # clean_date_reactive_text -------------------------------------------------
                output$clean_date_reactive_text <- renderText({
                    #Changed from covid19_schools_active to covid19_schools_summary, which has the correct latest date matching with the case count given
                    format(max(covid19_schools_summary$reported_date), '%d %B %Y')
                })
                #Update weekly summary tab when timeslider goes away
                #7 days
                output$weekly_summary_1_dt <- renderTable({
                    get_weekly_summary_table(TRUE, 0)
                }, align = 'r', striped = TRUE, width = '100%')
                # clean_week_old_date_text -------------------------------------------------
                output $clean_week_old_date_text <- renderText ({
                    dates <- last_week_obtain(0)
                    #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
                    dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
                    dateString
                })
                #14 days
                output$weekly_summary_3_dt <- renderTable({
                    get_weekly_summary_table(FALSE, 0)
                }, align = 'r', striped = TRUE, width = '100%')
                # clean_two_weeks_old_date_text -------------------------------------------------
                output $clean_two_weeks_old_date_text <- renderText ({
                    dates <- last_two_weeks_obtain(0)
                    #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
                    dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
                    dateString
                })
            }
            else{
                vTimeSlider <<- TRUE
                updateCheckboxInput(session, "visOp1", value = FALSE)
                updateCheckboxInput(session, "visOp2", value = FALSE)
                #Make timeslider appear
                output$timesliderViewer <- renderUI({
                    div(
                        p("Select a date to view data reported at that time:", style = "color:white;font:Helvetica;padding-left:10px;padding-top:15px;"),
                        sliderInput("obs", label = NULL,
                                    min = as.Date("2021-09-13","%Y-%m-%d"), max = as.Date("2021-12-22","%Y-%m-%d"), value = as.Date("2021-09-13"), timeFormat="%Y-%m-%d", width = '95%'
                        ), style = "position:absolute;bottom:0;left:0;right:0;background-color:#d34615;padding-left:3%")
                })
            }
        }
        else{
            suppressFirstResponse3 <<- FALSE
        }
        
    })
    
    #Observes the activity for Mapper2021-2020 "View Timeslider" option
    observeEvent(input$visTS20_21,{
        if (!suppressFirstResponse3_20_21){
            if (!input$visTS20_21){
                vTimeSlider20_21 <<- FALSE
                #Remove the timeslider
                output$timesliderViewer20_21 <- renderUI({
                    #Render nothing here
                })
                updateMarkers20_21()
                # cumulative_case_count_text_20_21 -----------------------------------------------
                output$cumulative_case_count_text_20_21 <- renderText({
                    idx <- max(which(covid19_schools_summary_20_21$collected_date <= as.Date(now())))
                    count <- last(covid19_schools_summary_20_21[ idx, 'cumulative_school_related_cases' ])
                    paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
                })
            }
            else{
                vTimeSlider20_21 <<- TRUE
                updateCheckboxInput(session, "visOp1_20_21", value = FALSE)
                updateCheckboxInput(session, "visOp2_20_21", value = FALSE)
                #Make timeslider appear
                output$timesliderViewer20_21 <- renderUI({
                    div(
                        p("Select a date to view data reported at that time:", style = "color:white;font:Helvetica;padding-left:10px;padding-top:15px;"),
                        sliderInput("obs20_21", label = NULL,
                                    min = as.Date("2020-09-10","%Y-%m-%d"), max = as.Date("2021-04-14","%Y-%m-%d"), value = as.Date("2020-09-10"), timeFormat="%Y-%m-%d", width = '95%'
                        ), style = "position:absolute;bottom:0;left:0;right:0;background-color:#d34615;padding-left:3%")
                })
            }
        }
        else{
            suppressFirstResponse3_20_21 <<- FALSE
        }
    })
    
    #Observes activity (movement) on the timeslider and adjusts data being viewed accordingly (2022-2021)
    observeEvent(input$obs,{
        geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
                                 str_trim(covid19_schools_active_with_demographics$school.name), 
                                 covid19_schools_active_with_demographics$municipality)
        
        selected_date <- input$obs
        
        #Update daily summary tab when timeslider input changes
        # cumulative_case_count_text -----------------------------------------------
        output$cumulative_case_count_text <- renderText({
            idx <- max(which(covid19_schools_summary$collected_date <= as.Date(selected_date)))
            count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
            paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
        })
        # daily_summary_1_dt -------------------------------------------------------
        output$daily_summary_1_dt <- renderTable({
            get_summary_table(selected_date)
        }, align = 'r', striped = TRUE, width = '100%')
        # clean_date_reactive_text -------------------------------------------------
        output$clean_date_reactive_text <- renderText({
            #Changed from covid19_schools_active to covid19_schools_summary, which has the correct latest date matching with the case count given
            format(selected_date, '%d %B %Y')
        })
        #Update weekly summary tab when timeslider input changes
        #7 days
        output$weekly_summary_1_dt <- renderTable({
            get_weekly_summary_table(TRUE, selected_date)
        }, align = 'r', striped = TRUE, width = '100%')
        # clean_week_old_date_text -------------------------------------------------
        output $clean_week_old_date_text <- renderText ({
            dates <- last_week_obtain(selected_date)
            #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
            dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
            dateString
        })
        #14 days
        output$weekly_summary_3_dt <- renderTable({
            get_weekly_summary_table(FALSE, selected_date)
        }, align = 'r', striped = TRUE, width = '100%')
        # clean_two_weeks_old_date_text -------------------------------------------------
        output $clean_two_weeks_old_date_text <- renderText ({
            dates <- last_two_weeks_obtain(selected_date)
            #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
            dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
            dateString
        })
        
        cases_pst <-  subset(covid19_schools_active_with_demographics, collected_date == selected_date)
        increment <- 1 #If we don't have any data from this date, then move forward until we get some
        while(nrow(cases_pst) == 0){
            if (selected_date >= as.Date("2021-12-22")){
                increment = -1 #If we reach the furthest possible date and have no data to see, go backwards until we do
            }
            selected_date = selected_date + increment
            cases_pst <-  subset(covid19_schools_active_with_demographics, collected_date == selected_date)
        }
        cases_pst[,"geo_query_str"] <- NA
        for(i in 0:nrow(cases_pst) - 1){
            cases_pst$geo_query_str[i] = sprintf('%s,%s,Ontario,Canada', 
                                                 str_trim(cases_pst$school[i]), 
                                                 cases_pst$municipality[i])
        }
        
        leafletProxy('basemap_leaflet') %>%
            clearMarkers()
    
        leafletProxy(mapId = 'basemap_leaflet', session = session) %>%
            addCircleMarkers( 
                data = cases_pst, 
                lng = cases_pst$longitude, 
                lat = cases_pst$latitude, 
                radius = cases_pst$total_confirmed_cases * 2,
                weight = 1, 
                color = '#d62728',
                fillOpacity = 0.3,
                label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                                cases_pst$school.name, 
                                cases_pst$city, 
                                cases_pst$school.level, 
                                cases_pst$board.name, 
                                cases_pst$school.language, 
                                cases_pst$enrolment, 
                                cases_pst$percentage.of.school.aged.children.who.live.in.low.income.households, 
                                cases_pst$percentage.of.students.whose.first.language.is.not.english, 
                                cases_pst$percentage.of.students.who.are.new.to.canada.from.a.non.english.speaking.country, 
                                cases_pst$percentage.of.students.whose.first.language.is.not.french, 
                                cases_pst$percentage.of.students.who.are.new.to.canada.from.a.non.french.speaking.country,
                                cases_pst$percentage.of.students.receiving.special.education.services,
                                cases_pst$total_confirmed_cases,
                                cases_pst$confirmed_staff_cases,
                                cases_pst$confirmed_student_cases,
                                cases_pst$confirmed_unidentified_cases) %>% 
                    lapply(htmltools::HTML), 
                labelOptions = labelOptions(
                    style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                    textsize = '15px', direction = 'auto'))
    
        
    })
    
    #Observes activity (movement) on the timeslider and adjusts data being viewed accordingly (2021-2020)
    observeEvent(input$obs20_21,{
        geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
                                 str_trim(covid19_schools_active_with_demographics_20_21$school.name), 
                                 covid19_schools_active_with_demographics_20_21$municipality)
        
        selected_date <- input$obs20_21
        # cumulative_case_count_text_20_21 -----------------------------------------------
        output$cumulative_case_count_text_20_21 <- renderText({
            idx <- max(which(covid19_schools_summary_20_21$collected_date <= as.Date(selected_date)))
            count <- last(covid19_schools_summary_20_21[ idx, 'cumulative_school_related_cases' ])
            paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
        })
        cases_pst_20_21 <-  subset(covid19_schools_active_with_demographics_20_21, collected_date == selected_date)
        increment <- 1 #If we don't have any data from this date, then move forward until we get some
        while(nrow(cases_pst_20_21) == 0){
            if (selected_date >= as.Date("2021-04-14")){
                increment = -1 #If we reach the furthest possible date and have no data to see, go backwards until we do
            }
            selected_date = selected_date + increment
            cases_pst_20_21 <-  subset(covid19_schools_active_with_demographics_20_21, collected_date == selected_date)
        }
        cases_pst_20_21[,"geo_query_str"] <- NA
        for(i in 0:nrow(cases_pst_20_21) - 1){
            cases_pst_20_21$geo_query_str[i] = sprintf('%s,%s,Ontario,Canada', 
                                                 str_trim(cases_pst_20_21$school[i]), 
                                                 cases_pst_20_21$municipality[i])
        }
        
        leafletProxy('map_leaflet20_21') %>%
            clearMarkers()
        
        leafletProxy(mapId = 'map_leaflet20_21', session = session) %>%
            addCircleMarkers( 
                data = cases_pst_20_21, 
                lng = cases_pst_20_21$longitude, 
                lat = cases_pst_20_21$latitude, 
                radius = cases_pst_20_21$total_confirmed_cases * 2,
                weight = 1, 
                color = '#d62728',
                fillOpacity = 0.3, 
                label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Parents have no university education: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                                cases_pst_20_21$school.name, 
                                cases_pst_20_21$city, 
                                cases_pst_20_21$school.level, 
                                cases_pst_20_21$board.name, 
                                cases_pst_20_21$school.language, 
                                cases_pst_20_21$enrolment,
                                cases_pst_20_21$percentage.of.school.aged.children.who.live.in.low.income.households, 
                                cases_pst_20_21$percentage.of.students.whose.first.language.is.not.english, 
                                cases_pst_20_21$percentage.of.students.who.are.new.to.canada.from.a.non.english.speaking.country, 
                                cases_pst_20_21$percentage.of.students.whose.first.language.is.not.french, 
                                cases_pst_20_21$percentage.of.students.who.are.new.to.canada.from.a.non.french.speaking.country,
                                cases_pst_20_21$percentage.of.students.whose.parents.have.some.university.education, 
                                cases_pst_20_21$total_confirmed_cases,
                                cases_pst_20_21$confirmed_staff_cases,
                                cases_pst_20_21$confirmed_student_cases,
                                cases_pst_20_21$confirmed_unidentified_cases) %>% 
                    lapply(htmltools::HTML), 
                labelOptions = labelOptions(
                    style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                    textsize = '15px', direction = 'auto'))
        
        
    })
    
    # basemap_leaflet ----------------------------------------------------------
    output$basemap_leaflet <- renderLeaflet({
        withProgress(max = 6, 
                     value = 0, 
                     message = 'please wait...', 
                     expr = {
                         incProgress(1, 'loading shapes')
                         # regenerate the basemap
                         # https://geohub.lio.gov.on.ca/datasets/province/data
                         ontario <- readOGR(dsn = 'data/shapefiles', layer = 'PROVINCE')
                         incProgress(1, 'generating map')
                         basemap <- leaflet(ontario)
                         incProgress(1, 'setting view')
                         basemap <- setView(basemap, lng = -79.7, lat = 44.39, zoom = 8) 
                         incProgress(1, 'adding polygons')
                         basemap <- addPolygons(basemap, weight = 3, fillColor = '#696969', opacity = 0.5)
                         incProgress(1, 'adding tiles')
                         basemap <- addProviderTiles(basemap, providers$Esri.NatGeoWorldMap)
                         
                         # add case data markers
                         incProgress(1, 'adding markers')
                         basemap <- addCircleMarkers(basemap, 
                                                     data = cases_per_school, 
                                                     lng = ~lon, 
                                                     lat = ~lat, 
                                                     radius = 2,
                                                     weight = 1, 
                                                     color = '#b00000',
                                                     fillOpacity = 1)
                         basemap <- addCircleMarkers(basemap, 
                                                     data = cases_per_school, 
                                                     lng = ~lon, 
                                                     lat = ~lat, 
                                                     radius = ~(cases_per_school) * 2, 
                                                     weight = 1, 
                                                     color = '#d62728',
                                                     fillOpacity = 0.3, 
                                                     label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                                                                     cases_per_school$school_name, 
                                                                     cases_per_school$city, 
                                                                     cases_per_school$school_level, 
                                                                     cases_per_school$school_board, 
                                                                     cases_per_school$school_language, 
                                                                     cases_per_school$school_enrolment, 
                                                                     cases_per_school$low_income, 
                                                                     cases_per_school$non_english, 
                                                                     cases_per_school$from_non_english, 
                                                                     cases_per_school$non_french, 
                                                                     cases_per_school$from_non_french,
                                                                     cases_per_school$special_education,
                                                                     cases_per_school$cases_per_school,
                                                                     cases_per_school$cases_per_school_staff,
                                                                     cases_per_school$cases_per_school_student,
                                                                     cases_per_school$cases_per_school_unidentified) %>% lapply(htmltools::HTML), 
                                                     labelOptions = labelOptions(
                                                         style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                                                         textsize = '15px', direction = 'auto'))
                         
                         basemap
                     })
        
    })
    
    # map_leaflet20_21 ----------------------------------------------------------
    output$map_leaflet20_21 <- renderLeaflet({
        withProgress(max = 6, 
                     value = 0, 
                     message = 'please wait...', 
                     expr = {
                         incProgress(1, 'loading shapes')
                         # regenerate the 20_21 map
                         # https://geohub.lio.gov.on.ca/datasets/province/data
                         ontario <- readOGR(dsn = 'data/shapefiles', layer = 'PROVINCE')
                         incProgress(1, 'generating map')
                         map20_21 <- leaflet(ontario)
                         incProgress(1, 'setting view')
                         map20_21 <- setView(map20_21, lng = -79.7, lat = 44.39, zoom = 8) 
                         incProgress(1, 'adding polygons')
                         map20_21 <- addPolygons(map20_21, weight = 3, fillColor = '#696969', opacity = 0.5)
                         incProgress(1, 'adding tiles')
                         map20_21 <- addProviderTiles(map20_21, providers$Esri.NatGeoWorldMap)
                         
                         # add case data markers
                         incProgress(1, 'adding markers')
                         map20_21 <- addCircleMarkers(map20_21,
                                                    data = cases_per_school_20_21, 
                                                    lng = ~lon, 
                                                    lat = ~lat, 
                                                    radius = 2,
                                                    weight = 1, 
                                                    color = '#b00000',
                                                    fillOpacity = 1)
                         map20_21 <- addCircleMarkers(map20_21, 
                                                    data = cases_per_school_20_21, 
                                                    lng = ~lon, 
                                                    lat = ~lat, 
                                                    radius = ~(cases_per_school_20_21$cases_per_school) * 2,
                                                    weight = 1, 
                                                    color = '#d62728',
                                                    fillOpacity = 0.3, 
                                                    label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Parents have no university education: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                                                                    cases_per_school_20_21$school_name, 
                                                                    cases_per_school_20_21$city, 
                                                                    cases_per_school_20_21$school_level, 
                                                                    cases_per_school_20_21$school_board, 
                                                                    cases_per_school_20_21$school_language, 
                                                                    cases_per_school_20_21$school_enrolment, 
                                                                    cases_per_school_20_21$low_income, 
                                                                    cases_per_school_20_21$non_english, 
                                                                    cases_per_school_20_21$from_non_english, 
                                                                    cases_per_school_20_21$non_french, 
                                                                    cases_per_school_20_21$from_non_french, 
                                                                    cases_per_school_20_21$some_university, 
                                                                    cases_per_school_20_21$cases_per_school,
                                                                    cases_per_school_20_21$cases_per_school_staff,
                                                                    cases_per_school_20_21$cases_per_school_student,
                                                                    cases_per_school_20_21$cases_per_school_unidentified) %>% lapply(htmltools::HTML), 
                                                    labelOptions = labelOptions(
                                                        style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
                                                        textsize = '15px', direction = 'auto'))
                         
                         map20_21
                     })
        
    })
    
    # cumulative_plot ----------------------------------------------------------
    output$cumulative_plot <- renderPlotly({
        df <- covid19_schools_summary
        fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative school-related cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative school-related student cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative school-related staff cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_unidentified_cases, name = 'Cumulative school-related unidentified cases', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Cumulative school-related cases', 
                              legend = list(x = 0.1, y = 0.9),
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'Cumulative cases'))
        fig
    })
    
    # cumulative_plot_20_21 ----------------------------------------------------------
    output$cumulative_plot_20_21 <- renderPlotly({
        df <- covid19_schools_summary_20_21
        fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative school-related cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative school-related student cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative school-related staff cases', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~cumulative_school_related_unidentified_cases, name = 'Cumulative school-related unidentified cases', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Cumulative school-related cases', 
                              legend = list(x = 0.1, y = 0.9),
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'Cumulative cases'))
        fig
    })
    
    # whichWeekView (2022-2021)-------------------------------------------------
    observeEvent(input$weeklyRadio,{
        if (input$weeklyRadio == "7-day view"){
            output$whichWeekView <- renderUI({
                div(
                    h6(div('Data reported from'), textOutput('clean_week_old_date_text'), align = 'right'),
                    
                    # weekly_summary_1_dt -----------------
                    div(tableOutput('weekly_summary_1_dt'), style = 'font-size: small; width: 100%'),
                    
                    h6('Drag this box to move it', align = 'right')
                )
            })
        }
        else { #14 day view
            output$whichWeekView <- renderUI({
                div(
                    h6(div('Data reported from'), textOutput('clean_two_weeks_old_date_text'), align = 'right'),
                    
                    # weekly_summary_3_dt -----------------
                    div(tableOutput('weekly_summary_3_dt'), style = 'font-size: small; width: 100%'),
                    
                    h6('Drag this box to move it', align = 'right')
                )
            })
        }
    })
    
    # whichWeekView2 (2021-2020) -----------------------------------------------
    observeEvent(input$weeklyRadio2,{
        if (input$weeklyRadio2 == "7-day view"){
            output$whichWeekView2 <- renderUI({
                div(
                    h6(div('Data reported from'), textOutput('clean_week_old_date_text20_21'), align = 'right'),
                    
                    # weekly_summary_4_dt -----------------
                    div(tableOutput('weekly_summary_4_dt'), style = 'font-size: small; width: 100%'),
                    
                    h6('Drag this box to move it', align = 'right')
                )
            })
        }
        else { #14 day view
            output$whichWeekView2 <- renderUI({
                div(
                    h6(div('Data reported from'), textOutput('clean_two_weeks_old_date_text20_21'), align = 'right'),
                    
                    # weekly_summary_2_dt -----------------
                    div(tableOutput('weekly_summary_2_dt'), style = 'font-size: small; width: 100%'),
                    
                    h6('Drag this box to move it', align = 'right')
                )
            })
        }
    })
    
    # daily_summary_1_dt -------------------------------------------------------
    output$daily_summary_1_dt <- renderTable({
        get_summary_table(0)
    }, align = 'r', striped = TRUE, width = '100%')
    
    # daily_summary_2_dt -------------------------------------------------------
    output$daily_summary_2_dt <- renderTable({
        get_summary_table(0)
    }, align = 'r', striped = TRUE, width = '100%')
    
    # weekly_summary_1_dt -------------------------------------------------------
    output$weekly_summary_1_dt <- renderTable({
        get_weekly_summary_table(TRUE, 0) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # weekly_summary_2_dt -------------------------------------------------------
    output$weekly_summary_2_dt <- renderTable({
        get_weekly_summary_table(FALSE, 0) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # 1 and 4, 2 and 3  have to exist separately because separate tabs can't use them at the same time
    #IE one tab can't be using 1 and another tab be using 1 as well
    #So different ones for 2022-2021 and 2021-2020
    
    # weekly_summary_3_dt -------------------------------------------------------
    output$weekly_summary_3_dt <- renderTable({
        get_weekly_summary_table(FALSE, 0) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # weekly_summary_4_dt -------------------------------------------------------
    output$weekly_summary_4_dt <- renderTable({
        get_weekly_summary_table(TRUE, 0) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # school_details_dt --------------------------------------------------------
    output$school_details_dt <- renderDT({
        #df1 <- covid19_schools_active_with_demographics_most_recent
        df <- covid19_schools_active_with_demographics_most_recent[ , c(2, 12, 4, 15, 6:8) ]
        colnames(df) <- str_replace_all(colnames(df), '_', ' ')
        colnames(df) <- str_replace_all(colnames(df), '\\.', ' ')
        colnames(df) <- str_to_title(colnames(df))
        datatable(
            df,
            options = list(
                pageLength = 10,
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
    
    # school_details_dt_20_21 --------------------------------------------------------
    output$school_details_dt_20_21 <- renderDT({
        df <- covid19_schools_active_with_demographics_most_recent_20_21[ , 2:8 ]
        colnames(df) <- str_replace_all(colnames(df), '_', ' ')
        colnames(df) <- str_to_title(colnames(df))
        datatable(
            df,
            options = list(
                pageLength = 10,
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
    
    # clean_date_reactive_text -------------------------------------------------
    output$clean_date_reactive_text <- renderText({
        #Changed from covid19_schools_active to covid19_schools_summary, which has the correct latest date matching with the case count given
        format(max(covid19_schools_summary$reported_date), '%d %B %Y')
    })
    
    # clean_date_reactive_text_20_21 -------------------------------------------------
    output$clean_date_reactive_text_20_21 <- renderText({
        format(max(covid19_schools_summary_20_21$reported_date), '%d %B %Y')
    })
    
    # clean_week_old_date_text -------------------------------------------------
    output $clean_week_old_date_text <- renderText ({
        dates <- last_week_obtain(0)
        #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
        dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
        dateString
    })
    
    # clean_two_weeks_old_date_text -------------------------------------------------
    output $clean_two_weeks_old_date_text <- renderText ({
        dates <- last_two_weeks_obtain(0)
        #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
        dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
        dateString
    })
    
    #Duplicates for logic on separate tabs, two tabs can't be using the same one at the same time!
    
    # clean_week_old_date_text20_21 -------------------------------------------------
    output $clean_week_old_date_text20_21 <- renderText ({
        dates <- last_week_obtain(0)
        #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
        dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
        dateString
    })
    
    # clean_two_weeks_old_date_text20_21 -------------------------------------------------
    output $clean_two_weeks_old_date_text20_21 <- renderText ({
        dates <- last_two_weeks_obtain(0)
        #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
        dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
        dateString
    })
    
    # cumulative_case_count_text -----------------------------------------------
    output$cumulative_case_count_text <- renderText({
        idx <- max(which(covid19_schools_summary$collected_date <= as.Date(now())))
        count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
        paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
    })
    
    # cumulative_case_count_text_20_21 -----------------------------------------------
    output$cumulative_case_count_text_20_21 <- renderText({
        idx <- max(which(covid19_schools_summary_20_21$collected_date <= as.Date(now())))
        count <- last(covid19_schools_summary_20_21[ idx, 'cumulative_school_related_cases' ])
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
        fig <- fig %>% add_trace(y = ~cumulative_school_related_unidentified_cases, name = 'Cumulative school-related unidentified cases', mode = 'lines+markers')
        fig <- fig %>% layout(title = 'Cumulative school-related cases',
                              xaxis = list(title = 'Collected date'),
                              yaxis = list (title = 'Cumulative cases'))
        fig
    })
    
    # school_related_new_cases_details_plot ------------------------------------
    output$school_related_new_cases_details_plot <- renderPlotly({
        df <- covid19_schools_summary
        idx <- which(df$collected_date >= as.Date(input$minimum_date))
        df <- df[ idx, ]
        fig <- plot_ly(df, x = ~collected_date, y = ~new_total_school_related_cases, name = 'New total school-related cases', type = 'scatter', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_student_cases, name = 'New school-related student cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_staff_cases, name = 'New school-related staff cases', mode = 'lines+markers')
        fig <- fig %>% add_trace(y = ~new_school_related_unidentified_cases, name = 'New school-related unidentified cases', mode = 'lines+markers')
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
    
    # school_summary_data_dt ---------------------------------------------------
    output$school_summary_data_dt <- renderDT({
        df <- covid19_schools_summary
        idx <- order(df$reported_date, decreasing = TRUE)
        df <- df[ idx, ]
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
    
    # school_summary_data_dt_20_21 ---------------------------------------------------
    output$school_summary_data_dt_20_21 <- renderDT({
        df <- covid19_schools_summary_20_21
        idx <- order(df$reported_date, decreasing = TRUE)
        df <- df[ idx, ]
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
                   'New School Related Unidentified Cases', 
                   'Recent Total School Related Cases', 
                   'Recent School Related Student Cases',
                   'Recent School Related Staff Cases',
                   'Recent School Related Unidentified Cases', 
                   'Past Total School Related Cases', 
                   'Past School Related Student Cases',
                   'Past School Related Staff Cases', 
                   'Past School Related Unidentified Cases', 
                   'Cumulative School Related Cases',
                   'Cumulative School Related Student Cases',
                   'Cumulative School Related Staff Cases',
                   'Cumulative School Related Unidentified Cases')
        description <- c('Date results collected',
                         'Date results reported',
                         'Count of schools with active cases currently',
                         'Count of schools closed',
                         'Total number of schools in province',
                         'Total new school-related cases of all types since last reporting date',
                         'New school-related student cases since last reporting date',
                         'New school-related staff cases since last reporting date',
                         'New school-related unidentified cases since last reporting date. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                         'Total recent school-related cases reported in the past 14 days',
                         'Recent school-related student cases reported in the past 14 days',
                         'Recent school-related staff cases reported in the past 14 days',
                         'Recent school-related unidentified cases reported in the past 14 days. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                         'Total past school-related cases',
                         'Past school-related student cases',
                         'Past school-related staff cases',
                         'Past school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                         'Cumulative total school-related cases',
                         'Cumulative school-related student cases',
                         'Cumulative school-related staff cases',
                         'Cumulative school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.')
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
    
    # school_summary_data_dictionary_dt_20_21 ----------------------------------------
    output$school_summary_data_dictionary_dt_20_21 <- renderDT({
        field <- c('Collected Date',
                   'Reported Date',
                   'Current Schools W Cases',
                   'Current Schools Closed',
                   'Current Total Number Schools', 
                   'New Total School Related Cases',
                   'New School Related Student Cases',
                   'New School Related Staff Cases', 
                   'New School Related Unidentified Cases', 
                   'Recent Total School Related Cases', 
                   'Recent School Related Student Cases',
                   'Recent School Related Staff Cases',
                   'Recent School Related Unidentified Cases', 
                   'Past Total School Related Cases', 
                   'Past School Related Student Cases',
                   'Past School Related Staff Cases', 
                   'Past School Related Unidentified Cases', 
                   'Cumulative School Related Cases',
                   'Cumulative School Related Student Cases',
                   'Cumulative School Related Staff Cases',
                   'Cumulative School Related Unidentified Cases')
        description <- c('Date results collected',
                         'Date results reported',
                         'Count of schools with active cases currently',
                         'Count of schools closed',
                         'Total number of schools in province',
                         'Total new school-related cases of all types since last reporting date',
                         'New school-related student cases since last reporting date',
                         'New school-related staff cases since last reporting date',
                         'New school-related unidentified cases since last reporting date. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                         'Total recent school-related cases reported in the past 14 days',
                         'Recent school-related student cases reported in the past 14 days',
                         'Recent school-related staff cases reported in the past 14 days',
                         'Recent school-related unidentified cases reported in the past 14 days. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                         'Total past school-related cases',
                         'Past school-related student cases',
                         'Past school-related staff cases',
                         'Past school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                         'Cumulative total school-related cases',
                         'Cumulative school-related student cases',
                         'Cumulative school-related staff cases',
                         'Cumulative school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.')
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
    
    # download_csv_button_1 ----------------------------------------------------
    output$download_csv_button_1 <- downloadHandler(
        filename = function() {
            paste('schoolcovidsummary_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_summary, file)
        }
    )
    
    # download_csv_button_1_20_21 ----------------------------------------------------
    output$download_csv_button_1_20_21 <- downloadHandler(
        filename = function() {
            paste('schoolcovidsummary_20_21_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_summary_20_21, file)
        }
    )
    
    # school_cases_demo_data_dt ------------------------------------------------
    output$school_cases_demo_data_dt <- renderDT({
        #df <- covid19_schools_active_with_demographics_most_recent[ , c(2, 12, 4, 15, 6:8) ]
        df <- covid19_schools_active_with_demographics[ , c(1, 2, 12, 4, 15, 6:11, 13, 14, 16:36)]
        idx <- order(df$reported_date, decreasing = TRUE)
        df <- df[ idx, ]
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
    
    # school_cases_demo_data_dt_20_21 ------------------------------------------------
    output$school_cases_demo_data_dt_20_21 <- renderDT({
        df <- covid19_schools_active_with_demographics_20_21
        idx <- order(df$reported_date, decreasing = TRUE)
        df <- df[ idx, ]
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
    
    # school_cases_demo_data_dictionary_dt -------------------------------------
    output$school_cases_demo_data_dictionary_dt <- renderDT({
        field <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
                   'Municipality', 'Confirmed Student Cases', 'Confirmed Staff Cases',
                   'Confirmed Unidentified Cases', 'Total Confirmed Cases',
                   'Board Number', 'Board Name', 'Board Type', 'School Number', 
                   'School Name', 'School Type', 'School Special Condition Code', 
                   'School Level', 'School Language', 'Grade Range', 'Street', 'City',
                   'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
                   'Percentage Of Students Whose First Language Is Not English', 
                   'Percentage Of Students Whose First Language Is Not French', 
                   'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
                   'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
                   'Percentage Of Students Identified As Gifted', 
                   'Percentage Of School Aged Children Who Live In Low Income Households', 
                   'Percentage Of Students Whose Parents Have Some University Education')
        description <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
                         'Municipality', NA, NA, NA, NA,
                         'Board Number', 'Board Name', 'Board Type', 'School Number', 
                         'School Name', 'School Type', 'School Special Condition Code', 
                         'School Level', 'School Language', 'Grade Range', 'Street', 'City',
                         'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
                         'Percentage Of Students Whose First Language Is Not English', 
                         'Percentage Of Students Whose First Language Is Not French', 
                         'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
                         'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
                         'Percentage Of Students Identified As Gifted', 
                         'Percentage Of School Aged Children Who Live In Low Income Households', 
                         'Percentage Of Students Whose Parents Have Some University Education')
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
    
    # school_cases_demo_data_dictionary_dt_20_21 -------------------------------------
    output$school_cases_demo_data_dictionary_dt_20_21 <- renderDT({
        field <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
                   'Municipality', 'Confirmed Student Cases', 'Confirmed Staff Cases',
                   'Confirmed Unidentified Cases', 'Total Confirmed Cases',
                   'Board Number', 'Board Name', 'Board Type', 'School Number', 
                   'School Name', 'School Type', 'School Special Condition Code', 
                   'School Level', 'School Language', 'Grade Range', 'Street', 'City',
                   'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
                   'Percentage Of Students Whose First Language Is Not English', 
                   'Percentage Of Students Whose First Language Is Not French', 
                   'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
                   'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
                   'Percentage Of Students Identified As Gifted', 
                   'Percentage Of School Aged Children Who Live In Low Income Households', 
                   'Percentage Of Students Whose Parents Have Some University Education')
        description <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
                         'Municipality', NA, NA, NA, NA,
                         'Board Number', 'Board Name', 'Board Type', 'School Number', 
                         'School Name', 'School Type', 'School Special Condition Code', 
                         'School Level', 'School Language', 'Grade Range', 'Street', 'City',
                         'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
                         'Percentage Of Students Whose First Language Is Not English', 
                         'Percentage Of Students Whose First Language Is Not French', 
                         'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
                         'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
                         'Percentage Of Students Identified As Gifted', 
                         'Percentage Of School Aged Children Who Live In Low Income Households', 
                         'Percentage Of Students Whose Parents Have Some University Education')
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
    
    # download_csv_button_2 ----------------------------------------------------
    output$download_csv_button_2 <- downloadHandler(
        filename = function() {
            paste('schoolsactivecovidwithdemographics_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_active_with_demographics, file)
        }
    )
    
    # download_csv_button_2_20_21 ----------------------------------------------------
    output$download_csv_button_2_20_21 <- downloadHandler(
        filename = function() {
            paste('schoolsactivecovidwithdemographics_20_21_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_active_with_demographics_20_21, file)
        }
    )
    
    # download_csv_button_3_20_21 ----------------------------------------------------
    output$download_csv_button_3_20_21 <- downloadHandler(
        filename = function() {
            paste('schoolcovidsummary20_21.csv', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_active_with_demographics_20_21, file)
        }
    )
    
    # risk_assessment_elementary_dt --------------------------------------------
    output$risk_assessment_elementary_dt <- renderDT({
        df <- risk_rank_elementary
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
    
    # risk_assessment_secondary_dt ---------------------------------------------
    output$risk_assessment_secondary_dt <- renderDT({
        df <- risk_rank_secondary
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
    
    # risk_assessment_neighborhood_dt ------------------------------------------
    output$risk_assessment_neighborhood_dt <- renderDT({
        df <- risk_rank_neighborhood
        colnames(df) <- str_replace_all(colnames(df), '_', ' ')
        colnames(df) <- str_to_title(colnames(df))
        brks <- seq(0, 1, by = 0.05)
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
            { paste0('rgb(255,', ., ',', ., ')') }
        datatable(
            df, 
            callback = JS(sprintf('var tips = [ %s],
                            firstRow = $("#risk_assessment_neighborhood_dt thead tr th");
                            for (var i = 1; i < tips.length; i++) {
                              $(firstRow[i]).attr("title", tips[i]);
                            }', paste0(rep('"Chance of Encountering a Transmissible COVID-19 Infection in a Group Of..."', 19), collapse = ', '))),
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE
            ),
            rownames = FALSE,
            class = 'display'
        ) %>% formatCurrency(2:27, '') %>% formatStyle(names(df)[ 2:20 ], backgroundColor = styleInterval(brks, clrs))
    })
    
    # variables_details_dt -----------------------------------------------------
    output$variables_details_dt <- renderDT({
        c1 <- c('COVID-19 Case Rate',
                '% Low-income',
                '% Living in multigenerational homes',
                '% Visible minority'
        )
        c2 <- c(2,
                1,
                1,
                1
        )
        c3 <- c('Number of COVID-19 cases in the neighbourhood (confirmed/probable cases, regardless of whether they were associated with an outbreak), per 100,000 neighbourhood population',
                'Low-income measure after tax (LIM-AT, see Statistics Canada4 for further details, Census, 2016)',
                'Multigenerational households include at least three generations of the same family (Census, 2016).',
                'Visible minority population as defined by the Employment Equity Act (Census, 2016)'
        )
        c4 <- c('Areas with a high concentration of cases, proportionate to area population, would result in a higher risk of transmission.',
                'Based on recent analysis, areas with a higher proportion of lower-income households have shown disproportional impacts of COVID-19.',
                'Multigenerational homes may put older adults at higher risk.',
                'Based on recent analysis, areas with more visible minorities how disproportional impacts of COVID-19.'
                
        )
        df <- data.frame(Indicator = c1, Weight = c2, Definition = c3, Rationale = c4)
        datatable(
            df,
            options = list(
                paging = FALSE,
                searching = FALSE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = FALSE,
                dom = 'Bfrti'
            ),
            rownames = FALSE,
            class = 'display'
        )
    })
}

# RUN THE APPLICATION ----------------------------------------------------------
shinyApp(ui = ui, server = server)
