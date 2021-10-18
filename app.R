# DEPENDENCIES -----------------------------------------------------------------

library(DT)
library(reshape2)
library(rgdal)
library(shiny)
library(shinythemes)
library(sp)
library(plotly)
library(xts)
# library(renv)

# renv::init()

# LOAD DATA --------------------------------------------------------------------

source('data_downloader.R')

# SETTINGS ---------------------------------------------------------------------

# FUNCTIONS --------------------------------------------------------------------

#' get_summary_table
#' 
#' generate Daily summary table
#' 
get_summary_table <- function() {
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
    df <- df[ , cn ]
    idx <- which(df$collected_date <= as.Date(now()))
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
    df$Variable <- str_to_sentence(df$Variable)
    df$Variable <- str_replace_all(df$Variable, ' w ', ' with ')
    df$Variable <- str_replace_all(df$Variable, 'school related', 'school\\-related')
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
    
    df
}

# last_week_obtain ---------------------------------------------------------
# Obtains the dates for the previous weeks Monday and Friday
last_week_obtain <- function() {
    theDate <- as.Date(max(covid19_schools_active$reported_date)) - 7
    while(weekdays(theDate) != "Friday"){
        theDate <- theDate + 1
    }
    earlyDate <- theDate - 4
    #dateString <- paste(format(earlyDate, '%d %B %Y'), "to", format(theDate, '%d %B %Y'))
    df <- list(earlyDate, theDate)
    return(df)
}

# last_two_weeks_obtain ---------------------------------------------------------
# Obtains the dates for 2 weeks ago Monday and last weeks Friday
last_two_weeks_obtain <- function() {
    theDate <- as.Date(max(covid19_schools_active$reported_date)) - 7
    while(weekdays(theDate) != "Friday"){
        theDate <- theDate + 1
    }
    earlyDate <- theDate - 11
    df <- list(earlyDate, theDate)
    return(df)
}

#' get_weekly_summary_table
#' 
#' generate Weekly summary table
#' if timeFrame == TRUE, 7 days view, otherwise 14 days view
get_weekly_summary_table <- function(timeFrame) {
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
    if (timeFrame == TRUE) {
        dates <- last_week_obtain()
    }
    if (timeFrame == FALSE) {
        print("Here we are")
        dates <- last_two_weeks_obtain()
    }
    #dates <- last_week_obtain()
    
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
               
               # TAB: COVID-19 Mapper 2021-22 ------------------------------------------
               tabPanel('Map - 2021-22',
                        div(class='outer',
                            
                            # tag: stylesheet ----------------------------------
                            tags$head(includeCSS('styles.css')),
                            
                            # leaflet: basemap  --------------------------------
                            leafletOutput('basemap_leaflet', width = '100%', height = '100%'),
                            
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
                                          
                                          
                            )
                            
                        )
                        
               ),
               # TAB: COVID-19 Mapper 2020-21 ------------------------------------------
               tabPanel('Map - 2020-21',
                        div(class='outer',
                            
                            # tag: stylesheet ----------------------------------
                            tags$head(includeCSS('styles.css')),
                            
                            # leaflet: oldmap  --------------------------------
                            leafletOutput('oldmap_leaflet', width = '100%', height = '100%'),
                            
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
                                                       
                                                       # cumulative_case_count_text_20_21 ---------
                                                       h3(textOutput('cumulative_case_count_text_20_21'), align = 'right'),
                                                       
                                                       # clean_date_reactive_text -----------
                                                       h6(div('Data last reported on'), textOutput('clean_date_reactive_text_20_21'), align = 'right'),
                                                       
                                                       h6('Drag this box to move it', align = 'right')
                                        )
                            )
               ),
               # TAB: Overview and Search --------------------------------------
               tabPanel('Overview and Search',
                        tabsetPanel(
                            tabPanel('2021-2022',
                                     # cumulative_plot --------------------------------------
                                     h3('Cumulative Case Chart'),
                                     plotlyOutput('cumulative_plot', width = '100%'),
                                     hr(),
                                     # daily_summary_2_dt -----------------------------------
                                     h3('Daily Summary', align = 'left'),
                                     div(tableOutput('daily_summary_2_dt'), style = 'font-size: small; width: 100%'),
                                     hr(),
                                     # weeklyRadio2 -----------------------------------
                                     h3('Weekly Summary', align = 'left'),
                                     div(
                                         radioButtons(
                                             inputId = "weeklyRadio2",
                                             label = strong("Select a timeframe:"),
                                             choices = list("7-day view", "14-day view"),
                                             inline = TRUE
                                         ), align = "right"),
                                     
                                     #whichWeekView2 ----------
                                     uiOutput("whichWeekView2"),
                                     hr(),
                                     # school_details_dt ------------------------------------
                                     h3('Search Function and Table', align = 'left'),
                                     div('Search schools, boards, municipalities for confirmed cases of COVID-19.', width = '100%', align = 'left'),
                                     br(),
                                     div(DTOutput('school_details_dt'), style = 'font-size: small; width: 100%')
                            ),
                            tabPanel('2020-2021',
                                     # cumulative_plot_20_21 --------------------------------------
                                     h3('Cumulative Case Chart'),
                                     plotlyOutput('cumulative_plot_20_21', width = '100%'),
                                     hr(),
                                     # school_details_dt_20_21 ------------------------------------
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
               tabPanel('Data Sources and Source Code',
                        h3('Data Sources'),
                        tags$ul(
                            tags$li(a(href = 'https://data.ontario.ca/dataset?keywords_en=COVID-19', 'All COVID-19 datasets', target = '_blank')),
                            tags$li(a(href = 'https://www.ontario.ca/page/covid-19-cases-schools-and-child-care-centres', 'COVID-19 cases in schools and child care centres', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', 'Schools COVID-19 data overview', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/new_sif_data_table_2018_2019prelim_en_august.xlsx', ' School information and student demographics dataset (.xlsx)', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics', ' School information and student demographics overview', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv', 'Schools with active COVID-19 cases dataset (.csv)', target = '_blank')),
                            tags$li(a(href = 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/7fbdbb48-d074-45d9-93cb-f7de58950418/download/schoolcovidsummary.csv', 'Summary of cases in schools dataset (.csv)', target = '_blank'))
                        ),
                        h3('Source Code'),
                        p('Source code for this site can be found ', a(href = 'https://github.com/connor-cozens/covid19-school-dashboard', 'here', target = '_blank'))
               ),
               
               # TAB: Plots ----------------------------------------------------
               # tabPanel('Plots',
               #          
               #          sidebarLayout(
               #              sidebarPanel(
               #                  sliderInput("minimum_date",
               #                              "Minimum date:",
               #                              min = as.Date(cv_min_date,"%Y-%m-%d"),
               #                              max = as.Date(current_date,"%Y-%m-%d"),
               #                              value=as.Date(cv_min_date),
               #                              timeFormat="%d %b")
               #                  
               #              ),
               #              
               #              mainPanel(
               #                  tabsetPanel(
               #                      
               #                      tabPanel('Cumulative school-related cases', 
               #                               br(), 
               #                               plotlyOutput('school_related_cases_details_plot')),
               #                      
               #                      tabPanel('New school-related cases', 
               #                               br(), 
               #                               plotlyOutput('school_related_new_cases_details_plot')),
               #                      
               #                      tabPanel('Active school-related cases by municipality', 
               #                               br(), 
               #                               plotlyOutput('active_cases_by_municipality_plot')),
               #                      
               #                      tabPanel('Active school-related cases by school board', 
               #                               br(), 
               #                               plotlyOutput('active_cases_by_board_plot'))
               #                      
               #                      # tabPanel('Schools with cases', 
               #                      #          br(), 
               #                      #          plotlyOutput('schools_with_cases_plot'))
               #                      
               #                  )
               #              )
               #          )
               # ),
               
               # TAB: Risk assessment ------------------------------------------
               # tabPanel('Risk assessment',
               # 
               #          # h3('Reducing COVID-19 Transmission Upon School Reopening: Identifying High-Risk Neighbourhoods'),
               #          # ('Prepared by: Toronto Public Health'),
               #          # br(),
               #          # ('Prepared date: August 20, 2020'),
               #          # br(),
               #          # ('Prepared for the Toronto Catholic District School Board (TCDSB)'),
               #          # br(),
               #          # h3('Background'),
               #          # p('Toronto elementary schools are set to re-open for in-person learning in September. This document outlines a method that can be used to inform decisions about areas of the city to prioritize for mitigation strategies in order to reduce the spread of COVID-19. Neighbourhood-level data is used to produce a risk score based on case information in combination with select socioeconomic indicators. This analysis can be used in conjunction with other considerations when deciding about COVID-19 risk mitigation strategies in schools. Since the evidence around COVID-19 is ever-changing, our method allows for flexibility and continuous updates based on available data.'),
               #          # h3('Methods'),
               #          # p('A composite index score was generated in order to rank neighbourhoods in terms of their risk for increased COVID-19 transmission when schools reopen.'),
               #          # h4('Rankings were generated using the following steps:'),
               #          # tags$ol(
               #          #     tags$li('For each neighbourhood with TCDSB schools, confirmed/probable COVID-19 case counts were obtained from May 29 (the date where widespread testing was announced in the province) to Aug 16, 2020 (most recent available data). Cases associated with outbreaks in long-term care or retirement homes among individuals aged 65+ were excluded from case rates, as they represent institutionalized individuals. Rates proportionate to neighbourhood population size were used. Select sociodemographic indicators were obtained using Census 2016 data (Table 1).'),
               #          #     tags$li('To generate the risk score, each variable was assigned a weight. Case rates were assigned a higher weight than other variables. Effects of each socioeconomic indicator on COVID-19 transmission are difficult to tease out and therefore they have been assigned the same weight.'),
               #          #     tags$li('Indicators within each neighbourhood were then multiplied by the assigned weight to generate a composite score. All neighbourhood scores (unique on neighbourhood level) were then subdivided into quintiles based on percentile score; higher quintiles indicate higher-risk.'),
               #          #     tags$li('sociodemographic indicators were available at the neighbourhood level only, all schools within the neighbourhood are considered of similar risk.')
               #          # ),
               #          # h4('Table 1: Variables used to generate neighbourhood risk scores'),
               #          #
               #          # # variables_details_dt ---------------------------------
               #          # div(DTOutput('variables_details_dt'), style = 'font-size: small; width: 100%'),
               #          #
               #          # h3('Results'),
               #          # p('Note: An initial list for elementary schools was produced on August 17, 2020 that included all case dates and without the additional exclusions indicated in Step 1. Tab B shows the revised table. The "Comparison" tab illustrates the differences between the two.'),
               # 
               #          # # risk_assessment_elementary_dt ------------------------
               #          # h4('Elementary School Risk Assessment'),
               #          # DTOutput('risk_assessment_elementary_dt'),
               #          #
               #          # # risk_assessment_secondary_dt -------------------------
               #          # h4('Secondary School Risk Assessment'),
               #          # DTOutput('risk_assessment_secondary_dt')
               # 
               #          # risk_assessment_secondary_dt -------------------------
               #          h3('Neighborhood Risk Assessment'),
               #          br(),
               #          DTOutput('risk_assessment_neighborhood_dt')
               # 
               # ),
               
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
                        # Overview -----
                        div(
                            h3(id = "Top of Page", 'COVID-19 SCHOOL DASHBOARD KEY AIMS & INFORMATION'),
                            p(a(href = 'http://covid19schooldashboard.com', 'covid19schooldashboard.com', target = '_blank'), ' reports and maps confirmed school-related cases of COVID-19 in publicly funded elementary and secondary schools in Ontario, Canada, and connects this to data on school social background characteristics (school-level demographic data).'),
                            p('The main aim of this site is to provide real-time data visualization of affected schools for broad dissemination. This will help to increase transparency and understanding of the education scenario as it evolves. It will help school communities (e.g., parents, students, teachers and staff, leaders and administrators), community members and neighbours, education and health professionals, officials, researchers, media, and the general public.'),
                            p('The site is currently best viewed on a desktop or tablet. Mobile device optimization is planned.'),
                            br(),
                            p('We are open to feedback and continuous improvements. Please report any data discrepancies or other suggestions.'),
                            p('Contact: ', a(href = 'mailto:covid19schooldashboard@gmail.com', 'covid19schooldashboard@gmail.com', target = '_blank')),
                            br(),
                            h4('Why is this important?'),
                            p('The effects of COVID-19 are more severe on high-risk communities, populations, and schools. There are strong equity concerns. Visualizing COVID-19 case data with data on school social background characteristics will give us a better understanding of the composition of affected schools.'),
                            p('In short, we will get closer to understanding the human dimension of COVID-19 on school populations.'),
                            br(),
                            h4('Update frequency'),
                            p('This site is automatically updated every weekday (excluding public holidays) following the release of school-related COVID-19 case data by the Ontario Ministry of Education. This site also uses the latest publicly available data on school information and student demographics released by the Ontario Ministry of Education for school background characteristics. This dataset is updated by the Ministry monthly.'),
                            br(),
                            p(tags$b('2021-22 School Year')),
                            p('Cumulative totals represent all total cases reported as of This report provides a summary of COVID-19 activity in Ontario schools. Cumulative totals represent all total cases reported to the Ministry of Education as of 2 August 2021, including resolved cases. The data were available for public access and download as on 14 September 2021. The first reported date of school-level cases was 26 August 2021.'),
                            p(tags$b('2020-21 School Year')),
                            p('Cumulative totals represent all total cases reported to the Ministry of Education as of 5 September 2020, including resolved cases. The first school-related cases appeared in the dataset on 10 September 2020.'),
                            p('The date shown in the Daily/Weekly Summary pane on the Mapper tab reports the last day on which official data were released by the Ontario Ministry of Education. This was 27 April 2021 for the 2020-21 school year.'),
                            p('See "Data Sources and Source Code" tab for more information on data sources used.'),
                            br(),
                            h4('Caveats'),
                            p('The main aim of the COVID-19 School Dashboard is to show which schools are affected by confirmed cases as reported in the official data, visually plot where the schools are, and show relevant school background characteristics of affected schools. This site should not be used to draw inferences on the broader COVID-19 situation in Ontario, or on case numbers generally. A number of complementary metrics are useful in that regard.'),
                            p('The numbers of cases are extracted from official data sources. A number of contextual factors will affect data changes. The following is an informational list of potential relevant factors, and not exhaustive. For example, changing testing scenarios can mean that as the frequency of testing increases or decreases, threshold of symptoms is widened or restricted, and backlog of results clears or increases, the number of new cases may show spikes or dips. As the situation evolves, vaccination rates and mass and partial school closures and reopening, amongst other factors, will affect changes in data. '),
                            p('There are known lags in data reported in the Ministry of Education dataset, which may result in real-time discrepancies.'),
                            p('There may be some discrepancies in school demographic data if they are in the official dataset. ', a(href = 'covid19schooldashboard@gmail.com', 'Please report them to us to fix.', target = '_blank')),
                            hr()
                        ),
                        # Policy -----
                        div(
                            h3(id = "Policy Context", 'POLICY CONTEXT'),
                            p('Pandemic-related school closures in Ontario affect over 2 million elementary and secondary school students. The situation for students and schools is rapidly evolving in Ontario.'),
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
                        # News by Year -----
                        div(
                            h4(id = "2021-2022", '2021-22 School Year'),
                            p('Schools operating on a modified/balanced calendar opened as early as 4 August 2021. According to board. The majority of schools opened according to regular board-level conventions from 7 to 10 September 2021. All schools should have been opened as on 13 September 2021.'),
                            br(),
                            h4(id = "2020-2021", '2020-21 School Year'),
                            p('Phased reopening of publicly funded schools in Ontario began on 8 September 2020 and continued until 21 September 2020, by which time all schools should have opened. This followed a period of province-wide and localised school closures. Schools have been operating virtually as of 19 April 2021, with special in-person provisions for special education needs services.'),
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
                        # Site Navigation -----
                        div(
                            h3(id = "Site Navigation", 'HOW TO NAVIGATE THE SITE'),
                            h4('Map 2021-22 - Affected Ontario Schools Tab'),
                            p('Shows daily updates to cumulative school-related cases. Affected schools are plotted by geocode on the map. Hovering on a school bubble shows school-specific data on case numbers and breakdown per school, administrative school-level data on school characteristics, and demographic data of the affected school population.'),
                            h4('Map 2020-21 - Affected Ontario Schools Tab'),
                            p('Shows the final cumulative school-related cases as last reported on the update of 27 April 2021. The first school-related cases appeared in the dataset on 10 September 2020. Affected schools are plotted by geocode on the map.'), 
                            br(),
                            h5(tags$b('Bubbles')),
                            p('The size of the bubbles indicates the magnitude of cumulative cases (student, staff, unidentified) at specific schools relative to others. ', tags$b('The bigger the bubble, the more cumulative cases at that school – that is, the more it has been affected relative to other schools.')),
                            p(tags$b('Hovering on a bubble reveals school-specific COVID-19 case data and school social background information. '), 'Currently, the bubbles show: '),
                            tags$ul(
                                tags$li('cumulative cases;'), 
                                tags$li('city;'), 
                                tags$li('level;'), 
                                tags$li('board;'), 
                                tags$li('main language of instruction;'), 
                                tags$li('enrolment;'), 
                                tags$li('proportion of students from low-income households;'), 
                                # tags$li('proportion of students receiving special education services;'),
                                tags$li('proportion of students whose first language is not English;'),
                                tags$li('proportion of students whose first language is not French;'), 
                                tags$li('proportion of students who are immigrants from a non-English country;'),
                                tags$li('proportion of students who are immigrants from a non-French country;'),
                                # tags$li('proportion of students whose parents have some university education')
                            ),
                            # p('View the Data Dictionary for definitions of these indicators.'),
                            br(),
                            h5(('Quick view summary pane')),
                            tags$ul(
                                # tags$li(tags$i('Cumulative Case Chart:'), ' Shows the total number of cumulative school-related cases in Ontario and disaggregated to show cumulative school-related student cases, cumulative school-related staff cases, and unidentified cases. "Unidentified cases" is used by the Ministry of Education to refer to the following: "In some instances, the type of case has not been identified as either student/child or staff/provider/partner due to privacy considerations. These "individuals" only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases will be tracked as "individuals" but not included in the "student/child" or "staff/provider" columns.'),
                                tags$li(em('Daily Summary:'), ' Summarizes cumulative school-related cases, new total school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario). It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.'),
                                tags$li(em('Weekly Summary:'), 'Summarizes cumulative school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario) for 7- or 14-day period from last known data reporting date in the Ministry of Education dataset.')
                            ),
                            br(),
                            # h4('Plots Tab'),
                            # p('These graphs provide an indication of the evolution of school-related cases over time. The first school-related cases appeared in the dataset on 10 September 2020. Currently, there are four graphs.'),
                            # br(),
                            # h5(('Cumulative school-related cases')),
                            # p('Shows all cumulative school-related cases, including resolved cases in Ontario and the breakdown of student cases, staff, and unidentified individual cases.'),
                            # br(),
                            # h5(('New school-related cases')),
                            # p('Shows all new school-related cases in Ontario, and the breakdown of student, staff, and unidentified cases.'),
                            # br(),
                            # h5(('Active school-related cases by municipality')),
                            # p('Shows the top 10 municipalities with active school-related cases.'),
                            # br(),
                            # h5(('Active school-related cases by school board')),
                            # p('Shows the top 10 school boards with active school-related cases.'),
                            # br(),
                            # h5(('Slider')),
                            # p('Keep the slider to the minimum date (10 Sept) to see the full evolution of cases up to the most current date for every graph.'),
                            # br(),
                            h4('Overview and Search Tab'),
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
                            h5('Schools with active cases and school demographic data [To be updated for 2021-22]'),
                            p('Presents raw data of cases in schools combined with demographic data. Data table can be manipulated in ascending or descending order by variable of interest. Table can be downloaded as a .CSV file for independent analysis.'),
                            p('Use the search function to see if a specific school, board, or municipality has been affected.'),
                            br(),
                            h5('Data Dictionary [To be updated for 2021-22]'),
                            p('Lists definitions of terms and variables as defined in the dataset and on COVID-19 cases in schools and child care centres Ontario Ministry of Education website. '),
                            br(),
                            h4('Data Sources and Source Code Tab'),
                            # h5(('Data Dictionary')),
                            # p('Lists all definitions of variables and terms used. Extracted from data dictionaries of original datasets sourced.'),
                            # br(),
                            h5(('Data sources [Currently 2020-21. To be updated for 2021-22]')),
                            p('Lists all publicly available data sources used to generate the COVID-19 School Dashboard.'),
                            br(),
                            h5(('Source code')),
                            p('Source code for this site can be found ', a(href = 'https://github.com/connor-cozens/covid19-school-dashboard', 'here', target = "_blank"), 'To be updated foe 2021-22'),
                            hr()
                        ),
                        # Future Developments -----
                        div(
                            h3(id = "Future Developments", 'COMING SOON'),
                            p('The COVID-19 School Dashboard will soon add more indicators. The site has been published to balance the need for expedience in view of the need for timely public information given the effects of COVID-19 on education.'),
                            p('Planned indicators and basic functions in the short-term include:'),
                            tags$ul(
                                tags$li('Cumulative: % of schools per board affected with at least one case'),
                                tags$li('Number and % of schools with multiple cases'),
                                tags$li('Timeline functionality to view cases by they were reported'),
                                tags$li('Mobile viewing and optimization'),
                                tags$li('Encoding issues cause accented characters to display improperly on the site. A fix is in the works.'),
                            ),
                            p('We invite users to suggest further indicators for integration. Further web optimization, dynamic display features, and mobile device optimization are also planned.'),
                            hr()
                        ),
                        # Authorship -----
                        div(
                            h3(id = "Authorship", 'AUTHORSHIP, ATTRIBUTIONS, CITATION'),
                            h4('Cite the COVID-19 School Dashboard as:'),
                            p('Srivastava, P., & Taylor, P.J. (2021). ', tags$em('COVID-19 school dashboard (1.1 May 2021). '), '[Web application]. ', a(href = 'http://covid19schooldashboard.com/', 'http://covid19schooldashboard.com/')),
                            br(),
                            p(a(href = 'https://www.edu.uwo.ca/faculty-profiles/prachi-srivastava.html', target = '_blank', 'Dr. Prachi Srivastava'), ', Associate Professor, Faculty of Education, University of Western Ontario, Canada.'),
                            p(a(href = 'mailto:prachi.srivastava@uwo.ca', 'Prachi.srivastava@uwo.ca')),
                            p(a(href = 'https://twitter.com/PrachiSrivas', target = '_blank', '@PrachiSrivas')),
                            p(a(href = 'https://orcid.org/0000-0003-4865-8963', target = '_blank', 'ORCID iD: 0000-0003-4865-8963')),
                            br(),
                            p('COVID-19 School Dashboard technical lead development and design: Peter J. Taylor'),
                            p('Ongoing development and maintenance: Justin Marshall and Connor Cozens'),
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
               ('Media',
                   tags$div(),
                   h3('Covid-19 School Dashboard in the Media'),
                   p('Archive of articles and media appearances about the COVID-19 School Dashboard.'),
                   br(),
                   h4('3 May 2021'),
                   p(a('Western University COVID Next Research Campaign', href='https://uwo.ca/research/excellence/covidnext/#view/2a', target = '_blank')),
                   br(),
                   h4('29 December 2020'),
                   p(a('The Standard Hong Kong - \'Help in dash to reopen schools\'', href='https://www.thestandard.com.hk/section-news/fc/4/226080/Help-in-dash-to-reopen-schools', target = '_blank')),
                   br(),
                   h4('30 November 2020'),
                   p(a("The London Free Press - \'Western professor's tool makes school COVID-19 data easier to find, grasp\'", href='https://lfpress.com/news/local-news/western-professors-tool-makes-school-covid-19-data-easier-to-find-grasp', target = '_blank')), 
                   br(),
                   h4('29 November 2020'), 
                   p(a('CityNews Toronto - \'Covid-19 school data base to assist parents\'', href='https://toronto.citynews.ca/video/2020/11/29/covid-19-school-data-base-to-assist-parents/', target = '_blank')), 
                   br(),
                   h4('26 November 2020'), 
                   p(a('CBC News \'Ontario News with Rita Celli\' - \'Are schools safe enough?\'', href='https://www.cbc.ca/listen/live-radio/1-45-ontario-today/clip/15811055-are-schools-safe-enough', target = '_blank')),
                   br(),
                   h4('25 November 2020'),
                   p(a("980AM Radio Show - \'Mapping COVID-19 in Ontario schools to better understand the virus' impacts\'", href='https://omny.fm/shows/am980/mapping-covid-19-in-ontario-schools-to-better-unde', target = '_blank')),
                   p(a("DailyHive News Toronto - \'There's a map showing COVID-19 cases in Ontario schools\'", href='https://dailyhive.com/toronto/covid-19-map-ontario-schools', target = '_blank')),
                   p(a("Global News - \'Coronavirus: expert in global education launches interactive map of Ontario school cases\'", href='https://globalnews.ca/news/7481210/coronavirus-interactive-map-ontario-school-cases-covid-19/', target = '_blank')),
                   br(),
                   h4('24 November 2020'),
                   p(a('Western University News - \'New interactive dashboard tracks COVID-19 cases in Ontario schools\'', href='https://news.westernu.ca/2020/11/new-interactive-dashboard-tracks-covid-19-cases-in-ontario-schools/', target = '_blank')),
                   br(),
                   h4('23 November 2020'),
                   p(a("CTV London, Ontario - \'New website helps simplify and track school COVID-19 case data\'", href='https://london.ctvnews.ca/new-website-helps-simplify-and-track-school-covid-19-case-data-1.5201172', target = '_blank'))
               ),
               
               # TAB: Our Team ------------------------------------------
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
                        h4('Peter J. Taylor'),
                        p(em('Technical Lead and Lead Developer')),
                        p(a(href = 'https://twitter.com/br00t4c', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28')), ' - ', a(href = 'https://gitlab.com/br00t', target = "_blank", tags$img(src = 'gitlab_logo.png', height = '32', width = '32'))),
                        hr(),
                        h4('Connor Cozens'),
                        p(em('Developer')),
                        p('Connor is a recent graduate from Western University with an Honors BSc in Computer Science and a Minor in Software Engineering. He is currently joining the Tech & Operations team at RBC in Toronto. Connor is passionate about artificial intelligence and data science. He is always looking for projects in these areas to get involved with, to learn, and contribute to growing research fields in these areas.'),
                        p('Contact Connor at: ', a(href = 'mailto:covid19schooldashboard@gmail.com', 'covid19schooldashboard@gmail.com')),
                        p(a(href = 'https://twitter.com/ConnorCozens', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28')), ' - ', a(href = 'https://www.linkedin.com/in/connorcozens/', target = "_blank", tags$img(src = 'linkedin_logo.png', height = '24', width = '24')), ' - ', a(href = 'https://github.com/connor-cozens', target = "_blank", tags$img(src = 'github_logo.png', height = '32', width = '32'))),
                        hr(),
                        h4('Justin Marshall'),
                        p(em('Developer')),
                        p('Justin graduated from Western University with an Honors BSc in Computer Science and a Minor in Software Engineering. He is currently open to full-time roles and opportunities. In addition to data visualization, Justin is interested in fields including game development, and is currently working on creating his own games and building a portfolio.'),
                        p('Contact Justin  at: ', a(href = 'mailto:covid19schooldashboard@gmail.com', 'covid19schooldashboard@gmail.com')),
                        p(a(href = 'https://twitter.com/JuiceMarsh', target = "_blank", tags$img(src = 'twitter_logo.png', height = '24', width = '28')), ' - ', a(href = 'https://www.linkedin.com/in/JustinMarshall1998/', target = "_blank", tags$img(src = 'linkedin_logo.png', height = '24', width = '24')), ' - ', a(href = 'https://github.com/JustinMarshall98', target = "_blank", tags$img(src = 'github_logo.png', height = '32', width = '32'))),
                        hr(),
                        p('We are open to feedback and continuous improvements. Please report any data discrepancies or other suggestions.'),
                        p('Contact: ', a(href = 'mailto:covid19schooldashboard@gmail.com', 'covid19schooldashboard@gmail.com')),
               )
               
    )          
)

# SHINY SERVER -----------------------------------------------------------------

server <- function(input, output) {
    
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
                                                     radius = ~(cases_per_school) * 2, # ~(cases_per_school)^(1/5), 
                                                     weight = 1, 
                                                     color = '#d62728',
                                                     fillOpacity = 0.1, 
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
                                                                     #cases_per_school$some_university, 
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
    
    # oldmap_leaflet ----------------------------------------------------------
    output$oldmap_leaflet <- renderLeaflet({
        withProgress(max = 6, 
                     value = 0, 
                     message = 'please wait...', 
                     expr = {
                         incProgress(1, 'loading shapes')
                         # regenerate the oldmap
                         # https://geohub.lio.gov.on.ca/datasets/province/data
                         ontario <- readOGR(dsn = 'data/shapefiles', layer = 'PROVINCE')
                         incProgress(1, 'generating map')
                         oldmap <- leaflet(ontario)
                         incProgress(1, 'setting view')
                         oldmap <- setView(oldmap, lng = -79.7, lat = 44.39, zoom = 8) 
                         incProgress(1, 'adding polygons')
                         oldmap <- addPolygons(oldmap, weight = 3, fillColor = '#696969', opacity = 0.5)
                         incProgress(1, 'adding tiles')
                         oldmap <- addProviderTiles(oldmap, providers$Esri.NatGeoWorldMap)
                         
                         # add case data markers
                         incProgress(1, 'adding markers')
                         oldmap <- addCircleMarkers(oldmap, 
                                                    data = cases_per_school_20_21, 
                                                    lng = ~lon, 
                                                    lat = ~lat, 
                                                    radius = ~(cases_per_school_20_21$cases_per_school) * 2,
                                                    weight = 1, 
                                                    color = '#d62728',
                                                    fillOpacity = 0.1, 
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
                         
                         oldmap
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
        # fig <- config(fig, displayModeBar = FALSE)
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
        # fig <- config(fig, displayModeBar = FALSE)
        fig
    })
    
    # whichWeekView ------------------------------------------------------------
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
        else {
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
    
    # whichWeekView2 ------------------------------------------------------------
    observeEvent(input$weeklyRadio2,{
        if (input$weeklyRadio2 == "7-day view"){
            output$whichWeekView2 <- renderUI({
                div(
                    h6(div('Data reported from'), textOutput('clean_week_old_date_text2'), align = 'right'),
                    
                    # weekly_summary_1_dt -----------------
                    div(tableOutput('weekly_summary_4_dt'), style = 'font-size: small; width: 100%'),
                    
                    h6('Drag this box to move it', align = 'right')
                )
            })
        }
        else {
            output$whichWeekView2 <- renderUI({
                div(
                    h6(div('Data reported from'), textOutput('clean_two_weeks_old_date_text2'), align = 'right'),
                    
                    # weekly_summary_2_dt -----------------
                    div(tableOutput('weekly_summary_2_dt'), style = 'font-size: small; width: 100%'),
                    
                    h6('Drag this box to move it', align = 'right')
                )
            })
        }
    })
    
    # daily_summary_1_dt -------------------------------------------------------
    output$daily_summary_1_dt <- renderTable({
        get_summary_table()
    }, align = 'r', striped = TRUE, width = '100%')
    
    # daily_summary_2_dt -------------------------------------------------------
    output$daily_summary_2_dt <- renderTable({
        get_summary_table()
    }, align = 'r', striped = TRUE, width = '100%')
    
    # weekly_summary_1_dt -------------------------------------------------------
    output$weekly_summary_1_dt <- renderTable({
        get_weekly_summary_table(TRUE) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # weekly_summary_2_dt -------------------------------------------------------
    output$weekly_summary_2_dt <- renderTable({
        get_weekly_summary_table(FALSE) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # 1 and 4, 2 and 3  have to exist separately because separate tabs can't use them at the same time
    #IE one tab can't be using 1 and another tab be using 1 as well
    
    # weekly_summary_3_dt -------------------------------------------------------
    output$weekly_summary_3_dt <- renderTable({
        get_weekly_summary_table(FALSE) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # weekly_summary_4_dt -------------------------------------------------------
    output$weekly_summary_4_dt <- renderTable({
        get_weekly_summary_table(TRUE) #FIX
    }, align = 'r', striped = TRUE, width = '100%')
    
    # school_details_dt --------------------------------------------------------
    output$school_details_dt <- renderDT({
        df <- covid19_schools_active_with_demographics_most_recent[ , 2:8 ]
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
        dates <- last_week_obtain()
        #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
        dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
        dateString
    })
    
    # clean_two_weeks_old_date_text -------------------------------------------------
    output $clean_two_weeks_old_date_text <- renderText ({
        dates <- last_two_weeks_obtain()
        #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
        dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
        dateString
    })
    
    #Duplicates for logic on separate tabs, two tabs can't be using the same one at the same time!
    
    # clean_week_old_date_text2 -------------------------------------------------
    output $clean_week_old_date_text2 <- renderText ({
        dates <- last_week_obtain()
        #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
        dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
        dateString
    })
    
    # clean_two_weeks_old_date_text2 -------------------------------------------------
    output $clean_two_weeks_old_date_text2 <- renderText ({
        dates <- last_two_weeks_obtain()
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
        df <- covid19_schools_active_with_demographics
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

# DEPENDENCY MANAGEMENT --------------------------------------------------------
# renv::snapshot()
