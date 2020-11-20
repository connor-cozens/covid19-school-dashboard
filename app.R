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

# FUNCTIONS --------------------------------------------------------------------

#' get_summary_table
#' 
#' generate quick view summary table
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
    idx <- which(df$collected_date <= as.Date(now())) # as.Date(input$plot_date))
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
    df
}

# INITIALIZATION ---------------------------------------------------------------

# SHINY UI ---------------------------------------------------------------------
ui <- bootstrapPage(
    tags$head(includeHTML('gtag.html')),
    navbarPage(theme = shinytheme('united'), 
               collapsible = TRUE,
               'COVID-19 School Dashboard', 
               id = 'nav',
               
               # TAB: COVID-19 Mapper ------------------------------------------
               tabPanel('Mapper - Affected Ontario Schools',
                        div(class='outer',
                            
                            # tag: stylesheet ----------------------------------
                            tags$head(includeCSS('styles.css')),
                            
                            # leaflet: basemap  --------------------------------
                            leafletOutput('basemap_leaflet', width = '100%', height = '100%'),
                            
                            # panel: controls ----------------------------------
                            absolutePanel(id = 'controls', 
                                          class = 'panel panel-default',
                                          top = 80, 
                                          left = 55, 
                                          width = 500, 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          height = 'auto',
                                          
                                          h2('Quick View Daily Summary', align = 'right'),
                                          
                                          # cumulative_case_count_text ---------
                                          h3(textOutput('cumulative_case_count_text'), align = 'right'),
                                          
                                          # clean_date_reactive_text -----------
                                          h6(textOutput('clean_date_reactive_text'), align = 'right'),
                                          
                                          # daily_summary_1_dt -----------------
                                          div(tableOutput('daily_summary_1_dt'), style = 'font-size: small; width: 100%'),
                                          
                                          # clean_date_reactive_text -----------
                                          h6('Drag this box to move it', align = 'right')
                            )
                            
                        )
                        
               ),
               
               # TAB: Overview and Search --------------------------------------
               tabPanel('Overview and Search',
                        # cumulative_plot --------------------------------------
                        plotlyOutput('cumulative_plot', width = '100%'),
                        hr(),
                        # daily_summary_2_dt -----------------------------------
                        h3('Daily Summary', align = 'left'),
                        div(tableOutput('daily_summary_2_dt'), style = 'font-size: small; width: 100%'),
                        hr(),
                        # school_details_dt ------------------------------------
                        h3('Search Function and Table', align = 'left'),
                        div('Search schools, boards, municipalities for confirmed cases of COVID-19.', width = '100%', align = 'left'),
                        br(),
                        div(DTOutput('school_details_dt'), style = 'font-size: small; width: 100%')
               ),
               
               # TAB: Data Tables & Data Dictionary ----------------------------
               tabPanel('Data Tables & Data Dictionary',
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
                                     a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', 'Summary of cases in schools'),
                                     br(), 
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
                                     a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools', 'Schools with active COVID-19 cases'),
                                     ', ',
                                     a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics', 'School information and student demographics'),
                                     br(), 
                                     h3('Data dictionary'),
                                     DTOutput('school_cases_demo_data_dictionary_dt')
                                     
                            )
                        )
               ),
               
               # # TAB: Data Dictionary and Data Sources -----------------------
               # tabPanel('Data Dictionary and Data Sources',
               #          tabsetPanel(
               #              tabPanel('Summary of cases in schools', 
               #                       h3('Summary of cases in schools'),
               #                       br(), 
               #                       DTOutput('school_summary_data_dictionary_dt'),
               #                       br(),
               #                       'Adapted from data published by Government of Ontario: ', 
               #                       a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/7fbdbb48-d074-45d9-93cb-f7de58950418', 'Summary of cases in schools')
               #              ),
               #              tabPanel('Schools with active cases and school demographic data', 
               #                       h3('Schools with active cases and school demographic data'),
               #                       br(), 
               #                       DTOutput('school_cases_demo_data_dictionary_dt'),
               #                       br(),
               #                       'Adapted from data published by Government of Ontario: ', 
               #                       a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/8b6d22e2-7065-4b0f-966f-02640be366f2', 'Schools with active COVID-19 cases'),
               #                       ', ',
               #                       a(href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a', 'School information and student demographics')
               #              )
               #          )
               # ),
               
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
                        p('Source code for this site can be found ', a(href = 'https://gitlab.com/br00t/ontario-covid19-dashboard', 'here', target = '_blank'))
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
                        tags$div(),
                        h3('COVID-19 SCHOOL DASHBOARD KEY AIMS & INFORMATION'),
                        a(href = 'http://covid19schooldashboard.com', 'covid19schooldashboard.com'), ' reports and maps confirmed school-related cases of COVID-19 in publicly funded elementary and secondary schools in Ontario, Canada, and connects this to data on school social background characteristics (school-level demographic data).',
                        br(),
                        p('The main aim of this site is to provide real-time data visualization of affected schools for broad dissemination.'),
                        p('This will help to increase transparency and understanding of the education scenario as it evolves. It will help school communities (parents, students, teachers and staff, leaders and administrators), community members and neighbours, education and health professionals, officials, researchers, media, and the general public.'),
                        br(),
                        h4('Why is this important?'),
                        p('The effects of COVID-19 are more severe on high-risk communities, populations, and schools. There are strong equity concerns. Visualizing COVID-19 case data with data on school social background characteristics will give us a better understanding of the composition of affected schools. In short, we will get closer to understanding the human dimension of COVID-19 on school populations.'),
                        br(),
                        h4('Update frequency'),
                        p('This site is automatically updated every weekday (excluding public holidays) following the release of school-related COVID-19 case data by the Ontario Ministry of Education. Cumulative totals represent all total cases reported to the Ministry of Education as of 5 September 2020, including resolved cases. The first school-related cases appeared in the dataset on 10 September 2020.'),
                        p('This site uses the latest publicly available data on school information and student demographics released by the Ontario Ministry of Education for school background characteristics. This dataset is updated by the Ministry monthly.'),
                        p('See "Data Sources and Source Code" tab for more information on data sources used.'),
                        br(),
                        h4('Caveats'),
                        p('The main aim of the COVID-19 School Dashboard is to show which schools are affected by confirmed cases as reported in the official data, visually plot where the schools are, and to show relevant school background characteristics of affected schools. This site should not be used to draw inferences on the broader COVID-19 situation in Ontario, or on case numbers generally. A number of complementary metrics are useful in that regard.'),
                        p('The numbers of cases are extracted from official data sources and are also related to the changing testing scenario in Ontario. This can mean that as the frequency of testing increases or decreases, threshold of symptoms is widened or restricted, and backlog of results clears or increases, amongst other factors, the number of new cases may show spikes or dips. There are also known lags in data reported in the Ministry of Education dataset, which may result in real-time discrepancies.'),
                        p('Finally, Ontario instituted a phased re-opening of schools. Earlier school case data reflect a partial reopening of the system. Nearly all schools were meant to be re-opened by 21 September 2020.'),
                        br(),
                        h4('Coming soon'),
                        p('The COVID-19 School Dashboard will soon add more indicators. Version 1.0 of the site has been published to balance the need for expedience in view of the need for timely public information given the effects of COVID-19 on education.'),
                        p('Planned indicators and basic functions in the short-term include:'),
                        tags$ul(
                            tags$li('Recent cases (data available as of 1 October) '),
                            tags$li('Cumulative: % of schools per board affected with at least one case'),
                            tags$li('Number and % of schools with multiple cases '),
                            tags$li('Lists of schools with active cases. Currently can be extracted from the Data table.')
                        ),
                        p('We invite users to suggest further indicators for integration. Further web optimization and dynamic display features are also planned.'),
                        br(),
                        h3('CONTEXT'),
                        p('Pandemic-related school closures in Ontario affected over 2 million elementary and secondary school students. The province’s first school closure announcement was issued on 12 March 2020 for an initial period from 14 March to 4 April 2020. This compelled all publicly funded elementary and secondary schools to close. Public school closures were extended another three times – first until 4 May, then 31 May, and finally until the end of June 2020.'),
                        p('Phased reopening of publicly funded schools in the province began on 8 September 2020 and continued until 21 September 2020, by which time all schools should have opened.'),
                        p('When interpreting the data, it is important to keep in mind that data on COVID-19 school-related cases prior to 21 September 2020 will reflect partial education system reopening. The first school-related cases appeared in the dataset on 10 September 2020.'),
                        br(),
                        h3('HOW TO NAVIGATE THE SITE'),
                        h4('Mapper - Affected Ontario Schools Tab'),
                        p('The first school-related cases appeared in the dataset on 10 September 2020. Affected schools are plotted by geocode on the map.'), 
                        br(),
                        h5(('Bubbles')),
                        p('The size of the bubbles indicates the magnitude of cumulative cases (student, staff, unidentified) at specific schools relative to others. The bigger the bubble, the more cumulative cases at that school – that is, the more it has been affected relative to other schools.'),
                        p('Hovering on a bubble reveals school-specific COVID-19 case data and school social background information. Currently, the bubbles show: '),
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
                            tags$li('proportion of students whose parents have some university education')
                        ),
                        # p('View the Data Dictionary for definitions of these indicators.'),
                        br(),
                        h5(('Quick view summary pane')),
                        tags$ul(
                            # tags$li(tags$i('Cumulative Case Chart:'), ' Shows the total number of cumulative school-related cases in Ontario and disaggregated to show cumulative school-related student cases, cumulative school-related staff cases, and unidentified cases. "Unidentified cases" is used by the Ministry of Education to refer to the following: "In some instances, the type of case has not been identified as either student/child or staff/provider/partner due to privacy considerations. These "individuals" only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases will be tracked as "individuals" but not included in the "student/child" or "staff/provider" columns.'),
                            tags$li(tags$i('Daily Summary:'), ' Summarizes cumulative school-related cases, new total school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario). It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.')
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
                        p('Shows the number of cumulative school-related cases in Ontario. '),
                        br(),
                        h5(('Tools for added functionality')),
                        p('Hover over the legend to access tools for added functionality: download graph as .PNG image file, zoom, pan, box select, lasso select, zoom in, zoom out, autoscale, reseat axes, toggle spike lines, show closest data on hover, compare data on hover.'),
                        p('"Compare data on hover" is especially useful to see and compare the number of cases on all lines in the graph on a specific date.'),
                        br(),
                        h5(('Daily Summary')),
                        p('Summarizes cumulative school-related cases, new total school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario). It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.'),
                        br(),
                        h5(('Search Function and Table')),
                        p('Use this to search schools, boards, municipalities for data on confirmed cases of COVID-19. '),
                        br(),
                        h4('Data Tables & Data Dictionary Tab'),
                        h5(('Summary of cases in schools')),
                        p('Presents raw data of cases in schools. Data table can be manipulated in ascending or descending order by variable of interest. Table can be downloaded as a .CSV file for independent analysis.'),
                        p('Variables included: collected date; reported date; current schools with cases; current schools closed; current total number of schools; new (total school-related cases; student; staff; unidentified); recent (total school-related cases; student; staff; unidentified); past (total school-related cases; student; staff; unidentified); cumulative (total school-related cases; student; staff; unidentified).'),
                        p('Recent and past case data available as from 1 October 2020. See ', a(href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/7fbdbb48-d074-45d9-93cb-f7de58950418', 'Summary of cases in schools')),
                        p('Schools with active cases and school demographic data'),
                        p('Use the search function to see if a specific school, board, or municipality has been affected.'),
                        br(),
                        h5('Data Dictionary'),
                        p('Lists definitions of terms and variables as defined in the dataset and on COVID-19 cases in schools and child care centres Ontario Ministry of Education website. '),
                        br(),
                        h4('Data Sources and Source Code Tab'),
                        # h5(('Data Dictionary')),
                        # p('Lists all definitions of variables and terms used. Extracted from data dictionaries of original datasets sourced.'),
                        # br(),
                        h5(('Data sources')),
                        p('Lists all publicly available data sources used to generate the COVID-19 School Dashboard.'),
                        br(),
                        h5(('Source code')),
                        p('Source code for this site can be found ', a(href = 'https://gitlab.com/br00t/ontario-covid19-dashboard', 'here', target = "_blank")),
                        br(),
                        h3('AUTHORSHIP, ATTRIBUTIONS, CITATION'),
                        p(a(href = 'https://www.edu.uwo.ca/faculty-profiles/prachi-srivastava.html', 'Dr. Prachi Srivastava'), ', Associate Professor, Faculty of Education, University of Western Ontario, Canada.'),
                        p(a(href = 'mailto:prachi.srivastava@uwo.ca', 'Prachi.srivastava@uwo.ca')),
                        p(a(href = 'https://twitter.com/PrachiSrivas', '@PrachiSrivas')),
                        p(a(href = 'https://orcid.org/0000-0003-4865-8963', 'ORCID iD: 0000-0003-4865-8963')),
                        br(),
                        p('COVID-19 School Dashboard technical development and design: Peter Taylor.'),
                        br(),
                        h4('Preliminary site structure based on:'),
                        p('Parker, E., & Leclerc, Q. (2020). ', tags$em('COVID-19 tracker. '), '[Web application]. ',  a(href = 'https://vac-lshtm.shinyapps.io/ncov_tracker/', 'https://vac-lshtm.shinyapps.io/ncov_tracker/')),
                        br(),
                        h4('Cite the COVID-19 School Dashboard as:'),
                        p('Srivastava, P. (2020). ', tags$em('COVID-19 school dashboard (1.0 Nov 2020). '), '[Web application]. ', a(href = 'http://covid19schooldashboard.com/', 'http://covid19schooldashboard.com/')),
                        br(), 
                        a(href = 'https://www.edu.uwo.ca', tags$img(src = 'uwo_logo.png', height = '58', width = '243')),
                        br(),
                        br()
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
                                                     label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Parents have some university education: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                                                                     cases_per_school$school_name, 
                                                                     cases_per_school$city, 
                                                                     cases_per_school$school_level, 
                                                                     cases_per_school$school_board, 
                                                                     cases_per_school$school_language, 
                                                                     cases_per_school$school_enrolment, 
                                                                     cases_per_school$low_income, 
                                                                     # cases_per_school$special_education, 
                                                                     cases_per_school$non_english, 
                                                                     cases_per_school$from_non_english, 
                                                                     cases_per_school$non_french, 
                                                                     cases_per_school$from_non_french, 
                                                                     cases_per_school$some_university, 
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
    
    # daily_summary_1_dt -------------------------------------------------------
    output$daily_summary_1_dt <- renderTable({
        get_summary_table()
    }, align = 'r', striped = TRUE, width = '100%')
    
    # daily_summary_2_dt -------------------------------------------------------
    output$daily_summary_2_dt <- renderTable({
        get_summary_table()
    }, align = 'r', striped = TRUE, width = '100%')
    
    # school_details_dt --------------------------------------------------------
    output$school_details_dt <- renderDT({
        df <- covid19_schools_active_with_demographics_most_recent[ , 2:8 ]
        colnames(df) <- str_replace_all(colnames(df), '_', ' ')
        colnames(df) <- str_to_title(colnames(df))
        datatable(
            df,
            options = list(
                pageLength = 5,
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
        # format(as.POSIXct(input$plot_date), '%d %B %Y')
        format(as.POSIXct(now()), '%d %B %Y')
    })
    
    # cumulative_case_count_text -----------------------------------------------
    output$cumulative_case_count_text <- renderText({
        idx <- max(which(covid19_schools_summary$collected_date <= as.Date(now()))) # as.Date(input$plot_date)))
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
    
    # download_csv_button_1 ----------------------------------------------------
    output$download_csv_button_1 <- downloadHandler(
        filename = function() {
            paste('schoolcovidsummary_', format(now(), '%Y%m%d'), '.csv', sep='')
        },
        content = function(file) {
            write.csv(covid19_schools_summary, file)
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
                   # 'Percentage Of Students Receiving Special Education Services', 
                   'Percentage Of School Aged Children Who Live In Low Income Households', 
                   'Percentage Of Students Whose Parents Have Some University Education')
        description <- character(33)
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
                # dom = 'Bfrtip'
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
