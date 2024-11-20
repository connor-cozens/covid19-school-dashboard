# DEPENDENCIES -----------------------------------------------------------------

library(DT)
library(reshape2)
library(shiny)
library(shinythemes)
library(sp)
library(plotly)
library(xts)
library(ggplot2)
library(scales)
library(lubridate)
library(sf)

# renv::init()

# LOAD DATA --------------------------------------------------------------------

source('data_downloader.R')

# LOAD PANELS -------

source('pages/overview.R')
source('pages/data_tables.R')
source('pages/data_sources.R')
source('pages/about.R')
source('pages/media.R')
source('pages/team.R')

source('pages/school_closures.R')
source('pages/infections_map_2021_2022.R')
source('pages/infections_map_2020_2021.R')

source('functions.R')

# PLOT ---------

timeline_plot <- ggplot(df, aes(
  x = date,
  y = 0,
  col = status,
  label = milestone
))
timeline_plot <- timeline_plot + labs(col = "Milestones")
timeline_plot <- timeline_plot + scale_color_manual(values = status_colors,
                                                    labels = status_levels,
                                                    drop = FALSE)
timeline_plot <- timeline_plot + theme_classic()

# Plot horizontal black line for timeline
timeline_plot <- timeline_plot + geom_hline(yintercept = 0,
                                            color = "black",
                                            size = 0.3)

# Plot vertical segment lines for milestones
timeline_plot <- timeline_plot + geom_segment(
  data = df[df$month_count == 1, ],
  aes(y = position, yend = 0, xend = date),
  color = 'black',
  size = 0.2
)

# Plot scatter points at zero and date
timeline_plot <- timeline_plot + geom_point(aes(y = 0), size = 3)

# Don't show axes, appropriately position legend
timeline_plot <- timeline_plot + theme(
  axis.line.y = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(),
  legend.position = "bottom"
)

# Show text for each month
timeline_plot <- timeline_plot + geom_text(
  data = month_df,
  aes(x = month_date_range, y = -0.1, label = month_format),
  size = 2.5,
  vjust = 0.5,
  color = 'black',
  angle = 90
)
# Show year text
timeline_plot <- timeline_plot + geom_text(
  data = year_df,
  aes(
    x = year_date_range,
    y = -0.2,
    label = year_format,
    fontface = "bold"
  ),
  size = 2.5,
  color = 'black'
)
# Show text for each milestone
timeline_plot <- timeline_plot + geom_text(aes(y = text_position, label =
                                                 milestone), size = 2.5)
#ggsave('timeline_plot.jpg', timeline_plot, device="jpg", path="www")

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
  tags$head(includeHTML('gtag.html'), tags$style(
    HTML(
      "
                              @import url('https://fonts.googleapis.com/css2?family=Nunito+Sans&display=swap');
                              p { font-family: 'Nunito Sans';},
                              h1 { font-family: 'Nunito Sans';},
                              h2 { font-family: 'Nunito Sans';},
                              h3 { font-family: 'Nunito Sans';},
                              h4 { font-family: 'Nunito Sans';},
                              "
    )
  )),
  navbarPage(
    theme = shinytheme('united'),
    collapsible = TRUE,
    'COVID-19 School Dashboard',
    id = 'nav',
    
    # Tabs Imported from Separate Page Files
    school_closures_panel,
    infections_2021_2022_panel,
    infections_2020_2021_panel,
    overview_panel,
    data_tables_panel,
    data_sources_panel,
    about_panel,
    media_panel,
    team_panel,
  ),
  tags$head(tags$style(HTML(navbar_js)))
)

# SHINY SERVER -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # SECTION: School Closures Map ----
  
  ## School Closures View Options -------
  #Is the viewing options menu open?
  viewOptionsOpen_closures <- TRUE
  #Are we currently viewing schools with cases? (If timeslider is closed)
  schoolsWithCases_closures <- FALSE
  #Are we currently viewing schools without cases? (If timeslider is closed)
  schoolsWithoutCases_closures <- FALSE
  #Is the timeslider currently open for this tab
  showTimeslider_closures <- TRUE
  #Do we want to currently show demographic data?
  vDemographics_closures <- TRUE
  #Suppress the menu's opening for the first time counting as 'ticking' a checkbox in the menu
  suppressFirstResponse1_closures <- TRUE
  suppressFirstResponse2_closures <- TRUE
  suppressFirstResponse3_closures <- TRUE
  
  selected_date_closures <- as.Date("2021-09-13")
  
  #print("Updating Markers, Current School Closure Date is: ")
  #print(selected_date_closures)
  
  leafletProxy('basemap_leaflet_closures') %>%
    clearMarkers()
  
  school_closures_merged <- rbind(school_closures_sept_dec_21_21,
                                  school_closures_jan_may_22_22)
  
  school_closures_merged_with_demographics <- merge(
    school_closures_merged,
    school_demographics,
    by.x = "School Name",
    by.y = "school name",
    all.x = TRUE
  )
  
  # print("School Closures Merged With Demographics: ")
  # print(school_closures_merged_with_demographics[0])
  # print(head(school_closures_merged_with_demographics, 1))
  
  closures_merged <- subset(
    school_closures_merged_with_demographics,
    `Date of Closure` <= selected_date_closures & selected_date_closures <= `Date of Reopening`
  )
  
  # print("MAIN SECTION - Closures Merged: ")
  # print(closures_merged[0])
  # print(head(closures_merged))
  
  ## closures Leaflet Map ----
  output$basemap_leaflet_closures <- renderLeaflet({
    withProgress(
      max = 6,
      value = 0,
      message = 'please wait...',
      expr = {
        incProgress(1, 'loading shapes')
        # regenerate the basemap
        # https://geohub.lio.gov.on.ca/datasets/province/data
        ontario <- st_read(file.path('data/shapefiles', layer = 'PROVINCE.shp'))
        incProgress(1, 'generating map')
        basemap <- leaflet(ontario)
        incProgress(1, 'setting view')
        basemap <- setView(basemap,
                           lng = -79.7,
                           lat = 44.39,
                           zoom = 8)
        incProgress(1, 'adding polygons')
        basemap <- addPolygons(
          basemap,
          weight = 3,
          fillColor = '#696969',
          opacity = 0.5
        )
        incProgress(1, 'adding tiles')
        basemap <- addProviderTiles(basemap, providers$Esri.NatGeoWorldMap)
        
        # add case data markers
        incProgress(1, 'adding markers')
        basemap <- addCircleMarkers(
          basemap,
          data = closures_merged,
          lng = closures_merged$longitude,
          lat = closures_merged$latitude,
          # radius = filtered_cases_pst$total_confirmed_cases * 2,
          radius = 60,
          weight = 3,
          color = '#FF9900',
          # Use bright purple since we are only showing markers within the date range
          fillOpacity = 0.3,
          label = closures_merged %>%
            rowwise() %>%
            mutate(
              closure_date_formatted = format(as.Date(`Date of Closure`), "%Y-%m-%d"),
              reopening_date_formatted = format(as.Date(`Date of Reopening`), "%Y-%m-%d"),
              label_text = sprintf(
                '<div style="background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Closure Date: %s<br/>Reopening Date: %s<br/>Closing Authority: %s<br/></div>',
                `School Name`,
                city,
                `school level`,
                `board name`,
                `school language`,
                enrolment,
                `percentage of school-aged children who live in low-income households`,
                `percentage of students whose first language is not english`,
                `percentage of students who are new to canada from a non-english speaking country`,
                `percentage of students whose first language is not french`,
                `percentage of students who are new to canada from a non-french speaking country`,
                `percentage of students receiving special education services`,
                # total_confirmed_cases,
                # confirmed_staff_cases,
                # confirmed_student_cases,
                # confirmed_unidentified_cases,
                closure_date_formatted,
                reopening_date_formatted,
                `Reason for Closure`
              )
            ) %>%
            pull(label_text) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list(
              'font-weight' = 'normal',
              padding = '3px 8px',
              color = '#d62728'
            ),
            textsize = '15px',
            direction = 'auto'
          )
        )
        
        basemap
      }
    )
    
  })
  
  # Render Options
  output$mapperViewOptions_closures <- renderUI({
    absolutePanel(
      id = 'options_closures',
      class = 'panel panel-default',
      top = "5%",
      right = "0%",
      width = 'auto',
      #fixed = TRUE,
      draggable = FALSE,
      height = 'auto',
      style = "padding-left: 1%;
                              border-radius: 25px;",
      
      # checkboxInput("visOp1_closures", "Schools with Cases", value = schoolsWithCases_closures),
      # checkboxInput("visOp2_closures", "Schools without Cases", value = schoolsWithoutCases_closures),
      checkboxInput("visTS_closures", "View Timeslider, Case over time", value = showTimeslider_closures),
      checkboxInput("visDemos_closures", "Show Demographic Data", value = vDemographics_closures)
    )
  })
  
  
  # # Observes the button for getting the viewing options menu (closures)
  # observeEvent(input$getOptions_closures, {
  #   viewOptionsOpen_closures <<- !viewOptionsOpen_closures #flip when button is pressed
  #   if (!viewOptionsOpen_closures) {
  #     output$mapperViewOptions_closures <- renderUI({
  #       #Render nothing in this spot
  #     })
  #   }
  #   else{
  #     output$mapperViewOptions_closures <- renderUI({
  #       absolutePanel(
  #         id = 'options_closures',
  #         class = 'panel panel-default',
  #         top = "5%",
  #         right = "0%",
  #         width = 'auto',
  #         #fixed = TRUE,
  #         draggable = FALSE,
  #         height = 'auto',
  #         style = "padding-left: 1%;
  #                             border-radius: 25px;",
  #         
  #         checkboxInput("visOp1_closures", "Schools with Cases", value = schoolsWithCases_closures),
  #         checkboxInput("visOp2_closures", "Schools without Cases", value = schoolsWithoutCases_closures),
  #         checkboxInput("visTS_closures", "View Timeslider, Case over time", value = showTimeslider_closures)
  #       )
  #     })
  #   }
  # })
  
  ## Update School Closures Map Markers ------
  updateMarkers_closures <- function (selected_date_closures) {
    #print("Updating Markers, Current School Closure Date is: ")
    #print(selected_date_closures)
    
    leafletProxy('basemap_leaflet_closures') %>%
      clearMarkers()
    
    school_closures_merged <- rbind(school_closures_sept_dec_21_21,
                                             school_closures_jan_may_22_22)

    school_closures_merged_with_demographics <- merge(
      school_closures_merged,
      school_demographics,
      by.x = "School Name",
      by.y = "school name",
      all.x = TRUE
    )

    # print("School Closures Merged With Demographics: ")
    # print(school_closures_merged_with_demographics[0])
    # print(head(school_closures_merged_with_demographics, 1))

    closures_merged <- subset(
      school_closures_merged_with_demographics,
      `Date of Closure` <= selected_date_closures & selected_date_closures <= `Date of Reopening`
    )

    #print("UPDATE MARKERS FUNCTION - Closures Merged: ")
    #print(closures_merged[0])
    # print(head(closures_merged))

    # closures_merged[, "geo_query_str"] <- NA
    # for (i in 0:nrow(closures_merged) - 1) {
    #   closures_merged$geo_query_str[i] = sprintf('%s,%s,Ontario,Canada',
    #                                        str_trim(closures_merged$school[i]),
    #                                        closures_merged$municipality[i])
    # }
    
    if (nrow(closures_merged) > 0) {
      leafletProxy('basemap_leaflet_closures') %>%
        clearMarkers()
      
      leafletProxy(mapId = 'basemap_leaflet_closures', session = session) %>%
        addCircleMarkers(
          data = closures_merged,
          lng = closures_merged$longitude,
          lat = closures_merged$latitude,
          # radius = filtered_cases_pst$total_confirmed_cases * 2,
          radius = 20,
          weight = 1,
          color = '#00FFBB',
          # Use bright purple since we are only showing markers within the date range
          fillOpacity = 0.3,
          label = closures_merged %>%
            rowwise() %>%
            mutate(
              closure_date_formatted = format(as.Date(`Date of Closure`), "%Y-%m-%d"),
              reopening_date_formatted = format(as.Date(`Date of Reopening`), "%Y-%m-%d"),
              label_text = sprintf(
                '<div style="background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Closure Date: %s<br/>Reopening Date: %s<br/>Closing Authority: %s<br/></div>',
                `School Name`,
                city,
                `school level`,
                `board name`,
                `school language`,
                enrolment,
                `percentage of school-aged children who live in low-income households`,
                `percentage of students whose first language is not english`,
                `percentage of students who are new to canada from a non-english speaking country`,
                `percentage of students whose first language is not french`,
                `percentage of students who are new to canada from a non-french speaking country`,
                `percentage of students receiving special education services`,
                # total_confirmed_cases,
                # confirmed_staff_cases,
                # confirmed_student_cases,
                # confirmed_unidentified_cases,
                closure_date_formatted,
                reopening_date_formatted,
                `Reason for Closure`
              )
            ) %>%
            pull(label_text) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list(
              'font-weight' = 'normal',
              padding = '3px 8px',
              color = '#d62728'
            ),
            textsize = '15px',
            direction = 'auto'
          )
        )
    }
  }
  
  ## Closures Activity Observers -----
  ### Schools With Cases ----
  # Observes the activity for Mapper Closures "Schools With Cases" option
  observeEvent(input$visOp1_closures, {
    # if (!suppressFirstResponse1_closures && !input$visTS_closures) {
      if (!input$visOp1_closures) {
        schoolsWithCases <<- FALSE
        updateMarkers_closures(selected_date_closures)
      }
      else {
        schoolsWithCases <<- TRUE
        updateMarkers_closures(selected_date_closures)
      }
    # }
    # else {
    #   suppressFirstResponse1 <<- FALSE
    # }
  }, ignoreInit = TRUE)
  
  ### Schools Without Cases ----
  observeEvent(input$visOp2_closures, {
    # if (!suppressFirstResponse2_closures && !input$visTS_closures) {
      if (!input$visOp2_closures) {
        schoolsWithoutCases <<- FALSE
        updateMarkers_closures(selected_date_closures)
      }
      else {
        schoolsWithoutCases <<- TRUE
        updateMarkers_closures(selected_date_closures)
      }
    # }
    # else {
    #   suppressFirstResponse2_closures <<- FALSE
    # }
  }, ignoreInit = TRUE)
  
  ### View Timeslider ----
  observeEvent(input$visTS_closures, {
    if (!input$visTS_closures) {
      showTimeslider_closures <<- FALSE
      #Remove timeslider
      output$timesliderViewer_closures <- renderUI({
        #Render nothing here
      })
      updateMarkers_closures(selected_date_closures)
    }
    else{
      showTimeslider_closures <<- TRUE
      # updateCheckboxInput(session, "visOp1_closures", value = FALSE)
      # updateCheckboxInput(session, "visOp2_closures", value = FALSE)
      # Make timeslider appear
      output$timesliderViewer_closures <- renderUI({
        div(
          p("Select a date to view data reported at that time:", style = "color:white;font:Helvetica;padding-left:10px;padding-top:15px;"),
          sliderInput(
            "obs",
            label = NULL,
            min = as.Date("2021-09-13", "%Y-%m-%d"),
            max = as.Date("2022-06-30", "%Y-%m-%d"),
            value = as.Date("2021-09-13"),
            timeFormat = "%Y-%m-%d",
            width = '95%'
          ),
          style = "position:absolute;bottom:0;left:0;right:0;background-color:#d34615;padding-left:3%"
        )
      })
    }
  })
  
  
  ### Show Demographics Data Observer ----
  observeEvent(input$visDemos_closures, {
    if (!input$visDemos_closures) {
      vDemographics_closures <<- FALSE
      updateMarkers_closures(selected_date_closures)
    } else {
      vDemographics_closures <<- TRUE
      updateMarkers_closures(selected_date_closures)
    }
  })
  
  # Panel: TIME SLIDER SCHOOL CLOSURES -------------
  ## Activity Monitors --------
  ### Timeslider Activity ----
  # Observes activity (movement) on the timeslider and adjusts data being viewed accordingly (2022-2021)
  observeEvent(ignoreInit = TRUE, list(input$obs, input$visDemos_closures), {
    if (!input$visDemos_closures) {
      vDemographics_closures <<- FALSE
    } else {
      vDemographics_closures <<- TRUE
    }
    
    selected_date_closures <- input$obs
    print("In observer, selected date from the timeslider is: ")
    print(selected_date_closures)
    updateMarkers_closures(selected_date_closures)
    
    closures_merged <- subset(
      school_closures_merged_with_demographics,
      `Date of Closure` <= selected_date_closures &
        selected_date_closures <= `Date of Reopening`
    )
    
    #print("ACTIVITY MONITOR - Closures Merged: ")
    #print(closures_merged[0])
    # print(head(closures_merged))
    
    ### Circle Creation -------
    # CREATE SCHOOL CLOSURES DATA
    if (nrow(closures_merged)) {
      leafletProxy(mapId = 'basemap_leaflet_closures', session = session) %>%
        addCircleMarkers(
          data = closures_merged,
          lng = closures_merged$longitude,
          lat = closures_merged$latitude,
          radius = 20,
          weight = 1,
          color = '#FF00FF',
          # Use bright purple for markers
          fillOpacity = 0.3,
          label = closures_merged %>%
            rowwise() %>%
            mutate(
              closure_date_formatted = format(as.Date(`Date of Closure`), "%Y-%m-%d"),
              reopening_date_formatted = format(as.Date(`Date of Reopening`), "%Y-%m-%d"),
              label_text = if (!vDemographics_closures) {
                # Basic label fields when vDemographics_closures is FALSE
                sprintf(
                  '<div style="background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Closure Date: %s<br/>Reopening Date: %s<br/>Closing Authority: %s</div>',
                  `School Name`,
                  city,
                  `school level`,
                  `board name`,
                  closure_date_formatted,
                  reopening_date_formatted,
                  `Reason for Closure`
                )
              } else {
                # Extended label fields when vDemographics_closures is TRUE
                sprintf(
                  '<div style="background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Closure Date: %s<br/>Reopening Date: %s<br/>Closing Authority: %s</div>',
                  `School Name`,
                  city,
                  `school level`,
                  `board name`,
                  `school language`,
                  enrolment,
                  `percentage of school-aged children who live in low-income households`,
                  `percentage of students whose first language is not english`,
                  `percentage of students who are new to canada from a non-english speaking country`,
                  `percentage of students whose first language is not french`,
                  `percentage of students who are new to canada from a non-french speaking country`,
                  `percentage of students receiving special education services`,
                  closure_date_formatted,
                  reopening_date_formatted,
                  `Reason for Closure`
                )
              }
            ) %>%
            pull(label_text) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list(
              'font-weight' = 'normal',
              padding = '3px 8px',
              color = '#d62728'
            ),
            textsize = '15px',
            direction = 'auto'
          )
        )
    }
  })
  
  # SECTION: 2021-2022 MAP ----
  ## 2021-2022 Leaflet Map ----
  output$basemap_leaflet <- renderLeaflet({
    withProgress(max = 6, 
                 value = 0, 
                 message = 'please wait...', 
                 expr = {
                   incProgress(1, 'loading shapes')
                   # regenerate the basemap
                   # https://geohub.lio.gov.on.ca/datasets/province/data
                   ontario <- st_read(file.path('data/shapefiles', layer = 'PROVINCE.shp'))
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
  
  ## 2021-2022 View Options -------
  #Is the viewing options menu open?
  viewOptionsOpen <- FALSE
  #Are we currently viewing schools with cases? (If timeslider is closed)
  schoolsWithCases <- FALSE
  #Are we currently viewing schools without cases? (If timeslider is closed)
  schoolsWithoutCases <- FALSE
  #Is the timeslider currently open for this tab
  vTimeSlider <- TRUE
  #Suppress the menu's opening for the first time counting as 'ticking' a checkbox in the menu
  suppressFirstResponse1 <- TRUE
  suppressFirstResponse2 <- TRUE
  suppressFirstResponse3 <- TRUE
  
  ## Update 2021-2022 Map Markers ------
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
          data = Merged_School_Data_20_21, 
          lng = ~lon, 
          lat = ~lat, 
          radius = 2,
          weight = 1, 
          color = '#808080',
          fillOpacity = 1)
      leafletProxy('basemap_leaflet') %>%
        addCircleMarkers( 
          data = cases_per_school, 
          lng = ~lon, 
          lat = ~lat, 
          radius = ~(cases_per_school) * 2,
          weight = 1, 
          color = '#808080',
          fillOpacity = 0.3, 
          label = sprintf('<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>', 
                          Merged_School_Data_20_21$school_name, 
                          Merged_School_Data_20_21$city, 
                          Merged_School_Data_20_21$school_level, 
                          Merged_School_Data_20_21$school_board, 
                          Merged_School_Data_20_21$school_language, 
                          Merged_School_Data_20_21$school_enrolment, 
                          Merged_School_Data_20_21$low_income, 
                          Merged_School_Data_20_21$non_english, 
                          Merged_School_Data_20_21$from_non_english, 
                          Merged_School_Data_20_21$non_french, 
                          Merged_School_Data_20_21$from_non_french, 
                          Merged_School_Data_20_21$special_education,
                          Merged_School_Data_20_21$cases_per_school,
                          Merged_School_Data_20_21$cases_per_school_staff,
                          Merged_School_Data_20_21$cases_per_school_student,
                          Merged_School_Data_20_21$cases_per_school_unidentified) %>% lapply(htmltools::HTML), 
          labelOptions = labelOptions(
            style = list('font-weight' = 'normal', padding = '3px 8px', color = '#d62728'),
            textsize = '15px', direction = 'auto'))
    }
  }
  
  ## 2021-2022 Activity Observers -----
  ### Schools With Cases ----
  # Observes the activity for Mapper 2022-2021 "Schools With Cases" option
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
  ### Schools Without Cases ----
  observeEvent(input$visOp2,{
    if (!suppressFirstResponse2 && !input$visTS) {
      if (!input$visOp2) {
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
  
  ### View Timeslider ----
  observeEvent(input$visTS,{
    # if (suppressFirstResponse3){
    if (!input$visTS){
      vTimeSlider <<- FALSE
      #Remove timeslider
      output$timesliderViewer <- renderUI({
        #Render nothing here
      })
      updateMarkers()
      
      #Update daily summary tab when timeslider input changes
      # cumulative_case_count_text
      output$cumulative_case_count_text <- renderText({
        idx <- max(which(covid19_schools_summary$collected_date <= as.Date(now())))
        count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
        paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
      })
      # daily_summary_1_dt
      output$daily_summary_1_dt <- renderTable({
        get_summary_table(0)
      }, align = 'r', striped = TRUE, width = '100%')
      # clean_date_reactive_text
      output$clean_date_reactive_text <- renderText({
        #Changed from covid19_schools_active to covid19_schools_summary, which has the correct latest date matching with the case count given
        format(max(covid19_schools_summary$reported_date), '%d %B %Y')
      })
      #Update weekly summary tab when timeslider goes away
      #7 days
      output$weekly_summary_1_dt <- renderTable({
        get_weekly_summary_table(TRUE, 0)
      }, align = 'r', striped = TRUE, width = '100%')
      # clean_week_old_date_text
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
      # clean_two_weeks_old_date_text
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
    # }
    # else{
    #   suppressFirstResponse3 <<- FALSE
    # }
    
  })
  
  # Panel: TIME SLIDER 2021-2022 -------------
  ## Activity Monitors --------
  ### Timeslider Activity ----
  #Observes activity (movement) on the timeslider and adjusts data being viewed accordingly (2022-2021)
  observeEvent(input$obs,{
    geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
                             str_trim(covid19_schools_active_with_demographics$school.name), 
                             covid19_schools_active_with_demographics$municipality)
    
    selected_date <- input$obs
    
    # Update daily summary tab when timeslider input changes
    # cumulative_case_count_text
    output$cumulative_case_count_text <- renderText({
      idx <- max(which(covid19_schools_summary$collected_date <= as.Date(selected_date)))
      count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
      paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
    })
    # daily_summary_1_dt
    output$daily_summary_1_dt <- renderTable({
      get_summary_table(selected_date)
    }, align = 'r', striped = TRUE, width = '100%')
    # clean_date_reactive_text
    output$clean_date_reactive_text <- renderText({
      #Changed from covid19_schools_active to covid19_schools_summary, which has the correct latest date matching with the case count given
      format(selected_date, '%d %B %Y')
    })
    #Update weekly summary tab when timeslider input changes
    #7 days
    output$weekly_summary_1_dt <- renderTable({
      get_weekly_summary_table(TRUE, selected_date)
    }, align = 'r', striped = TRUE, width = '100%')
    # clean_week_old_date_text
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
    # clean_two_weeks_old_date_text
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
    
    school_closures_merged <- rbind(school_closures_sept_dec_21_21, school_closures_jan_may_22_22)
    cases_pst <- merge(cases_pst, school_closures_merged, by.x = "school", by.y = "School Name", all.x = TRUE)
    # print(cases_pst)
    
    leafletProxy('basemap_leaflet') %>%
      clearMarkers()
    
    ### Circle Creation -------
    leafletProxy(mapId = 'basemap_leaflet', session = session) %>%
      addCircleMarkers(
        data = cases_pst,
        lng = cases_pst$longitude,
        lat = cases_pst$latitude,
        radius = cases_pst$total_confirmed_cases * 2,
        weight = 1,
        color = '#d62728',
        fillOpacity = 0.3,
        label = sprintf(
          '<div style = "background-color: white; color:black;"><strong>%s</strong><br/>City: %s<br/>Level: %s<br/>Board: %s<br/>Language: %s<br/>Enrolment: %s<br/>Low-income households: %s%%<br/>First language not English: %s%%<br/>Immigrant from non-English country: %s%%<br/>First language not French: %s%%<br/>Immigrant from non-French country: %s%%<br/>Students receiving Special Education Services: %s%%<br/>Confirmed cases (cumulative): %s<br/>Confirmed cases staff (cumulative): %s<br/>Confirmed cases student (cumulative): %s<br/>Confirmed cases unidentified (cumulative): %s<br/></div>',
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
          cases_pst$confirmed_unidentified_cases
        ) %>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list(
            'font-weight' = 'normal',
            padding = '3px 8px',
            color = '#d62728'
          ),
          textsize = '15px',
          direction = 'auto'
        )
      )
  })
  
  # Plots ----
  ## Cumulative ----
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
  
  ## Which Week View ----
  observeEvent(input$weeklyRadio,{
    ### 7-Day View ----
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
      ### 14-day view ----
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
  
  
  # SECTION: 2020-2021 MAP ----
  
  ## 2020-2021 View Options -------
  #Is the viewing options menu open?
  viewOptionsOpen20_21 <- FALSE
  #Are we currently viewing schools with cases? (If timeslider is closed)
  schoolsWithCases20_21 <- FALSE
  #Are we currently viewing schools without cases? (If timeslider is closed)
  schoolsWithoutCases20_21 <- FALSE
  #Is the timeslider currently open for this tab
  vTimeSlider20_21 <- TRUE
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
      # viewOptionsOpen <<-  FALSE #flip when button is pressed
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
  Merged_School_Data_20_21 <- merge(cases_per_school_20_21, school_closures_sept_april_20_21, by.x = "school_name", by.y = "School Name", all=TRUE)
  
  # 2020-2021 Activity Observer ----
  ## Schools Without Cases ----
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
  
  
  # 
  # Observes the button for getting the viewing options menu (2020-2021)
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
  
  # Update Map Markers for the 2021-2020 map
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
          color = '#808080',
          fillOpacity = 1)
      leafletProxy('map_leaflet20_21') %>%
        addCircleMarkers(
          data = cases_per_school_20_21, 
          lng = ~lon, 
          lat = ~lat, 
          radius = ~(cases_per_school_20_21$cases_per_school) * 2,
          weight = 1, 
          color = '#808080',
          fillOpacity = 0.3, 
          label = sprintf('<div style = "background-color: gray; color:black;"><strong>%s</strong><br/>School Name: %s<br/>Date of Closure: %s<br/>Date of Reopening: %s<br/>Reason for Close: %s<br/>Board Number: %s<br/>Board_Name: %s%%<br/></div>', 
                          COVID_School_Closures_V2$School_Name, 
                          COVID_School_Closures_V2$Date_of_Closure, 
                          COVID_School_Closures_V2$Date_of_Reopening,
                          COVID_School_Closures_V2$Reason_for_Closure,
                          COVID_School_Closures_V2$Board_Number,
                          COVID_School_Closures_V2$Board_Name)%>% 
            lapply(htmltools::HTML),
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
  
  
  ## Activity Monitor - 2020-2021 ------
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
        # cumulative_case_count_text_20_21
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
  
  #Observes activity (movement) on the timeslider and adjusts data being viewed accordingly (2021-2020)
  observeEvent(input$obs20_21,{
    geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
                             str_trim(covid19_schools_active_with_demographics_20_21$school.name), 
                             covid19_schools_active_with_demographics_20_21$municipality)
    
    selected_date <- input$obs20_21
    # cumulative_case_count_text_20_21
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
  
  # Panel: TIME SLIDER 2020-2021 ---------
  output$map_leaflet20_21 <- renderLeaflet({
    withProgress(max = 6, 
                 value = 0, 
                 message = 'please wait...', 
                 expr = {
                   incProgress(1, 'loading shapes')
                   # regenerate the 20_21 map
                   # https://geohub.lio.gov.on.ca/datasets/province/data
                   ontario <- st_read(file.path('data/shapefiles', layer = 'PROVINCE.shp')) %>%
                     st_transform(crs = 4326)
                   
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
                   # ensure numeric lat long
                   cases_per_school_20_21 <- cases_per_school_20_21 %>%
                     dplyr::mutate(lon = as.numeric(lon), lat = as.numeric(lat))
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
