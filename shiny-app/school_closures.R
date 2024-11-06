school_closures_panel <- tabPanel(
  'School Closures',
  div(
    class = 'outer',
    
    tags$head(includeCSS('styles.css')),
    
    leafletOutput(
      'basemap_leaflet_closures',
      width = '100%',
      height = '100%'
    ),
    
    absolutePanel(
      id = 'viewOptions_closures',
      class = 'panel panel-default',
      top = "0%",
      right = "0%",
      width = 'auto',
      #fixed = TRUE,
      draggable = FALSE,
      height = 'auto',
      actionButton("getOptions_closures", "Viewing Options", icon("cog"))
    ),
    
    uiOutput('mapperViewOptions_closures'),
    
    absolutePanel(
      id = 'controls',
      class = 'panel panel-default',
      top = "5%",
      left = 55,
      width = 500,
      fixed = TRUE,
      draggable = TRUE,
      height = 'auto',
      
      tags$style(HTML(
        ".tabbable > .nav > li[class=active] > a {color:#e95420;}"
      )),
      tags$style(HTML(".tabbable > .nav > li > a {color:#777777;}")),
      
      tabsetPanel(
        tabPanel(
          id = "daily",
          
          h2('Daily Summary', align = 'right', style =
               "font-size:150%;"),
          
          # cumulative_case_count_text
          h3(textOutput('cumulative_case_count_text_closures'), align = 'right'),
          
          # clean_date_reactive_text
          h6(
            div('Data reported on'),
            textOutput('clean_date_reactive_text_closures'),
            align = 'right'
          ),
          
          # daily_summary_1_dt
          div(tableOutput('daily_summary_1_dt_closures'), style = 'font-size: small; width: 100%'),
          
          h6('Drag this box to move it', align = 'right')
        ),
        tabPanel(
          id = "weekly",
          
          h2('Weekly Summary', align = 'right', style =
               "font-size:150%;"),
          
          # weeklyRadio
          div(
            radioButtons(
              inputId = "weeklyRadio_closures",
              label = strong("Select a timeframe:"),
              choices = list("7-day view", "14-day view"),
              inline = TRUE
            ),
            align = "right"
          ),
          
          #whichWeekView
          uiOutput("whichWeekView_closures")
          
        )
      )
      
    ),
    
    
  ),
  
  tags$style(
    HTML(
      ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: teal}"
    )
  ),
  ### TIMESLIDER -------------------------------------------
  uiOutput('timesliderViewer_closures')
)