infections_2021_2022_panel <- tabPanel(
  'Map - 2021-22',
  div(
    class = 'outer',
    
    # tag: stylesheet
    tags$head(includeCSS('styles.css')),
    
    # leaflet: basemap
    leafletOutput('basemap_leaflet', width = '100%', height = '100%'),
    
    # panel: button: viewOptions
    absolutePanel(
      id = 'viewOptions',
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
    
    # panel: controls
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
          h3(textOutput('cumulative_case_count_text'), align = 'right'),
          
          # clean_date_reactive_text
          h6(
            div('Data reported on'),
            textOutput('clean_date_reactive_text'),
            align = 'right'
          ),
          
          # daily_summary_1_dt
          div(tableOutput('daily_summary_1_dt'), style = 'font-size: small; width: 100%'),
          
          h6('Drag this box to move it', align = 'right')
        ),
        tabPanel(
          id = "weekly",
          
          h2('Weekly Summary', align = 'right', style =
               "font-size:150%;"),
          
          # weeklyRadio
          div(
            radioButtons(
              inputId = "weeklyRadio",
              label = strong("Select a timeframe:"),
              choices = list("7-day view", "14-day view"),
              inline = TRUE
            ),
            align = "right"
          ),
          
          #whichWeekView
          uiOutput("whichWeekView")
          
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
  uiOutput('timesliderViewer')
  
)