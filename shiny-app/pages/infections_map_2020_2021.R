infections_2020_2021_panel <- tabPanel(
  'Map - 2020-21',
  div(
    class = 'outer',
    
    # tag: stylesheet
    tags$head(includeCSS('styles.css')),
    
    # leaflet: map20_21
    leafletOutput('map_leaflet20_21', width = '100%', height = '100%'),
    
    # panel: button: viewOptions20_21
    absolutePanel(
      id = 'viewOptions20_21',
      class = 'panel panel-default',
      top = "0%",
      right = "0%",
      width = 'auto',
      draggable = FALSE,
      height = 'auto',
      actionButton("getOptions20_21", "Viewing Options", icon("cog"))
    ),
    
    uiOutput('mapperViewOptions20_21'),
    
    # panel: controls
    absolutePanel(
      id = 'controls',
      class = 'panel panel-default',
      top = "5%",
      left = 55,
      width = 300,
      fixed = TRUE,
      draggable = TRUE,
      height = 'auto',
      
      tags$style(HTML(
        ".tabbable > .nav > li[class=active] > a {color:#e95420;}"
      )),
      tags$style(HTML(".tabbable > .nav > li > a {color:#777777;}")),
      h2('Year Summary', align = 'center', style =
           "font-size:200%;"),
      
      # cumulative_case_count_text_20_21
      h3(textOutput('cumulative_case_count_text_20_21'), align = 'right'),
      
      # clean_date_reactive_text
      h6(
        div('Data last reported on'),
        textOutput('clean_date_reactive_text_20_21'),
        align = 'right'
      ),
      
      h6('Drag this box to move it', align = 'right')
    )
  ),
  tags$style(
    HTML(
      ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: teal}"
    )
  ),
  ### TIMESLIDER -------------------------------------------
  uiOutput('timesliderViewer20_21')
)