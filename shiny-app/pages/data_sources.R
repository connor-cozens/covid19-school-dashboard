data_sources_panel <- tabPanel(
  'Data Sources & Code',
  h3('Data Sources 2021-22'),
  tags$ul(
    tags$li(
      a(href = 'https://data.ontario.ca/dataset?keywords_en=COVID-19', 'All COVID-19 datasets', target = '_blank')
    ),
    tags$li(
      a(
        href = 'https://www.ontario.ca/page/covid-19-cases-schools-and-child-care-centres',
        'COVID-19 cases in schools and child care centres',
        target = '_blank'
      )
    ),
    tags$li(
      a(
        href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools',
        'Schools COVID-19 data overview',
        target = '_blank'
      )
    ),
    tags$li(
      a(
        href = 'https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/new_sif_data_table_2018_2019prelim_en_august.xlsx',
        ' School information and student demographics dataset (.xlsx)',
        target = '_blank'
      )
    ),
    tags$li(
      a(
        href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics',
        ' School information and student demographics overview',
        target = '_blank'
      )
    ),
    tags$li(
      a(
        href = 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv',
        'Schools with active COVID-19 cases dataset (.csv)',
        target = '_blank'
      )
    ),
    tags$li(
      a(
        href = 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/7fbdbb48-d074-45d9-93cb-f7de58950418/download/schoolcovidsummary.csv',
        'Summary of cases in schools dataset (.csv)',
        target = '_blank'
      )
    )
  ),
  h3('Data 2020-21'),
  downloadButton(
    'download_csv_button_3_20_21',
    'schoolcovidsummary20_21.csv'
  ),
  h3('Source Code'),
  p(
    'Source code for this site can be found ',
    a(href = 'https://github.com/connor-cozens/covid19-school-dashboard', 'here', target = '_blank')
  ),
  h3('Archive'),
  p(
    'The archival material, code for the site can be found ',
    a(href = 'https://doi.org/10.5683/SP3/Z9SNP0', 'here', target = '_blank'),
    'and the integrated dataset can be found ',
    a(href = 'https://doi.org/10.5683/SP3/D0QXGQ', 'here.', target = '_blank')
  )
)
