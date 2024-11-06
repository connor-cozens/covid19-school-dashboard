about_panel <- tabPanel(
  'About This Site',
  absolutePanel(
    id = 'contents',
    class = 'panel panel-default',
    top = '20%',
    left = '75%',
    width = 200,
    fixed = TRUE,
    draggable = TRUE,
    height = 'auto',
    tags$style(
      "#contents {background-color: #eeeeee;padding: 5px 5px 15px 5px;border: 1px solid black;}"
    ),
    
    div(
      align = "center",
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
  ### Overview ---------------------------------------------
  div(
    h3(id = "Top of Page", 'COVID-19 SCHOOL DASHBOARD KEY AIMS & INFORMATION'),
    p(
      a(
        href = 'http://covid19schooldashboard.com',
        'covid19schooldashboard.com',
        target = '_blank'
      ),
      ' reports and maps confirmed school-related cases of COVID-19 in publicly funded elementary and secondary schools in Ontario, Canada, and connects this to data on school social background characteristics (school-level demographic data). The site covers the period September 2020 to June 2021 and September 2021 to December 2021, the last date for which school infection data for Ontario are publicly available.'
    ),
    p(
      'The main aim of this site is to provide real-time data visualization of affected schools for broad dissemination. This will help to increase transparency and understanding of the education scenario as it evolves. It will help school communities (e.g., parents, students, teachers and staff, leaders and administrators), community members and neighbours, education and health professionals, officials, researchers, media, and the general public.'
    ),
    p('The site is best viewed on a desktop or tablet.'),
    br(),
    h4('Why is this important?'),
    p(
      'The effects of COVID-19 are more severe on high-risk communities, populations, and schools. There are strong equity concerns. Visualizing COVID-19 case data with data on school social background characteristics will give us a better understanding of the composition of affected schools.'
    ),
    p(
      'In short, we will get closer to understanding the human dimension of COVID-19 on school populations.'
    ),
    br(),
    h4('Update frequency'),
    p(
      'This site is no longer automatically updated as the ',
      a(
        href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools',
        'Ministry of Education ceased reporting data on school infections as of January 2022.',
        target = '_blank'
      ),
      ' From September 2020 to December 2021, this site was automatically updated every weekday (excluding public holidays) following the release of school-related COVID-19 case data by the Ontario Ministry of Education. This site also used the latest publicly available data on school information and student demographics released by the Ontario Ministry of Education for school background characteristics.'
    ),
    br(),
    h4('Archive of the COVID-19 School Dashboard'),
    p(
      'The archival material, code for the site can be found ',
      a(href = 'https://www.google.com/url?q=https://doi.org/10.5683/SP3/Z9SNP0&sa=D&source=docs&ust=1665177381441979&usg=AOvVaw1BTM0Gh6Z4wNMXF5sPbP3y', 'here', target = '_blank'),
      ', and the integrated dataset can be found ',
      a(href = 'https://www.google.com/url?q=https://doi.org/10.5683/SP3/D0QXGQ&sa=D&source=docs&ust=1665177381442561&usg=AOvVaw0P4TWDPfxDqe56k1Dzj9vT', 'here.', target = '_blank')
    ),
    p(
      'See also "Data Sources and Source Code" tab for more information on data sources used.'
    ),
    br(),
    p(tags$b('2021-22 School Year')),
    p(
      'Cumulative totals represent all total cases reported as of 23 December 2021. This report provides a summary of COVID-19 activity in Ontario schools. Cumulative totals represent all total cases reported to the Ministry of Education as of 23 December 2021, including resolved cases. The data were available for public access and download as on 14 September 2021. The first reported date of school-level cases was 26 August 2021. The last reported date of school-level cases was 23 December 2021. No public data were made available by the Ministry as of January 2022.'
    ),
    p(tags$b('2020-21 School Year')),
    p(
      'Cumulative totals represent all total cases reported to the Ministry of Education as of 5 September 2020, including resolved cases. The first school-related cases appeared in the dataset on 10 September 2020.'
    ),
    p(
      'The date shown in the Daily/Weekly Summary pane on the Mapper tab reports the last day on which official data were released by the Ontario Ministry of Education. This was 27 April 2021 for the 2020-21 school year.'
    ),
    br(),
    h4('Caveats'),
    p(
      'The main aim of the COVID-19 School Dashboard is to show which schools are affected by confirmed cases as reported in the official data, visually plot where the schools are, and show relevant school background characteristics of affected schools. This site should not be used to draw inferences on the broader COVID-19 situation in Ontario, or on case numbers generally. A number of complementary metrics are useful in that regard.'
    ),
    p(
      'The numbers of cases are extracted from official data sources. A number of contextual factors will affect data changes. The following is an informational list of potential relevant factors. It is not exhaustive. For example, changing testing scenarios can mean that as the frequency of testing increases or decreases, threshold of symptoms is widened or restricted, and backlog of results clears or increases, the number of new cases may show spikes or dips. As the situation evolves, vaccination rates and mass and partial school closures and reopening, amongst other factors, affect changes in data. '
    ),
    p(
      'There are known lags in data reported in the Ministry of Education dataset, which may result in real-time discrepancies.'
    ),
    p(
      'There may be some discrepancies in school demographic data if they are in the official dataset.'
    ),
    hr()
  ),
  ### Policy -----------------------------------------------
  div(
    h3(id = "Policy Context", 'POLICY CONTEXT'),
    p(
      'Pandemic-related school closures in Ontario affected over 2 million elementary and secondary school students. The situation for students and schools evolved rapidly.'
    ),
    p(
      'The following provides a brief policy context of provincial policy responses on school closures and reopening. It does not outline decisions of individual school boards or regional public health units (PHUs), unless they were named in provincial announcements.'
    ),
    p(
      'The figure below shows the main Ontario-level school closure and re-opening periods from September 2020 – April 2021. Schools did not reopen for face-to-face instruction for the remainder of the school year (30 June 2021). There were exceptions for schools and programs serving children with special needs.'
    ),
    #renderPlot(create_timeline),
    #img(src='timeline_plot.jpg', width='80%', height='80%'),
    img(
      src = 'timeline.png',
      width = '80%',
      height = '80%'
    ),
    tags$style("#subnote"),
    p(
      id = "subnote",
      tags$b(
        'Figure 1 Ontario-level school closures and reopening policy tracing (March 2020 – April 2021)'
      )
    ),
    p(
      id = "subnote",
      'Cite as: Srivastava, P., Taylor, P.J. (2021). COVID-19 school dashboard (1.1 May 2021). [Web application]. http://covid19schooldashboard.com/'
    ),
    p(
      id = "subnote",
      'Notes. *: School closures: defined here as the suspension of in-school, face-to-face instruction. Public schools only. Special provisions for face-to-face instruction were made for special education needs services. These are not presented here. Only provincial-level decisions are presented. Individual school boards and PHUs may have additionally instituted localised school closures. These are not presented here.'
    ),
    p(
      id = "subnote",
      tags$sup("1"),
      ': All schools in PHUs of Algoma, North Bay Parry Sound, Northwestern, Porcupine, Sudbury, Thunder Bay, Timiskaming. ',
      tags$sup("2"),
      ': All schools in PHUs of Grey Bruce, Haliburton, Kawartha, Pine Ridge, Hastings and Prince Edward Counties, Kingston, Frontenac and Lennox & Addington, Leeds, Grenville and Lanark, Peterborough, Renfrew County. ',
      tags$sup("3"),
      ': All schools in PHUs of Eastern Ontario, Middlesex-London, Ottawa, Southwestern. ',
      tags$sup("4"),
      ': All schools in PHUs of: Brant County, Chatham-Kent, Durham, Haldimand-Norfolk, Halton, Hamilton, Huron Perth, Lambton, Niagara, Simcoe-Muskoka, Waterloo, Wellington-Dufferin-Guelph, Windsor-Essex. ',
      tags$sup("5"),
      ': All schools in PHUs of: Peel, Toronto, York.'
    ),
    p(
      id = "subnote",
      'Source. Data extracted from official provincial government announcements  and verified on ICES COVID-19 Dashboard.'
    ),
    p(
      id = "subnote",
      tags$br(),
      tags$sup("i"),
      'https://news.ontario.ca/en/release/59790/ontario-announces-provincewide-shutdown-to-stop-spread-of-covid-19-and-save-lives'
    ),
    p(
      id = "subnote",
      'https://news.ontario.ca/en/statement/60033/over-100000-ontario-students-return-to-class-beginning-next-week'
    ),
    p(
      id = "subnote",
      'https://news.ontario.ca/en/statement/60154/280000-more-ontario-students-to-return-to-class'
    ),
    p(
      id = "subnote",
      'https://news.ontario.ca/en/release/60228/enhanced-safety-measures-in-place-as-in-person-learning-resumes-across-ontario'
    ),
    p(
      id = "subnote",
      'https://news.ontario.ca/en/release/60228/enhanced-safety-measures-in-place-as-in-person-learning-resumes-across-ontario'
    ),
    p(
      id = "subnote",
      'https://news.ontario.ca/en/release/61106/ontario-moves-schools-to-remote-learning-following-spring-break'
    ),
    br()
  ),
  ### News by Year -----------------------------------------
  div(
    h4(id = "2021-2022", '2021-22 School Year'),
    p(
      'Schools operating on a modified/balanced calendar opened as early as 4 August 2021. The majority of schools opened according to regular board-level conventions from 7 to 10 September 2021. All schools should have been opened as on 13 September 2021 for the regular school year.. On 3 January 2022, it was announced schools would not reopen for in-person instruction until 17 January 2022. This was the only systems-wide closure in the 2021-22 school year.'
    ),
    br(),
    h4(id = "2020-2021", '2020-21 School Year'),
    p(
      'Phased reopening of publicly funded schools in Ontario began on 8 September 2020 and continued until 21 September 2020, by which time all schools should have opened. This followed a period of province-wide and localised school closures. Schools operated virtually as of 19 April 2021 for the remainder of the school year (end June 2021), with special in-person provisions for special education needs services.'
    ),
    p(
      'At the provincial level, the winter break commenced as on 21 December 2020 for an initial planned return to in-person instruction on 4 January 2021 for elementary and secondary schools.'
    ),
    p(
      'On 21 December 2020, the provincial government announced a ',
      a(href = 'https://news.ontario.ca/en/release/59790/ontario-announces-provincewide-shutdown-to-stop-spread-of-covid-19-and-save-lives', target = '_blank', 'province-wide shutdown'),
      '. Virtual learning was announced for all schools for the period 4-8 January 2021.'
    ),
    p(
      'Return to in-person instruction was planned for elementary and secondary schools on 11 January 2021 in the following public health units: Algoma, North Bay Parry Sound, Northwestern, Porcupine, Sudbury and District, Thunder Bay, Timiskaming.'
    ),
    p(
      'For the rest of the province, return to in-person instruction was planned for 11 January for elementary schools and 25 January 2021 for secondary schools.'
    ),
    p(
      'On 7 January 2020, the provincial government ',
      a(
        href = 'https://news.ontario.ca/en/release/59890/ontario-extends-teacher-led-online-learning-until-january-25-to-keep-students-and-staff-safe-in-sout',
        target = '_blank',
        'extended the province-wide shutdown'
      ),
      '. This extended virtual instruction for all elementary schools in Ontario until 25 January 2021. It  extended the shutdown in Northern Ontario, aligning with the shutdown in Southern Ontario.'
    ),
    p(
      'On 12 January 2021, the provincial government issued a ',
      a('second state of emergency', href = 'https://news.ontario.ca/en/release/59922/ontario-declares-second-provincial-emergency-to-address-covid-19-crisis-and-save-lives', target = '_blank'),
      '. This included a further extension for virtual learning for all elementary and secondary schools in four regions: Hamilton, Peel, Toronto, Windsor-Essex, and York, until 10 February 2021.'
    ),
    p(
      'On 20 January 2021, the Government announced that ',
      a(
        href = 'https://news.ontario.ca/en/statement/60033/over-100000-ontario-students-return-to-class-beginning-next-week',
        target = '_blank',
        'elementary and secondary schools in the following PHUs would be permitted to resume in-person learning on 25 January, 2021'
      ),
      ': Grey Bruce Health Unit; Haliburton, Kawartha, Pine Ridge District Health Unit; Hastings and Prince Edward Counties Health Unit; Kingston, Frontenac and Lennox & Addington Health Unit;  Leeds, Grenville and Lanark District Health Unit; Peterborough Public Health; Renfrew County and District Health Unit.'
    ),
    p(
      'All schools in the following 7 schools boards could resume in-person learning on January 25: Limestone District School Board; Renfrew County District School Board; Hastings and Prince Edward District School Board; Bruce-Grey Catholic District School Board; Renfrew County Catholic District School Board; Algonquin and Lakeshore Catholic District School Board; and Bluewater District School Board.'
    ),
    p(
      'Some additional school boards (9) that span multiple PHUs could have some schools that resume in-person: Kawartha Pine Ridge District School Board; Trillium Lakelands District School Board; Upper Canada District School Board; Catholic District School Board of Eastern Ontario; Peterborough Victoria Northumberland and Clarington Catholic DSB; Conseil des écoles publiques de l`Est de l`Ontario; Conseil scolaire catholique Providence; Conseil scolaire catholique Mon Avenir; Conseil scolaire de district catholique du Centre-Est de l\'Ontario.'
    ),
    p(
      'Northern PHUs that were permitted to return to in-person learning on 11 January 2021 would continue in-person learning unless otherwise directed by local PHUs.'
    ),
    p(
      'On 28 January 2021 it was announced that ',
      a(
        href = 'https://news.ontario.ca/en/statement/60154/280000-more-ontario-students-to-return-to-class',
        target = '_blank',
        'elementary and secondary schools in the following 4 additional PHUs can return to in-person learning on 1 February 2021'
      ),
      ': Eastern Ontario Health Unit; Middlesex-London Health Unit; Southwestern Public Health; Ottawa Public Health. '
    ),
    p(
      'All schools in the following 9 schools boards could resume in-person learning: Catholic District School Board of Eastern Ontario; Conseil des écoles publiques de l\'Est de l\'Ontario; Conseil scolaire de district catholique de l\'Est ontarien; Conseil scolaire de district catholique du Centre-Est de l\'Ontario; London District Catholic School Board; Ottawa Catholic District School Board; Ottawa-Carleton District School Board; Thames Valley District School Board; Upper Canada District School Board'
    ),
    p(
      'Schools in 2 additional school boards that span multiple PHUs could have schools in the appropriate PHU resume in-person learning: Conseil scolaire catholique Providence; Conseil scolaire Viamonde.'
    ),
    p(
      'On 3 February 2021, it was announced ',
      a(
        href = 'https://news.ontario.ca/en/release/60228/enhanced-safety-measures-in-place-as-in-person-learning-resumes-across-ontario',
        target = '_blank',
        'all elementary and secondary schools in the following 13 PHUs could return to in-person learning on 8 February 2021'
      ),
      ': Brant County Health Unit; Chatham-Kent Public Health; Durham Region Health Department; Haldimand-Norfolk Health Unit; Halton Region Public Health; City of Hamilton Public Health Services; Huron Perth Public Health; Lambton Public Health; Niagara Region Public Health; Simcoe-Muskoka District Health Unit; Region of Waterloo Public Health and Emergency Services; Wellington-Dufferin-Guelph Public Health; Windsor-Essex County Health Unit.'
    ),
    p(
      'And, all elementary and secondary schools in the following 3 PHUs to in-person learning on 16 February 2021: Peel Public Health, Toronto Public Health, and York Region Public Health.'
    ),
    p(
      'On 12 April 2021, it was announced that ',
      a(
        href = 'https://news.ontario.ca/en/release/61106/ontario-moves-schools-to-remote-learning-following-spring-break',
        target = '_blank',
        'all schools would revert to virtual schooling for an indeterminate period as of 19 April 2021'
      ),
      'following the re-scheduled spring break of 12-16 April 2021 during which time schools were closed. Schools did not reopen for face-to-face instruction for the remainder of the school year (i.e., until 30 June 2021).'
    ),
    br(),
    h4(id = "2019-2020", '2019-20 School Year'),
    p(
      'The first school closure announcement in Ontario was issued on 12 March 2020 for an initial period from 14 March to 4 April 2020. This compelled all publicly funded elementary and secondary schools to close. Public school closures were extended another three times – first until 4 May, then 31 May, and finally until the end of June 2020.'
    ),
    hr()
  ),
  ### Site Navigation --------------------------------------
  div(
    h3(id = "Site Navigation", 'HOW TO NAVIGATE THE SITE'),
    h4('Map 2021-22 - Affected Ontario Schools Tab'),
    p(
      'Shows daily updates to cumulative school-related cases. All schools are plotted by geocode on the map. The default view shows all affected schools. Hovering on a school bubble shows school-specific data on case numbers and breakdown per school, administrative school-level data on school characteristics, and demographic data of the affected school population. The default view shows the final cumulative cases in all schools.'
    ),
    h4('Map 2020-21 - Affected Ontario Schools Tab'),
    p(
      'Shows the final cumulative school-related cases as last reported on the update of 27 April 2021. The first school-related cases appeared in the dataset on 10 September 2020. All schools are plotted by geocode on the map. The default view shows the final cumulative cases in all affected schools.'
    ),
    br(),
    h5(tags$b('Bubbles')),
    p(
      'The size of the bubbles indicates the magnitude of cumulative cases (student, staff, unidentified) at specific schools relative to others. ',
      tags$b(
        'The bigger the bubble, the more cumulative cases at that school – that is, the more it has been affected relative to other schools.'
      )
    ),
    p(
      tags$b(
        'Hovering on a bubble reveals school-specific COVID-19 case data and school social background information. '
      ),
      'Currently, the bubbles show: '
    ),
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
      tags$li(
        'proportion of students who are immigrants from a non-English country;'
      ),
      tags$li(
        'proportion of students who are immigrants from a non-French country;'
      ),
      tags$li(
        'proportion of students who are receiving special education services (for 2021-2022 school year only);'
      ),
      tags$li(
        'parents have no university education (for 2020-21 school year only)'
      ),
    ),
    br(),
    h5(tags$b('Viewing Options')),
    p(
      'Click on \'View Options\' to customize which schools you see (with cases, without cases, or both). Affected schools are visualized by red bubbles. Schools without cases are visualized in blue. ‘View timeslider, case over time’ shows the evolution of cumulative cases in schools over time.'
    ),
    br(),
    h5(tags$b('Timeslider')),
    p(
      'The timeslider date at the bottom of the screen will appear by clicking on ‘View timeslider, case over time’. Drag the circle across the select dates to see cumulative cases. Data in the quick view summary pane and visual data on the map change accordingly.'
    ),
    br(),
    h5(('Quick view summary pane')),
    tags$ul(
      tags$li(
        em('Daily Summary:'),
        ' Summarizes cumulative school-related cases, new total school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario). It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.'
      ),
      tags$li(
        em('Weekly Summary:'),
        'Summarizes cumulative school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario) for 7- or 14-day period from last known data reporting date in the Ministry of Education dataset.'
      )
    ),
    br(),
    h4('Overview & Search Tab'),
    h5(('Cumulative Case Chart')),
    p(
      'Shows the total number of cumulative school-related cases in Ontario, and disaggregated to show cumulative school-related student cases, cumulative school-related staff cases, and unidentified cases. "Unidentified cases" is used by the Ministry of Education to refer to the following: "In some instances, the type of case has not been identified as either student/child or staff/provider/partner due to privacy considerations. These "individuals" only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases will be tracked as "individuals" but not included in the "student/child" or "staff/provider" columns.'
    ),
    br(),
    h5(('Tools for added functionality')),
    p(
      'Hover over the legend to access tools for added functionality: download graph as .PNG image file, zoom, pan, box select, lasso select, zoom in, zoom out, autoscale, reseat axes, toggle spike lines, show closest data on hover, compare data on hover.'
    ),
    p(
      '"Compare data on hover" is especially useful to see and compare the number of cases on all lines in the graph on a specific date.'
    ),
    br(),
    h5(('Daily Summary')),
    p(
      'Summarizes cumulative school-related cases, new total school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario). It also shows the count and change (+/-) from the most current date with data to the date immediately preceding. No changes will be seen on or between weekend dates (i.e., on Saturday and Sunday and between Friday and Saturday; Saturday and Sunday) or public holidays since data are only released by the Ministry on weekdays.'
    ),
    br(),
    h5(('Weekly Summary')),
    p(
      'Summarizes cumulative school-related cases, current schools with cases (and as % of schools in Ontario), and current schools closed (and as % of schools in Ontario) for 7- or 14-day period from last known data reporting date in the Ministry of Education dataset.'
    ),
    br(),
    h5(('Search Function and Table')),
    p(
      'Use this to search schools, boards, municipalities for data on confirmed cases of COVID-19.'
    ),
    br(),
    h4('Data Tables & Data Dictionary Tab'),
    h5(('Summary of cases in schools')),
    p(
      'Presents raw data of cases in schools. Data table can be manipulated in ascending or descending order by variable of interest. Table can be downloaded as a .CSV file for independent analysis.'
    ),
    p(
      'Variables included: collected date; reported date; current schools with cases; current schools closed; current total number of schools; new (total school-related cases; student; staff; unidentified); recent (total school-related cases; student; staff; unidentified); past (total school-related cases; student; staff; unidentified); cumulative (total school-related cases; student; staff; unidentified).'
    ),
    p(
      'For 2020-21: Recent and past case data available as from 1 October 2020. See ',
      a(
        href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools/resource/7fbdbb48-d074-45d9-93cb-f7de58950418',
        target = '_blank',
        'Summary of cases in schools'
      )
    ),
    br(),
    h5('Schools with active cases and school demographic data'),
    p(
      'Presents raw data of cases in schools combined with demographic data. Data table can be manipulated in ascending or descending order by variable of interest. Table can be downloaded as a .CSV file for independent analysis.'
    ),
    p(
      'Use the search function to see if a specific school, board, or municipality has been affected.'
    ),
    br(),
    h5('Data Dictionary'),
    p(
      'Lists definitions of terms and variables as defined in the dataset and on COVID-19 cases in schools and child care centres Ontario Ministry of Education website. '
    ),
    br(),
    h5('Data Sources and Source Code Tab'),
    p(
      'Lists all publicly available data sources used to generate the COVID-19 School Dashboard.'
    ),
    br(),
    h5(('Source code')),
    p(
      'Source code for this site can be found on GitHub ',
      a(href = 'https://github.com/connor-cozens/covid19-school-dashboard', 'here', target = "_blank"),
      ' or on the archive ',
      a(href = 'https://doi.org/10.5683/SP3/Z9SNP0', 'here', target = "_blank")
    ),
    br(),
    h5('Research and Media Tab'),
    p(
      'This tab lists research-related applications and media coverage of the COVID-19 School Dashboard.'
    ),
    br(),
    hr()
  ),
  ### Authorship -------------------------------------------
  div(
    h3(id = "Authorship", 'AUTHORSHIP, ATTRIBUTIONS, CITATION'),
    h4('Cite the COVID-19 School Dashboard as:'),
    p(
      'Srivastava, P., Marshall, J., Cozens, C., & Taylor, P.J. (2022). ',
      tags$em('COVID-19 school dashboard (2.0 March 2022). '),
      '[Web application]. ',
      a(href = 'http://covid19schooldashboard.com/', 'http://covid19schooldashboard.com/')
    ),
    br(),
    p(
      a(href = 'https://www.edu.uwo.ca/faculty-profiles/prachi-srivastava.html', target = '_blank', 'Dr. Prachi Srivastava'),
      ', Associate Professor, Faculty of Education, University of Western Ontario, Canada.'
    ),
    p(
      a(href = 'mailto:prachi.srivastava@uwo.ca', 'prachi.srivastava@uwo.ca')
    ),
    p(
      a(href = 'https://twitter.com/PrachiSrivas', target = '_blank', '@PrachiSrivas')
    ),
    p(
      a(
        href = 'https://orcid.org/0000-0003-4865-8963',
        target = '_blank',
        'ORCID iD: 0000-0003-4865-8963'
      )
    ),
    br(),
    p('Development: '),
    p('Technical lead development and design: Peter J. Taylor'),
    p('Further development:'),
    p(
      'Justin Marshall',
      a(href = 'mailto:powtatow@gmail.com', 'powtatow@gmail.com')
    ),
    p(
      'Connor Cozens',
      a(href = 'mailto:cozcon@gmail.com', 'cozcon@gmail.com')
    ),
    br(),
    h5('Preliminary site structure based on:'),
    p(
      'Parker, E., & Leclerc, Q. (2020). ',
      tags$em('COVID-19 tracker. '),
      '[Web application]. ',
      a(
        href = 'https://vac-lshtm.shinyapps.io/ncov_tracker/',
        target = '_blank',
        'https://vac-lshtm.shinyapps.io/ncov_tracker/'
      )
    ),
    br(),
    br(),
    a(
      href = 'https://www.edu.uwo.ca',
      target = '_blank',
      tags$img(
        src = 'uwo_logo.png',
        height = '58',
        width = '243'
      )
    ),
    br(),
    br()
  )
)