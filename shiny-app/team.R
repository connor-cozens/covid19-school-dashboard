team_panel <- tabPanel(
  'Our Team',
  tags$div(),
  h3('THE TEAM AND CONTACT'),
  hr(),
  h4('Dr. Prachi Srivastava'),
  p(
    em('Principal Investigator and Project Lead, Western University')
  ),
  p(
    'Dr. Prachi Srivastava is a tenured Associate Professor, Western University, specialising in education and global development. She is also Member, World Bank Expert Advisory Council on Citizen Engagement, and Senior Research Fellow, NORRAG. Previously, she served with the United Nations Mission in Kosovo and the International Rescue Committee. She holds a doctorate from the University of Oxford.'
  ),
  p(
    'For inquiries, contact Dr. Srivastava at: ',
    a(href = 'mailto:prachi.srivastava@uwo.ca', 'prachi.srivastava@uwo.ca')
  ),
  p(
    a(
      href = 'https://twitter.com/PrachiSrivas',
      target = "_blank",
      tags$img(
        src = 'twitter_logo.png',
        height = '24',
        width = '28'
      )
    ),
    ' - ',
    a(
      href = 'https://www.linkedin.com/in/prachi-srivastava-9ab6122/',
      target = "_blank",
      tags$img(
        src = 'linkedin_logo.png',
        height = '24',
        width = '24'
      )
    ),
    ' - ',
    a(
      href = 'https://www.edu.uwo.ca/faculty-profiles/prachi-srivastava.html',
      target = "_blank",
      tags$img(
        src = 'uwo_logo.png',
        height = '24',
        width = '120'
      )
    )
  ),
  hr(),
  splitLayout(
    verticalLayout(
      h4('Justin Marshall'),
      p(em('Developer')),
      p(
        'Justin graduated from Western University with an Honors BSc in Computer Science and a Minor in Software Engineering. He is currently open to full-time roles and opportunities. In addition to data visualization, Justin is interested in fields including game development, and is currently working on creating his own games and building a portfolio.'
      )
    ),
    verticalLayout(
      h4('Connor Cozens'),
      p(em('Developer')),
      p(
        'Connor is a recent graduate from Western University with an Honors BSc in Computer Science and a Minor in Software Engineering. He is currently joining the Tech & Operations team at RBC in Toronto. Connor is passionate about artificial intelligence and data science. He is always looking for projects in these areas to get involved with, to learn, and contribute to growing research fields in these areas.'
      )
    )
    ,
    cellArgs = list(style = 'white-space: normal; overflow: hidden;')
  ),
  splitLayout(
    verticalLayout(p(
      'Contact Justin  at: ',
      a(href = 'mailto:powtatow@gmail.com', 'powtatow@gmail.com')
    ), p(
      a(
        href = 'https://twitter.com/JuiceMarsh',
        target = "_blank",
        tags$img(
          src = 'twitter_logo.png',
          height = '24',
          width = '28'
        )
      ),
      ' - ',
      a(
        href = 'https://www.linkedin.com/in/JustinMarshall1998/',
        target = "_blank",
        tags$img(
          src = 'linkedin_logo.png',
          height = '24',
          width = '24'
        )
      ),
      ' - ',
      a(
        href = 'https://github.com/JustinMarshall98',
        target = "_blank",
        tags$img(
          src = 'github_logo.png',
          height = '32',
          width = '32'
        )
      )
    )),
    verticalLayout(p(
      'Contact Connor at: ',
      a(href = 'mailto:cozcon@gmail.com', 'cozcon@gmail.com')
    ), p(
      a(
        href = 'https://twitter.com/ConnorCozens',
        target = "_blank",
        tags$img(
          src = 'twitter_logo.png',
          height = '24',
          width = '28'
        )
      ),
      ' - ',
      a(
        href = 'https://www.linkedin.com/in/connorcozens/',
        target = "_blank",
        tags$img(
          src = 'linkedin_logo.png',
          height = '24',
          width = '24'
        )
      ),
      ' - ',
      a(
        href = 'https://github.com/connor-cozens',
        target = "_blank",
        tags$img(
          src = 'github_logo.png',
          height = '32',
          width = '32'
        )
      )
    ))
    ,
    cellArgs = list(style = 'white-space: normal; overflow: hidden;')
  ),
  hr(),
  h4('Nathan Lau'),
  p(em('Research Engineer')),
  p(
    'Nathan is a Post-Doctoral Fellow with a Doctoral Degree in Psychology from Western University.'
  ),
  p(
    'Contact Nathan at: ',
    a(href = 'mailto:tlau97@uwo.ca', 'tlau97@uwo.ca')
  ),
  p(
    a(
      href = 'https://twitter.com/nathanlau16',
      target = "_blank",
      tags$img(
        src = 'twitter_logo.png',
        height = '24',
        width = '28'
      )
    )
  ),
  hr(),
  h4('Peter J. Taylor'),
  p(em('Technical Advisor')),
  p(
    a(
      href = 'https://twitter.com/br00t4c',
      target = "_blank",
      tags$img(
        src = 'twitter_logo.png',
        height = '24',
        width = '28'
      )
    ),
    ' - ',
    a(
      href = 'https://gitlab.com/br00t',
      target = "_blank",
      tags$img(
        src = 'gitlab_logo.png',
        height = '32',
        width = '32'
      )
    )
  ),
  hr(),
  h4('Kelly Bairos'),
  p(em('Project Coordinator')),
  p(
    'Kelly has a Master’s in Education with a focus on Education Policy, and has been a project manager on several education research projects since 2010. Her professional work not only includes oversight of administrative, organizational, and financial project matters, but also planning and executing knowledge mobilization strategies.'
  ),
  p(
    'Contact Kelly at: ',
    a(href = 'mailto:kbairos@uwo.ca', 'kbairos@uwo.ca')
  ),
  hr(),
  p('For general inquiries:'),
  p(
    'Contact: ',
    a(href = 'mailto:covid19schooldashboard@gmail.com', 'covid19schooldashboard@gmail.com')
  ),
)