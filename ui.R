shinyUI(pageWithSidebar(
  headerPanel('Poisson Regression and Negative Binomial Regression Probabilities'),
  sidebarPanel(
    sliderInput('mu', 'Select mu parameter (no explanatory variables)', value = 1, min = 0, max = 10, step = 0.25),
    sliderInput('b_0', 'Select intercept parameter', value = 1, min = -10, max = 10, step = 0.25),
    sliderInput('b_1', 'Select coefficient parameter', value = 1, min = -10, max = 10, step = 0.25),
    sliderInput('alpha', 'Select alpha parameter (negative binomial)', value = 1, min = 0, max = 10, step = 0.25),
    sliderInput('x', 'Select explanatory variable value', value = -1, min = -10, max = 10, step = 0.25)),
  mainPanel(
    plotOutput('plot1', width = "600px", height = "600px"),
    plotOutput('plot2', width = "600px", height = "600px"),
    plotOutput('plot3', width = "600px", height = "600px"),
    plotOutput('plot4', width = "600px", height = "600px"),
    plotOutput('plot5', width = "600px", height = "600px")#,
    # plotOutput('plot6', width = "600px", height = "600px")
  )
))
