library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Inference for Proportions"
APP_DESCP  <<- paste(
  "This app shows how the confidence level and sample size affect the 
  outcome confidence interval for a single proportion under the null hypothesis
  of no bias. The app also explores the same issues for a confidence interval
  for the difference between two population proportions."
)
## End App Meta Data------------------------------------------------------------

dashboardPage(skin="purple",
              #Title
              dashboardHeader(
                title="Inference for Proportions",
                titleWidth=250,
                tags$li(class = "dropdown",
                        tags$a(href='https://shinyapps.science.psu.edu/',
                               icon("home")))
              ),
              #Sidebar
              dashboardSidebar(
                width = 250,
                sidebarMenu(id = "tabs",
                  menuItem("Overview", tabName = "over", icon = icon("dashboard")),
                  menuItem("UP Residency Perentage", tabName = "UPRes", icon = icon("wpexplorer")),
                  menuItem("Difference of Proportions", tabName = "popdiff", icon = icon("wpexplorer")),
                  menuItem("Finding the \\(Z^*\\) Multiplier", tabName = "findz", icon = icon("wpexplorer")),
                  menuItem("References",tabName = "Ref",icon = icon("leanpub"))
                ),
                #PSU Logo
                tags$div(
                  class = "sidebar-logo",
                  boastUtils::psu_eberly_logo("reversed")
                )),
              #Content within the tabs
              dashboardBody(tags$head(
                tags$link(rel = "stylesheet", type = "text/css", 
                          href="https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
              ),
              tabItems(
                tabItem(tabName = "over",
                        h1("Inference for Proportions"),
                          p("This app shows how the confidence level and sample size affect the outcome confidence interval for a single proportion under the null hypothesis of no bias. The app also explores the same issues for a confidence interval for the difference between two population proportions."),
                          br(),         
                          h2("Instructions"),
                          tags$ol(
                            tags$li("Move the sample size and level sliders to see how they affect  confidence intervals for the proportion of Penn State students who are residents of Pennsylvania  or two-sample tests for differences in the proportion of Pennsylvania residents between the University Park campus and the other campuses."),
                            tags$li("Click on the generate buttons to draw new samples and click on the center of an interval to show data for that sample.")
                            ),   
                          
                          div(style = "text-align: center", bsButton("explore", "Explore", icon("bolt"), size = "large",class = "circle grow")),
                          br(),
                          h2("Acknowledgements"),
                          p("This app was developed and programmed by  Yingjie (Chelsea) Wang and updated by Zhuolin Luo in 2020.",
                            br(),
                            br(),
                            br(),
                            div(class = "updated", "Last Update: 07/22/2020 by ZL.")
                          )
                  ),
                tabItem(tabName = "UPRes",
                          fluidPage(
                            fluidRow(
                              h2("Confidence Intervals for Enrollment by Residency in 2016 (p = 59.5%)"),
                              box(
                              title = strong("Context"), # This is the header of the box. Consider using "Story Context"
                              status = "primary",
                              collapsible = TRUE,
                              collapsed = FALSE,
                              width = '100%', 
                              "A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the proportion of Penn State Students who are Pennsylvania residents based on her study. This app shows  how confidence intervals of that type would come out when there is no bias."
                              )
                            ),
                            fluidRow(
                              column(4,
                                wellPanel(
                                h3(strong("Hypothesis: ")),
                                uiOutput("nullhypo"),
                                sliderInput("level", "Confidence Level",
                                            min=.50, max = 0.99, value = 0.90, step = 0.01),
                                sliderInput("nsamp", "Sample Size (n > 30)",
                                            min= 30, max = 500, value = 30, step = 5),
                                actionButton("new", "Generate 50 New Samples",icon("retweet")),
                                bsPopover("new","Note","Click to generate 50 new samples, each with  the sample size you have input.",
                                          trigger="hover",placement="right")
                                )
                              ),
                              column(4,
                                     plotOutput("popMean",height = "350px"),
                                     tags$script(HTML(
                                     "$(document).ready(function() {
                                     document.getElementById('popMean').setAttribute('aria-label',
                                     `proportion bar graph for population`)
                                      })"
                                      ))
                              ),
                              column(4,
                                     plotOutput("sampProp",height = "350px")),
                                     tags$script(HTML(
                                     "$(document).ready(function() {
                                     document.getElementById('sampProp').setAttribute('aria-label',
                                     `proportion bar graph for selected sample`)
                                      })"
                              ))
                            ),
                            fluidRow(
                                     wellPanel(
                                       plotOutput("CIplot",height = "700px", click = "plot_click"),
                                       tags$script(HTML(
                                         "$(document).ready(function() {
                                                   document.getElementById('CIplot').setAttribute('aria-label',
                                                   `plot out the confidence intervals`)
                                                   })"
                                       )),br(),
                                       textOutput("CoverageRate"),
                                       br(),
                                       textOutput("notice"),
                                       textOutput("navy"),
                                       textOutput("red"),
                                       bsPopover("sampProp","Sample Bar Graph","This is the bar plot of the sample you selected on Confidence Interval Plot. The green line is the true proportion.",
                                               trigger="hover",placement="top"),
                                       bsPopover("popMean","Population Bar Graph","This is the bar plot based on true proportion.",
                                               trigger="hover",placement="top"),
                                       bsPopover("CIplot","Confidence Interval Plot","Click on an interval to show a histogram for the underlying sample.",
                                               trigger="hover",placement="bottom"),     
                                       tags$head(tags$style("#CoverageRate{color: green;
                                          font-size: 18px;
                                          font-style: italic;
                                                     }"
                                                     
                                     ))
                                     )
                            )
                          )
                ),
                                     
                  tabItem(tabName = "findz",
                          fluidPage(
                            fluidRow(
                              h2("Confidence Intervals for a population mean (μ = 0 and σ = 1)")
                            ),
                            fluidRow(
                              column(4,
                                     wellPanel(
                                       h3(strong("Finding the \\(Z^*\\) Multiplier")),
                                       p("The value of the \\(Z^*\\) multiplier is dependent on the level of confidence."),
                                       sliderInput("zlevel", "Confidence Level", min=.50, max = 0.99, value = 0.90, step = 0.01)
                                     )
                              ),
                              column(8,
                                     plotOutput("zplot"),
                                     tags$script(HTML(
                                       "$(document).ready(function() {
                                                   document.getElementById('zplot').setAttribute('aria-label',
                                                   `confidence interval plot for standard normal distribution`)
                                                   })"
                                     )),
                                     bsPopover("zplot","Z Score Plot","This is the confidence interval plot for standard normal distribution. Multiplier Number (\\(Z^*\\)) is the absolute value of the boundary value. Use the value showed on this graph for following questions",
                                                trigger="hover",placement="bottom")
                              )
                            ),
                            fluidRow(
                              wellPanel(
                                       textOutput("feedback"),
                                       h3("Quiz"),
                                       h4("What is \\(Z^*\\) Multiplier for 90% confidence level?",style="font-size:90%"),
                                       div(style="display:inline-block",textInput("question1", " ", width='2cm',"")),
                                       div(style="display:inline-block",htmlOutput('pic1')),
                                       h4("What is \\(Z^*\\) Multiplier for 95% confidence level?",style="font-size:90%"),
                                       div(style="display:inline-block",textInput("question2", " ", width='2cm',"")),
                                       div(style="display:inline-block",htmlOutput('pic2')),
                                       h4("What is \\(Z^*\\) Multiplier for 99% confidence level?",style="font-size:90%"),
                                       div(style="display:inline-block",textInput("question3", " ", width='2cm',"")),
                                       div(style="display:inline-block",htmlOutput('pic3')),
                                       h4("Increasing the confidence level makes the confidence interval wider.",style="font-size:90%"),
                                       div(style="display:inline-block",selectInput("question4", " ",
                                                                                    c("True" = "y",
                                                                                      "False" = "n",
                                                                                      " " = "null"),width='2cm',selected = "null")),
                                       div(style="display:inline-block",htmlOutput('pic4'))
                                       
                                     ))
                            )
                          ),

                  tabItem(tabName = "popdiff",
                          fluidPage(
                            fluidRow(
                              h2("Tests for Enrollment by Residency between University Park and Commonwealth Campuses"),
                              box(
                                title = strong("Context"), # This is the header of the box. Consider using "Story Context"
                                status = "primary",
                                collapsible = TRUE,
                                collapsed = FALSE,
                                width = '100%', 
                                "A researcher wants to sample a group of n University Park students and n students from other Penn State campuses to ask them about their experiences in college. 
                                Although the proportion of Pennsylvania residents is 0.249 lower at University Park, a critic believes her sampling technique might change that difference. 
                                The researcher uses her samples to create a confidence interval for the difference between the University Park campus and the Commonwealth campuses for the proportion who are Pennsylvania residents."
                                )
                            ),
                            fluidRow(
                              column(4,
                                   wellPanel(
                                     h3(strong("Population info:")),
                                     tableOutput("popInfo"),
                                     textOutput("pop"),
                                     br(),
                                     h3(strong("New Sample Info")),
                                     p("(Penn student percentage in two locations are shown below)"),
                                     tableOutput("sampleinfotable"),
                                     uiOutput("Diffinfo"),  
                                     br(),
                                     actionButton("newSample", "Generate New Samples",icon("retweet")),
                                     br()
                                   )
                            ),
                              column(8,
                                   plotOutput("dpopMean",height = "300px"),
                                   tags$script(HTML(
                                     "$(document).ready(function() {
                                                   document.getElementById('dpopMean').setAttribute('aria-label',
                                                   `stacked bar graph for population`)
                                                   })"
                                   )),
                                   bsPopover("dpopMean","Population Stacked Bar Graph","The two bars show proportions of Enrollment by Residency in University Park and Other Campuses.",
                                             trigger="hover",placement="bottom"),br(),
                                   plotOutput("sampleDiff",height = "300px"),
                                   tags$script(HTML(
                                     "$(document).ready(function() {
                                                   document.getElementById('sampleDiff').setAttribute('aria-label',
                                                   `stacked bar graph for sample`)
                                                   })"
                                   )),
                                   bsPopover("sampleDiff","Sample Stacked Bar Graph","These stacked bar graphs show the generated sample proportions of penn residency in University Park and Other Campuses. The horizontal lines on each bar indicate the population proportions for the two groups.",
                                             trigger="hover",placement="bottom"),
                                   
                                   bsPopover("newSample","Note","By clicking on this button, new sample with the size you input will be generated on the Sample Stacked Bar Graph.",
                                             trigger="hover",placement="bottom"))
                          ),
                          fluidRow(
                            column(4,
                                   wellPanel(
                                     sliderInput("dlevel", "Confidence Level",
                                                 min=.10, max = 0.99, value = 0.90, step = 0.01),
                                     sliderInput("nSamp", "Sample Size for two groups",
                                                 min=30, max = 150, value = 50, step = 5)
                                   )),
                            column(8,
                                   checkboxInput("CIcheckbox","Show Confidence Interval:", FALSE), 
                                   tableOutput("CItable"))
                            
                          )
                          )

                  ),
 
                  tabItem(
                    tabName = "Ref",
                    withMathJax(),
                    h2("References"),
                    p(class = "hangingindent",
                      "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R package. Available from https://CRAN.R-project.org/package=shinyBS"),
                    p(class = "hangingindent",
                      "Bates D. and Maechler M. (2019), Matrix: Sparse and Dense Matrix Classes and Maethods, R package. Available from https://cran.r-project.org/web/packages/Matrix/index.html"),
                    p(class = "hangingindent",
                      "Budget Office in PSU. (2019), Enrollment by Residency Fall 2019. Available at https://factbook.psu.edu/factbook/StudentDynamic/PANonPASummary.aspx?YearCode=2019Enr&FBPlusIndc=N"),
                    p(class = "hangingindent",
                      "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create dashboards with 'Shiny', R Package. Available from https://CRAN.R-project.org/package=shinydashboard"),
                    p(class = "hangingindent",
                      "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019), shiny: Web application framework for R, R Package. Available from https://CRAN.R-project.org/package=shiny"),
                    p(class = "hangingindent",
                      "Harrell, F.E. (2020), Confidence Intervals for Binomial Probability. We needed to bypass the loading of the foreign package for R 3.6.3, thus we are using the definition of the binconf which is all we needed from Hmisc. Available from https://CRAN.R-project.org/package=Hmisc"),
                    p(class = "hangingindent",
                      "Pruim, R., Kaplan, D.T., and Horton, N.J. (2020), mosaic: Project MOSAIC statistics and mathematics teaching utilities, R Package. Avaliable from https://CRAN.R-project.org/package=mosaic"),
                    p(class = "hangingindent",
                      "Wickham, H., Francois R., Henry L., and Muller K. (2020), dplyr: A Grammar of Data Manipulation, R Package. Available from https://cran.r-project.org/web/packages/dplyr/index.html"),
                    p(class = "hangingindent",
                      "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis, R Package, New York: Springer-Verlag. Available from https://ggplot2.tidyverse.org"),
                    p(class = "hangingindent",
                      "Wickham, H., Seidel, D., and R Studio. (2020), scales: Scale function for visualization, R Package. Availabel from https://CRAN.R-project.org/package=scales"),
                    
                    )
                
                )#end of tabItem
              )#end of dashboardBody
              
)

