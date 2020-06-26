library(shiny)
library(shinydashboard)
library(shinyBS)


dashboardPage(skin="purple",
              
              #Title
              dashboardHeader(title="Inference for Proportions",titleWidth=250,
                              tags$li(class = "dropdown", actionLink("info", icon("info"))),
                              tags$li(class = "dropdown",
                                      tags$a(href='https://github.com/EducationShinyAppTeam/BOAST',
                                             icon("github"))),
                              tags$li(class = "dropdown",
                                      tags$a(href='https://shinyapps.science.psu.edu/',
                                             icon("home")))
                              ),
              
              #Sidebar
              dashboardSidebar(
                width = 250,
                sidebarMenu(id = "tabs",
                  menuItem("Overview", tabName = "over", icon = icon("dashboard")),
                  menuItem("UP Residency Percentage", tabName = "UPRes", icon = icon("wpexplorer")),
                  menuItem("Finding the z∗ Multiplier", tabName = "findz", icon = icon("wpexplorer")),
                  menuItem("Difference of Proportions", tabName = "popdiff", icon = icon("wpexplorer")),
                  #menuItem("Z-score Table", tabName = "table", icon = icon("flag"))
                  menuItem("Reference",tabName = "Ref",icon = icon("leanpub"))
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
                          h1("Inference for proportions"),
                          p("This app represents an interactive supplementary module embedded in statistics lessons with two features. The first feature visualizes how the variations in confidence levels and sample size affect the outcome confidence interval in a single mean. The second feature tests the difference between two means by adjusting confidence levels and sample size and generating calculation results with explanations. The app requires students to engage in the interaction with the scenarios provided in context."),
                          p("This lesson explores the behavior of confidence intervals for a single proportion and two sample tests for the difference in proportions as the level and sample size changes."),
                          br(),         
                          h2("Instructions"),
                          tags$ol(
                            tags$li("Move the sample size and level sliders to see how they affect  confidence intervals for the proportion of Penn State students who are residents of Pennsylvania  or two-sample tests for differences in the proportion of Pennsylvania residents between the University Park campus and the other campuses."),
                            tags$li("Click on the generate buttons to draw new samples and - for the confidence interval app - click on the center of an interval to show data for that sample."),
                            tags$li("The Z* multiplier page allows the user to find critical values (multiplier numbers) needed in making a confidence interval. Complete a short quiz to show you have mastered the concept."),
                            tags$li("On the Test for Differences page the user can change the sample size and/or the confidence level to explore the behavior of a z-test for differences between the UP campus and other Penn State campuses on percentage fo students who are Pennsylvania residents based on sample data.")
                          ),   
                          
                          div(style = "text-align: center", bsButton("explore", "Explore", icon("bolt"), size = "large",class = "circle grow")),
                          br(),
                          h2("Acknowledgements"),
                          p("Information about confidence interval graph was drawn from Randall Pruim's shiny app."),
                          p(
                            "Budget Office in PSU. (2019), Enrollment by Residency Fall 2019. Available at https://factbook.psu.edu/factbook/StudentDynamic/PANonPASummary.aspx?YearCode=2019Enr&FBPlusIndc=N"
                          ),
                          
                          helpText(
                            a("Click Here to view original official page",
                              href = "https://factbook.psu.edu/factbook/StudentDynamic/PANonPASummary.aspx?YearCode=2019Enr&FBPlusIndc=N",
                              target = "blank"
                            )
                          ),
                          br(),
                          p("We would like to extend a special thanks to the Shiny Program Students.",
                            br(),
                            br(),
                            br(),
                            div(class = "updated", "Last Update: 06/26/2020 by ZL.")
                          )
                  ),
                  
                  
                  tabItem(tabName = "UPRes",
                          fluidPage(
                            
                            titlePanel("Confidence Intervals for Enrollment by Residency in 2016 (p = 59.5%)"),
                            box(
                              title = strong(tags$em("Design:")), # This is the header of the box. Consider using "Story Context"
                              status = "primary",
                              collapsible = TRUE,
                              collapsed = FALSE,
                              width = '100%', 
                              "A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study. This app shows  how confidence intervals of that type would come out when there is no bias.")
                            ,
                            sidebarLayout(
                              sidebarPanel(
                                # h3(strong("Confidence Intervals for a population mean (n > 30): ")),
                                # h4("For large random samples a confidence interval for a population mean is given by"),
                                # h3("sample mean ± z* s / sqrt(n)"),
                                # h4("where z* is a multiplier number that comes form the normal curve and determines the level of confidence."),
                                
                                
                                #h4("A researcher plans to take a random sample of size n students to do a survey about their experiences in studying at the University Park campus of Penn State University. However, she worries that sample results could be biased because the students who agree to participate might be different from those who don't (this would be an example of non-response bias). The researcher makes a confidence interval for the percentage of Penn State Students who are Pennsylvania residents based on her study and compares it to the mean of 59.5% for the population of all Penn State University Park students. This app shows  how confidence intervals of that type would come out when there is no bias."),
                                #h4("TIP: Click on an interval to show a histogram for the underlying sample."),
                                h3(strong("Hypothesis: ")),
                                uiOutput("nullhypo"),
                                
                                
                                
                                sliderInput("level", "Confidence Level",
                                            min=.50, max = 0.99, value = 0.90, step = 0.01),
                                sliderInput("nsamp", "Sample Size (n > 30)",
                                            min= 30, max = 500, value = 30, step = 5),
                                
                                actionButton("new", "Generate 50 New Samples",icon("retweet")),
                                bsPopover("new","Note","By clicking on this button, new 50 sample with the size you have input in each sample will be generated.",
                                          trigger="hover",placement="right"),br(),
                                br(),br(),
                                
                                wellPanel(
                                textOutput("notice"),br(),
                                textOutput("navy"),br(),
                                textOutput("red"),br()
                                )
                                
                                
                              ),
                              
                              mainPanel(fluidRow(
                                splitLayout(cellWidths = c("50%", "50%"), plotOutput("popMean",height = "350px"),plotOutput("sampProp",height = "350px")),br(),
                                wellPanel(plotOutput("CIplot",height = "600px", click = "plot_click"),br(),
                                          textOutput("CoverageRate")),
                                br(),
                                bsPopover("sampProp","Sample Bar Graph","This is the bar plot of the sample you selected on Confidence Interval Plot. The green line is the true percentage.",
                                          trigger="hover",placement="top"),br(),
                                
                                bsPopover("popMean","Population Bar Graph","This is the bar plot based on true percentage.",
                                          trigger="hover",placement="top"),br(),
                                
                                bsPopover("CIplot","Confidence Interval Plot","Click on an interval to show a histogram for the underlying sample.",
                                          trigger="hover",placement="bottom"),br(),
                                
                                
                                tags$head(tags$style("#CoverageRate{color: green;
                                 font-size: 18px;
                                                     font-style: italic;
                                                     }"
                                )
                                )
                              )
                              )
                              )
                            )
                          ),
              
                  tabItem(tabName = "findz",
                          fluidPage(
                           
                            titlePanel("Confidence Intervals for a population mean (μ = 0 and σ = 1)"),
                            sidebarLayout(
                              sidebarPanel(
                                
                                h3(strong("Finding the z∗ Multiplier")),
                                h4("The value of the z∗ multiplier is dependent on the level of confidence."),
                                sliderInput("zlevel", "Confidence Level",
                                            min=.50, max = 0.99, value = 0.90, step = 0.01),
                                br(),
                                
                                wellPanel(
                                  
                                  style = "background-color: #A9A9A9;",
                                  
                                  h3("Quiz"),
                                  h4("What is z∗  Multiplier for 90% confidence level?",style="font-size:90%"),
                                  div(style="display:inline-block",textInput("question1", " ", width='2cm',"")),
                                  div(style="display:inline-block",htmlOutput('pic1')),
                                  h4("What is z∗  Multiplier for 95% confidence level?",style="font-size:90%"),
                                  div(style="display:inline-block",textInput("question2", " ", width='2cm',"")),
                                  div(style="display:inline-block",htmlOutput('pic2')),
                                  h4("What is z∗  Multiplier for 99% confidence level?",style="font-size:90%"),
                                  div(style="display:inline-block",textInput("question3", " ", width='2cm',"")),
                                  div(style="display:inline-block",htmlOutput('pic3')),
                                  h4("Increasing the confidence level makes the confidence interval wider.",style="font-size:90%"),
                                  div(style="display:inline-block",selectInput("question4", " ",
                                                                               c("True" = "y",
                                                                                 "False" = "n",
                                                                                 " " = "null"),width='2cm',selected = "null")),
                                  div(style="display:inline-block",htmlOutput('pic4'))
                                  #textInput("question4", "Increasing the confidence level makes confidence interval getting larger? (YES or NO)", "input 'YES' or 'NO'"),
                                  # br(),
                                  # actionButton("submit", "Submit Answers")
                                )
                                
                              ),
                              
                              mainPanel(
                                plotOutput("zplot"),
                                bsPopover("zplot","Z Score Plot","This is the confidence interval plot for standard normal distribution. Multiplier Number (z*) is the absolute value of the boundary value. Use the value showed on this graph for following questions",
                                          trigger="hover",placement="bottom"),
                                br(),
                                br(),
                                h3("Feedback: "),
                                textOutput("feedback"),
                                tags$head(tags$style("#feedback{color:green;
                                                    font-size: 35px;
                                                    }"))
                              )
                            )
                          )
                  ),
                  tabItem(tabName = "popdiff",
                          
                          titlePanel("Tests for Enrollment by Residency between University Park and Commonwealth Campuses"),
                          sidebarLayout(
                            sidebarPanel(
                              h3(strong("Design:")),
                              checkboxInput("testdesigncheckbox","Show design info:", TRUE),
                              uiOutput("testdesign"),
                              #h4("A researcher wants to sample a group of n University Park students and n students from other Penn State campuses to ask them about their experiences in college.  Although the percentage of Pennsylvania residents is 24.9% lower at University Park, a critic believes her sampling technique would provide a sample of students with a proportion (p) that does not depend on the campus (the null hypothesis). The researcher uses her samples to conduct a test of that null hypothesis and this app shows how that test would behave when the sampling is really unbiased and the University Park campus has a proportion that is 0.249 lower lower. "),
                              # h3(strong("For large random samples a confidence interval for the difference between two population means is given by")),
                              # h3("Difference Between the Sample Means ± z ∗ (Standard Error for Difference) "),
                              # h4("When two samples are independent of each other, Standard Error for a Difference between two sample summaries ="),
                              # img(src="2sample.png",height = 50,width = 350,algin = "middle"),
                              # h4("sqrt((standard error in first sample)^2+(standard error in second sample)^2)"),
                              #div(tableOutput('matrixScore'),style = "font-size:80%"), 
                              h3(strong("Population info:")),
                              img(src="2016Diff.png",height = "100%", width = "100%",algin = "middle"),
                              #img(src="2016Diff.png",height = 100,width = 350,algin = "middle"),
                              # h4("difference between twp μ = 524 - 494 = 30"),
                              # h4("difference between two σ = sqrt(0.021+0.015) = 0.190"),
                              h3(strong("Hypothesis: ")),
                              h4("Assume there is no difference of penn residency percentage between University Park and Other Campuses"),
                              h4("Ho: p(University Park) = p(Other Campuses)"),
                              h4("Ha: p(University Park) ≠ p(Other Campuses)"),
        
                              wellPanel(
                                h3(strong("New Sample Info")),
                                h4("Penn student percentage in two locations are shown below"),
                                tableOutput("sampleinfotable"),
                                uiOutput("Diffinfo")
                              ),
                               sliderInput("dlevel", "Confidence Level",
                                          min=.10, max = 0.99, value = 0.90, step = 0.01),
                               sliderInput("nSamp", "Sample Size for two groups",
                                          min=30, max = 150, value = 50, step = 5)
                            ),
                            
                            mainPanel(
                              
                              plotOutput("dpopMean",height = "300px"),
                              bsPopover("dpopMean","Population Stacked Bar Graph","The two bars show precentage of Enrollment by Residency in University Park and Other Campuses.",
                                        trigger="hover",placement="bottom"),br(),
                              plotOutput("sampleDiff",height = "300px"),
                              bsPopover("sampleDiff","Sample Stacked Bar Graphs","These stacked bar graphs show the generated sample proportions of penn residency in University Park and Other Campuses. The horizontal lines on each bar indicate the population proportions for the two groups.",
                                        trigger="hover",placement="bottom"),
                              actionButton("newSample", "Generate New Samples",icon("retweet")),
                              bsPopover("newSample","Note","By clicking on this button, new sample with the size you input will be generated on the Sample Histogram.",
                                        trigger="hover",placement="bottom"),
                              
                              splitLayout(
                                wellPanel(
                                  checkboxInput("CIcheckbox","Show Confidence Interval:", FALSE), 
                                  tableOutput("CItable")
                                ),
                                wellPanel(
                                  checkboxInput("testcheckbox","Show Test Output:", FALSE), 
                                  tableOutput("testtable")
                                )
                              ),
                             
                              wellPanel(
                                checkboxInput("decisioncheckbox","Decision about the null hypothesis:", FALSE), 
                                textOutput("decisionZ"),
                                br(),
                                textOutput("decisionP")
                              )
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
                      "Frank E. Harrell, Jr. (2020), Confidence Intervals for Binomial Probability. We needed to bypass the loading of the foreign package for R 3.6.3, thus we are using the definition of the binconf which is all we needed from Hmisc. Available from https://CRAN.R-project.org/package=Hmisc"),
                    p(class = "hangingindent",
                      "Pruim, R., Kaplan, D., and Horton, N. (2020), mosaic: Project MOSAIC statistics and mathematics teaching utilities, R Package. Avaliable from https://CRAN.R-project.org/package=mosaic"),
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

