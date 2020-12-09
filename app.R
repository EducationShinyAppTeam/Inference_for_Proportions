# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(dplyr)
library(Matrix)
library(mosaic)
library(ggplot2)
library(scales)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Inference for Proportions"
APP_DESCP  <<- paste(
  "This app shows how the confidence level and sample size affect the
  outcome confidence interval for a single proportion under the null hypothesis
  of no bias. The app also explores the same issues for a confidence interval
  for the difference between two population proportions."
)
## End App Meta Data------------------------------------------------------------

# Global constants, functions, and data ----
#-------------------------------------------------------------------------------
#  Title: Confidence Intervals for Binomial Probability
#  Author: Frank E. Harrell, Jr.
#  Date: 2020
#  Code version: 4.4-0
#  Availability: https://CRAN.R-project.org/package=Hmisc
#  We needed to bypass the loading of the foreign package for R 3.6.3, thus
#  we are using the definition of the binconf which is all we needed from Hmisc.
#-------------------------------------------------------------------------------
binconf <- function (x, n, alpha = 0.05,
                     method = c("wilson", "exact", "asymptotic", "all"),
                     include.x = FALSE, include.n = FALSE, return.df = FALSE)
{
  method <- match.arg(method)
  bc <- function(x, n, alpha, method) {
    nu1 <- 2 * (n - x + 1)
    nu2 <- 2 * x
    ll <- if (x > 0)
      x/(x + qf(1 - alpha/2, nu1, nu2) * (n - x + 1))
    else 0
    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if (x < n)
      qf(1 - alpha/2, nu1p, nu2p)
    else 1
    ul <- ((x + 1) * pp)/(n - x + (x + 1) * pp)
    zcrit <- -qnorm(alpha/2)
    z2 <- zcrit * zcrit
    p <- x/n
    cl <- (p + z2/2/n + c(-1, 1) * zcrit *
             sqrt((p * (1 - p) + z2/4/n)/n))/(1 + z2/n)
    if (x == 1)
      cl[1] <- -log(1 - alpha)/n
    if (x == (n - 1))
      cl[2] <- 1 + log(1 - alpha)/n
    asymp.lcl <- x/n - qnorm(1 - alpha/2) * sqrt(((x/n) *
                                                    (1 - x/n))/n)
    asymp.ucl <- x/n + qnorm(1 - alpha/2) * sqrt(((x/n) *
                                                    (1 - x/n))/n)
    res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
    res <- cbind(rep(x/n, 3), res)
    switch(method, wilson = res[2, ], exact = res[1, ], asymptotic = res[3,
    ], all = res, res)
  }
  if ((length(x) != length(n)) & length(x) == 1)
    x <- rep(x, length(n))
  if ((length(x) != length(n)) & length(n) == 1)
    n <- rep(n, length(x))
  if ((length(x) > 1 | length(n) > 1) & method == "all") {
    method <- "wilson"
    warning("method=all will not work with vectors...setting method to wilson")
  }
  if (method == "all" & length(x) == 1 & length(n) == 1) {
    mat <- bc(x, n, alpha, method)
    dimnames(mat) <- list(c("Exact", "Wilson", "Asymptotic"),
                          c("PointEst", "Lower", "Upper"))
    if (include.n)
      mat <- cbind(N = n, mat)
    if (include.x)
      mat <- cbind(X = x, mat)
    if (return.df)
      mat <- as.data.frame(mat)
    return(mat)
  }
  mat <- matrix(ncol = 3, nrow = length(x))
  for (i in 1:length(x)) mat[i, ] <- bc(x[i], n[i], alpha = alpha,
                                        method = method)
  dimnames(mat) <- list(rep("", dim(mat)[1]), c("PointEst",
                                                "Lower", "Upper"))
  if (include.n)
    mat <- cbind(N = n, mat)
  if (include.x)
    mat <- cbind(X = x, mat)
  if (return.df)
    mat <- as.data.frame(mat, row.names = NULL)
  mat
}
#End of Harrell's code----------------------------------------------------------

# Define the UI ----
ui <- list(
  dashboardPage(
    skin="purple",
    #Title
    dashboardHeader(
      title="Inference for Proportions",
      titleWidth=250,
      tags$li(class = "dropdown",
              tags$a(target = "_blank", icon("comments"),
                     href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Inference_for_Proportions"
              )
      ),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    #Sidebar
    dashboardSidebar(
      width = 250,
      sidebarMenu(id = "pages",
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
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", 
                  href="https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
      ),
      tabItems(
        tabItem(
          tabName = "over",
          h1("Inference for Proportions"),
          p("This app shows how the confidence level and sample size affect 
           the outcome confidence interval for a single proportion under 
           the null hypothesis of no bias. The app also explores the same 
           issues for a confidence interval for the difference between two 
           population proportions."),
          br(),         
          h2("Instructions"),
          tags$ol(
            tags$li("Move the sample size and level sliders to see how they affect 
                   confidence intervals for the proportion of Penn State students 
                   who are residents of Pennsylvania  or two-sample tests for 
                   differences in the proportion of Pennsylvania residents between
                   the University Park campus and the other campuses."),
            tags$li("Click on the generate buttons to draw new samples and 
                   click on the center of an interval to show data for that sample.")
          ),   
          div(style = "text-align: center", bsButton("explore", "Explore", 
                                                     icon("bolt"), size = "large",
                                                     class = "circle grow")),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and programmed by  Yingjie (Chelsea) Wang and 
          updated by Zhuolin Luo in 2020.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/10/2020 by ZL."))
        ),
        
        tabItem(
          tabName = "UPRes",
          h2("Confidence Intervals for Enrollment by Residency in 2016 (p = 59.5%)"),
          box(
            title = "Context", 
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%', 
            p("A researcher plans to take a random sample of size n students
             to do a survey about their experiences in studying at the University Park 
             campus of Penn State University. However, she worries that sample results 
             could be biased because the students who agree to participate might be
             different from those who don't (this would be an example of non-response bias). 
             The researcher makes a confidence interval for the proportion of Penn State
             Students who are Pennsylvania residents based on her study. 
             This app shows  how confidence intervals of that type would come 
             out when there is no bias."
            )
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Hypothesis"),
                uiOutput("nullhypo"),
                sliderInput(
                  inputId = "level", 
                  label = "Confidence Level",
                  min=.50, 
                  max = 0.99, 
                  value = 0.90, 
                  step = 0.01
                ),
                sliderInput(
                  inputId = "nsamp", 
                  label = "Sample Size (n > 30)",
                  min= 30, 
                  max = 500, 
                  value = 30, 
                  step = 5
                ),
                p("Please click on this button to check the sample Histogram and 
                  its confidence interval."),
                bsButton(
                  inputId = "new",
                  label = "Generate 50 Samples",
                  icon = icon("retweet"),
                  size = "large"
                ),
                bsPopover(
                  id = "new",
                  title = "Note",
                  ## Possible Error ----
                  content = paste(
                    "Click to generate 50 new samples, 
                  each with  the sample size you have input."),
                  trigger = "hover",
                  placement = "bottom"
                )
              ),
              plotOutput("sampProp",height = "400px"),
              tags$script(HTML(
                "$(document).ready(function() {
             document.getElementById('sampProp').setAttribute('aria-label',
             `proportion bar graph for selected sample`)
             })"
              )),
              bsPopover(
                id = "sampProp",
                title = "Sample Bar Graph",
                content = paste("This is the bar plot of the sample you selected 
              on Confidence Interval Plot.The green line is the true proportion."),
                trigger="hover",
                placement="top"
              )
            ),
            column(
              width = 8,
              plotOutput("popMean",height = "450px"),
              tags$script(HTML(
                "$(document).ready(function() {
                    document.getElementById('popMean').setAttribute('aria-label',
                    `proportion bar graph for population`)
                    })"
              )),
              bsPopover(
                id = "popMean",
                title = "Population Bar Graph",
                content = paste("This is the bar plot based on true proportion.
                                      The green line is the true proportion."),
                trigger="hover",
                placement="top"
              ),
              plotOutput(
                outputId = "CIplot",
                height = "400px", 
                click = "plot_click"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
               document.getElementById('CIplot').setAttribute('aria-label',
               `plot out the confidence intervals`)
                })"
              )),
              textOutput("CoverageRate"),
              bsPopover(
                id = "CIplot",
                title = "Confidence Interval Plot",
                content = paste("Click on an interval to show a histogram for the underlying sample."),
                trigger="hover",
                placement="top"
              ),
              tags$head(tags$style("#CoverageRate{color: green;
                                          font-size: 18px;
                                          font-style: italic;
                                                     }"
                                   
              ))
            )
          )
        ),
        
        tabItem(
          tabName = "findz",
          h2("Finding the \\(Z^*\\) Multiplier"),
          p("The value of the \\(Z^*\\) multiplier is dependent on the level of
            confidence."),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "zlevel",
                  label = "Confidence level",
                  min = 85,
                  max = 99,
                  value = 90,
                  post = "%"
                )
              )
            ),
            column(
              width = 8,
              plotOutput("zplot"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('zplot').setAttribute('aria-label',
                `confidence interval plot for standard normal distribution`)
              })"
              )),
              bsPopover(
                id = "zplot",
                title = "Z Score Plot",
                content = paste("This is the confidence interval plot for standard normal distribution. 
                            Multiplier Number (\\(Z^*\\)) is the absolute value of the boundary value. 
                            Use the value showed on this graph for following questions."),
                trigger="hover",
                placement="bottom"
              )
            )
          ),
          h3("Check Your Skills"),
          textOutput("feedback"),
          p("What is \\(Z^*\\) Multiplier for 90% confidence level?"),
          fluidRow(
            column(
              width = 2,
              div(
                numericInput(
                  inputId = "question1", 
                  label = "Your answer", 
                  step = 0.001,
                  value = "")
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput(outputId = 'pic1')
            )
          ),
          p("What is \\(Z^*\\) Multiplier for 95% confidence level?"),
          fluidRow(
            column(
              width = 2,
              div(
                numericInput(
                  inputId = "question2", 
                  label = "Your answer", 
                  step = 0.001,
                  value = "")
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput(outputId = 'pic2')
            )
          ),
          p("What is \\(Z^*\\) Multiplier for 99% confidence level?"),
          fluidRow(
            column(
              width = 2,
              div(
                numericInput(
                  inputId = "question3", 
                  label = "Your answer", 
                  step = 0.001,
                  value = "")
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput(outputId = 'pic3')
            )
          ),
          p("Increasing the confidence level makes the confidence interval wider."),
          fluidRow(
            column(
              width = 2,
              div(
                selectInput(
                  inputId = "question4", 
                  label = "Your answer", 
                  choices = list(
                    "Select an answer" = "select",
                    "True" = "y",
                    "False" = "n"
                  ),
                  selected = "select"
                )
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput(outputId = 'pic4')
            )
          )
        ),
        
        tabItem(
          tabName = "popdiff",
          h2("Tests for Enrollment by Residency between University Park and Commonwealth Campuses"),
          box(
            title = "Context", # This is the header of the box. Consider using "Story Context"
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%', 
            p("A researcher wants to sample a group of n University Park 
           students and n students from other Penn State campuses to ask 
           them about their experiences in college. Although the proportion 
           of Pennsylvania residents is 0.249 lower at University Park, 
           a critic believes her sampling technique might change that difference. 
           The researcher uses her samples to create a confidence interval for the 
           difference between the University Park campus and the Commonwealth campuses 
           for the proportion who are Pennsylvania residents."
            ),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  h3("Population Information"),
                  tableOutput("popInfo"),
                  textOutput("pop"),
                  br(),
                  h3("Sample Information"),
                  uiOutput("Diffinfo"),
                  tableOutput("sampleinfotable"),
                  sliderInput("dlevel", "Confidence Level",
                              min=.10, max = 0.99, value = 0.90, step = 0.01),
                  sliderInput("nSamp", "Sample Size for two groups",
                              min=30, max = 150, value = 50, step = 5),
                  br(),
                  actionButton("newSample", "Generate New Samples",icon("retweet")),
                  bsPopover("newSample","Note","By clicking on this button,
                            new sample with the size you input will be
                            generated on the Sample Stacked Bar Graph.",
                            trigger="hover",placement="bottom"
                  )
                )
              ),
              column(
                width = 8,
                plotOutput("dpopMean",height = "300px"),
                tags$script(HTML(
                  "$(document).ready(function() {
                    document.getElementById('dpopMean').setAttribute('aria-label',
                    `stacked bar graph for population`)
                    })"
                )),
                bsPopover(
                  id = "dpopMean",
                  title = "Population Stacked Bar Graph",
                  content = paste("The two bars show proportions of Enrollment 
                            by Residency in University Park and Other Campuses."),
                  trigger="hover",
                  placement="bottom"
                ),
                br(),
                plotOutput("sampleDiff",height = "300px"),
                tags$script(HTML(
                  "$(document).ready(function() {
                    document.getElementById('sampleDiff').setAttribute('aria-label',
                    `stacked bar graph for sample`)
                                                   })"
                )),
                bsPopover(
                  id = "sampleDiff",
                  title = "Sample Stacked Bar Graph",
                  content = paste("These stacked bar graphs show the generated 
                            sample proportions of penn residency in University
                            Park and Other Campuses. The horizontal lines on 
                            each bar indicate the population proportions for the two groups."),
                  trigger="hover",
                  placement="top"
                ),
                tableOutput("CItable"),
                checkboxInput(
                  inputId = "CIcheckbox",
                  label = "Show Confidence Interval:", 
                  value = FALSE
                )
              )
            )
          )
        ),
        
        tabItem(
          tabName = "Ref",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R package. 
           Available from https://CRAN.R-project.org/package=shinyBS"),
          p(class = "hangingindent",
            "Bates D. and Maechler M. (2019), Matrix: Sparse and Dense Matrix Classes and Maethods, R package. 
           Available from https://cran.r-project.org/web/packages/Matrix/index.html"),
          p(class = "hangingindent",
            "Budget Office in PSU. (2019), Enrollment by Residency Fall 2019. Available at 
           https://factbook.psu.edu/factbook/StudentDynamic/PANonPASummary.aspx?YearCode=2019Enr&FBPlusIndc=N"),
          p(class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: 
           Create dashboards with 'Shiny', R Package. Available from 
           https://CRAN.R-project.org/package=shinydashboard"),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019), 
           shiny: Web application framework for R, R Package. 
           Available from https://CRAN.R-project.org/package=shiny"),
          p(class = "hangingindent",
            "Harrell, F.E. (2020), Confidence Intervals for Binomial Probability. 
           We needed to bypass the loading of the foreign package for R 3.6.3, 
           thus we are using the definition of the binconf which is all we needed 
           from Hmisc. Available from https://CRAN.R-project.org/package=Hmisc"),
          p(class = "hangingindent",
            "Pruim, R., Kaplan, D.T., and Horton, N.J. (2020), mosaic: 
           Project MOSAIC statistics and mathematics teaching utilities,
           R Package. Avaliable from https://CRAN.R-project.org/package=mosaic"),
          p(class = "hangingindent",
            "Wickham, H., Francois R., Henry L., and Muller K. (2020), dplyr: 
           A Grammar of Data Manipulation, R Package. Available from 
           https://cran.r-project.org/web/packages/dplyr/index.html"),
          p(class = "hangingindent",
            "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis, 
           R Package, New York: Springer-Verlag. Available from https://ggplot2.tidyverse.org"),
          p(class = "hangingindent",
            "Wickham, H., Seidel, D., and R Studio. (2020), scales: Scale 
           function for visualization, R Package. Availabel from 
           https://CRAN.R-project.org/package=scales")
        )
        
      )#end of tabItem
    )#end of dashboardBody
  )
)
  

# Define the server ----
server <- function(input, output, session) {
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "pages", "UPRes")
  })
  #Null hypothesis
  output$nullhypo = renderUI({
    
    h3("Ho: p = 0.595")
  })
  
  output$design = renderUI({
    
    h4("A researcher plans to take a random sample of size n students to do a 
       survey about their experiences in studying at the University Park campus
       of Penn State University. However, she worries that sample results could 
       be biased because the students who agree to participate might be different 
       from those who don't (this would be an example of non-response bias). The
       researcher makes a confidence interval for the proportion of Penn State
       Students who are Pennsylvania residents based on her study. This app shows 
       how confidence intervals of that type would come out when there is no bias.")
    
  })
  
  output$notice = renderText({
    paste0("The green lines in the population plot and sample plot represent 
           true population proportion.")
  })
  
  output$navy = renderText({
    paste0("The navy lines in the CI plot indicate a confidence interval covers 
           the population proportion.")
  })
  
  output$red = renderText({
    paste0("The red lines in the CI plot indicate that the population proportion
           is outside of the confidence interval.")
  })
  
  
  
  #population plot with true prop
  output$popMean  = renderPlot({
    my_vector=c(0.595, 1-0.595)
    names(my_vector)=c("PA Students","Non-PA Students")
    ggplot() + geom_bar(aes(x=names(my_vector), y=my_vector), stat='identity',
                        width=0.3, fill = "steelblue")+
      lims(y = c(0,1))+
      geom_hline(yintercept = 0.595, color = "forestgreen", size = 1.2)+
      labs(
        title = paste0("Population"),
        x = "Pennsylvania residency status",
        y = "Proportion Enrollment by Residency")+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      )
    
    
  })
  
  
  
  #Calculating alpha by the confidence level input
  alpha <- reactive({
    (1 - input$level) 
  })
  
  #Updating Sample Size
  N <- reactive({
    as.integer(input$nsamp)
  })
  
  
  #generate 50 new sample
  Data <- eventReactive(input$new,{
    data.frame(
      x = do.call(
        paste0("rbinom"),
        c(list(n = as.integer(input$nsamp) * 50), 
          list(1,0.595)))
    ) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
  })
  
  #calculate the interval
  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        Count = sum(x),
        sampleProp = binconf(Count, N(), alpha=alpha())[1],
        lowerbound = binconf(Count, N(), alpha=alpha())[2],
        upperbound = binconf(Count, N(), alpha=alpha())[3],
        cover = (lowerbound < 0.595) & (0.595 < upperbound)) %>%
      ungroup()
  })
  
  
  #default as all the samples are selected
  selected_sample <- 50
  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > 50) selected_sample <<- 50
    }
    selected_sample
  })
  
  OneSample <- reactive({
    Data() %>%
      filter( idx == selectedSample() )
  })
  
  OneSampleColor <- reactive({
    colors <- c("TRUE" = "navy", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })
  
  #print the CIplot
  output$CIplot <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size")
    )
    
    # validate(
    #   need(input$nsamp >=30,
    #        message = "Please input samle size larger than 30")
    # )
    
    ggplot(data = Intervals()) +
      geom_pointrange(
        aes(x=idx, ymin = lowerbound, ymax = upperbound, y = sampleProp, colour = cover,
            alpha = idx == selectedSample(),
            size = idx == selectedSample()
        )) +
      geom_hline(yintercept = 0.595, size = 2, colour = "forestgreen", alpha = 0.5) +
      coord_flip() +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .7), guide = FALSE) +
      scale_color_manual(values = c("TRUE" = "navy", "FALSE" = "red"), guide = FALSE) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
      lims(y = c(-0.01,1.01)) +
      labs(title = paste0(100 * input$level, "% Confidence Intervals for the proportion"),
           x = "50 samples are generated every time",y="vertical line shows null proportion") +
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.caption = element_text(size = 18),
            text = element_text(size = 18),
            axis.title = element_text(size = 16))
  })
  
  
  output$sampProp  = renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size")
    )
    my_vector=c(round(mean(OneSample()$x), 3), 1-round(mean(OneSample()$x), 3))
    names(my_vector)=c("PA Students","Non-PA Students")
    ggplot() + geom_bar(aes(x=names(my_vector), y=my_vector),width =0.3,stat = 'identity', fill = OneSampleColor())+
      lims(y = c(0,1))+
      geom_hline(yintercept = 0.595, color = "forestgreen", size = 1.2)+
      labs(
        title = paste0("Sample proportion for residency in UP = ",
                       round(mean(OneSample()$x), 2)),
        x = "Whether PA Resident",
        y = "Proportion Enrollment by Residency"
      )+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      )
    #barplot(my_vector,col=rgb(0.2,0.4,0.6,0.6),ylim=c(0,1), ylab="precentage")
    
  })
  
  ## Text messages ----
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size")
    )
    
    paste(sum(Intervals()$cover), "of these",
          nrow(Intervals()), "intervals cover the parameter value. And coverage
          rate is ", round(100 *  sum(Intervals()$cover)/ nrow(Intervals()), 2),
          "%.")
  })
  
  # This is "Finding the Z* Multiplier" Part ----
  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })
  
  observeEvent(c( input$A, input$B, input$n, input$level),
               { rate$cover <- sum(Intervals()$cover); rate$total <- nrow(Intervals()) }
  )
  
  ## Calculating alpha ----
  zalpha <- reactive({
    (1 - (input$zlevel/100)) / 2
  })
  
  zlowerbound <- reactive({
    round(qnorm(zalpha()), digits = 3)
  })
  
  zupperbound <- reactive({
    round(qnorm(zalpha(), lower.tail = FALSE), digit = 3)
  })
  
  output$zplot = renderPlot({
    
    ## draw the normal curve ----
    curve(
      dnorm(x, mean = 0, sd = 1),
      xlim = c(-3,3),
      xaxt = "n",
      main = "Normal Distribution Plot (Mean = 0, StDev = 1)",
      ylab = "Density",
      cex.lab = 1.5,
      cex.main = 1.5,
      cex.axis = 1.2)
    cord.x <- c(zlowerbound(), seq(zlowerbound(), zupperbound(), 0.01), zupperbound())
    cord.y <- c(0, dnorm(seq(zlowerbound(), zupperbound(), 0.01)), 0)
    
    polygon(cord.x, cord.y, col = boastUtils::psuPalette[6])
    xtick <- seq(-3, 3, by = 1)
    axis(side = 1, at = c(xtick, zlowerbound(), zupperbound()), labels = TRUE, cex.axis = 1.2)
  })
  
  output$feedback <- renderPrint({
    validate(
      need(
        !is.na(input$question1) & !is.na(input$question2) &
          !is.na(input$question3) & input$question4 != "select",
        message = 'Please answer all questions'),
      errorClass = "temp")
    if(
      (input$question1 == 1.645 | input$question1 == 1.65 |
       input$question1 == 1.64)
      &(input$question2 == 1.960)
      &(input$question3 == 2.576 | input$question3 == 2.58 |
        input$question3 == 2.6)
      &(input$question4 == 'y')){
      cat('All correct. Great Job!')
    }
    
    ## Render pic1
    if (input$question1 != ''){
      output$pic1 <- boastUtils::renderIcon(
        icon = ifelse(
          abs(input$question1 - 1.645) <= 0.005,
          ifelse(
            input$question1 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36 #Note this is larger than what you currently have
      )
    }
    
    ## Render pic2
    if (input$question2 != ''){
      output$pic2 <- boastUtils::renderIcon(
        icon = ifelse(
          abs(input$question2 - 1.960) <= 0.005,
          ifelse(
            input$question2 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36 #Note this is larger than what you currently have
      )}
    
    ## Render pic3
    if (input$question3 != ''){
      output$pic3 <- boastUtils::renderIcon(
        icon = ifelse(
          abs(input$question3 - 2.576) <= 0.005,
          ifelse(
            input$question3 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36 #Note this is larger than what you currently have
      )}
    
    ## Render pic4
    if (input$question4 != "select"){
      output$pic4 <- boastUtils::renderIcon(
        icon = ifelse(
          input$question4 == 'y',
          ifelse(
            input$question4 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36
      )}
  })
  
  ####################################################################
  #####################################################################
  
  # output$matrixScore <- renderTable({
  #   
  #  
  #   testtakers <- c(762247, 875342)
  #   meanScore <- c(524,494)
  #   sdScore <- c(126,116)
  #   groupScore <- c(testtakers, meanScore, sdScore)
  #   
  #   matrixfinal <- matrix(groupScore, nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(rownames = c("n", "μ", "σ"), c("Male","Female")))
  #   matrixfinal
  # })
  
  output$testdesign = renderUI({
    if(input$testdesigncheckbox)
    {
      paste("A researcher wants to sample a group of n University Park students 
            and n students from other Penn State campuses to ask them about 
            their experiences in college. Although the proportion of Pennsylvania
            residents is 0.249 lower at University Park, a critic believes her 
            sampling technique might change that difference. The researcher uses 
            her samples to create a confidence interval for the difference between 
            the University Park campus and the Commonwealth campuses for the 
            proportion who are Pennsylvania residents.")
    }
  })
  
  
  #Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - input$dlevel) / 2
  })
  
  #Updating Sample Size
  dN <- reactive({
    as.integer(input$nSamp)
  })
  
  
  standardError <- reactive({
    sqrt(0.595*0.405/dN() + 0.844*0.156/dN())
  })
  
  #population mean plot with true diffmean
  output$dpopMean  = renderPlot({
    # dat <- read.table(text = "University_Park Other_Campuses
    #                           Pennsylvania_Students 0.595 0.844
    #                           Out-of-State_Students 0.405	 0.156",
    #                   sep = "",header = TRUE)
    
    dfPop <- data.frame(types = rep(c("Pennsylvania Students", "Out-of-State Students"), each=2),
                        location=rep(c("University Park", "Other Campuses"),2),
                        samplepercent=c(0.595,0.844,0.405,0.156))
    
    g1<-ggplot2::ggplot(data = dfPop,aes(x = location,y = samplepercent, fill = types)) +
      
      ggplot2::geom_bar(position = position_fill(),stat="identity", width=0.3) +
      scale_fill_brewer(palette="Paired")+
      labs(
        title = paste0("Population Stacked Bar Graph"), 
        y = "Enrollment by Proportion",
        x = "Location")
    g1+ggplot2::theme(
      plot.caption = element_text(size = 18),
      text = element_text(size = 18)
    )
    
    
  })
  
  UPS <- reactive({
    input$newSample
    rbinom(n=dN(), 1, 0.595)
  })
  
  UWS <- reactive({
    input$newSample
    rbinom(n=dN(), 1, 0.844)
  })
  
  Diff <- reactive({
    mean(UPS()) - mean(UWS())
  })
  
  output$sampleDiff  = renderPlot({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    input$newSample
    
    dfSample <- data.frame(types = rep(c("Pennsylvania Students", "Out-of-State Students"), each=2),
                           location=rep(c("University Park", "Other Campuses"),2),
                           samplepercent=c(mean(UPS()),mean(UWS()),1-mean(UPS()),1-mean(UWS())),
                           ref=c(0.595,0.844), 2)
    
    
    g2<-ggplot2::ggplot(data=dfSample,aes(x = location,y = samplepercent, fill = types)) +
      geom_bar(position = position_fill(),stat="identity", width=0.3) +
      scale_fill_brewer(palette="Set2")+
      geom_errorbar(aes(ymin = ref, ymax = ref, col = "True proportion"), width = 0.3, colour = "#191919", size = 1) + 
      labs(
        title = paste0("Sample Stacked Bar Graph"),
        y ="Sample Enrollment by Proportion",
        x="Location")
    
    
    g2+ggplot2::theme(
      plot.caption = element_text(size = 18),
      text = element_text(size = 18)
    )
  })
  
  output$pop<- renderText({
    paste("Population proportion(diff) = -0.249, Population standard deviation 
          for the difference in proportions = ",round(sqrt(0.595*0.405 + 0.844*0.156),3))
  })
  
  
  dlowerbound <- reactive({
    Diff() + qnorm(dalpha()) * standardError()
  })
  dupperbound <- reactive({
    Diff() - qnorm(dalpha()) * standardError()
  })
  
  output$CItable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    if(input$CIcheckbox)
    {
      ctable = matrix(c(dlowerbound(), dupperbound()),nrow=1)
      colnames(ctable) = c("Lower bound","Upper bound")
      ctable
    }
  })
  
  pvalue <- reactive({
    2*(1-pnorm(abs(zstatistic())))
  })
  
  zstatistic <- reactive({
    Diff()/standardError()
    
  })
  
  output$popInfo = renderTable({
    
    ctable = matrix(c(percent(0.595), percent(0.844)), nrow=1)
    colnames(ctable) = c("University Park","Other Campuses")
    ctable
  })
  
  output$sampleinfotable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    ctable = matrix(c(percent(mean(UPS())), percent(mean(UWS()))), nrow=1)
    colnames(ctable) = c("University Park","Other Campuses")
    ctable
  })
  
  output$Diffinfo = renderUI({
    validate(
      need(is.numeric(input$nSamp),
           message = "")
    )
    paste("The difference between UP and other campuses sample (UP-other) is ", Diff(),
          ", Sample standard deviation for the difference in proportions = ",
          round(standardError(),3),", UP sample = ",dN(),", others sample = ",dN())
  })
  
  output$table = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    if(input$testcheckbox)
    {
      ctable = matrix(c(zstatistic(),pvalue()),nrow=1)
      colnames(ctable) = c("z-statistic","p-value")
      #rownames(ctable) = paste((input$dlevel),"% CI",sep="")
      ctable
    }
  })
  
  zstandard <- reactive({
    -qnorm(dalpha())
  })
  
  output$decisionZ = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    if(input$decisioncheckbox)
    {
      if(abs(zstatistic()) <= zstandard()){
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3)),
              " is less than \\(Z^*\\) score = ",round(zstandard(),3),
              ", the null hypothesis provides a reasonable explanation of the 
              data so we can NOT conclude that University Park campus has a different 
              proportion of Pennsylvania residents  when student's are chosen by
              the researcher's sampling procedure.")
        
      }else{
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3)),
              " is larger than \\(Z^*\\) score = ",round(zstandard(),3),
              ", the null hypothesis is not a reasonable explanation of the data 
              so we have evidence that there is a difference between the proportion 
              of Pennsylvania residents at the University Park campus and the 
              proportion at other campuses when students are chosen by the researcher's 
              sampling procedure.")
      }
    }
    
  })
  
  output$decisionP = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    if(input$decisioncheckbox)
    {
      if(pvalue() >= (2*dalpha())){
        paste("Since it is observed that p-value = ",round(pvalue(),3)," is larger than ",
              round(2*dalpha(),3),", the null hypothesis provides a reasonable explanation 
              of the data so we can NOT conclude that University Park campus has 
              a different proportion of Pennsylvania residents  when student's 
              are chosen by the researcher's sampling procedure.")
      }else{
        paste("Since it is observed that p-value = ",round(pvalue(),3)," is less than ",
              round(2*dalpha(),3),", the null hypothesis is not a reasonable 
              explanation of the data so we have evidence that there is a difference
              between the proportion of Pennsylvania residents at the University 
              Park campus and the proportion at other campuses when students are 
              chosen by the researcher's sampling procedure.")
      }
    }
  })
}


# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)