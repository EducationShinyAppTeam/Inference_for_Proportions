# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(dplyr)
library(ggplot2)

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
binconf <- function(x, n, alpha = 0.05,
                    method = c("wilson", "exact", "asymptotic", "all"),
                    include.x = FALSE, include.n = FALSE, return.df = FALSE) {
  method <- match.arg(method)
  bc <- function(x, n, alpha, method) {
    nu1 <- 2 * (n - x + 1)
    nu2 <- 2 * x
    ll <- if (x > 0) {
      x / (x + qf(1 - alpha / 2, nu1, nu2) * (n - x + 1))
    } else {
      0
    }
    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if (x < n) {
      qf(1 - alpha / 2, nu1p, nu2p)
    } else {
      1
    }
    ul <- ((x + 1) * pp) / (n - x + (x + 1) * pp)
    zcrit <- -qnorm(alpha / 2)
    z2 <- zcrit * zcrit
    p <- x / n
    cl <- (p + z2 / 2 / n + c(-1, 1) * zcrit *
      sqrt((p * (1 - p) + z2 / 4 / n) / n)) / (1 + z2 / n)
    if (x == 1) {
      cl[1] <- -log(1 - alpha) / n
    }
    if (x == (n - 1)) {
      cl[2] <- 1 + log(1 - alpha) / n
    }
    asymp.lcl <- x / n - qnorm(1 - alpha / 2) * sqrt(((x / n) *
      (1 - x / n)) / n)
    asymp.ucl <- x / n + qnorm(1 - alpha / 2) * sqrt(((x / n) *
      (1 - x / n)) / n)
    res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
    res <- cbind(rep(x / n, 3), res)
    switch(method, wilson = res[2, ], exact = res[1, ], asymptotic = res[3, ], all = res, res)
  }
  if ((length(x) != length(n)) & length(x) == 1) {
    x <- rep(x, length(n))
  }
  if ((length(x) != length(n)) & length(n) == 1) {
    n <- rep(n, length(x))
  }
  if ((length(x) > 1 | length(n) > 1) & method == "all") {
    method <- "wilson"
    warning("method=all will not work with vectors...setting method to wilson")
  }
  if (method == "all" & length(x) == 1 & length(n) == 1) {
    mat <- bc(x, n, alpha, method)
    dimnames(mat) <- list(
      c("Exact", "Wilson", "Asymptotic"),
      c("PointEst", "Lower", "Upper")
    )
    if (include.n) {
      mat <- cbind(N = n, mat)
    }
    if (include.x) {
      mat <- cbind(X = x, mat)
    }
    if (return.df) {
      mat <- as.data.frame(mat)
    }
    return(mat)
  }
  mat <- matrix(ncol = 3, nrow = length(x))
  for (i in 1:length(x)) {
    mat[i, ] <- bc(x[i], n[i],
      alpha = alpha,
      method = method
    )
  }
  dimnames(mat) <- list(rep("", dim(mat)[1]), c(
    "PointEst",
    "Lower", "Upper"
  ))
  if (include.n) {
    mat <- cbind(N = n, mat)
  }
  if (include.x) {
    mat <- cbind(X = x, mat)
  }
  if (return.df) {
    mat <- as.data.frame(mat, row.names = NULL)
  }
  mat
}
# End of Harrell's code----------------------------------------------------------

## Single Proportion Population Plot ----
upResData <- data.frame(
  status = c("PA Students", "Non-PA Students"),
  proportion = c(0.595, 1 - 0.595)
)

upResidency <- ggplot(
  data = upResData,
  mapping = aes(x = status, y = proportion)
) +
  geom_bar(
    stat = "identity",
    width = 0.3,
    fill = psuPalette[6]
  ) +
  geom_hline(
    yintercept = 0.595,
    color = boastPalette[3],
    size = 1.2
  ) +
  labs(
    title = "Residency Breakdown for University Park",
    x = "Residency status",
    y = "Proportion"
  ) +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = 0, add = c(0, 0.01))
  ) +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    axis.title = element_text(size = 16)
  )

## Difference of Population Plot ----
dfPop <- data.frame(
  residency = rep(c("Pennsylvania Students", "Out-of-State Students"), each = 2),
  location = rep(c("University Park", "Other Campuses"), times = 2),
  proportion = c(0.595, 0.844, 0.405, 0.156)
)

diffPopPlot <- ggplot(
  data = dfPop,
  mapping = aes(x = location, y = proportion, fill = residency)
) +
  geom_bar(
    position = "fill",
    stat = "identity",
    width = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "Pennsylvania Students" = boastPalette[1],
      "Out-of-State Students" = boastPalette[8]
    )
  ) +
  labs(
    title = paste0("Population Stacked Bar Graph"),
    y = "Enrollment proportion",
    x = "Location",
    fill = "Residency"
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    legend.position = "bottom"
  )

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    # Header ----
    dashboardHeader(
      title = "Conf. Int. for Proportions",
      titleWidth = 250,
      tags$li(
        class = "dropdown",
        tags$a(
          target = "_blank", icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Inference_for_Proportions"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://shinyapps.science.psu.edu/",
          icon("home")
        )
      )
    ),
    # Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "over", icon = icon("tachometer-alt")),
        menuItem("UP Residency Percentage", tabName = "UPRes", icon = icon("wpexplorer")),
        menuItem("Difference of Proportions", tabName = "popdiff", icon = icon("wpexplorer")),
        menuItem("Finding the \\(Z^*\\) Multiplier", tabName = "findz", icon = icon("wpexplorer")),
        menuItem("References", tabName = "Ref", icon = icon("leanpub"))
      ),
      # PSU Logo
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    # Body ----
    dashboardBody(
      tabItems(
        # Overview ----
        tabItem(
          tabName = "over",
          h1("Confidence Intervals for One or Two Proportions"),
          p("This app shows how the confidence level and sample size affect
           the outcome confidence interval for a single proportion under
           the null hypothesis of no bias. The app also explores the same
           issues for a confidence interval for the difference between two
           population proportions."),
          br(),
          h2("Instructions"),
          tags$ul(
            tags$li(strong("UP Residency Percentage and Difference of Proportions
                           pages")),
            tags$ul(
              tags$li("Move the sample size and level sliders to see how they
              affect confidence intervals for the proportion of Penn State
              students who are residents of Pennsylvania or two-sample tests for
              differences in the proportion of Pennsylvania residents between the
              University Park campus and the other campuses."),
              tags$li("Click on the generate buttons to draw new samples and for
                      the confidence interval app, click on the center of an
                      interval to show data for that sample."),
              tags$li(
                strong("Difference of Proportions page: "),
                "You can change the sample size and/or the confidence level
                      and explore the behavior of a Z-test for differences in
                      proportions for out-of-state and Pennsylvania students."
              )
            ),
            br(),
            tags$li(strong("Finding the \\(Z^*\\) Multipler page")),
            tags$ul(
              tags$li("The \\(Z^*\\) multiplier page allows the user to find
                      critical values (multiplier numbers) needed in making a
                      confidence interval. Complete a short quiz to show you have
                      mastered the concept.")
            )
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "explore",
              label = "Explore",
              icon = icon("bolt"),
              size = "large"
            )
          ),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and programmed by  Yingjie (Chelsea) Wang and
            updated by Zhuolin Luo in 2020.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/03/2020 by NJH.")
          )
        ),
        ## UP Residency-Single Proportion ----
        tabItem(
          tabName = "UPRes",
          h2("Confidence Intervals for Enrollment by Residency in 2016"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            p(
              "A researcher plans to take a random sample of size ", em("n"),
              " students to do a survey about their experiences in studying at
              the University Park campus of Penn State University. The researcher
              makes a confidence interval for the proportion of Penn State
              students who are Pennsylvania residents based on her study.",
              br(),
              br(),
              "According to Penn State's records, 59.5% of Penn State students
              at University Park are residents of Pennsylvania."
            )
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Hypothesis"),
                p("\\(H_0\\!:p = 0.595\\)", class = "largerFont"),
                sliderInput(
                  inputId = "level",
                  label = "Confidence level",
                  min = 85,
                  max = 99,
                  value = 90,
                  post = "%"
                ),
                sliderInput(
                  inputId = "nsamp",
                  label = "Sample size (n > 30)",
                  min = 30,
                  max = 500,
                  value = 30,
                  step = 1
                ),
                p("Click the following button to create 50 samples of your
                  selected sample size to see their confidence intervals and
                  histograms."),
                bsButton(
                  inputId = "new",
                  label = "Generate 50 Samples",
                  icon = icon("retweet"),
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              plotOutput("popMean", height = "400px"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('popMean').setAttribute('aria-labelledby',
                'singlePopPara')
                })"
              )),
              p(
                id = "singlePopPara",
                "The population chart shows break down of all University Park
                students; the green horizontal line shows the actual percentage
                of them who are Pennsylvania residents (59.5%)."
              )
            )
          ),
          uiOutput("sampleMessage"),
          fluidRow(
            column(
              width = 6,
              plotOutput("sampProp"),
              tags$script(HTML(
                "$(document).ready(function() {
             document.getElementById('sampProp').setAttribute('aria-label',
             `proportion bar graph for selected sample`)
             })"
              )),
              uiOutput("sampleColors")
            ),
            column(
              width = 6,
              plotOutput(
                outputId = "CIplot",
                click = "plot_click"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
               document.getElementById('CIplot').setAttribute('aria-label',
               `plot out the confidence intervals`)
                })"
              )),
              textOutput("CoverageRate")
            )
          )
        ),
        ## Difference in Proportions ----
        tabItem(
          tabName = "popdiff",
          h2("Tests for Differences in Proportions of Pennsylvania Student
             Enrollment"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            p("A researcher wants to sample a group of ", em("n"), " University
              Park students and ", em("n"), " students from other Penn State
              campuses to ask them about their experiences in college. Although
              the proportion of Pennsylvania residents is 0.249 lower at
              University Park, a critic believes her sampling technique might
              change that difference. The researcher uses her samples to create
              a confidence interval for the difference between the University
              Park campus and the Commonwealth campuses for the proportion who
              are Pennsylvania residents."),
            p(
              "Hypothesis: There is no statistically significant difference in
              the proportion of Pennsylvania student enrollment between Univeristy
              Park and the Commonwealth campuses."
            ),
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  h3("Population Information"),
                  p("Subtraction order: Other − UP"),
                  tableOutput("popInfo"),
                  textOutput("pop"),
                  br(),
                  h3("Sample Information"),
                  uiOutput("Diffinfo"),
                  tableOutput("sampleinfotable"),
                  br(),
                  sliderInput(
                    inputId = "dlevel",
                    label = "Confidence level",
                    min = 85,
                    max = 99,
                    value = 90,
                    step = 1,
                    post = "%"
                  ),
                  sliderInput(
                    inputId = "nSamp",
                    label = "Sample size for both groups",
                    min = 30,
                    max = 150,
                    value = 50,
                    step = 5
                  ),
                  br(),
                  bsButton(
                    inputId = "newSample",
                    label = "Generate New Samples",
                    icon = icon("retweet"),
                    size = "large"
                  )
                )
              ),
              column(
                width = 8,
                plotOutput("dpopMean"),
                tags$script(HTML(
                  "$(document).ready(function() {
                    document.getElementById('dpopMean').setAttribute('aria-label',
                    `stacked bar graph for population`)
                    })"
                )),
                br(),
                plotOutput("sampleDiff"),
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
                  trigger = "hover",
                  placement = "top"
                ),
                tableOutput("CItable"),
                checkboxInput(
                  inputId = "CIcheckbox",
                  label = "Show Confidence Interval",
                  value = FALSE
                )
              )
            )
          )
        ),
        # Find the Z* Mutltiplier ----
        tabItem(
          tabName = "findz",
          h2("Finding the \\(Z^*\\) Multiplier"),
          p("When constructing a confidence interval for a mean or a difference
            of two means, we often need to find a critical value to help us
            calculate the appropriate width. One of the most common tools to do
            this is the Standard Normal distribution which has a mean of zero and
            a standard devation of one. The values we get from this distribution
            are often denoted \\(Z^*\\) and function as a multipler. The value of
            the \\(Z^*\\) multiplier will depend on our selected level of
            confidence."),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "zlevel",
                  label = "Selected confidence level",
                  min = 85,
                  max = 99,
                  value = 90,
                  post = "%"
                )
              ),
              br(),
              p("Observe what happens in the plot when you adjust the confidence
                  level slider. When you are ready, check your skill by answering
                  the questions below."),
              p("You will have to answer all of the questions before your answers
                will be checked.")
            ),
            column(
              width = 8,
              plotOutput("zplot"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('zplot').setAttribute('aria-label',
                  `This is a Z-score plot, which shows the multipler value as
                  the boundaries of the shaded region. Use these values to
                  answer the following questions.`)
                 })"
              ))
            )
          ),
          h3("Check Your Skills"),
          uiOutput("feedback"),
          p("What is \\(Z^*\\)  Multiplier for 90% confidence level?"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = "question1",
                label = "Your answer",
                step = 0.001,
                value = ""
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput(outputId = "pic1")
            )
          ),
          p("What is \\(Z^*\\)  Multiplier for 95% confidence level?"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = "question2",
                label = "Your answer",
                step = 0.001,
                value = ""
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput("pic2")
            )
          ),
          p("What is \\(Z^*\\)  Multiplier for 99% confidence level?"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = "question3",
                label = "Your answer",
                step = 0.001,
                value = ""
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput("pic3")
            )
          ),
          p("True or False: Increasing the confidence level makes the confidence interval wider."),
          fluidRow(
            column(
              width = 3,
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
            ),
            column(
              width = 9,
              br(),
              uiOutput("pic4")
            )
          )
        ),
        # References ----
        tabItem(
          tabName = "Ref",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R package.
           Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Bates D. and Maechler M. (2019), Matrix: Sparse and Dense Matrix Classes and Maethods, R package.
           Available from https://cran.r-project.org/web/packages/Matrix/index.html"
          ),
          p(
            class = "hangingindent",
            "Budget Office in PSU. (2019), Enrollment by Residency Fall 2019. Available at
           https://factbook.psu.edu/factbook/StudentDynamic/PANonPASummary.aspx?YearCode=2019Enr&FBPlusIndc=N"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (20202), boastUtils: BOAST Utilities,
            R Package.
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard:
           Create dashboards with 'Shiny', R Package. Available from
           https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019),
           shiny: Web application framework for R, R Package.
           Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(class = "hangingindent",
            "Harrell, F.E. (2020), Confidence Intervals for Binomial Probability.
           We needed to bypass the loading of the foreign package for R 3.6.3,
           thus we are using the definition of the binconf which is all we needed
           from Hmisc. Available from https://CRAN.R-project.org/package=Hmisc"),
          p(
            class = "hangingindent",
            "Wickham, H., Francois R., Henry L., and Muller K. (2020), dplyr:
           A Grammar of Data Manipulation, R Package. Available from
           https://cran.r-project.org/web/packages/dplyr/index.html"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016), ggplot2: Elegant graphics for data analysis,
           R Package, New York: Springer-Verlag. Available from https://ggplot2.tidyverse.org"
          )
        )
      ) # end of tabItem
    ) # end of dashboardBody
  )
)


# Define the server ----
server <- function(input, output, session) {
  # Explore Button ----
  observeEvent(input$explore, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "UPRes"
    )
  })

  # Sample messages ----
  observeEvent(input$new, {
    output$sampleMessage <- renderUI({
      "Click on the confidence intervals (on the right) to view the histogram of
      the sample on the left."
    })

    output$sampleColors <- renderUI({
      "The sample histogram will change colors (red or blue) depending on whether
      the confidence interval constructed from the sample contains the population
      mean."
    })
  })

  # Population plot with UP Residency ----
  output$popMean <- renderPlot({
    upResidency
  })

  # Calculating alpha by the confidence level input ----
  alpha <- eventReactive(input$new, {
    (1 - input$level / 100)
  })

  # Updating Sample Size ----
  N <- eventReactive(input$new, {
    as.integer(input$nsamp)
  })


  # Generate 50 new samples ----
  Data <- eventReactive(input$new, {
    data.frame(
      x = do.call(
        paste0("rbinom"),
        c(
          list(n = as.integer(input$nsamp) * 50),
          list(1, 0.595)
        )
      )
    ) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
  })

  # Calculate the intervals ----
  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        .groups = "keep",
        Count = sum(x),
        sampleProp = binconf(Count, N(), alpha = alpha())[1],
        lowerbound = binconf(Count, N(), alpha = alpha())[2],
        upperbound = binconf(Count, N(), alpha = alpha())[3],
        cover = (lowerbound < 0.595) & (0.595 < upperbound)
      ) %>%
      ungroup()
  })


  # Default as all the samples are selected ----
  selected_sample <- 50
  selectedSample <- reactive({
    if (!is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > 50) selected_sample <<- 50
    }
    selected_sample
  })

  OneSample <- reactive({
    Data() %>%
      filter(idx == selectedSample())
  })

  OneSampleColor <- reactive({
    colors <- c("TRUE" = psuPalette[1], "FALSE" = psuPalette[2])
    covers <- (Intervals() %>% filter(idx == selectedSample()))$cover
    colors[as.character(covers)]
  })

  # Print the CIplot for Single Proportion ---
  output$CIplot <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
        message = "Please input sample size"
      )
    )

    ggplot(data = Intervals()) +
      geom_pointrange(
        mapping = aes(
          x = idx,
          ymin = lowerbound,
          ymax = upperbound,
          y = sampleProp,
          colour = cover,
          alpha = idx == selectedSample(),
          size = idx == selectedSample()
        )
      ) +
      geom_hline(
        mapping = aes(yintercept = 0.595, color = "zpopValue"),
        size = 1.25,
        alpha = 1
      ) +
      coord_flip() +
      scale_size_manual(
        values = c("TRUE" = 1.5, "FALSE" = .7),
        guide = FALSE
      ) +
      scale_color_manual(
        name = NULL,
        labels = c(
          "TRUE" = "Captures",
          "FALSE" = "Fails",
          "zpopValue" = "Pop. Proportion"
        ),
        values = c(
          "TRUE" = psuPalette[1],
          "FALSE" = psuPalette[2],
          "zpopValue" = boastPalette[3]
        )
      ) +
      scale_alpha_manual(
        values = c("TRUE" = 1, "FALSE" = .5),
        guide = FALSE
      ) +
      labs(
        title = paste0(input$level, "% Confidence Intervals for the Proportion"),
        x = NULL,
        y = "PA residency proportion"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      )
  })

  # Sample Plot for One Proportion ----
  output$sampProp <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
        message = "Please input sample size"
      )
    )
    sampData <- data.frame(
      residency = c("PA Students", "Non-PA Students"),
      proportions = round(c(mean(OneSample()$x), 1 - mean(OneSample()$x)), digits = 3)
    )
    ggplot(
      data = sampData,
      mapping = aes(x = residency, y = proportions)
    ) +
      geom_bar(
        width = 0.3,
        stat = "identity",
        fill = OneSampleColor()
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        expand = expansion(mult = 0, add = c(0, 0.01))
      ) +
      geom_hline(
        mapping = aes(yintercept = 0.595, color = "popValue"),
        size = 1.2
      ) +
      scale_color_manual(
        name = NULL,
        labels = c("popValue" = "Population Proportion"),
        values = c("popValue" = boastPalette[3])
      ) +
      labs(
        title = paste0(
          "Sample proportion for UP = ",
          round(mean(OneSample()$x), 2)
        ),
        x = "Residency status",
        y = "Proportion"
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
  })

  ## Text messages ----
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
        message = "Please input sample size"
      )
    )

    paste0(
      sum(Intervals()$cover),
      " of these ",
      nrow(Intervals()),
      " intervals cover the parameter value. The coverage rate is ",
      round(100 * sum(Intervals()$cover) / nrow(Intervals()), 2),
      "%."
    )
  })

  # This is "Finding the Z* Multiplier" Part ----
  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })

  observeEvent(c(input$A, input$B, input$n, input$level), {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })

  ## Calculating alpha ----
  zalpha <- reactive({
    (1 - (input$zlevel / 100)) / 2
  })

  zlowerbound <- reactive({
    round(qnorm(zalpha()), digits = 3)
  })

  zupperbound <- reactive({
    round(qnorm(zalpha(), lower.tail = FALSE), digits = 3)
  })

  output$zplot <- renderPlot({
    shadedPortion <- function(fun, alpha) {
      function(x) {
        y <- fun(x)
        y[x <= qnorm(alpha, lower.tail = TRUE) |
          x >= qnorm(alpha, lower.tail = FALSE)] <- NA
        return(y)
      }
    }

    ggplot(
      data = data.frame(x = c(-3, 3)),
      mapping = aes(x = x)
    ) +
      stat_function(fun = dnorm) +
      stat_function(
        fun = shadedPortion(dnorm, zalpha()),
        geom = "area",
        fill = boastUtils::psuPalette[6],
        alpha = 1
      ) +
      geom_segment(
        mapping = aes(
          x = zlowerbound(), y = 0,
          xend = zlowerbound(), yend = dnorm(zlowerbound())
        ),
        color = "black",
        size = 2,
        lineend = "round"
      ) +
      geom_segment(
        mapping = aes(
          x = zupperbound(), y = 0,
          xend = zupperbound(), yend = dnorm(zupperbound())
        ),
        color = "black",
        size = 2,
        lineend = "round"
      ) +
      geom_text(
        mapping = aes(x = zlowerbound(), y = dnorm(zlowerbound())),
        nudge_x = -0.1,
        nudge_y = 0.025,
        label = zlowerbound(),
        size = 8
      ) +
      geom_text(
        mapping = aes(x = zupperbound(), y = dnorm(zupperbound())),
        nudge_x = 0.1,
        nudge_y = 0.025,
        label = zupperbound(),
        size = 8
      ) +
      theme_bw() +
      xlab("Z Score") +
      ylab("Density") +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      ) +
      scale_x_continuous(breaks = seq.int(from = -3, to = 3, by = 1)) +
      scale_y_continuous(expand = expansion(mult = 0, add = c(0, 0.01)))
  })

  output$feedback <- renderUI({
    validate(
      need(
        !is.na(input$question1) & !is.na(input$question2) &
          !is.na(input$question3) & input$question4 != "select",
        message = "Please answer all questions"
      ),
      errorClass = "leftParagraphError"
    )

    ## Render pic1
    if (input$question1 != "") {
      success <- abs(abs(input$question1) - 1.645) <= 0.005

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "What is Z* Multiplier for 90% confidence level?",
        interactionType = "numeric",
        response = input$question1,
        success = success
      )

      boastUtils::storeStatement(session, stmt)

      output$pic1 <- boastUtils::renderIcon(
        icon = ifelse(
          success,
          ifelse(
            input$question1 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36
      )
    }

    ## Render pic2
    if (input$question2 != "") {
      success <- abs(abs(input$question2) - 1.960) <= 0.005

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "What is Z* Multiplier for 95% confidence level?",
        interactionType = "numeric",
        response = input$question2,
        success = success
      )

      boastUtils::storeStatement(session, stmt)

      output$pic2 <- boastUtils::renderIcon(
        icon = ifelse(
          success,
          ifelse(
            input$question2 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36 # Note this is larger than what you currently have
      )
    }

    ## Render pic3
    if (input$question3 != "") {
      success <- abs(abs(input$question3) - 2.576) <= 0.005

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "What is Z* Multiplier for 99% confidence level?",
        interactionType = "numeric",
        response = input$question3,
        success = success
      )

      boastUtils::storeStatement(session, stmt)

      output$pic3 <- boastUtils::renderIcon(
        icon = ifelse(
          success,
          ifelse(
            input$question3 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36
      )
    }

    ## Render pic4
    if (input$question4 != "select") {
      success <- input$question4 == "y"

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "True or False: Increasing the confidence level makes
        the confidence interval wider.",
        interactionType = "choice",
        response = input$question4,
        success = success
      )

      boastUtils::storeStatement(session, stmt)

      output$pic4 <- boastUtils::renderIcon(
        icon = ifelse(
          success,
          "correct",
          "incorrect"
        ),
        width = 36
      )
    }

    # renderUI expects something to be returned
    return()
  })

  ## Difference of Two Proportions? ----

  # Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - input$dlevel / 100) / 2
  })

  # Updating Sample Size
  dN <- reactive({
    as.integer(input$nSamp)
  })

  standardError <- reactive({
    sqrt(0.595 * 0.405 / dN() + 0.844 * 0.156 / dN())
  })

  # population mean plot with true diffmean
  output$dpopMean <- renderPlot({
    diffPopPlot
  })

  UPS <- reactive({
    input$newSample
    rbinom(n = dN(), 1, 0.595)
  })

  UWS <- reactive({
    input$newSample
    rbinom(n = dN(), 1, 0.844)
  })

  Diff <- reactive({
    mean(UPS()) - mean(UWS())
  })

  output$sampleDiff <- renderPlot({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input samle size"
      )
    )
    input$newSample

    dfSample <- data.frame(
      residency = rep(c("Pennsylvania Students", "Out-of-State Students"), each = 2),
      location = rep(c("University Park", "Other Campuses"), times = 2),
      proportion = c(mean(UPS()), mean(UWS()), 1 - mean(UPS()), 1 - mean(UWS())),
      ref = c(0.595, 0.844), # TODO: What is the purpose of this line?
      2 # TODO: What is the purpose of this line?
    )

    ggplot(
      data = dfSample,
      mapping = aes(x = location, y = proportion, fill = residency)
    ) +
      geom_bar(
        position = "fill",
        stat = "identity",
        width = 0.3
      ) +
      scale_fill_manual(
        values = c(
          "Pennsylvania Students" = psuPalette[2],
          "Out-of-State Students" = psuPalette[3]
        )
      ) +
      geom_errorbar(
        mapping = aes(ymin = ref, ymax = ref, col = "trueProp"),
        width = 0.3,
        size = 1
      ) +
      scale_color_manual(
        name = NULL,
        labels = c("trueProp" = "True Proportions"),
        values = c("trueProp" = "black")
      ) +
      labs(
        title = "Sample Stacked Bar Graph",
        y = "Enrollment proportion",
        x = "Location",
        fill = "Residency"
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        legend.position = "bottom"
      )
  })

  output$pop <- renderText({
    paste(
      "Population proportion(diff) = -0.249, Population standard deviation
          for the difference in proportions = ",
      round(sqrt(0.595 * 0.405 + 0.844 * 0.156), 3)
    )
  })

  dlowerbound <- reactive({
    Diff() + qnorm(dalpha()) * standardError()
  })
  dupperbound <- reactive({
    Diff() - qnorm(dalpha()) * standardError()
  })

  output$CItable <- renderTable({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )
    if (input$CIcheckbox) {
      ctable <- matrix(c(dlowerbound(), dupperbound()), nrow = 1)
      colnames(ctable) <- c("Lower bound", "Upper bound")
      ctable
    }
  })

  pvalue <- reactive({
    2 * (1 - pnorm(abs(zstatistic())))
  })

  zstatistic <- reactive({
    Diff() / standardError()
  })

  output$popInfo <- renderTable({
    ctable <- matrix(c(percent(0.595), percent(0.844)), nrow = 1)
    colnames(ctable) <- c("University Park", "Other Campuses")
    ctable
  })

  output$sampleinfotable <- renderTable({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )
    ctable <- matrix(c(percent(mean(UPS())), percent(mean(UWS()))), nrow = 1)
    colnames(ctable) <- c("University Park", "Other Campuses")
    ctable
  })

  output$Diffinfo <- renderUI({
    validate(
      need(is.numeric(input$nSamp),
        message = ""
      )
    )
    paste(
      "The difference between UP and other campuses sample (UP-other) is ",
      Diff(),
      ", Sample standard deviation for the difference in proportions = ",
      round(standardError(), 3), ", UP sample = ", dN(), ", others sample = ",
      dN()
    )
  })

  output$table <- renderTable({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )
    if (input$testcheckbox) {
      ctable <- matrix(c(zstatistic(), pvalue()), nrow = 1)
      colnames(ctable) <- c("z-statistic", "p-value")
      ctable
    }
  })

  zstandard <- reactive({
    -qnorm(dalpha())
  })

  ## Where is the decision stuff? Do we still need it?
  output$decisionZ <- renderText({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )
    if (input$decisioncheckbox) {
      if (abs(zstatistic()) <= zstandard()) {
        paste(
          "Since it is observed that |z| = ", abs(round(zstatistic(), 3)),
          " is less than \\(Z^*\\) score = ", round(zstandard(), 3),
          ", the null hypothesis provides a reasonable explanation of the
              data so we can NOT conclude that University Park campus has a different
              proportion of Pennsylvania residents  when student's are chosen by
              the researcher's sampling procedure."
        )
      } else {
        paste(
          "Since it is observed that |z| = ", abs(round(zstatistic(), 3)),
          " is larger than \\(Z^*\\) score = ", round(zstandard(), 3),
          ", the null hypothesis is not a reasonable explanation of the data
              so we have evidence that there is a difference between the proportion
              of Pennsylvania residents at the University Park campus and the
              proportion at other campuses when students are chosen by the researcher's
              sampling procedure."
        )
      }
    }
  })

  ## Where is the decision check box?
  output$decisionP <- renderText({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )
    if (input$decisioncheckbox) {
      if (pvalue() >= (2 * dalpha())) {
        paste(
          "Since it is observed that p-value = ", round(pvalue(), 3), " is larger than ",
          round(2 * dalpha(), 3), ", the null hypothesis provides a reasonable explanation
              of the data so we can NOT conclude that University Park campus has
              a different proportion of Pennsylvania residents  when student's
              are chosen by the researcher's sampling procedure."
        )
      } else {
        paste(
          "Since it is observed that p-value = ", round(pvalue(), 3), " is less than ",
          round(2 * dalpha(), 3), ", the null hypothesis is not a reasonable
              explanation of the data so we have evidence that there is a difference
              between the proportion of Pennsylvania residents at the University
              Park campus and the proportion at other campuses when students are
              chosen by the researcher's sampling procedure."
        )
      }
    }
  })
}


# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
