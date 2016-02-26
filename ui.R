###############################
### DDP Assignment - ui.R ###
###############################

library(shiny)
library(DT)

# UI definition

shinyUI(pageWithSidebar( 
    
    headerPanel("Process Analysis"), 
    
    sidebarPanel(
        
        radioButtons(inputId = 'Transform',
                     label = "Data transformation",
                     choices = list("No transformation" = "None",
                                    "Log transformation" = "Log"),
                     selected = "None"
        ),
        
        # Initialize list of processes from the data
        uiOutput("procList"),
        
        helpText("You can remove and re-add processes to be plotted."),
        
        selectInput(
            'ggOutl', 'Select outlier removal passes', 
            choices = c("No removal", "Remove 1st pass", "Remove 2nd pass", "Remove 3rd pass"),
            selectize = FALSE
        ),
        
        helpText("You can select up to three passes to remove outliers form the data."),
        

        # Initialize year range slider from the data        
        uiOutput("yearRange"),
                
        conditionalPanel(
            condition = "input.TabsPanel == 'Distributions'",      

            hr(),
            
            checkboxGroupInput(inputId = "chartType",
                               label = "Select chart type to show",
                               choices = list("Violin Plot" = "VPlot",
                                              "Box Plot" = "BPlot",
                                              "Jitter Plot" = "JPlot"),
                               selected = c("VPlot", "BPlot")
            ),

            h5(strong("Additional plot options")),
            
            conditionalPanel(
                condition = "input.chartType.indexOf('BPlot') != -1",
                checkboxInput(inputId = "labelOutl",
                              label = "Label outliers?")
            ),
            
            checkboxInput(inputId = "mean",
                          label = "Show distribution means?")
            
            # uiOutput("yZoom")
            # 
        ),
        
        conditionalPanel(
            condition = "input.TabsPanel == 'Time'",      
            
            hr(),
            
            checkboxInput(inputId = "smoother",
                          label = "Add smoother?",
                          value = FALSE),

            conditionalPanel(
                condition = "input.smoother == true",
                selectInput("linearModel", "Smoother type",
                            list("lm", "loess"))
            )
            
        )
        
    ),
    
    mainPanel(
        tabsetPanel(id ="TabsPanel",
                    tabPanel("Distributions",
                             br(),
                             plotOutput("ggDistrib", height = 500),
                             downloadButton("downloadDistribution","Download Graph"),
                             br(),
                             hr(),
                             h3("Data Diagnostics and Inference"),
                             br(),
                             h4("Statistic summary"),
                             verbatimTextOutput("summary"),
                             br(),
                             h4("Shapiro-Wilk test of distribution normality"),
                             DT::dataTableOutput("shapiro"),
                             br(),
                             h4("t-test of means"),
                             helpText("The t-test of the means helps in determining if the two compared distributions are statistically identical or not.
                                      For lognormal distributions, comparing their means is equivalent to comparing
                                      the medians of the original distributions. Therefore, if the distribution after the log
                                      transformation is not normal, the eqivalence of the mean with the median of pre-transformed
                                      distribution might be only approximate or not hold at all. Caution is advised in such cases."),
                             helpText("Big circle in the plot below indicates statistically signifficant difference between the compared means."),
                             br(),
                             plotOutput("ttest"),
                             br(),
                             value = "Distributions"),
                    tabPanel("Trends", br(),
                             plotOutput("trendGraph", height = 500),
                             downloadButton("downloadTrends","Download Graph"),
                             hr(),
                             h3("Linear models fit to time series"),
                             br(),
                             helpText("The information provided below refers to the linear trends (models) fit to the data. In general one should be very
                                      cautious when fitting the linear models to time series due to possible autocorrelation. In this case however the events
                                      (process iterations) are independen from each other. Nevertheless, the usual diagnostics of the model need to be performed
                                      before drawing conclusions from the model parameters."),
                             helpText("In general we seek here any indication of possible time-dependent factors that increase or decrease process performance,
                                      but not those factors themselves."),
                             br(),
                             h4("Trend line slope and confidence intervals"),
                             helpText("Select a process from the table to show the model diagnostic plots below."),
                             DT::dataTableOutput('lmodel'),
                             br(),
                             # verbatimTextOutput("ProcID"),
                             plotOutput("lmDiag", height = 500),
                             br(),
                             value = "Time"),
                    tabPanel("Description",
                             h2("Process Data Analysis"),
                             h3("Purpose of the application"),
                             p("This appplication helps to explore and analyze a performance metric of a certain class of processes. Each
                               instance (run) of a process results in a certain value of this process performace metric. The lower the value
                               of the metric, the better. The analysis looks at the data from two perspectives: distribution and time domain."),
                             p("Distribution perspective provides information helpful in comparing processes with each other by providing inferential
                               information."),
                             p("Time domain perspective helps in assessing the evolution of the processes performance over time."),
                             p("Both perspectives provide insights into the processes through their statistics as the population of instances and
                               through linear models with time as an independent variable."),
                             h3("Scenario"),
                             p("The prospective user of this application is responsible for improving performance of a certain class of processes, which are
                               comparable between each other, both functionally and from performance point of view. That user needs to demonstrate constantly to
                               the stakeholders how the processes compare, quantify this comparision, and how their performance develop over time. Additionally
                               this user is reponsible for proposing and implementing process improvements and to demonstrate and quantify those improvements."),
                             p("For this purpose the user extracts the performance metrics directly from the process-running infrastructure and performs exploratory
                               and inferential analysis along with some linear modelling in the time domain."),
                             h3("Usage"),
                             h4("Sidebar panel"),
                             p("The sidebar panel consists of a permanent part for data manipulation and tab-specific part."),
                             p("The manipulation consists of:"),
                             tags$div(
                                 tags$ul(
                                     tags$li("Data transformation: no transformation or log transformation"),
                                     tags$li("Process selection"),
                                     tags$li("Outliers removal"),
                                     tags$li("Time range selection (years)")
                                     
                                 )
                             ),
                             p("The 'Distributions' tab specific part consists of:"),
                             tags$div(
                                 tags$ul(
                                     tags$li("Chart type selection (Violin, Box and/or Jitter Plot). At least one chart type must be selected (default: Violin Plot)"),
                                     tags$li("Additional plot options controlling whether:"),
                                     tags$ul(
                                        tags$li("outliers are labelled on the chart or not"),
                                        tags$li("distribution mean value is marked on the chart or not")
                                     )
                                 )
                             ),
                             p("Processes can be removed by clicking in the input field on the process to remove and hitting Delete or Backspace key
                               on the keyboard. If not all processes are selected, then clicking in the input field opens a drop-down menu allowing for
                               additional process selection"),
                             p("Outlier removal is an iterative process allowing for 3 iterations. First, the outliers are identified by means of enabling Box Plot.
                               Outliers, if they exist, are marked by red dots. They can be labelled on the chart for posterior analysis. Identified outliers are 
                               marked in the input data so that they are not considered in analysis after selecting desired number of removal passes. The criterion
                               if and how many removal iterations to apply is left to the user."),
                             h4("'Distributions' tab"),
                             p("'Distribution' tab consists of three main parts:"),
                             tags$div(
                                 tags$ul(
                                     tags$li("Plot consisting of various charts, which allow to characterize the process metric samples distribution and their basic statistics"),
                                     tags$li("Summary statistics per process"),
                                     tags$li("Shapiro-Wilk test of distributions normality"),
                                     tags$li("Graphical representation of the significance of the pair-wise t-test of distributions means.")
                                 )
                             ),
                             p("The data to be analyzed and chart options are controlled from the sidebar panel. The plot can be exported to a png file."),
                             p("The Shapiro-Wilk test of normality assists at inference about the untransformed data median comparision. If the distribution after log transformation
                               results in the normal distribution then inference about logged distributions means differences is applicable to original distributions medians differences."),
                             p("Graphical representation of the significance of the pair-wise t-test of distributions means assists in identifying the pairs of distributions with
                               statistically significant differences in the means. Such a pair of distribution is marked with a dark green solid circle."),
                             h4("'Trends' tab"),
                             p("Similar to the 'Distributions' tab, the 'Trends' tab consists also of three parts:"),
                             tags$div(
                                 tags$ul(
                                     tags$li("Scatterplot of the performance metric value for each process over time"),
                                     tags$li("Table with linear model (trend line) parameters for each displayed process"),
                                     tags$li("Linear model diagnostic plots")
                                 )
                             ),
                             p("The scatterplot is controlled by the settings in the sidebar panel. Specifically for this tab the smooter can be plotted with available 
                               lm and loess methods. The plot can be exported to a png file."),
                             p("The model parameters' table provides the following information, per process:"),
                             tags$div(
                                 tags$ul(
                                     tags$li("Slope coefficient"),
                                     tags$li("Lower and upper Confidence Interval limits"),
                                     tags$li("P-value for slope coefficient"),
                                     tags$li("Interpretation of the p-value")
                                 )
                             ),
                             p("Selecting a row in the table displays the diagnostic plot for the linear model associated
                               with the given process. Those diagnostic plots help to assess the validity of the conclusions
                               that can be drawn from the linear model parameters about the process."),
                             h4("Known issues"),
                             p("To identify outliers the boxplot function is used. On some ocasions this function does not
                               identify as an outlier a data point which is displayed as an outlier by ggplot. Hence ocasionally
                               an outlying point visible in the chart is not labelled and removed, if Outliers removal option is
                               selected. Please note however that on other dataset some outliers can be correctly identified
                               but not removed due to designed removal limited to 3 iterations."),
                             br(),
                             value = "descr")
        )
    )
))