#====================================== USER INTERFACE ===========================================================# 
# PROJECT:      Exercise and Cognition project
# PROGRAMMER:   Alan Ho, MMStat
#=================================================================================================================#

# LIBRARIES --------------------------
library("shinydashboard")
library("shinyBS")
library("tidyverse")
library("broom")
library("DT")
library("nlme")
library("ggplot2")
library("plotly")
library("purrr")
library("magrittr")
library("DBI")
library("RSQLite")
library("tools")
library("lsmeans")
library("geepack")
library("dbplyr")
library("dplyr")
# install.packages("tidyverse",
#                  repos = c("http://rstudio.org/_packages",
#                            "http://cran.rstudio.com"))

# DATABASE -----------------------------------------------------
# Specify the current raw data to be used 

# setup database
# DBI::dbDisconnect(exercise_database)
##Load filepaths from config file
configfile <- file.path(path.expand('~'),'.Rconfig.csv')
try(config <-read.csv(configfile, sep=',', quote='\"', header=T))

setwd(as.character(config$DATADIR)) # sets the working directory
exercise_database <- DBI::dbConnect(RSQLite::SQLite(),as.character(config$DB)) # connects to database

# to upload data to database
lapply(dir()[file_ext(dir()) == "csv"], function(file) {
  
  dbWriteTable(exercise_database,
               name = file_path_sans_ext(file),
               value = read.csv(file),
               row.names = F,
               overwrite = T)
})


# variable names file
varnames <-  read.csv(as.character(config$VARSFILE),
                   header = T, 
                   stringsAsFactors = F)

DBI::dbListTables(exercise_database)


# subject names for inputs
subjects <- tbl(exercise_database, 
                sql(
                  "SELECT s.Subject, d.interval, d.xnat_subjectdata_sub_group
                  FROM opex_subjects as s
                  LEFT JOIN opex_cantabDMS as d
                  ON s.Subject = d.xnat_subjectdata_subject_label 
                  GROUP BY Subject
                  ")) %>% 
  collect() %>% 
  filter(!is.na(Subject))


# STUFF ----------------------------------------------------------------------
Logged = FALSE

# USER INTERFACE -------------------------------------------------------------------------------------------------
  # header ---------------
  header <-  dashboardHeader(title = "Human Exercise Trial: Analytics"
                             
  )
  
  
  # sidebar --------------  
  sidebar <- dashboardSidebar(
    
    sidebarMenu(id = "sidebarmenu",
                
                menuItem("Univariate", tabName = "uni", icon = icon("bolt")),
                # menuItem("Multivariate", tabName = "multi", icon = icon("bolt")),
                menuItem("Longitudinal", tabName = "long", icon = icon("bolt"))
    )
    
  )

  # body ------------------        
  body <-  dashboardBody(
    
    verbatimTextOutput("dataInfo"),
    
    tabItems(
 
    # UNIVARIATE ------------------------------------------------------------------    
    tabItem(tabName = "uni",   
            
            tabBox(title = "Linear Regression",
                   side = "left", height = "700px",
                   selected = "Regression",
                   tabPanel("Regression",
                            column(1),
                            column(4,  plotOutput("uniplot1",
                                                  height = "330px",
                                                  width = "450px",
                                                  click = T)),
                            column(2)
                            
                   ),
                   
                   tabPanel("Data",
                            
                            DT::dataTableOutput("mytable")
                            
                            
                   ),  
                   
                   tabPanel("Diagnostics",
                            
                            tableOutput('results')
                            
                            
                            # plotlyOutput("plotres",
                            #              height = "330px",
                            #              width = "450px")
                            
                            
                   )         
            ),
            
            
            box(title = "", height = "700px",
                
                column(6, selectizeInput("data1", label = "Dataset 1",
                                         choices = DBI::dbListTables(exercise_database), 
                                         multiple = F,
                                         options = list(maxItems = 2, placeholder = 'Select Dataset'),
                                         selected = NULL)
                ),
                
                column(6, selectizeInput("xvar", label = "Predictor",
                                         choices = NULL, 
                                         multiple = T,
                                         options = list(maxItems = 10, placeholder = 'Select Predictor'),
                                         selected = "MMSE")
                ),
                
                column(6, selectizeInput("data2", label = "Dataset 2",
                                         choices = DBI::dbListTables(exercise_database),
                                         multiple = T,
                                         options = list(maxItems = 2, placeholder = 'Select Dataset'),
                                         selected = "opex_health")
                ),
                
                column(6, selectizeInput("yvar", label = "Response",
                                         choices = NULL, 
                                         multiple = T,
                                         options = list(maxItems = 10, placeholder = 'Select Response'),
                                         selected = 'r_bps')),
                
                column(6, selectizeInput("selInterval", label = "Interval",
                                         choices = c(0:6), 
                                         multiple = T,
                                         options = list(maxItems = 1, placeholder = 'Select Interval'),
                                         selected = 0)),
                
                column(6, selectizeInput("split", label = "Group by",
                                         choices = c("Exercise Group", "Age", "Gender"), 
                                         multiple = T,
                                         options = list(maxItems = 1, placeholder = 'Split by'),
                                         selected = NULL)),
                
                
                column(6, actionButton("go", "Go"))
                
            )
            
            
    ),
    
    
    # LONGITUDINAL ---------------------------------------------------------------------------------------------      
    tabItem(tabName = "long", 
            
            tabBox(title = "Trajectory Plots",
                   side = "left", height = "800px",
                   selected = "Group",
                   tabPanel("Group", 
                            
                            # the plot itself
                            plotlyOutput("plot1"),
                            
                            
                            column(6, selectizeInput("data3", label = "Dataset",
                                                     choices = DBI::dbListTables(exercise_database), 
                                                     multiple = F,
                                                     options = list(maxItems = 1, placeholder = 'Select Dataset'),
                                                     selected = "opex_cantabDMS")
                            ),
                            
                            
                            column(6, selectizeInput("var", label = "Select Variable",
                                                     choices = NULL,
                                                     multiple = F,
                                                     options = list(maxItems = 1, placeholder = 'Select Variable'),
                                                     selected = varnames[2])
                            ),
                            
                            
                            
                            column(6, selectizeInput("name", label = "Select Subject",
                                                     choices = list(
                                                       ALL = list("ALL"),
                                                       AIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "AIT"))$Subject),
                                                       MIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "MIT"))$Subject),
                                                       LIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "LIT"))$Subject),
                                                       withdrawn = c((subjects %>% filter(xnat_subjectdata_sub_group == "withdrawn"))$Subject)
                                                     ),
                                                     multiple = T,
                                                     options = list(maxItems = 30, placeholder = 'Select Subject'),
                                                     selected = NULL)
                            ), 
                            
                            column(6, radioButtons(inputId = "plottype", 
                                                   label = "Choose plot type",
                                                   choices = list("line", "box"),
                                                   selected = "line",
                                                   inline = T)
                            ),
                            
                            
                            column(6, radioButtons("adjust", "Adjust for Baseline",
                                                   choices = c('No' = "none", 
                                                               "Yes" = "base", 
                                                               "Delta Change" = "delta"),
                                                   selected = "none", 
                                                   inline = T)
                            )
                            
                            
                            
                            
                   ),
                   
                   tabPanel("Data",
                            
                            dataTableOutput("dt_long")
                            
                            
                   )
                   
                   
            ),
            
            
            tabBox(title = "Diagnostic Plots",
                   side = "left", height = "800px",
                   selected = "Models",
                   tabPanel("Models", tableOutput("model")),
                   tabPanel("Multiple Comparisons", dataTableOutput("contrasts"))
                   
            )
            
            
    )
    
    
  )    
)



# ui ------
ui <- dashboardPage(header,sidebar,body)

