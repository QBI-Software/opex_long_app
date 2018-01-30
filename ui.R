#====================================== USER INTERFACE ===========================================================# 
# PROJECT:      Exercise and Cognition project
# PROGRAMMER:   Alan Ho, MMStat
#=================================================================================================================#
source('init.R', local=FALSE)

configfile <- file.path(path.expand('~'),'.Rconfig.csv')
try(config <-read.csv(configfile, sep=',', quote='\"', header=T))


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
    tabItems(
      
    # UNIVARIATE ------------------------------------------------------------------    
    tabItem(tabName = "uni",   
            
            tabBox(title = "Linear Regression",
                   side = "left", height = "700px",
                   selected = "Regression",
                   tabPanel("Regression",
                  
            fluidRow(
              
              column(6, plotlyOutput("uniplot1", width = 330)),
              column(2, tableOutput('results'))
              
            ),
            
            fluidRow(
              
              column(6, selectizeInput("xvar", label = "Predictor",
                                       choices = varnames$variable, 
                                       multiple = T,
                                       options = list(maxItems = 10, placeholder = 'Select Predictor'),
                                       selected = "bmi_dexa")
              ),
              
              column(6, selectizeInput("yvar", label = "Response",
                                       choices = varnames$variable, 
                                       multiple = T,
                                       options = list(maxItems = 10, placeholder = 'Select Response'),
                                       selected = 'paltea8')),
              
              column(6, selectizeInput("selInterval", label = "Interval",
                                       choices = c("0", "6", "delta", "pc"), 
                                       multiple = T,
                                       options = list(maxItems = 1, placeholder = 'Select Interval'),
                                       selected = 0))
            )
                   

                            
                   ),
                   
                   tabPanel("Data",
                            
                            DT::dataTableOutput("mytable")
                            
                            
                   ),  
                   
                   tabPanel("Diagnostics",
                            
                            "hello"
                            # plotlyOutput("plotres",
                            #              height = "330px",
                            #              width = "450px")
                            
                            
                   )         
            ),
            
            
tabBox(title = "", height = "700px", side = "left",
                   
tabPanel("Delta Change",
  
  fluidRow(
    
    column(6, plotlyOutput("uniplot2", width = 550), offset = 1)
    
  ),                

  fluidRow(
    
    column(6, selectizeInput("choose1", label = "Choose variables",
                             choices = varnames$variable, 
                             multiple = T,
                             options = list(maxItems = 10, placeholder = 'Select Predictor'),
                             selected = c("bmi_dexa", "bmc"))),
           
    column(6, selectizeInput("selSubj", label = "Select Subject",
                                    choices = list(
                                      ALL = list("ALL"),
                                      AIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "AIT"))$Subject),
                                      MIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "MIT"))$Subject),
                                      LIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "LIT"))$Subject),
                                      withdrawn = c((subjects %>% filter(xnat_subjectdata_sub_group == "withdrawn"))$Subject)
                                    ),
                                    multiple = T,
                                    options = list(maxItems = 30, placeholder = 'Select Subject'),
                                    selected = NULL)),
    column(4, radioButtons(inputId = "id1011",
                   label = "Delta",
                   choices = c("Raw" = "delta", "% Change" = "pc"), 
                   selected = "pc"))
           
    
    
                                           
  
      )
), # end of tab panel 
  
tabPanel("Subject Trajectories",
         
         fluidRow(
           
           plotlyOutput("uniplot3")
           
           ),
         
         
         fluidRow(
           column(6, selectizeInput("choose3", label = "Choose variables",
                                    choices = varnames$variable, 
                                    multiple = T,
                                    options = list(maxItems = 10, placeholder = 'Select Predictor'),
                                    selected = "bmi_dexa")),
           
           column(6, selectizeInput("selSubj3", label = "Select Subject",
                                    choices = list(
                                      AIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "AIT"))$Subject),
                                      MIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "MIT"))$Subject),
                                      LIT = c((subjects %>% filter(xnat_subjectdata_sub_group == "LIT"))$Subject),
                                      withdrawn = c((subjects %>% filter(xnat_subjectdata_sub_group == "withdrawn"))$Subject)
                                    ),
                                    multiple = T,
                                    options = list(maxItems = 30, placeholder = 'Select Subject'),
                                    selected = "1001DS"))
         )
         
         
         
         )                
        
                
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
                                                     selected = c(varnames[2]))
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

