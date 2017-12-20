#============================================== SERVER ===========================================================# 
  # PROJECT:      Exercise and Cognition project
  # PROGRAMMER:   Alan Ho, MMStat
#=================================================================================================================#
# FUNCTIONS --------------------------
##Load filepaths from config file
configfile <- file.path(path.expand('~'),'.Rconfig.csv')
try(config <-read.csv(configfile, sep=',', quote='\"', header=T))

setwd(as.character(config$DATADIR)) # sets the working directory
exercise_database <- DBI::dbConnect(RSQLite::SQLite(),as.character(config$DB)) # connects to database
varnames <-  read.csv(as.character(config$VARSFILE),
                      header = T, 
                      stringsAsFactors = F)

scatter_LS <- function(data, xlab, ylab, out, mult = F) {
  
  if(length(xlab) == 1) { x = as.data.frame(data)[ , xlab] } else { x = rowSums(as.data.frame(data)[ , xlab]) }
  if(length(ylab) == 1) { y = as.data.frame(data)[ , ylab] } else { y = rowSums(as.data.frame(data)[ , ylab]) }
  model <- lm(y~x)
  
  lm_eqn <- function(m){
    pval <- 1 - pf(summary(m)$fstatistic[1], summary(m)$fstatistic[2], summary(m)$fstatistic[3]);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r[adj])^2~"="~r2 ~","~italic(p)~"="~pval, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$adj.r.squared, digits = 3),
                          pval = format(pval, digits = 3)
                     ))
    as.character(as.expression(eq));                 
  }
  
  
  g <- 
    if(mult == F) {
      ggplot(as.data.frame(data), aes(x = x, y = y)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +  
        geom_label(aes(x = -Inf, y = Inf), 
                   hjust = 0, 
                   vjust = 1, 
                   label = lm_eqn(model),
                   parse = TRUE, 
                   size = 5) + 
        labs(x = paste(xlab ,collapse = " + "), 
             y = paste(ylab ,collapse = " + ")) + 
        theme_classic() 
      
    } else {
      
      ggplot(as.data.frame(data), aes(x = x, y = y)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +  
        # geom_label(aes(x = -Inf, y = Inf), 
        #            hjust = 0, 
        #            vjust = 1, 
        #            label = lm_eqn(model),
        #            parse = TRUE, 
        #            size = 5) + 
        labs(x = paste(xlab ,collapse = " + "), 
             y = paste(ylab ,collapse = " + ")) + 
        theme_classic() 
      
      
    }
  
  
  if(out == "model") {
    
    return(model)
    
  } else if (out == "eqn") {
    
    return(lm_eqn(model))
    
  } else if (out == "plot"){
    
    return(g)
  }
  
}


# selects the colours
grpcol <- c("red", "blue", "green", "purple")
grpcol <- setNames(grpcol, c("AIT", "MIT", "LIT", "withdrawn"))



# SERVER ----------------------------------------------------------------------------------------------------------
server <- (function(input, output, session) {
  # UNIVARIATE ----------------------------------------------------------------------------------------------------
  
  observeEvent(input$data1,  {
    
    
    updateSelectizeInput(session = session, inputId = "xvar", 
                         choices = 
                           
                           
                           (varnames %>% filter(dataset == input$data1))$variable
                         
        
                         
    )
    
    
  }) # this updates the variable names for data set 1
  observeEvent(input$data2,  {
    
    
    updateSelectizeInput(session = session, inputId = "yvar", 
                         choices = 
                           
                           (varnames %>% filter(dataset == input$data2))$variable
                         
                         
    )
  
    
  }) # this updates the variable names for data set 2
  uni_data <- eventReactive(input$go, {
    
    if(input$data1 == input$data2) {
      
      tbl(exercise_database, sql(paste("SELECT * FROM ", input$data1, sep = ""))) %>% 
        collect() %>% 
        dplyr::select(xnat_subjectdata_subject_label, 
                      xnat_subjectdata_sub_group,
                      interval, 
                      input$xvar, 
                      input$yvar) %>% 
        filter(interval == input$selInterval)
      
    } else {
      
      as.data.frame(
        
        (function(data1, data2){
          
          lapply(list(data1, data2), function(df)
            tbl(exercise_database, sql(paste("SELECT * FROM ", df, sep = ""))) %>% 
              # filter(interval == 0) %>% 
              collect()
            
          ) %>% 
            reduce(full_join, by = c("xnat_subjectdata_subject_label",
                                     "xnat_subjectdata_sub_group",
                                     "interval"))
          
        })(input$data1,input$data2) %>% 
          dplyr::select(xnat_subjectdata_subject_label, 
                        xnat_subjectdata_sub_group,
                        interval, 
                        input$xvar, 
                        input$yvar) %>% 
          filter(interval == input$selInterval)
        
      )
      
      
    }
    
    
    
    
  }) # this pulls out the data from the exercise database 
  output$mytable = isolate(DT::renderDataTable({
    uni_data()
  },
  options = list(filter = "top",
                 scrollX = TRUE, 
                 scrollY = "200px"))) # this renders the data table from the joined data 1 and 2 
  
  
  output$uniplot1 <- isolate(renderPlot({
    
    if (is.null(input$split)) {
      
      scatter_LS(uni_data(),
                 input$xvar, input$yvar, out = "plot")
      
    } else if (input$split == "Exercise Group") {
      
      
      # scatter_LS(uni_data() %>%
      #             filter(!is.na(xnat_subjectdata_sub_group) & xnat_subjectdata_sub_group != ""),
      #             input$xvar, input$yvar, out = "plot") + facet_grid(~xnat_subjectdata_sub_group)
      
      (function(x,y) {cowplot::plot_grid(
        
        plotlist = (uni_data() %>%
                      filter(!is.na(xnat_subjectdata_sub_group) & xnat_subjectdata_sub_group != "") %>%
                      split(.$xnat_subjectdata_sub_group) %>%
                      map(~scatter_LS(data = ., x = x, y = y, out = "plot", mult = T)))
        , ncol = 2,
        labels = names(uni_data() %>%
                         filter(!is.na(xnat_subjectdata_sub_group) & xnat_subjectdata_sub_group != "") %>%
                         split(.$xnat_subjectdata_sub_group))
        
      )
      })(input$xvar, input$yvar)
      
      
    }
    
  }, width = 600, height = 400))
  
  
  
  # LONGITUDINAL ----------------------  
  
  # Reactive Inputs - reacts to other inputs
  formula <- reactive({
    
    (function(adjust, var) {
      
      if(adjust == "none"){
        
        formula <- as.formula(paste(var,"Group * factor(interval)", sep = "~"))
        
      } else if(adjust == "base"){  
        
        formula <- as.formula(paste(var,"baseline + Group * factor(interval)", sep = "~"))
        
      } else if(adjust == "delta"){
        
        formula <- as.formula(paste("change","Group * factor(interval)", sep = "~"))
        
      }
      
      return(formula)
      
    })(input$adjust, input$var)
    
  })        # creates the formula for the model
  observeEvent(input$data3,  {
    
    updateSelectizeInput(session = session, inputId = "var", 
                         choices = 
                           
                           # if(!is.null(input$data3)){
                             
                             (varnames %>% filter(dataset == input$data3))$variable
               
                             
                           # } else { NULL }
                         
                         
                         
    )
    
    
  }) # this updates the variable names for data set 2 
  
  
  # The data
  data <- reactive({
    
    # pulls out relevant data and relabels Subject, Group with appropriate tags
    cleanData <- (function(data,var) {
      
      df_long <- (function(df){
        tbl(exercise_database, sql(paste("SELECT * FROM ", df, sep = ""))) %>%
          filter(!(xnat_subjectdata_sub_group %in% c("not assigned ", "excluded ", "withdrawn"))) %>%
          filter(!(xnat_subjectdata_subject_label == "1080MM" & Date == "2017-03-30")) %>% 
          filter(!(xnat_subjectdata_subject_label == "1048JM" & Date == "2017-03-27")) %>%
          filter(interval <= 6) %>% 
          collect() })(data)
      
      colnames(df_long)[which(colnames(df_long) 
                              %in% c("xnat_subjectdata_subject_label",
                                     "xnat_subjectdata_sub_group"))] <- c("Group", "Subject")
      
      df_long <- df_long %>%
        dplyr::select(Subject, Group, var, interval)
      
      return(df_long)
      
    })(input$data3, input$var)
    
    (function(input, var){
      if(input == "none"){
        
        df = cleanData
        
      } else if(input == "base"){
        
        # in this data set, create a column for baseline removing it from interval
        
        df = join(cleanData,
                  
                  tidyr::spread(cleanData, interval, var),
                  by = "Subject",
                  type = "left",
                  match = "all"
                  
        ) %>%
          filter(interval!=0) %>%
          dplyr::select("Subject","interval","Group","0", var)
        
        colnames(df)[which(colnames(df)=="0")] <- "baseline"
        
      } else if(input == "delta"){
        
        df = cleanData %>%
          group_by(Subject) %>%
          mutate(change = (get(var) - get(var)[interval == 0])/get(var)[interval == 0]) %>%
          filter(interval!=0) %>%
          dplyr::select(Subject, interval, Group, change)
        
      }
      
      return(df)
      
    })(input = input$adjust, var = input$var)
    
  }) 
  
  sumData <- reactive({
    
    means <- tidy(
      lsmeans::lsmeans( # the sneaky way to deal with the bugs of the lsmeans package
        lm(lm(formula(), data()), data()), 
        "interval",
        by = "Group"
      )
    )
    
    n <- data() %>% 
      group_by(Group, interval) %>% 
      dplyr::summarise(count = n())
    
    means <- plyr::join(means, n, match = "all") 
    
    means
    
    
  })
  
  # The plots
  output$plot1 <- renderPlotly({
    
    ##### LINE PLOT    
    if (input$plottype == "line") {
      
      # the base plot will be used to construct all the other plots
      basePlot <-  plot_ly(data(), x = ~interval, color = ~Group, colors = grpcol) %>%
        add_trace(data = sumData() %>% filter(Group == "AIT"),
                  y = ~estimate,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "red"),
                  error_y = list(array = ~std.error, color = "red"),
                  hoverinfo = 'text',
                  text = ~paste('Mean = ', round(estimate,2),
                                '</br>',
                                '</br> SE = ', round(std.error,2),
                                '</br> n = ', count)
        ) %>%
        add_trace(data = sumData() %>% filter(Group == "MIT"),
                  y = ~estimate,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "blue"),
                  error_y = list(array = ~std.error, color = "blue"),
                  hoverinfo = 'text',
                  text = ~paste('Mean = ', round(estimate,2),
                                '</br>',
                                '</br> SE = ', round(std.error,2),
                                '</br> n = ', count)
        ) %>%
        add_trace(data = sumData() %>% filter(Group == "LIT"),
                  y = ~estimate,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "green"),
                  error_y = list(array = ~std.error, color = "green"),
                  hoverinfo = 'text',
                  text = ~paste('Mean = ', round(estimate,2),
                                '</br>',
                                '</br> SE = ', round(std.error,2),
                                '</br> n = ', count)
                  
        ) %>%
        
        layout(xaxis = list(title = "Interval", rangemode = "tozero"),
               yaxis = list(title = ifelse(input$adjust == "none" | input$adjust == "base",
                                           input$var,
                                           "Delta Change %"
               ))
        )
      
      
      # subsets by subject list specified in subName
      sub_line <- data() %>%
        filter(Subject %in% input$name) %>%
        dplyr::select("Subject", "interval", input$var, "Group") %>% 
        arrange(Subject, interval)
      
      
      
      # this creates the PLOTLY graphs 
      if(input$adjust == "none") {
        
        if(is.null(input$name)) { # conditions for subject trajectories
          
          basePlot
          
        } else if (input$name %in% "ALL") {
          
          basePlot %>%  
            add_trace(data = data() %>% group_by(Subject) %>% arrange(interval), 
                      y = ~ get(input$var),
                      split = ~Group,
                      type = "scatter",
                      mode = "lines",
                      line = list(width = 1, dash = 'dot'),
                      alpha = 0.5,
                      hoverinfo = 'text',
                      text = ~paste("Subject :", Subject)
            ) %>% 
            add_trace(data = sub_line, y = ~get(input$var),
                      type = "scatter",
                      mode = "lines",
                      split = ~Subject,
                      line = list(width = 4, dash = 'dot'),
                      alpha = 0.5)
          
          
        } else  {# conditions for subject trajectories
          
          basePlot %>% 
            add_trace(data = sub_line, y = ~get(input$var),
                      type = "scatter",
                      mode = "lines",
                      split = ~Subject,
                      line = list(width = 1, dash = 'dot'),
                      alpha = 0.5)
          
        }
        
        
      } else if (input$adjust == "base") {
        
        basePlot
        
      } else if (input$adjust == "delta") {
        
        basePlot
        
      }    
    } else if (input$plottype == "box") {
      
      data() %>% 
        plot_ly(y = ~get(input$var), 
                x = ~interval, 
                color = ~Group, 
                colors = grpcol,
                type = "box",
                boxpoints = 'all') %>% 
        layout(boxmode = "group", xaxis = list(title = "Interval", 
                                               rangemode = "tozero",
                                               tick0 = 0,
                                               dtick = 1),
               yaxis = list(title = ifelse(input$adjust == "none" | input$adjust == "base",
                                           input$var,
                                           "Delta Change %"
               )))
      
      
    }
    
    
    
  })       # fits longitudinal plotly graphs to data()
  output$corplot <- renderPlot({
    
    df1.l <- reshape(data(), idvar = c("Subject","Group"), timevar = "interval", direction = "wide")
    
    corrplot(cor(df1.l[,c(4:9)], use = "complete.obs"), method = "ellipse", type = "upper") 
    
  })       # fits correlation matrix to data()
  output$subjectPlot <- renderPlotly({
    
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = input$var2
    )
    
    cleanData %>% 
      filter(Subject %in% input$subName2) %>% 
      dplyr::select("Subject", "interval", input$var1, input$var2) %>% 
      arrange(Subject, interval) %>% 
      
      # plotting this subject's scores
      plot_ly(x = ~interval) %>% 
      add_trace(y = ~get(input$var1), 
                type = "scatter", 
                mode = "lines"
      ) %>% 
      add_trace(y = ~get(input$var2), 
                type = "scatter", 
                mode = "lines",
                yaxis = "y2"
      ) %>% 
      layout(
        title = "Subject Trajectories", 
        yaxis = list(title = input$var1),
        yaxis2 = ay,
        xaxis = list(title="Interval")
      )
    
  }) # fits subject trajectory plots
  
  
  # The models
  output$model <- renderTable({
    
    formula <- as.formula(paste(input$var,"Group * factor(interval)", sep = "~"))
    
    # anova(geeglm(formula, 
    #        
    #        data = data(), 
    #        
    #        corstr="exchangeable", 
    #        id = factor(Subject), 
    #        std.err = "jack"))
    
    
    anova(lme(formula, 
              data = data(),
              random = ~1|Subject,
              na.action = na.omit)
    )
    
  }, rownames = TRUE)   # fits model to data() 
  
  output$contrasts <- renderDataTable({
    
    formula <- as.formula(paste(input$var,"Group * factor(interval)", sep = "~"))
    
    gee.model <- geeglm(formula,
                        data = data(),
                        
                        corstr="exchangeable", 
                        id = factor(Subject), 
                        std.err = "jack")
    
    tidy(
      
      lsmeans::lsmeans(gee.model, 
                       pairwise ~ Group |factor(interval), 
                       adjust = "bonferroni")$contrasts
      
    )
    
  },
  options = list(pageLength = 3)) # fits multiple comparisons onto model()
  
  output$dt_long <- renderDataTable({
    
    data() %>% arrange(Subject, interval)
    
  }, options = list(pageLength = nrow(data()), 
                    scrollY = "150px"))
  
}) 
