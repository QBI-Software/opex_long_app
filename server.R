#============================================== SERVER ===========================================================# 
  # PROJECT:      Exercise and Cognition project
  # PROGRAMMER:   Alan Ho, MMStat
#=================================================================================================================#
configfile <- file.path(path.expand('~'),'.Rconfig.csv')
try(config <-read.csv(configfile, sep=',', quote='\"', header=T))

# selects the colours
grpcol <- c("red", "blue", "green", "purple")
grpcol <- setNames(grpcol, c("AIT", "MIT", "LIT", "withdrawn"))


# SERVER ----------------------------------------------------------------------------------------------------------
server <- (function(input, output, session) {
  # UNIVARIATE ----------------------------------------------------------------------------------------------------

  uni_data <- reactive({
    
    join_df(v1 = input$xvar, 
            v2 = input$yvar, 
            delta = input$selInterval,
            out = "data")
    
  })


  output$mytable = DT::renderDataTable({
    uni_data()
  },
  options = list(filter = "top",
                 scrollX = TRUE, 
                 scrollY = "200px")) # this renders the data table from the joined data 1 and 2 

  
  
  output$uniplot1 <- renderPlotly({

   join_df(v1 = input$xvar,
            v2 = input$yvar,
            delta = input$selInterval,
            out = "plot") %>% 
      layout(legend = list(x = 0.1, y = - 0.3, orientation = 'h'))


    
    
  })
  
  output$results <- renderTable({
    
    pp <- join_df(v1 = input$xvar,
            v2 = input$yvar,
            delta = input$selInterval,
            out = "stat")
    
    
  })
  
  output$uniplot2 <- renderPlotly({
    
    subj_change_plot(varlist = input$choose1, 
                     subjects = input$selSubj,
                     del = input$id1011) %>% 
      layout(legend = list(x = 0.1, y = - 0.3, orientation = 'h'))
    
    
  })
  
  
  output$uniplot3 <- renderPlotly({
    
    subj_grid_plots(varlist = input$choose3, 
                    subjlist = input$selSubj3)
    
  })
  
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
  
  lsmeans
  # The data
  data <- reactive({
    
    # pulls out relevant data and relabels Subject, Group with appropriate tags

    (function(data, input, var){
      
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
        
      })(data, var)
      
      if(input == "none"){
        
        df = cleanData
         
        # } else if(input == "base"){
        # 
        #   # in this data set, create a column for baseline removing it from interval
        # 
        #   df = join(cleanData,
        # 
        #             tidyr::spread(cleanData, interval, var),
        #             by = "Subject",
        #             type = "left",
        #             match = "all"
        # 
        #   ) %>%
        #     filter(interval!=0) %>%
        #     dplyr::select("Subject","interval","Group","0", var)
        # 
        #   colnames(df)[which(colnames(df)=="0")] <- "baseline"
        # 
      } else if(input == "delta"){
        
        df = cleanData %>%
          left_join(
            (cleanData %>%
               group_by(Subject) %>%
               filter(interval == 0) %>%
               mutate(base = get(var)) %>%
               select(-one_of(var, "interval"))),
            
            by = c("Subject", "Group")
            
          ) %>%
          group_by(Subject, interval) %>%
          arrange(Subject, interval) %>%
          mutate(change = (get(var) - base)/base) %>%
          select(-one_of(var, "base")) %>%
          filter(interval != 0)
        
      }
      
      return(df)
      
    })(data = input$data3, 
                                      input = input$adjust, 
                                      var = input$var)
  }) 

  sumData <- reactive({
    
    formula <- (function(adjust, var) {
      
      if(adjust == "none"){
        
        formula <- as.formula(paste(var,"Group * factor(interval)", sep = "~"))
        
      } else if(adjust == "base"){  
        
        formula <- as.formula(paste(var,"baseline + Group * factor(interval)", sep = "~"))
        
      } else if(adjust == "delta"){
        
        formula <- as.formula(paste("change","Group * factor(interval)", sep = "~"))
        
      }
      
      return(formula)
      
    })(input$adjust, input$var)
    
    means <- tidy(
      lsmeans::lsmeans( # the sneaky way to deal with the bugs of the lsmeans package
        lm(lm(formula, data()), data()), 
        "interval",
        by = "Group"
      )
    )
    
    n <- data() %>% 
      group_by(Group, interval) %>% 
      dplyr::summarise(count = n())
    
    means <- plyr::join(means, n, match = "all") 
    
    
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
    
    # formula <- as.formula(paste(input$var,"Group * factor(interval)", sep = "~"))
    # 
    # gee.model <- geeglm(formula,
    #                     data = data(),
    #                     
    #                     corstr="exchangeable", 
    #                     id = factor(Subject), 
    #                     std.err = "jack")
    # 
    # tidy(
    #   
    #   lsmeans::lsmeans(gee.model, 
    #                    pairwise ~ Group |factor(interval), 
    #                    adjust = "bonferroni")$contrasts
    #   
    # )
    
    formula <- as.formula(paste(input$var,"Group * factor(interval)", sep = "~"))
    
    gee.model <- geeglm(formula,
                        data = data(),
                        
                        corstr="exchangeable", 
                        id = factor(Subject), 
                        std.err = "jack")
    
    con_table <- tidy(
      
      lsmeans::lsmeans(gee.model, 
                       pairwise ~ Group |factor(interval), 
                       adjust = "bonferroni")$contrasts
      
    ) %>% 
      select(level1, level2, interval, estimate, std.error, p.value) 
    
    con_table[,4:6] <- round(con_table[,4:6], 4)
    
    con_table
    
    
    
  },
          options = list(pageLength = 3), 
          rownames = FALSE,
          filter = 'bottom'
  ) # fits multiple comparisons onto model()
  
  output$dt_long <- renderDataTable({
    
    data() %>% arrange(Subject, interval)
    
  }, options = list(pageLength = nrow(data()), 
                    scrollY = "150px"))
  
}) 
