##================= FUNCTIONS =====================+============================
# PROJECT:      Exercise and Cognition project
# PROGRAMMER:   Alan Ho, MMStat
# DESCRIPTION:  Functions for use in the app
#===============================================================================

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

# fixing duplicate fields and input (varnames)
list(
  
  list("dexa", "health", "godin", "insomnia", "psqi"),
  as.list(c(rep("bmi", 2), rep("total", 3)))
  
) %>% 
  pmap(.f = function(tb, var) {
    
    s1 <- paste("ALTER TABLE opex_", tb, " ADD ", var, "_", tb, " NUMERIC;", sep = "")
    s2 <- paste("UPDATE opex_", tb, " SET ", var,"_", tb, "=",var, ";", sep = "")
    
    list(s1, s2) %>% 
      map(function(s) DBI::dbSendStatement(exercise_database, s))
    
  })

varnames <- varnames %>% 
  mutate(variable = 
           ifelse(dataset == "opex_dexa" & variable == "bmi", 
                  "bmi_dexa", variable)
  ) %>% 
  mutate(variable = 
           ifelse(dataset == "opex_health" & variable == "bmi", 
                  "bmi_health", variable)
  ) %>% 
  mutate(variable = 
           ifelse(dataset == "opex_godin" & variable == "total", 
                  "total_godin", variable)
  ) %>% 
  mutate(variable = 
           ifelse(dataset == "opex_insomnia" & variable == "total", 
                  "total_insomnia", variable)
  ) %>% 
  mutate(variable = 
           ifelse(dataset == "opex_psqi" & variable == "total", 
                  "total_psqi", variable)
  )



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


# FUNCTIONS --------------------------------------------------------------------
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

get_df <- function(var){
  (function(df){
    dplyr::tbl(exercise_database, sql(paste("SELECT * FROM ", df, sep = ""))) %>%
      dplyr::filter(!(xnat_subjectdata_sub_group %in% c("not assigned ", "excluded ", "withdrawn"))) %>%
      dplyr::filter(!(xnat_subjectdata_subject_label == "1080MM" & Date == "2017-03-30")) %>% 
      dplyr::filter(!(xnat_subjectdata_subject_label == "1048JM" & Date == "2017-03-27")) %>%
      dplyr::filter(interval <= 6) %>% 
      collect() %>% 
      rename("Group" = "xnat_subjectdata_sub_group",
             "Subject" ="xnat_subjectdata_subject_label",
             "Gender" = "xnat_subjectdata_gender_text"
      ) 
  })((varnames %>% filter(variable == var))$dataset)
} # gets the data 

# joins any two datasets based on chosen variable and then 
# outputs either data (raw or delta change), plots or lm stats
join_df <- function(v1, v2, delta = "0", out = "data") {
  
  if(varnames$dataset[varnames$variable == v1] == 
     varnames$dataset[varnames$variable == v2] ) {
    
    df <- get_df(v1)
    
  } else {
    
    df <- get_df(v1) %>% 
      inner_join(get_df(v2), 
                 
                 by = c("Subject", "Group", "interval", "Gender"))
  }
  
  
  # filter by 
  if(delta == "0") {
    
    df <- df %>% 
      filter(interval == 0)
    
  } else if (delta == "6") {
    
    df <- df %>% 
      filter(interval == 6)
    
  } else if (delta == "delta"){
    
    df <- df %>% 
      select(Subject, Group, interval, v1, v2) %>% 
      gather(var, value, -Subject, -Group, -interval) %>% 
      spread(interval, value) %>% 
      mutate(delta = `6` - `0`) %>% 
      group_by(var) %>% 
      select(Subject, Group, var, delta) %>% 
      spread(var, delta) 
    
  } else if (delta == "pc") {
    
    df <- df %>% 
      select(Subject, Group, interval, v1, v2) %>% 
      gather(var, value, -Subject, -Group, -interval) %>% 
      spread(interval, value) %>% 
      mutate(deltapc = (`6` - `0`)/`0` * 100) %>% 
      group_by(var) %>% 
      select(Subject, Group, var, deltapc) %>% 
      spread(var, deltapc) 
    
  }
  
  if( out == "plot") {
    
    output <- (function(var1, var2, data){
      
      p <- data %>% 
        ggplot(aes_string(x = var1, y = var2)) +
        geom_smooth(method = "lm", se = F, color = "black", lty = "dashed") +
        geom_point(aes(color = Group, text = Subject)) +  
        geom_smooth(aes(color = Group), method = "lm", se = F) +
        theme_classic() + 
        theme(legend.position = "top")
      
      
      mytext=paste("Subject:",df$Subject) 
      
      pp=plotly_build(p)   
      style( pp, text=mytext, hoverinfo = "text")
      
      return(pp)
      
    })(
      var1 = v1, 
      var2 = v2, 
      data = df)
    
  } else if (out == "data") {
    
    output <- df
    
  } else if (out == "stat") {
    
    # remove infinity
    df_wk <- df %>% 
      filter(get(v1) != Inf, get(v2) != Inf)
    
    # formula
    form = as.formula(paste(v2, "~", v1, sep =""))
    
    output <- append(
      
      list(
        
        overall = (function() {
          mod <- summary(lm(form, data = df_wk))
          
          mod$coefficients %>% 
            tidy %>% 
            filter(.rownames != "(Intercept)") %>% 
            mutate(r = mod$adj.r.squared) %>% 
            mutate(n = mod$df[2]+2) %>%
            select(r, Estimate, Pr...t.., n)
        })()
      ), 
      
      df_wk %>% 
        split(.$Group) %>% 
        map(function(.) {
          
          mod <- summary(lm(form, data = .))
          
          mod$coefficients %>% 
            tidy %>% 
            filter(.rownames != "(Intercept)") %>% 
            mutate(r = mod$adj.r.squared) %>% 
            mutate(n = mod$df[2]+2) %>%
            select(r, Estimate, Pr...t.., n)
          
          
        })
    ) %>% 
      enframe %>% 
      unnest() %>% 
      rename("Group" = "name", 
             "Slope" = "Estimate", 
             "P.value" = "Pr...t..")
    
    
  }
  
  return(output)
  
} 

join_many <- function(varlist, delta = NA) {
  
  data <- as.list(varlist) %>% 
    map(function(var) 
      get_df(var) %>% 
        select("Subject", "interval", "Group", var)
      
    ) %>% 
    reduce(full_join, by = c("Subject", "interval", "Group")) 
  
  if (is.na(delta)) {
    
    out <- data %>% 
      arrange(Subject, interval)
    
  } else if (delta == "delta") {
    
    out <- data %>% 
      gather(variable, value, -Subject, -Group, -interval) %>% 
      spread(interval, value) %>% 
      mutate(delta = `6` - `0`) %>% 
      select("Subject", "Group", variable, delta)
    
  } else if (delta == "pc") {
    
    out <- data %>% 
      gather(variable, value, -Subject, -Group, -interval) %>% 
      spread(interval, value) %>% 
      mutate(pc = (`6` - `0`)/`0` * 100) %>% 
      select("Subject", "Group", variable, pc) %>% 
      filter(pc != Inf)
    
  }
  
  return(out)
}

subj_change_plot <- function(varlist, subjects = NULL, del) {
  if (is.null(subjects)) {
    
    d <- join_many(varlist, 
                   delta = del) 
    
  } else {
    
    d <- join_many(varlist, 
                   delta = del) %>% 
      filter(Subject %in% subjects)
    
  }
  
  
  # making the plot
  if( del == "pc") {
    
    lab <- "Delta Change %"
    
    p <- d %>% ggplot(aes(x = variable, 
                          y = pc,
                          color = Group,
                          group = Group)) +
      geom_point(position = position_jitterdodge(0.1),
                 aes(text = Subject)) +
      geom_hline(yintercept = 0) +
      theme_classic() +
      {if (is.null(subjects)) 
        stat_summary(fun.y = "mean", geom = "bar", 
                     position = position_dodge(),
                     alpha = 0.2)
      } +
      {if (is.null(subjects)) 
        stat_summary(fun.data = "mean_se", geom = "errorbar", 
                     position = position_dodge(),
                     alpha = 0.5)
      } +
      ylab(lab)
    
    
    
  } else if ( del == "delta") {
    
    lab <- "Delta Change"
    
    p <- d %>% ggplot(aes(x = variable, 
                          y = delta,
                          color = Group,
                          group = Group)) +
      geom_point(position = position_jitterdodge(),
                 aes(text = Subject)) +
      geom_hline(yintercept = 0) +
      stat_summary(fun.y = "mean", geom = "bar", 
                   position = position_dodge(),
                   alpha = 0.2) +
      stat_summary(fun.data = "mean_se", geom = "errorbar", 
                   position = position_dodge(),
                   alpha = 0.5) +
      theme_classic() +
      ylab(lab)
    
    
  }
  
  pp=plotly_build(p)   
  
  return(pp)
  
}

subj_grid_plots <- function(varlist, subjlist) {
  all <- join_many(varlist) %>% 
    arrange(Subject, interval) %>% 
    gather(variable, value, -Subject, -interval, - Group) %>% 
    group_by(variable) %>% 
    filter(Subject %in% subjlist) %>% 
    nest() %>% 
    mutate(plot = map2(variable, data, function(var, df) {
      
      p <- df %>% 
        filter(!is.na(value)) %>% 
        ggplot(aes(x = interval, 
                   y = value, 
                   group = Subject,
                   color = Subject)) +
        geom_line(stat = "identity") +
        scale_x_continuous(breaks = c(0:6)) +
        labs(x = "Interval", y = variable) +
        theme_classic() +
        theme(legend.position = 'none')
      
      plotly_build(p) %>% 
        layout(yaxis = list(title = var))
      
      
    }))
  
  subplot(all$plot, nrows = ifelse(length(varlist) == 1, 1, 2), titleY = T, margin = 0.1)
}


