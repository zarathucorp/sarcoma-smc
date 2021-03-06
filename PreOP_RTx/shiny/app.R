## Shiny error log option
options(shiny.sanitize.errors = F)

source("global.R")              ## Load global.R
library(shinycustomloader)      ## Loading image
library(DT)                     ## datatable 
library(jsmodule) ## my package: include xxxUI, xxxmodule, xxxModule
library(survival);library(jskm);library(ggplot2);library(treemap)
## 범주 21 이상인 변수는 아예 빼버림
nfactor.limit <- 21


Makekaplan <- function(event.original = "TLF", day.original = "TLFDay", var.group = "SBintervention", data = out, data.label = out.label, yr.event = c(1, 5), cut.yr = F,  timeby = 365, xlims = c(0, 5 * 365), ylims = c(0 ,0.5)){
  var.event <- event.original
  var.day <- day.original
  form <- as.formula(paste("Surv(", var.day, ",", var.event, ") ~ ", var.group, sep = ""))
  if ("survey.design" %in% class(data)){
    if (cut.yr == T){
      data <- subset(data, !(get(var.day) < 365 * yr.event[1]))
      data$variables[[var.event]] <- ifelse(data$variables[[var.day]] >= 365 * yr.event[2] & data$variables[[var.event]] == "1", 0,  as.numeric(as.vector(data$variables[[var.event]])))
      data$variables[[var.day]]  <- ifelse(data$variables[[var.day]] >= 365 * yr.event[2], 365 * yr.event[2], data$variables[[var.day]])
      #data$variables[[var.event]] <- ifelse(data$variables[[var.day]] < 365 * yr.event[1] & data$variables[[var.event]] == "1", 0,  as.numeric(as.vector(data$variables[[var.event]])))
      #data$variables[[var.day]]  <- ifelse(data$variables[[var.day]] < 365 * yr.event[1], 365 * yr.event[1] - 1, data$variables[[var.day]])
      
    }
    
    data$variables[[var.day]] <- as.numeric(as.vector(data$variables[[var.day]]))
    data$variables[[var.event]] <- as.numeric(as.vector(data$variables[[var.event]]))
    #data$variables[[var.day]] <- data$variables[[var.day]]
    res.kap <- survey::svykm(form, design = data)  
    p <- svyjskm(res.kap, xlabs = "Days", ylab = "Cumulative Incidence", cumhaz = T, xlims = xlims, ylims = ylims, timeby = timeby,
                 pval.coord = c(365 * (yr.event[1] + 1), 0.01), legendposition = c(0.3, 0.8),
                 ystrataname = data.label[var.event, var_label][1], ystratalabs = data.label[variable == var.group][level %in% levels(data$variables[[var.group]]), val_label],
                 surv.scale = "percent", mark = F, pval = T, table = T, design = data, pval.testname = F)
  } else{
    if (cut.yr == T){
      data <- data[!(get(var.day) < 365 * yr.event[1])]
      data[[var.event]] <- ifelse(data[[var.day]] >= 365 * yr.event[2] & data[[var.event]] == "1", 0,  as.numeric(as.vector(data[[var.event]])))
      data[[var.day]] <- ifelse(data[[var.day]] >= 365 * yr.event[2], 365 * yr.event[2], data[[var.day]])
      
      #data[[var.event]] <- ifelse(data[[var.day]] < 365 * yr.event[1] & data[[var.event]] == "1", 0,  as.numeric(as.vector(data[[var.event]])))
      #data[[var.day]] <- ifelse(data[[var.day]] < 365 * yr.event[1], 365 * yr.event[1] - 1, data[[var.day]])
      
      
    }
    data[[var.day]] <- as.numeric(as.vector(data[[var.day]]))
    data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
    #data[[var.day]] <- data[[var.day]]
    
    res.kap <- survfit(form, data = data)
    res.kap$call$formula <- form
    p <- jskm(res.kap, xlabs = "Days", ylab = "Cumulative Incidence", cumhaz = T, xlims, ylims = ylims, 
              ystrataname = data.label[variable == var.event, var_label][1], ystratalabs = data.label[variable == var.group][level %in% levels(data[[var.group]]), val_label],
              pval.coord = c(365 * (yr.event[1] + 1), 0.01), legendposition = c(0.3, 0.8), timeby = timeby,
              surv.scale = "percent", mark = F, pval = T, table = T, data = data)
  }
  
  
  return(p)
  
  
  
}



ui <- navbarPage("PreOP RTx",
                 tabPanel("Table 1-3", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("group_tb1", "Group", c("1 vs 2 vs 3"), select = "1 vs 2 vs 3", inline = T),
                              radioButtons("seltb1", "Table", c("Table 1", "Table 2 + RTgray"), select = "Table 1", inline = T)
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )
                          
                 ),
                 tabPanel("Pie chart",
                          sidebarPanel(
                            radioButtons("group_pie", "Data", c("All", out.label[variable == "Group", val_label]), selected = "All", inline = T),
                            radioButtons("type_pie", "Plot type", c("Pie", "Treemap"), selected = "Pie", inline = T),
                            selectInput("variable_pie", "Main variable", choices = "ClavienDindoComplication01", selected = "ClavienDindoComplication01"),
                            selectInput("sub_pie", "Subgroup variable", choices = "ClavienDindoGrade", selected = "ClavienDindoGrade")
                            
                          ),
                          mainPanel(
                            withLoader(plotOutput("pie"), type="html", loader="loader6"),
                            h3("Download options"),
                            wellPanel(
                              uiOutput("downloadControls_pie"),
                              downloadButton("downloadButton_pie", label = "Download the plot")
                            )
                          )
                 ),
                 
                 tabPanel("Kaplan-meier plot",
                          sidebarPanel(
                            selectInput("event_kap", "Outcome", choices = varlist[names(varlist)[3]], selected = varlist$Event[1]),
                            sliderInput("year_kap", "Cut month",  min = 0 , max = 120, value = c(0, 24)),
                            selectInput("group_kap", "Group", varlist[names(varlist)[1:2]], selected = "Group", multiple = F),
                            uiOutput("cutconti"),
                            sliderInput("ylims_kap", "Y axis ranges", min = 0, max = 1, value = c(0, 0.6), step = 0.05)
                          ),
                          mainPanel(
                            withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                            h3("Download options"),
                            wellPanel(
                              uiOutput("downloadControls_kap"),
                              downloadButton("downloadButton_kap", label = "Download the plot")
                            )
                          )
                          
                 ),
                 tabPanel("Cox model",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("dep_cox", "Outcome", choices = varlist[names(varlist)[3]], selected = varlist$Event[1]),
                              sliderInput("year_cox", "Cut month", min = 0 , max = 120, value = c(0, 24)),
                              selectInput("cov_cox", "Covariates", choices = c("Group", "Group1_23", varlist$Base), selected = c("Group", "Age", "Sex"), multiple = T),
                              checkboxInput("step_cox", "Backward stepwise selection", value = F)
                            ),
                            mainPanel(
                              withLoader(DTOutput("coxtable"), type="html", loader="loader6")
                            )
                          )
                          
                 ),
                 tabPanel("Table 5",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("dep_tb5", "Outcome", choices = c("ClavienDindoComplication01", "ClavienDindoComplication_wo_2", "Resection"), selected = "Resection"),
                              uiOutput("indeptb5"),
                              checkboxInput("step_tb5", "Stepwise variable selection"),
                              sliderInput("decimal_tb5", "Decimal", min = 1, max = 4, value = 2)
                            ),
                            mainPanel(
                              withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                            )
                          )
                 )
                 
)

#ui <- secure_app(ui, enable_admin = T)

server <- function(input, output, session) {
  
  # check_credentials returns a function to authenticate users
  #res_auth <- secure_server(
  #  check_credentials = check_credentials("database.sqlite")
  #)
  
  data.tb1 <- reactive({
    if (input$group_tb1 == "1 vs 2"){
      dd <- out[Group != 3]
      dd$Group <- factor(dd$Group)
      return(dd)
    } else{
      out
    }
  })
  
  strata.tb1 <- reactive({
    if (input$group_tb1 == "1 vs 2/3"){
      "Group1_23"
    } else{
      "Group"
    }
  })
  
  label.tb1 <- reactive({
    if (input$group_tb1 == "1 vs 2"){
      out.label[!(variable == "Group" & level == 3)]
    } else{
      out.label
    }
  })
  
  vars.tb1 <- reactive({
    switch (input$seltb1,
            "Table 1" = c(setdiff(varlist$Base, c("Group", "Group1_23")), "day_FU"),
            "Table 2 + RTgray" = varlist$Complication
    )
  })
  
  
  
  
  output$table1 <- renderDT({
    out_tb1 <- CreateTableOneJS(vars = vars.tb1(), strata = strata.tb1(), data = data.tb1(), labeldata = out.label, Labels = T, showAllLevels = F)
    tb <- out_tb1$table
    cap <- out_tb1$caption
    out.tb1 <- datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                         options = c(jstable::opt.tb1("tb1"),
                                     list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                     ),
                                     list(scrollX = TRUE)
                         )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  
  
  output$cutconti <- renderUI({
    if (class(out[[input$group_kap]]) %in% c("integer", "numeric")){
      data.km <- out[, .SD]
      var.event <- input$event_kap
      var.day <- varlist$Day[varlist$Event == input$event_kap]
      data.km[[var.event]] <- as.numeric(as.vector(data.km[[var.event]]))
      mstat <- maxstat::maxstat.test(as.formula(paste("survival::Surv(",var.day,",", var.event,") ~ ", input$group_kap, sep="")), data= data.km, smethod="LogRank", pmethod="condMC", B=999)
      cut5 <- mstat$cuts[order(-mstat$stats)][1:5]
      
      vec <- data.km[[input$group_kap]][!is.na(data.km[[input$group_kap]])]
      numericInput("cut_conti", "Cut-off", value = cut5[1], min = quantile(vec, 0.05), max = quantile(vec, 0.95))
    }
  })
  
  
  
  obj.km <- reactive({
    req(input$group_kap)
    req(input$event_kap)
    
    data <- out[, .SD]
    label <- out.label
    
    
    if (class(data[[input$group_kap]]) %in% c("numeric", "integer")){
      req(input$cut_conti)
      data$xcat <- factor(as.integer(data[[input$group_kap]] > input$cut_conti))
      gvar <- "xcat"
      addlabel <- mk.lev(data[, .SD, .SDcols = "xcat"])
      addlabel[, var_label := paste(label[variable == input$group_kap, var_label][1], "group")]
      addlabel[, val_label := paste(label[variable == input$group_kap, var_label][1], paste(c("\u2264", ">"), input$cut_conti, sep=""))]
      label <- rbind(label, addlabel)
      setkey(label, variable)
    } else{
      gvar <- input$group_kap
    }
    
    Makekaplan(event.original = input$event_kap, day.original = varlist$Day[varlist$Event == input$event_kap],  var.group = gvar, data = data, data.label = label,
               yr.event = input$year_kap/12, cut.yr = T, timeby = 60, xlims = input$year_kap/12 * 365, ylims = input$ylims_kap) 
  })
  
  output$kaplan_plot <- renderPlot({
    print(obj.km())
  })
  
  output$downloadControls_kap <- renderUI({
    fluidRow(
      column(4,
             selectizeInput("kap_file_ext", "File extension (dpi = 300)", 
                            choices = c("jpg","pdf", "tiff", "svg", "emf"), multiple = F, 
                            selected = "jpg"
             )
      ),
      column(4,
             sliderInput("fig_width_kap", "Width (in):",
                         min = 5, max = 20, value = 8
             )
      ),
      column(4,
             sliderInput("fig_height_kap", "Height (in):",
                         min = 5, max = 20, value = 6
             )
      )
    )
  })
  
  output$downloadButton_kap <- downloadHandler(
    filename =  function() {
      paste(input$event_kap, "_", input$data_kap, "_", input$group_kap , "_plot.", input$kap_file_ext ,sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$kap_file_ext == "emf"){
                       devEMF::emf(file, width = input$fig_width_kap, height =input$fig_height_kap, coordDPI = 300, emfPlus = F)
                       plot(obj.km())
                       dev.off()
                       
                     } else{
                       ggsave(file, obj.km(), dpi = 300, units = "in", width = input$fig_width_kap, height =input$fig_height_kap)
                     }
                     
                   })
      
      
    })
  
  
  
  
  
  
  output$indeptb5 <- renderUI({
    varsIni <- sapply(varlist$Base,
                      function(v){
                        forms <- as.formula(paste(input$dep_tb5, "~", v))
                        coef <- tryCatch(summary(glm(forms, data = out, family = binomial))$coefficients, error = function(e){return(NULL)})
                        sigOK <- ifelse(is.null(coef), F, !all(coef[-1, 4] > 0.05))
                        return(sigOK)
                        
                      })
    selectInput("indep_tb5", "Risk factors to include", choices = c("Group", "Group1_23", varlist$Base), selected = c("Group", varlist$Base[varsIni]), multiple = T)
    
  })
  
  output$logistictable <- renderDT({
    y <- input$dep_tb5
    xs <- input$indep_tb5
    validate(
      need(!is.null(input$indep_tb5) , "Please select at least 1 variable")
    )
    form = as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))
    mf <- model.frame(form, out)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
    )
    
    
    res.logistic <- glm(form, data = out, family = binomial)
    if (input$step_tb5 == T){
      
      res.logistic <- stats::step(glm(form, data = out[complete.cases(out[, .SD, .SDcols = c(y, xs)])],  family = binomial), direction = "backward", scope = list(upper = "~.", lower = "~1"))
    }
    
    tb.logistic <- jstable::glmshow.display(res.logistic, decimal = input$decimal_tb5)
    cap.logistic <- paste("Logistic regression predicting ", out.label[variable == y, var_label][1], sep="")
    
    if (input$step_tb5 == T){
      cap.logistic <- paste0(cap.logistic, "- stepwise selection")
    }
    out.logistic <- jstable::LabelepiDisplay(tb.logistic, label = T, ref = out.label)
    
    
    hide = which(colnames(out.logistic) == "sig")
    datatable(out.logistic, rownames=T, extensions = "Buttons", caption = cap.logistic,
              options = c(jstable::opt.tbreg(cap.logistic),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  
  output$coxtable <- renderDT({
    validate(
      need(!is.null(input$cov_cox), "Please select at least 1 independent variable.")
    )
    data <- out[, .SD]
    label <- out.label
    
    var.event <- input$dep_cox
    var.day <- varlist$Day[varlist$Event == input$dep_cox]
    
    data <- data[!( get(var.day) < 365 * input$year_cox[1]/12)]
    
    data[[var.event]] <- ifelse(data[[var.day]] >= 365 * input$year_cox[2]/12 & data[[var.event]] == "1", 0,  as.numeric(as.vector(data[[var.event]])))
    data[[var.day]] <- ifelse(data[[var.day]] >= 365 * input$year_cox[2]/12, 365 * input$year_cox[2]/12, data[[var.day]])
    
    #data[[var.event]] <- ifelse(data[[var.day]] < 365 * input$year_cox[1]/12 & data[[var.event]] == "1", 0,  as.numeric(as.vector(data[[var.event]])))
    #data[[var.day]] <- ifelse(data[[var.day]] < 365 * input$year_cox[1]/12, 365 * input$year_cox[1]/12 - 1, data[[var.day]])
    
    
    data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
    data[[var.day]] <- as.numeric(as.vector(data[[var.day]]))
    forms.cox <- as.formula(paste("Surv(", var.day,",", var.event,") ~ ", paste(input$cov_cox, collapse = "+"), sep=""))
    
    
    cc <- substitute(survival::coxph(.form, data= data, model = T), list(.form= forms.cox))
    res.cox <- eval(cc)
    
    if (input$step_cox == T){
      data.cox.step <- data[complete.cases(data[, .SD, .SDcols = c(var.day, var.event, input$cov_cox)])]
      cc.step <- substitute(survival::coxph(.form, data= data.cox.step, model = T), list(.form= forms.cox))
      
      res.cox <- stats::step(eval(cc.step), direction = "backward", scope = list(lower = ~1))
    }
    
    
    tb.cox <- jstable::cox2.display(res.cox, dec = 2)
    tb.cox <- jstable::LabeljsCox(tb.cox, ref = label)
    out.cox <- rbind(tb.cox$table, tb.cox$metric)
    sig <- out.cox[, ncol(out.cox)]
    sig <- gsub("< ", "", sig)
    sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
    out.cox <- cbind(out.cox, sig)
    
    cap.cox <- paste("Cox's proportional hazard model on time ('", label[variable == var.day, var_label][1] , "') to event ('", label[variable == var.event, var_label][1], "')", sep="")
    
    hide <- which(colnames(out.cox) == c("sig"))
    datatable(out.cox, rownames=T, extensions= "Buttons", caption = cap.cox,
              options = c(opt.tbreg(cap.cox),
                          list(columnDefs = list(list(visible=FALSE, targets= hide))
                          )
              )
    )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  
  
  
  obj.pie <- reactive({
    
    data <- out[, .SD]
    if (input$group_pie != "All"){
      data <- out[Group == out.label[variable == "Group" & val_label == input$group_pie, level]] 
    }
    
    data[, ClavienDindoGrade := factor(ifelse(ClavienDindoComplication01 == 0, "1", as.character(ClavienDindoGrade)))]
    df <- data[, .N, keyby = c("ClavienDindoComplication01", "ClavienDindoGrade")][, prop:= round(N/sum(N) * 100, 1)]
    
    df$ClavienDindoComplication01 <- ifelse(df$ClavienDindoComplication01 == 0, "No", "Yes")
    names(df)[4] <- "Clavien-Dindo complication(%)"
    
    if (input$type_pie == "Treemap"){
      treemap(df, index=c("ClavienDindoComplication01","ClavienDindoGrade"), vSize="Clavien-Dindo complication(%)", type="value", vColor = "Clavien-Dindo complication(%)",
              fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
              fontcolor.labels=c("white","orange"),    # Color of labels
              fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
              bg.labels=c("transparent"),              # Background color of labels
              align.labels=list(
                c("center", "center"), 
                c("right", "bottom")
              ),                                   # Where to place labels in the rectangle?
              overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
              inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
              
      )
    } else{
      par(mfrow= c(1, 2))
      df1 <- df[, lapply(.SD, sum), .SDcols = c("N","Clavien-Dindo complication(%)"), keyby = "ClavienDindoComplication01"]
      pie(df1$N, label = paste0(df1[["Clavien-Dindo complication(%)"]], "%"), col = gray.colors(nrow(df1)), init.angle = 90)
      legend("topright", legend = df1$ClavienDindoComplication01, fill = gray.colors(nrow(df1)))
      pie(df[-1]$N, label = paste0(df[-1][["Clavien-Dindo complication(%)"]], "%"), col = gray.colors(nrow(df[-1])), init.angle = 90)
      legend("topright", legend = df[-1]$ClavienDindoGrade, fill = gray.colors(nrow(df[-1])))
    }
    
  })
  
  output$pie <- renderPlot({
    print(obj.pie())
    
    
    
  })
  
  output$downloadControls_pie <- renderUI({
    fluidRow(
      column(4,
             selectizeInput("pie_file_ext", "File extension (dpi = 300)", 
                            choices = c("emf"), multiple = F, 
                            selected = "emf"
             )
      ),
      column(4,
             sliderInput("fig_width_pie", "Width (in):",
                         min = 5, max = 20, value = 8
             )
      ),
      column(4,
             sliderInput("fig_height_pie", "Height (in):",
                         min = 5, max = 20, value = 6
             )
      )
    )
  })
  
  output$downloadButton_pie <- downloadHandler(
    filename =  function() {
      paste("pie_plot.", input$pie_file_ext ,sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$pie_file_ext == "emf"){
                       if (input$type_pie == "Treemap"){
                         devEMF::emf(file, width = input$fig_width_pie, height =input$fig_height_pie, coordDPI = 300, emfPlus = F)
                         print(obj.pie())
                         dev.off() 
                       } else{
                         devEMF::emf(file, width = input$fig_width_pie, height =input$fig_height_pie, coordDPI = 300, emfPlus = F)
                         data <- out[, .SD]
                         if (input$group_pie != "All"){
                           data <- out[Group == out.label[variable == "Group" & val_label == input$group_pie, level]] 
                         }
                         
                         data[, ClavienDindoGrade := factor(ifelse(ClavienDindoComplication01 == 0, "1", as.character(ClavienDindoGrade)))]
                         df <- data[, .N, keyby = c("ClavienDindoComplication01", "ClavienDindoGrade")][, prop:= N/sum(N) * 100]
                         
                         df$ClavienDindoComplication01 <- ifelse(df$ClavienDindoComplication01 == 0, "No", "Yes")
                         names(df)[4] <- "Clavien-Dindo complication(%)"
                         par(mfrow= c(1, 2))
                         df1 <- df[, lapply(.SD, sum), .SDcols = c("N","Clavien-Dindo complication(%)"), keyby = "ClavienDindoComplication01"]
                         pie(df1$N, label = paste0(df1[["Clavien-Dindo complication(%)"]], "%"), col = gray.colors(nrow(df1)), init.angle = 90)
                         legend("topright", legend = df1$ClavienDindoComplication01, fill = gray.colors(nrow(df1)))
                         pie(df[-1]$N, label = paste0(df[-1][["Clavien-Dindo complication(%)"]], "%"), col = gray.colors(nrow(df[-1])), init.angle = 90)
                         legend("topright", legend = df[-1]$ClavienDindoGrade, fill = gray.colors(nrow(df[-1])))
                         dev.off() 
                       }
                       
                       
                     } else{
                       ggsave(file, obj.pie(), dpi = 300, units = "in", width = input$fig_width_pie, height =input$fig_height_pie)
                     }
                     
                   })
      
      
    })
}



shinyApp(ui, server)