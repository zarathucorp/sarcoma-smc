## Shiny error log option
options(shiny.sanitize.errors = F)

source("global.R")              ## Load global.R
library(shinycustomloader)      ## Loading image
library(DT)                     ## datatable 
library(jsmodule) ## my package: include xxxUI, xxxmodule, xxxModule
library(survival);library(jskm)

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

ui <- navbarPage("DP",
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("binary_check"),
                              uiOutput("binary_var"),
                              uiOutput("binary_val"),
                              uiOutput("ref_check"),
                              uiOutput("ref_var"),
                              uiOutput("ref_val"),
                              uiOutput("subset_check"),
                              uiOutput("subset_var"),
                              uiOutput("subset_val")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                              )
                            )
                          )
                 ),
                 tabPanel("Table 1", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
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
                 tabPanel("Logistic regression",
                          sidebarLayout(
                            sidebarPanel(
                              regressModuleUI("logistic")
                            ),
                            mainPanel(
                              withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                            )
                          )
                 ),
                 tabPanel("Kaplan-meier plot",
                          sidebarPanel(
                            radioButtons("event_kap", "Outcome", choices = varlist$Event, selected = varlist$Event[1], inline = T),
                            sliderInput("year_kap", "Cut month",  min = 0 , max = 120, value = c(0, 24)),
                            selectInput("group_kap", "Group", varlist[names(varlist)[1:2]], selected = "DP", multiple = F),
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
                              radioButtons("dep_cox", "Outcome", choices = varlist$Event, selected = varlist$Event[1], inline = T),
                              sliderInput("year_cox", "Cut month", min = 0 , max = 120, value = c(0, 24)),
                              selectInput("cov_cox", "Covariates", choices = varlist[names(varlist)[1:2]], selected = c("DP", "Age", "Sex"), multiple = T),
                              checkboxInput("step_cox", "Backward stepwise selection", value = F)
                            ),
                            mainPanel(
                              withLoader(DTOutput("coxtable"), type="html", loader="loader6")
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
  output$binary_check <- renderUI({
    checkboxInput("check_binary", "Make binary variables")
  })
  
  output$ref_check <- renderUI({
    checkboxInput("check_ref", "Change reference of categorical variables")
  })
  
  
  output$subset_check <- renderUI({
    checkboxInput("check_subset", "Subset data")
  })
  
  
  observeEvent(input$check_binary, {
    var.conti <- setdiff(names(out), factor_vars)
    output$binary_var <- renderUI({
      req(input$check_binary == T)
      selectInput("var_binary", "Variables to dichotomize",
                  choices = var.conti, multiple = T,
                  selected = var.conti[1])
    })
    
    output$binary_val <- renderUI({
      req(input$check_binary == T)
      req(length(input$var_binary) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_binary)){
        med <- stats::quantile(out[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = T)
        outUI[[v]] <- splitLayout(cellWidths = c("25%", "75%"),
                                  selectInput(paste0("con_binary", v), paste0("Define reference:"),
                                              choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"
                                  ),
                                  numericInput(paste0("cut_binary", v), input$var_binary[[v]],
                                               value = med[2], min = med[1], max = med[3]
                                  )
        )
        
      }
      outUI
      
    })
  })
  
  
  observeEvent(input$check_ref, {
    var.factor <- factor_vars
    output$ref_var <- renderUI({
      req(input$check_ref == T)
      selectInput("var_ref", "Variables to change reference",
                  choices = var.factor, multiple = T,
                  selected = var.factor[1])
    })
    
    output$ref_val <- renderUI({
      req(input$check_ref == T)
      req(length(input$var_ref) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_ref)){
        outUI[[v]] <- selectInput(paste0("con_ref", v), paste0("Reference: ", input$var_ref[[v]]),
                                  choices = levels(factor(out[[input$var_ref[[v]]]])), selected = levels(factor(out[[input$var_ref[[v]]]]))[2])
        
      }
      outUI
      
    })
  })
  
  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      #factor_subset <- c(data.list$factor_original, input$factor_vname)
      
      #validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      #)
      
      tagList(
        selectInput("var_subset", "Subset variables",
                    choices = names(out), multiple = T,
                    selected = names(out)[1])
      )
    })
    
    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(length(input$var_subset) > 0)
      var.factor <- factor_vars
      
      outUI <- tagList()
      
      for (v in seq_along(input$var_subset)){
        if (input$var_subset[[v]] %in% var.factor){
          varlevel <- levels(as.factor(out[[input$var_subset[[v]]]]))
          outUI[[v]] <- selectInput(paste0("val_subset", v), paste0("Subset value: ", input$var_subset[[v]]),
                                    choices = varlevel, multiple = T,
                                    selected = varlevel[1])
        } else{
          val <- stats::quantile(out[[input$var_subset[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(paste0("val_subset", v), paste0("Subset range: ", input$var_subset[[v]]),
                                    min = val[1], max = val[5],
                                    value = c(val[2], val[4]))
        }
        
      }
      outUI
    })
  })
  
  data.info <- reactive({
    out1 <- out[, .SD]
    out1[, (conti_vars) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = conti_vars]
   
    out.label1 <- out.label[, .SD]
    #out.label[, var_label := ref[out.label$variable, name.old]]
    
    req(!is.null(input$check_binary))
    if (input$check_binary == T){
      validate(
        need(length(input$var_binary) > 0 , "No variables to dichotomize")
      )
      sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
      names(sym.ineq) <- sym.ineq[4:1]
      sym.ineq2 <- c("le", "ge", "l", "g")
      names(sym.ineq2) <- sym.ineq
      for (v in seq_along(input$var_binary)){
        req(input[[paste0("con_binary", v)]])
        req(input[[paste0("cut_binary", v)]])
        if (input[[paste0("con_binary", v)]] == "\u2264"){
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) <= input[[paste0("cut_binary", v)]]))]
        } else if (input[[paste0("con_binary", v)]] == "\u2265"){
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) >= input[[paste0("cut_binary", v)]]))]
        } else if (input[[paste0("con_binary", v)]] == "\u003c"){
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) < input[[paste0("cut_binary", v)]]))]
        } else{
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) > input[[paste0("cut_binary", v)]]))]
        }
        
        cn.new <- paste0(input$var_binary[[v]], "_group_", sym.ineq2[input[[paste0("con_binary", v)]]], input[[paste0("cut_binary", v)]])
        data.table::setnames(out1, "BinaryGroupRandom", cn.new)
        
        label.binary <- mk.lev(out1[, .SD, .SDcols = cn.new])
        label.binary[, var_label := paste0(input$var_binary[[v]], " _group")]
        label.binary[, val_label := paste0(c(input[[paste0("con_binary", v)]], sym.ineq[input[[paste0("con_binary", v)]]]), " ", input[[paste0("cut_binary", v)]])]
        out.label1 <- rbind(out.label1, label.binary)
      }
      
    }
    
    if (!is.null(input$check_ref)){
      if (input$check_ref){
        validate(
          need(length(input$var_ref) > 0 , "No variables to change reference")
        )
        for (v in seq_along(input$var_ref)){
          req(input[[paste0("con_ref", v)]])
          out1[[input$var_ref[[v]]]] <- stats::relevel(out1[[input$var_ref[[v]]]], ref = input[[paste0("con_ref", v)]])
          out.label1[variable == input$var_ref[[v]], ':='(level = levels(out1[[input$var_ref[[v]]]]), val_label = levels(out1[[input$var_ref[[v]]]]))]
        }
        
      }
    }
    
    
    if (!is.null(input$check_subset)){
      if (input$check_subset){
        validate(
          need(length(input$var_subset) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting")
        )
        var.factor <- factor_vars
        #var.conti <- setdiff(data()$conti_original, input$factor_vname)
        
        for (v in seq_along(input$var_subset)){
          if (input$var_subset[[v]] %in% var.factor){
            out1 <- out1[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
            out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
            data.table::setkey(out.label1, "variable", "class", "level")
            data.table::setkey(out.label2, "variable", "class", "level")
            out.label1 <- out.label1[out.label2]
          } else{
            out1 <- out1[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
            out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
            data.table::setkey(out.label1, "variable", "class", "level")
            data.table::setkey(out.label2, "variable", "class", "level")
            out.label1 <- out.label1[out.label2]
          }
        }
        
      }
    }
    #for (vn in ref[["name.new"]]){
    #  w <- which(ref[["name.new"]] == vn)
    #  out.label1[variable == vn, var_label := ref[["name.old"]][w]]
    #}
    
    return(list(data = out1, label = out.label1))
  })
  
  
  data <- reactive({
    data.info()$data
  })
  
  data.label <- reactive(data.info()$label)
  
  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
              options = c(jstable::opt.data("data"), list(scrollX = TRUE))
    )
  })
  
  
  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
              options = c(jstable::opt.data("label"), list(scrollX = TRUE))
    )
  })
  

  
  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit, showAllLevels = T)
  
  output$table1 <- renderDT({
    tb <- out_tb1()$table
    cap <- out_tb1()$caption
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
  
  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = reactive(varlist[names(varlist)[c(2, 1, 3)]]), nfactor.limit = nfactor.limit)
  
  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == "sig")
    datatable(out_logistic()$table, rownames=T, extensions= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
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

  
}



shinyApp(ui, server)