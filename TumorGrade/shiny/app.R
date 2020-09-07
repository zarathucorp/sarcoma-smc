## Shiny error log option
options(shiny.sanitize.errors = F)

source("global.R")
library(shiny)
library(DT)
library(shinycustomloader) 
library(survival)
library(data.table)
library(jskm)

vars.tb1 <- c("Age","Sex","Height","Weight",
              "underlying_DM","underlying_HTN","underlying_CAD","underlying_CRD",
              "Histology_primary","FNCLCCgrade_primary","Necrosis_primary","Mitosis_primary", "Necrosis_firstLR", "Mitosis_firstLR", 
              "Histology_firstLR","FNCLCCgrade_firstLR",
              "RTx","Chemo","Day_FU","Day_LR")

ui<-navbarPage(
    "Tumor Grade",
    tabPanel("Table1",
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("group_tb1","Group",  c("2 group", "3 group"), "3 group", inline = T),
                 ),
                 mainPanel(
                     DTOutput("table1")
                 )
             )
    ),
    
    tabPanel("Kaplan-meier plot",
             sidebarPanel(
                 radioButtons("event_kap", "Outcome", choices = c("Death", "SecondLR"), selected = "Death", inline =T),
                 selectInput("group_kap", "Group", "2 group", selected = "2 group", multiple = F),
                 sliderInput("ylims_kap", "Y axis ranges", min = 0, max = 1, value = c(0, 1), step = 0.05)
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
                     radioButtons("dep_cox", "Outcome", choices = c("Death", "SecondLR"), selected = "Death", inline = T),
                     selectInput("cov_cox", "Covariates", choices = setdiff(c("Group", "Group2", vars.tb1), c("Day_FU", "Day_LR")), 
                                 selected = c("Group2", "Age", "Sex"), multiple = T),
                     checkboxInput("step_cox", "Backward stepwise selection", value = F)
                 ),
                 mainPanel(
                     withLoader(DTOutput("coxtable"), type="html", loader="loader6")
                 )
             )
             
    ),
    fluid=TRUE
)
server<-function(input,output,session){
    
    strata.tb1 <- reactive({
        switch(input$group_tb1, 
               "2 group" = "Group2",
               "3 group" = "Group") 
    })
    
    
    output$table1<-renderDT({
        tb1 <- CreateTableOneJS(vars.tb1, strata = strata.tb1(), data = out, nonnormal = c("Day_FU","Day_LR"), Labels = T, labeldata = out.label)
        tb <- tb1$table
        cap <- tb1$caption
        out.tb1 <- datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                             options = c(jstable::opt.tb1("tb1"),
                                         list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                         ),
                                         list(scrollX = TRUE)
                             )
        )
        if ("sig" %in% colnames(tb)){
            out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
        }
        return(out.tb1)
    })
    
    
    
    
    
    
    obj.km <- reactive({
        req(input$group_kap)
        req(input$event_kap)
        
        data <- out
        label <- out.label
        
        
        var.event <- input$event_kap
        data[[var.event]] <- as.numeric(as.character(data[[var.event]]))
        var.day <- switch(var.event, "Death" = "Day_FU", "SecondLR" = "Day_SecondLR")
        var.group <- "Group2"
        
        form <- as.formula(paste("Surv(", var.day, ",", var.event, ") ~ ", var.group, sep = ""))
        
        res.kap <- survfit(form, data = data)
        res.kap$call$formula <- form
        
        p <- jskm(res.kap, xlabs = "Days", cumhaz = F,  ylims = input$ylims_kap, marks = F, 
                  ystrataname = label[variable == var.event, var_label][1], ystratalabs = label[variable == var.group][level %in% levels(data[[var.group]]), val_label],
                  legendposition = c(0.2, 0.45), timeby = 365,
                  surv.scale = "percent", mark = F, pval = T, table = T, data = data)
        
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
        data <- data.table(out)[, .SD]
        label <- out.label
        
        var.event <- input$dep_cox
        var.day <- switch(var.event, "Death" = "Day_FU", "SecondLR" = "Day_SecondLR")
        
        
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

shinyApp(ui,server)