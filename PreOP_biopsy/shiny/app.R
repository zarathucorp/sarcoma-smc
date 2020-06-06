## Shiny error log option
options(shiny.sanitize.errors = F)

source("global.R")              ## Load global.R
library(shinycustomloader)      ## Loading image
library(DT)                     ## datatable 
library(jsmodule) ## my package: include xxxUI, xxxmodule, xxxModule

## 범주 21 이상인 변수는 아예 빼버림
nfactor.limit <- 21




ui <- navbarPage("PreOP biopsy",
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
                 tabPanel("Kaplan-meier plot",
                          sidebarPanel(
                            kaplanUI("kaplan")
                          ),
                          mainPanel(
                            optionUI("kaplan"),
                            withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                            ggplotdownUI("kaplan")
                          )
                          
                 ),
                 tabPanel("Cox model",
                          sidebarLayout(
                            sidebarPanel(
                              coxUI("cox")
                            ),
                            mainPanel(
                              withLoader(DTOutput("coxtable"), type="html", loader="loader6")
                            )
                          )
                          
                 )
                 
)


server <- function(input, output, session) {
  
  ## Data: Module에서 읽으려면 reactive 형태여야 함 
  data <- reactive({
    out
  })
  
  ## Label : 마찬가지
  data.label <- reactive(out.label)
  
  
  ## Table 1
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
  

  
  ## KM plot
  out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit)
  
  output$kaplan_plot <- renderPlot({
    print(out_kaplan())
  })
  
  
  ## Cox model
  out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = reactive(varlist), default.unires = T, nfactor.limit = nfactor.limit)
  
  output$coxtable <- renderDT({
    hide <- which(colnames(out_cox()$table) == c("sig"))
    datatable(out_cox()$table, rownames=T, extensions= "Buttons", caption = out_cox()$caption,
              options = c(opt.tbreg(out_cox()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets= hide))
                          )
              )
    )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })

  
}



shinyApp(ui, server)