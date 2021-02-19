#' Cars Table Module UI/Repo Summary
#'
#' The UI portion of the module for displaying the datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
require(rpivotTable)
cars_table_module_ui4 <- function(id) {
  ns <- NS(id)

  tagList(
    
      fluidRow(div(style="display: inline-block;vertical-align:top; ", 
                   box(
                     title = "Settings",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,width = 4, 
                     style = 'display:block;width:100%;overflow-y: scroll;',
                     useShinyalert(),br(),
                     selectInput(
                       inputId = ns("airlines"),
                       label = "Event Status:",
                      choices = c('ALL',unique(ram$Event_Status)),
                       size = 10,selectize = FALSE,
                       selected = "ALL"
                     ),
                     downloadButton(ns('describe_download'),"Download Report",class="butt" ),br(),
                     tags$head(tags$style(".butt{background-color:#4ee4ff;} .butt{color: #e6ebef;}")),
                     radioButtons(ns('format'), 'Document format', c('PDF', 'Word'),
                                  inline = TRUE),
                     
                     
                     fluidRow(
                       column(
                         width = 12,
                         helpText("Note: Any comments made in the box will be printed if you download the summary report.")) ,
                       column(
                         width = 12,
                         tags$textarea(
                           "Please using any **markdown** syntax!",
                           id    = ns('markdowninput'),
                           rows  = 3,
                           style = 'width:100%;')),
                       column(
                         width = 12,
                         helpText(ns("Preview:")),
                         htmlOutput(ns('htmlmarkdown'))))
                   ),
                   
                   # Show a plot of the generated distribution
                   
                   
                   div(style="display: inline-block;vertical-align:top; ", 
                       box(solidHeader = TRUE,status = "primary", title="Bar & Donut Plots",collapsible = TRUE,width = NULL,
                           style = 'display:block;width:100%;overflow-y: scroll;',
                                                                                
                                                                                
                                                                                div(style="display: inline-block;vertical-align:top; width: 300px;",
                                                                                    checkboxInput(ns("OneMore"), label = h5("Show and Report donut Chart?"),T)),
                                                                                
                                                                                
                                                                                div(style="display: inline-block;vertical-align:top; width: 300px;",
                                                                                    checkboxInput(ns("thirdmore"), label = h5("Show and Report Pivot Interactive Chart?"),T)),
                                                                                
                                                                                div(style="display: inline-block;vertical-align:top; width: 300px;",
                                                                                    checkboxInput(ns("twomore"), label = h5("Show and Report SourceTable Chart?"),T)),
                                                                                
                                                                                
                                                                                fluidRow(
                                                                                  column(
                                                                                    width = 6,
                                                                                    d3Output(ns("airbar"))%>%
                                                                                      withSpinner(color="#0275D8",type=6,size=1,color.background="#ffffff")
                                                                                  ),
                                                                                  
                                                                                  div(id=ns('Hide_pie_graph'),
                                                                                      column(
                                                                                        width = 6,            
                                                                                        box(solidHeader = TRUE, status = "primary",title="Donut Chart",collapsible = TRUE,width = NULL, style = 'display:block;width:100%;overflow-y: scroll;',
                                                                                            nivopieOutput(ns("airpie"))%>%
                                                                                              withSpinner(color="#0275D8",type=6,size=1,color.background="#ffffff"))
                                                                                      )
                                                                                  )
                                                                                  
                                                                                )
                   )))
      ),
      hr(),
      fluidRow(

        div(id=ns('Hide_Pivot_table'),
            column(
              width = 12,
              box(solidHeader = TRUE,status = "primary",
                  title="Pivot Table",collapsible = TRUE,width = NULL, style = 'display:block;width:100%;overflow-y: scroll;'
                  , tags$head(
                    tags$style(
                      HTML(
                        ".realGone { background-color: #F08080 !important; }"
                      )
                    )
                  ),
                  tags$script('$(window).load(function(){
    var i = setInterval(function() {
        if ($(".pvtVal").length) {
            clearInterval(i);

            $(".pvtVal").each(function(index) {

                var value = parseInt($(this).text());

                if (value < 12) {
                    $(this).addClass("expired");
                } else if (value > 12 && value < 14) {
                    $(this).addClass("dead");
                } else {
                    $(this).addClass("realGone");
                }
            });
        }
    }, 100);
});
'),
                  tags$script(src="pivot.js"),
                  rpivotTableOutput(ns("test")) %>%
                    withSpinner(color="#0275D8",type=6,size=1,color.background="#ffffff")
              )
            )
        )

      ),

      hr(),

      fluidRow(

        div(id=ns('Hide_D3_table'),
            column(
              width = 12,
              box(solidHeader = TRUE,status = "primary",
                  title="Source Table",collapsible = TRUE,width = NULL, style = 'display:block;width:100%;overflow-y: scroll;',

                  d3tfOutput(ns('car_tabler2')) %>%
                    withSpinner(color="#0275D8",type=6,size=1,color.background="#ffffff")
              )
            ))
      )
      ,

      hr() , tags$script(src = "bar_plot.js")
      )
  

  
}

#' Cars Table Module Server/Repo Summary
#'
#' The Server portion of the module for displaying the datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

check_for_update <- function() {
  dbGetQuery(conn, "SELECT MAX(event_manager_created_at) FROM mtcars") # edit this part in case
}
get_data <- function() {
  dbGetQuery(conn, "select * from mtcars")
}

cars_table_module4 <- function(input, output, session) {

  observe(
    out <- reactivePoll(500, session, checkFunc = check_for_update, valueFunc = get_data))

  auth_out<- secure_server(
    check_credentials = check_credentials(
      db="credentials.sqlite",
      passphrase = "supersecret"
    ),timeout=0,
    inputs_list = list(group=list(fun="selectInput",
                                  args=list(choices=c("all","restricted"),
                                            multiple=TRUE,
                                            selected=c("all","restricted")
                                  )
    ))
  )
 

  #######################################################
  #######################################################
  # trigegr to reload data from the table
   session$userData$mtcars_trigger <- reactiveVal(0)
 
  # Read in table from the database
  cars3 <- reactive({
    session$userData$mtcars_trigger()
    req(reactiveValuesToList(auth_out)$user)
    if (reactiveValuesToList(auth_out)$user == "manager"){ 
      out <- NULL
      tryCatch({
        out <- conn %>%
          tbl('mtcars') %>%select(         Event_Status,
                                           Event_Type,
                                           BU_Info,
                                           Marketing_channel,
                                           Geo,
                                           Fiscal_Quarter,
                                           Fiscal_Year,
                                           Impact_BPS,
                                           Impact_Revenue,Ranking,
                                           everything())%>% collect()%>%
          mutate(
            event_manager_created_at = as.POSIXct(event_manager_created_at, tz = "UTC"),
            event_manager_modified_at = as.POSIXct(event_manager_modified_at, tz = "UTC")
          ) %>%rowwise() %>%filter(!is.na(Event_Status)) %>%
          
          arrange(desc(event_manager_modified_at))
      }, error = function(err) {
        
        
        msg <- "Database Connection Error"
        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      })
      
      out}
    else { 
      out <- NULL
      tryCatch({
        out <- conn %>%
          tbl('mtcars') %>%select(         Event_Status,
                                           Event_Type,
                                           BU_Info,
                                           Geo,
                                           Marketing_channel,
                                           Fiscal_Quarter,
                                           Fiscal_Year,
                                           Impact_BPS,
                                           Impact_Revenue,Ranking,
                                           everything())%>% collect() %>%
          mutate(
            event_manager_created_at = as.POSIXct(event_manager_created_at, tz = "UTC"),
            event_manager_modified_at = as.POSIXct(event_manager_modified_at, tz = "UTC")
          )%>% filter(event_manager_modified_by==reactiveValuesToList(auth_out)$user) %>%
          rowwise() %>%filter(!is.na(Ranking)) %>%filter(!is.na(Event_Status)) %>%
          arrange(desc(event_manager_modified_at))
      }, error = function(err) {
        
        
        msg <- "Database Connection Error"
        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      })
      
      out}
  })
  
  
  cars4 <- reactive({
    req(cars3())
    out <- cars3()
    tab<-out%>%as.data.frame()%>%select(c(Event_Status,Event_Type,
                   BU_Info,
                   Geo,
                   Marketing_channel,
                   Fiscal_Quarter,
                   Fiscal_Year,
                   Impact_BPS,
                   Impact_Revenue,Ranking))
    
    
    tab
  })
  

 
  
  useShinyjs()
  
  
  observe({
    shinyjs::toggle(id = "Hide_pie_graph", condition = input$OneMore,anim = TRUE, animType = "fade")
  })
  
  observe({
    shinyjs::toggle(id = "Hide_D3_table", condition = input$twomore,anim = TRUE, animType = "fade")
  })
  
  observe({
    shinyjs::toggle(id = "Hide_Pivot_table", condition = input$thirdmore,anim = TRUE, animType = "fade")
  })
  
  output$htmlmarkdown = reactive({
    note_in_html(input$markdowninput)
  })
  
  sel_flights <- reactive({
    req(cars4())
    ram <- cars4()
    if (input$airlines!= "ALL") ram <- filter(ram, Event_Status == input$airlines)
    ram
  })
  
  bar_graphD3=reactive({
    req(cars4())
    req(sel_flights())
    ram <- cars4()
    grouped <- ifelse(input$airlines!= "ALL", expr(Fiscal_Year), expr(Event_Status))
    
    flightdata <- sel_flights() %>%
      group_by(!!grouped) %>%
      tally() %>%
      collect() %>%
      mutate(
        y = n,
        x = !!grouped
      ) %>%
      select(x, y)
    
    flightdata <- flightdata %>%
      mutate(label = x)
    
    r2d3(flightdata, r2d3_file)
  })
  
  
  pie_graph=reactive({
    grouped2 <- ifelse(input$airlines != "ALL", expr(Event_Type), expr(Event_Status))
    
    flightdata2 <- sel_flights() %>%
      group_by(!!grouped2) %>%
      tally() %>%
      collect() %>%
      mutate(
        value = n,
        id = !!grouped2
      ) %>%
      select(id, value)
    
    flightdata3=data.frame(flightdata2)
    flightdata3$id=as.factor(flightdata3$id)
    nivopie(flightdata3, innerRadius=0.5, cornerRadius=5, fit=T,sortByValue=T,
            colors='paired', enableRadialLabels=F, radialLabelsLinkDiagonalLength=1,
            radialLabelsLinkHorizontalLength=8,
            enableSlicesLabels=T, sliceLabel='id',isInteractive=T)
    
  })

  output$airbar = renderD3({
    bar_graphD3()
  })
  
  output$airpie=renderNivopie({
    pie_graph()
  })
  
  
  output$car_tabler2 <- renderD3tf({
    req(cars4())
    ram <- cars4()
    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = TRUE,      rows_counter = TRUE,  
      rows_counter_text = "Rows: ",
      on_keyup = TRUE,  
      on_keyup_delay = 800,
      filters_row_index = 1,
      # alphabetic sorting for the row names column, numeric for all other columns
      col_types = c("string", rep("number", ncol(ram)))
    );
    
    d3tf(ram,
         tableProps = tableProps,
         extensions <-  list(
           list(name = "sort")
         ),         enableTf = TRUE,
         
         
         showRowNames = T,
         tableStyle = "table table-bordered");
  })
  
  
  output$test <- renderRpivotTable({
    req(cars4())
    ram <- cars4()
    rpivotTable(data = ram)          })

  
  
  output$describe_download = downloadHandler(
    filename<- function(){
      paste("Summary",Sys.Date(),switch(
        input$format, PDF = '.pdf', Word = '.docx'
      ),sep = "")
    },
    
    content = function(file) {
      if (input$format=="PDF"){
        #### Progressing indicator
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report.Rmd', pdf_document())
                       file.rename(out, file)
                       
                     })
        ### below is the end of pdf content
      }else{
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report_word.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report_word.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report_word.Rmd', word_document())
                       file.rename(out, file)
                     })
      }
      
    })
  
  
  
  
}
