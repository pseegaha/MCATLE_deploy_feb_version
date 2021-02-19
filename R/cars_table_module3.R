#' Cars Table Module UI/Performance Manager
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

cars_table_module_ui3 <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 2,
        actionButton(
          ns("add_car"),
          "Add",
          class = "btn-success",
          style = "color: #fff;",
          icon = icon('plus'),
          width = '100%'
        ),
        tags$br(),
        tags$br()
      )
    ),
    fluidRow(
      column(
        width = 12,
        title = "MCATLE",
        DTOutput(ns('car_tabler')) %>%
          withSpinner(color="#0275D8",type=6,size=1,color.background="#ffffff"),
        tags$br()
      )
    ),
    hr() ,
    fluidRow(
      column(
        width = 12,
        title = "MCATLE",
        box(solidHeader = TRUE,status = "primary",
            title="Source Table",collapsible = TRUE,width = NULL, style = 'display:block;width:100%;overflow-y: scroll;',
        d3tfOutput(ns('car_tabler3')) %>%
          withSpinner(color="#0275D8",type=6,size=1,color.background="#ffffff")),
        hr() ,
        box(solidHeader = TRUE,status = "primary",
            title="Pivot Table",collapsible = TRUE,width = NULL, style = 'display:block;width:100%;overflow-y: scroll;',
        rpivotTableOutput(ns("test"))),
      )
    ),
    hr() ,
    tags$script(src = "cars_table_module.js"),
    tags$script(paste0("cars_table_module_js('", ns(''), "')"))
  )
}

#' Cars Table Module Server/Performance Manager
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
  dbGetQuery(conn, "SELECT MAX(perf_manager_created_at) FROM mtcars") # edit this part in case

}
get_data <- function() {
  dbGetQuery(conn, "select * from mtcars")
}

cars_table_module3 <- function(input, output, session) {

  # trigegr to reload data from the table
  session$userData$mtcars_trigger <- reactiveVal(0)

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
  # Read in table from the database
  cars <- reactive({
    
    session$userData$mtcars_trigger()
    
    out <- NULL
    req(reactiveValuesToList(auth_out)$user)
    if (reactiveValuesToList(auth_out)$user == "manager"){
    tryCatch({
      out <- conn %>%
        tbl('mtcars') %>%
        collect() %>%
        mutate(
          perf_manager_created_at = as.POSIXct(perf_manager_created_at, tz = "UTC"),
          perf_manager_modified_at = as.POSIXct(perf_manager_modified_at, tz = "UTC")
        ) %>%
        arrange(desc(perf_manager_modified_at))
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
      tryCatch({
        out <- conn %>%
          tbl('mtcars') %>%
          collect() %>%
          mutate(
            perf_manager_created_at = as.POSIXct(perf_manager_created_at, tz = "UTC"),
            perf_manager_modified_at = as.POSIXct(perf_manager_modified_at, tz = "UTC")
          ) %>% filter(perf_manager_modified_by==reactiveValuesToList(auth_out)$user) %>%
          arrange(desc(perf_manager_modified_at))
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
      
      out
    }
 } )
  
  
  car_table_prep <- reactiveVal(NULL)
  
  observeEvent(cars(), {
    out <- cars()
    
    ids <- out$uid
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })
    
    # Remove the `uid` column. We don't want to show this column to the user
    out <- out %>%
      select(-uid)
    
    # Set the Action Buttons row to the first column of the `mtcars` table
    out <- cbind(
      tibble(" " = actions),
      out
    )
    
    if (is.null(car_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      car_table_prep(out)
      
    } else {
      
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(car_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
      
    }
  })
  
  # observeEvent(input$foobar, {
  #   req(car_table_prep())
  #   out <- car_table_prep()
  #   replaceData(car_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
  #     })
  # observeEvent(input$foobar, {
  # 
  #   session$userData$mtcars_trigger<-session$userData$mtcars_trigger*10
  # })
  
  output$car_table <- renderDT({
    req(car_table_prep())
    out <- car_table_prep()
    
    datatable(
      out,
      rownames = FALSE,
      colnames = c(
        'Event_Type',
        'Event_Name',
        'Event_Description',
        'BU_Info',
        'BU_Sub_channel',
        'Marketing_channel',
        'Owner',
        'Event_Objective',
        'Event_Hypothesis',
        'Geo',
        'Culture',
        'Affiliate',
        'Impact_Population_Percentage',
        'Expected_Lift',
        'Journey_Start',
        'Journey_End',
        'Development_Effort',
        'Additional_Effort',
        'Data_Backup',
        'Fiscal_Quarter',
        'Fiscal_Year',
        'Primary_KPI',
        'Secondary_KPI',
        'Tertiry_KPI',
        'NPrimary_KPI',
        'NSecondary_KPI',
        'NTertiry_KPI',
        'Customer_Type',
        'Start_Date',
        'End_Date',
        'Package',
        'Previous_Analysis_Insight',
        'Impact_BPS',
        'Impact_Revenue',
        'Impact_Traffic',
        'Customer_Journey',
        'Impact_Score',
        'Effort_Score',
        'Confidence',
        'Prioritization_Score',
        'Ranking',
        'Journey_Expiry',
        'Expiry_Date_Range',
        'Expiry_Timeline',
        'Existing_Segment',
        'Existing_Campaign',
        'Brief',
        'ATP_BPS',
        'Direct_BPS',
        'Adobe_BPS',
        'Retention_BPS',
        'Channel_Paid_BPS',
        'Variation',
        'Days_from_launch',
        'Days_to_go',
        'Significance',
        'Customer_Feedback',
        'Customer_Problem_Validation',
        'Customer_Benefit_Validation',
        'Customer_Solution_Validation',
        'Article_BestPractice_Competitor_Reference',
        'Data_Backup_Docs',
        'Customer_Feedback_Docs',
        'Customer_Problem_Validation_Docs',
        'Customer_Benefit_Validation_Docs',
        'Customer_Solution_Validation_Docs',
        'Study_Reference_Docs',
        'Previous_Analysis_Insight_Docs',
        'Moment_of_Truth',
        'Event_Status',
        'Idea_Completion_Status',
        'Prioritization_Status',
        'EManager_Status',
        'Performance_Status',
        'Idea_Validation_Status',
        'Idea_Comments',
        'Priority_Comments',
        'Emanager_Comments',
        'Performance_Comments',
        'Lower_BPS',
        'Upper_BPS',
        'Est_ATP_BPS_upr',
        'Est_ATP_BPS_lwer',
        'Est_ATP_BPS_mean',
        'Est_Direct_BPS_upr',
        'Est_Direct_BPS_lwer',
        'Est_Direct_BPS_mean',
        'Est_Adobe_BPS_upr',
        'Est_Adobe_BPS_lwer',
        'Est_Adobe_BPS_mean',
        'Est_Retention_BPS_upr',
        'Est_Retention_BPS_lwer',
        'Est_Retention_BPS_mean',
        'Est_Channel_Paid_BPS_upr',
        'Est_Channel_Paid_BPS_lwer',
        'Est_Channel_Paid_BPS_mean',
        'Real_ATP_BPS_upr',
        'Real_ATP_BPS_lwer',
        'Real_ATP_BPS_mean',
        'Real_Direct_BPS_upr',
        'Real_Direct_BPS_lwer',
        'Real_Direct_BPS_mean',
        'Real_Adobe_BPS_upr',
        'Real_Adobe_BPS_lwer',
        'Real_Adobe_BPS_mean',
        'Real_Retention_BPS_upr',
        'Real_Retention_BPS_lwer',
        'Real_Retention_BPS_mean',
        'Real_Channel_Paid_BPS_upr',
        'Real_Channel_Paid_BPS_lwer',
        'Real_Channel_Paid_BPS_mean',
        'E_UID' ,
        'Exception_Flag',
        'Idea_created_at',
        'Idea_created_by',
        'Idea_modified_at',
        'Idea_modified_by',
        'priority_created_at',
        'priority_created_by',
        'priority_modified_at',
        'priority_modified_by',
        'event_manager_created_at',
        'event_manager_created_by',
        'event_manager_modified_at',
        'event_manager_modified_by',
        'perf_manager_created_at',
        'perf_manager_created_by',
        'perf_manager_modified_at',
        'perf_manager_modified_by'
      ),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      extensions = c("FixedColumns"),
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs=list(list(className="dt-center",targets="_all"),
                        list(visible=F, targets=c(0)),
                        list(targets = 0, orderable = FALSE)
        ),
        fixedColumns=list(leftColumns=3),
        
        dom = 'Bftip',
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("mtcars-", Sys.Date()),
            exportOptions = list(
              columns = 1:(length(out) - 1)
            )
          )
        ),
        columnDefs = list(
          list(targets = 0, orderable = FALSE)
        )
      )
    ) %>%
      formatDate(
        columns = c("perf_manager_created_at", "perf_manager_modified_at"),
        method = 'toLocaleString'
      )
    
  })
  car_table_proxy <- DT::dataTableProxy('car_table')
  
  # observe({
  #   replaceData(car_table_proxy, cars(), resetPaging = FALSE)
  # })
  
  
  callModule(
    car_edit_module3,
    "add_car",
    modal_title = "Add Idea",
    car_to_edit = function() NULL,
    modal_trigger = reactive({input$add_car})
  )
  
  car_to_edit <- eventReactive(input$car_id_to_edit, {
    
    cars() %>%
      filter(uid == input$car_id_to_edit)
  })
  
  callModule(
    car_edit_module3,
    "edit_car",
    modal_title = "Edit Idea",
    car_to_edit = car_to_edit,
    modal_trigger = reactive({input$car_id_to_edit})
  )
  
  car_to_delete <- eventReactive(input$car_id_to_delete, {
    
    out <- cars() %>%
      filter(uid == input$car_id_to_delete) %>%
      as.list()
  })
  
  
  callModule(
    car_delete_module,
    "delete_car",
    modal_title = "Delete Idea",
    car_to_delete = car_to_delete,
    modal_trigger = reactive({input$car_id_to_delete})
  )

  #######################################################
  #######################################################

  # Read in table from the database
  cars1 <- reactive({
    session$userData$mtcars_trigger()
    req(reactiveValuesToList(auth_out)$user)
    if (reactiveValuesToList(auth_out)$user == "manager"){ 
      out <- NULL
      tryCatch({
        out <- conn %>%
          tbl('mtcars') %>%select(        Fiscal_Quarter,
                                          Fiscal_Year,          
                                          Event_Name,
                                          Owner,
                                          Start_Date,
                                          End_Date,          
                                          Event_Type,
                                          BU_Info,
                                          Marketing_channel,
                                          BU_Sub_channel,
                                          
                                          Customer_Type,
                                          Variation,
                                          Expiry_Timeline,
                                          Expiry_Date_Range,
                                          Affiliate,
                                          Geo,
                                          Culture,
                                          Days_from_launch,
                                          Days_to_go,        
                                          NPrimary_KPI,
                                          
                                          NSecondary_KPI,
                                          NTertiry_KPI,          
                                          Significance,
                                          Event_Status,
                                          Impact_BPS,
                                          Brief,
                                          Performance_Comments,
                                          Performance_Status,
                                          ATP_BPS,
                                          Direct_BPS,
                                          
                                          Adobe_BPS,
                                          Retention_BPS,
                                          Channel_Paid_BPS,
                                          Idea_Completion_Status,
                                          Idea_Validation_Status,
                                         everything())%>% collect()%>%
          mutate(
            perf_manager_created_at = as.POSIXct(perf_manager_created_at, tz = "UTC"),
            perf_manager_modified_at = as.POSIXct(perf_manager_modified_at, tz = "UTC")
          ) %>%rowwise() %>%filter(!is.na(Event_Status)) %>%
          arrange(desc(perf_manager_modified_at))
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
          tbl('mtcars') %>%select(        Fiscal_Quarter,
                                          Fiscal_Year,          
                                          Event_Name,
                                          Owner,
                                          Start_Date,
                                          End_Date,          
                                          Event_Type,
                                          BU_Info,
                                          Marketing_channel,
                                          BU_Sub_channel,
                                          
                                          Customer_Type,
                                          Variation,
                                          Expiry_Timeline,
                                          Expiry_Date_Range,
                                          Affiliate,
                                          Geo,
                                          Culture,
                                          Days_from_launch,
                                          Days_to_go,        
                                          NPrimary_KPI,
                                          
                                          NSecondary_KPI,
                                          NTertiry_KPI,          
                                          Significance,
                                          Event_Status,
                                          Impact_BPS,
                                          Brief,
                                          Performance_Comments,
                                          Performance_Status,
                                          ATP_BPS,
                                          Direct_BPS,
                                          
                                          Adobe_BPS,
                                          Retention_BPS,
                                          Channel_Paid_BPS,
                                          Idea_Completion_Status,
                                          Idea_Validation_Status,everything())%>% collect() %>%
          mutate(
            perf_manager_created_at = as.POSIXct(perf_manager_created_at, tz = "UTC"),
            perf_manager_modified_at = as.POSIXct(perf_manager_modified_at, tz = "UTC")
          )%>% filter(perf_manager_modified_by==reactiveValuesToList(auth_out)$user) %>%
          rowwise() %>%filter(!is.na(Event_Status)) %>%
          arrange(desc(perf_manager_modified_at))
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
  
  
  car_tabler_prep <- reactiveVal(NULL)
  
  observeEvent(cars1(), {
    out <- cars1()
    
    ids <- out$uid
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })
    
    # Remove the `uid` column. We don't want to show this column to the user
    out <- out %>%
      select(-uid)
    
    # Set the Action Buttons row to the first column of the `mtcars` table
    out <- cbind(
      tibble(" " = actions),
      out
    )
    
    if (is.null(car_tabler_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      car_tabler_prep(out)
      
    } else {
      
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(car_tabler_proxy, out, resetPaging = FALSE, rownames = FALSE)
      
    }
  })
  
  output$car_tabler <- renderDT({
    req(car_tabler_prep())
    out <- car_tabler_prep()
    datatable(
      out,
      rownames = FALSE,
      # colnames = c(
      #   'Fiscal_Quarter',
      #   'Fiscal_Year',          
      #   'Event_Name',
      #   'Owner',
      #   'Start_Date',
      #   'End_Date',          
      #   'Event_Type',
      #   'BU_Info',
      #   'Marketing_channel',
      #   'BU_Sub_channel',
      #   
      #   'Customer_Type',
      #   'Variation',
      #   'Expiry_Timeline',
      #   'Expiry_Date_Range',
      #   'Affiliate',
      #   'Geo',
      #   'Culture',
      #   'Days_from_launch',
      #   'Days_to_go',        
      #   'NPrimary_KPI',
      #   
      #   'NSecondary_KPI',
      #   'NTertiry_KPI',          
      #   'Significance',
      #   'Event_Status',
      #   'Impact_BPS',
      #   'Brief',
      #   'Performance_Comments',
      #   'Performance_Status',
      #   'ATP_BPS',
      #   'Direct_BPS',
      #   
      #   'Adobe_BPS',
      #   'Retention_BPS',
      #   'Channel_Paid_BPS',
      #   'Idea_Completion_Status',
      #   'Idea_Validation_Status',
      #   '36','37','38','39','40','41',
      #   '42','43','44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59',
      #   '60','61','62','63','64','65','66','67','68','69','70','71','72','73','74','75','76','77','78',
      #   '79','80','81','82','83','84','85','86','87','88','89','90','91','92','93','94','95','96','97','98','99','100',
      #   '111','112','113','114','115','116','117','118','119','120','121',
      #   'perf_manager_created_at',
      #   'perf_manager_created_by',
      #   'perf_manager_modified_at',
      #   'perf_manager_modified_by'
      #   
      #   
      # ),
      selection = "none",
      class = "compact stripe row-border nowrap", filter = "top",callback=JS("
           //hide column filters for the first column
          $.each([0], function(i, v) {
                $('input.form-control').eq(v).hide()
              });"),
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      extensions = c("Buttons", "FixedColumns","ColReorder"),
      options = list(
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all"),
          list(visible=F, targets=c(36:126)),
          list(targets = 0, orderable = FALSE)
        ),
        #buttons = I('colvis'),
        
        fixedColumns = list(leftColumns = 2),
        # disable realtime updating
        colReorder = list(realtime = FALSE),
        scrollX = TRUE
        # dom = 'Bftip',
        # buttons = list(
        #   list(
        #     extend = "excel",
        #     text = "Download",
        #     title = paste0("mtcars-", Sys.Date()),
        #     exportOptions = list(
        #       columns = 1:(length(out) - 1)
        #     )
        #   )
        # ),
        
      )
    ) %>%
      formatDate(columns = c("perf_manager_created_at", "perf_manager_modified_at"),
                 method = 'toLocaleString')
    
  })
  
  
  car_tabler_proxy <- DT::dataTableProxy('car_tabler')
  ###########################################################################################
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
          rowwise() %>%filter(!is.na(Event_Status)) %>%
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
    out<-out%>%as.data.frame()%>%select(c(Event_Status,Event_Type,
                        BU_Info,
                        Geo,
                        Marketing_channel,
                        Fiscal_Quarter,
                        Fiscal_Year,
                        Impact_BPS,
                        Impact_Revenue,Ranking))
    
    
    out
  })
  
  
  output$car_tabler3 <- renderD3tf({
    req(cars4())
    out <- cars4()

    tableProps <- list(
      btn_reset = TRUE, btn_reset = TRUE,      rows_counter = TRUE,  
      rows_counter_text = "Rows: ",
      on_keyup = TRUE,  
      on_keyup_delay = 800,
      filters_row_index = 1,
      # alphabetic sorting for the row names column, numeric for all other columns
      col_types = c("string", rep("number", ncol(out)))
    );
    
    d3tf( out,
          tableProps = tableProps,
          extensions <-  list(
            list(name = "sort")
          ),enableTf = TRUE,
          showRowNames = TRUE,
          tableStyle = "table table-bordered");
  })
  
  
  output$test <- renderRpivotTable({
    req(cars4())
    out <- cars4()
    rpivotTable(data = out)})
  
  
  

}
