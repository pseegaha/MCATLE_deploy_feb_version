require(readr)
#BU_Affiliate_Breakdown<-read.csv("BU_Affiliate_Breakdown.csv",header = T)
BU_Affiliate_Breakdown<-read_csv("BU_Affiliate_Breakdown.csv")
Geo_Culture_Breakdown<-read_csv("Geo_Culture_Breakdown.csv")


#remotes::install_github("rstudio/shiny")
#source("BU_Affiliate_Breakdown.csv")
#source("Geo_Culture_Breakdown.csv")
#BU_Affiliate_Breakdown<-read.csv("/Users/pnaraya1/Documents/GitHub/MCATLE/www/BU_Affiliate_Breakdown.csv",header=T)
#Geo_Culture_Breakdown<-read.csv("/Users/pnaraya1/Documents/GitHub/MCATLE/www/Geo_Culture_Breakdown.csv",header=T)
#' Car Add & Edit Module
#'
#' Module to add & edit idea in the database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param car_to_edit reactive returning a 1 row data frame of the Idea to edit
#' from the table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
car_edit_module3 <- function(input, output, session, modal_title, car_to_edit, modal_trigger) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        fluidRow(
            column(width=3,                       
                 selectInput(  ns('Fiscal_Quarter'), 'Fiscal Quarter', choices = 1:4,  selected = ifelse(is.null(hold), "", hold$Fiscal_Quarter) ),

                 selectInput(  ns('Fiscal_Year'), 'Fiscal Year', choices = 2014:as.numeric(format(Sys.Date(),"%Y")),  selected = ifelse(is.null(hold), "", hold$Fiscal_Year) ),
                 
                 textInput(ns('Event_Name'), 'Event Name',  value = ifelse(is.null(hold), "", hold$Event_Name)),
                 
                 textInput(ns('Owner'), ' Owner',  value = ifelse(is.null(hold), "", hold$Owner) ),
                 
                 dateInput(ns('Start_Date'), label = 'Start Date: yyyy-mm-dd',  value = ifelse(is.null(hold), "", hold$Start_Date), language = "en", min = NULL, max=NULL ),
                 
                 dateInput(ns('End_Date'), label = 'End Date: yyyy-mm-dd',  value = ifelse(is.null(hold), "", hold$End_Date), language = "en", min = NULL, max=NULL ),
                 selectInput(ns('Event_Type'), 'Event Type',  choices = c('Test', 'Rollout','Seasonal'), selected = ifelse(is.null(hold), "", hold$Event_Type) ),
                 selectInput(ns('BU_Info'), 'BU Info',  choices = c('ATP', 'Direct','Retention'), selected = ifelse(is.null(hold), "", hold$BU_Info )),
                 selectInput(
                   ns('Marketing_channel'),
                   'Marketing_channel',
                   #choices=c('Cart','Landing Page','Store','NGM','Email','Store'),
                   choices = NULL,
                   selected = ifelse(is.null(hold), "", hold$Marketing_channel)
                 ),
                 selectInput(ns('BU_Sub_channel'), 'BU Sub channel',  choices = c('Social', 'Adobe','Paid Affliate','Paid Search','Paid Display','Organic'), selected = ifelse(is.null(hold), "", hold$BU_Sub_channel) )
                 
                 ),
          column(width=3,        
                 selectInput(ns('Customer_Type'), 'Customer_Type',  choices=NULL,  selected = ifelse(is.null(hold), "", hold$Customer_Type) ),
                 selectInput(ns('Variation'), 'Variation',  choices = c('a', 'b'), selected = ifelse(is.null(hold), "", hold$Variation) ),
                 selectInput(ns('Expiry_Timeline'), 'Expiry Timeline',  choices = c('D30-D0', 'D30-D1'), selected = ifelse(is.null(hold), "", hold$Expiry_Timeline) ),
                 selectInput(ns('Expiry_Date_Range'), 'Expiry Date Range',  choices = c('Pre-expiry', 'Post-expiry'), selected = ifelse(is.null(hold), "", hold$Expiry_Date_Range) ),
                 selectInput(ns('Affiliate'), 'Affiliate',  choices = NULL, selected = ifelse(is.null(hold), "", hold$Affiliate) ),
                 selectInput(ns('Geo'), 'Geo', choices = unique(Geo_Culture_Breakdown$Event_Geo), selected = ifelse(is.null(hold), "", hold$Geo) ),
                 selectInput(ns('Culture'), 'Culture', choices=NULL, selected = ifelse(is.null(hold), "", hold$Culture) ),
                 numericInput(  ns('Days_from_launch'), 'Days from launch', value = ifelse(is.null(hold), "", hold$Days_from_launch),  min=0, step=1 ),
                 numericInput(  ns('Days_to_go'), 'Days to go', value = ifelse(is.null(hold), "", hold$Days_to_go),  min=0, step=1 ),
                 numericInput(  ns('NPrimary_KPI'), 'Numero Primary KPI(%)', value = ifelse(is.null(hold), "", hold$NPrimary_KPI),  min=0, step=1 )
          ),
          column(width=3,        
                 numericInput(  ns('NSecondary_KPI'), 'Numero Secondary KPI(%)', value = ifelse(is.null(hold), "", hold$NSecondary_KPI),  min=0, step=1 ),
                 numericInput(  ns('NTertiry_KPI'), 'Numero Tertiary KPI(%)', value = ifelse(is.null(hold), "", hold$NTertiry_KPI),  min=0, step=1 ),
                 numericInput(  ns('Significance'), 'Significance', value = ifelse(is.null(hold), "", hold$Significance),  min=0, step=1 ),
                 selectInput(ns('Event_Status'), 'Event Status',  choices = c('Creative', 'Dev','QA','Audience','Inflight','Pause','Complete','Relaunch'), selected = ifelse(is.null(hold), "", hold$Event_Status) ),
                 numericInput(  ns('Impact_BPS'), 'Impact BPS', value = ifelse(is.null(hold), "", hold$Impact_BPS),  min=0, step=1 ),
                 selectInput(ns('Brief'), 'Brief',  choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Brief) ),
                 textInput(ns('Performance_Comments'), 'Performance Comments',  value = ifelse(is.null(hold), "", hold$Performance_Comments)),
                 selectInput(ns('Performance_Status'), 'Performance Status',  choices = c('Green Flag', 'Red Flag'), selected = ifelse(is.null(hold), "", hold$Performance_Status) ),
                 numericInput(  ns('ATP_BPS'), 'ATP BPS', value = ifelse(is.null(hold), "", hold$ATP_BPS),  min=0, step=1 ),
                 numericInput(  ns('Direct_BPS'), 'Direct BPS', value = ifelse(is.null(hold), "", hold$Direct_BPS),  min=0, step=1 )
          ),
          column(width=3,
                 numericInput(  ns('Adobe_BPS'), 'Adobe BPS', value = ifelse(is.null(hold), "", hold$Adobe_BPS),  min=0, step=1),
                 numericInput(  ns('Retention_BPS'), 'Retention BPS', value = ifelse(is.null(hold), "", hold$Retention_BPS),  min=0, step=1 ),
                 numericInput(  ns('Channel_Paid_BPS'), 'Channel Paid BPS', value = ifelse(is.null(hold), "", hold$Channel_Paid_BPS),  min=0, step=1),
                 selectInput(ns('Idea_Completion_Status'), 'Idea Completion Status',  choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Idea_Completion_Status) ),
                 selectInput(ns('Idea_Validation_Status'), 'Idea Validation Status',  choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Idea_Validation_Status) )
          )
        ),
        title = modal_title,
        size = 'm',
        footer = list(
          modalButton('Cancel'),
          actionButton(
            ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )
    
    toListen <- reactive({
      list(input$Start_Date,input$End_Date)
    })

    # Observe event for "Event_Type" text input in Add/Edit Car Modal
    # `shinyFeedback`
    # observeEvent(input$Event_Type, {
    #   if (input$Event_Type == "") {
    #     shinyFeedback::showFeedbackDanger(
    #       "Event_Type",
    #       text = "Must enter Event_Type of car!"
    #     )
    #     shinyjs::disable('submit')
    #   } else {
    #     shinyFeedback::hideFeedback("Event_Type")
    #     shinyjs::enable('submit')
    #   }
    # })

    
    observeEvent(toListen(), {
      if (!isTruthy(input$Start_Date) | !isTruthy(input$End_Date)){
        shinyjs::disable('submit')
      } else {
        shinyjs::enable('submit')
      }
    })
    
    observeEvent(input$Start_Date, {
      if (!isTruthy(input$Start_Date) ){
        shinyFeedback::showFeedbackDanger(
          "Start_Date",
          text = "Must enter Start_Date of Idea!"
        )
      } else {
        shinyFeedback::hideFeedback("Start_Date")
      }
    })
    
    
    observeEvent(input$End_Date, {
      if (!isTruthy(input$End_Date)) {
        shinyFeedback::showFeedbackDanger(
          "End_Date",
          text = "Must enter End_Date of Idea!"
        )
      } else {
        shinyFeedback::hideFeedback("End_Date")
      }
    })
    
#   observeEvent(input$Start_Date, {
#     if (!isTruthy(input$Start_Date) ){
#       shinyFeedback::showFeedbackDanger(
#         "Start_Date",
#         text = "Must enter Start_Date of car!"
#       )
#       shinyjs::disable('submit')
#     } else {
#       shinyFeedback::hideFeedback("Start_Date")
#       shinyjs::enable('submit')
#     }
#   })
#   
# 
# observeEvent(input$End_Date, {
#   if (!isTruthy(input$End_Date)) {
#     shinyFeedback::showFeedbackDanger(
#       "End_Date",
#       text = "Must enter End_Date of car!"
#     )
#     shinyjs::disable('submit')
#   } else {
#     shinyFeedback::hideFeedback("End_Date")
#     shinyjs::enable('submit')
#   }
# })

})
  
  
  #Observe event for automatic update on the linked choices
  observeEvent(input$BU_Info,{
    updateSelectInput(session,'Marketing_channel',
                      choices=unique(BU_Affiliate_Breakdown$Channel[BU_Affiliate_Breakdown$BU_Info==input$BU_Info]))
  }) 
  

  observeEvent(input$BU_Info,{
    updateSelectInput(session,'Affiliate',
                      choices=unique(BU_Affiliate_Breakdown$Affiliate[BU_Affiliate_Breakdown$BU_Info==input$BU_Info]))
  })
  
  observeEvent(input$BU_Info,{
    updateSelectInput(session,'Customer_Journey',
                      choices=unique(BU_Affiliate_Breakdown$Journey[BU_Affiliate_Breakdown$BU_Info==input$BU_Info]))
  })
  
  observeEvent(input$BU_Info,{
    updateSelectInput(session,'Customer_Type',
                      choices=unique(BU_Affiliate_Breakdown$Customer_type[BU_Affiliate_Breakdown$BU_Info==input$BU_Info]))
  })
  

  
  observeEvent(input$Geo,{
    updateSelectInput(session,'Culture',
                      choices=unique(Geo_Culture_Breakdown$Event_Culture[Geo_Culture_Breakdown$Event_Geo==input$Geo]))
  })

    # observeEvent(input$Event_Culture,{
    #   updateSelectInput(session,'Culture_base',
    #                     choices=unique(Geo_Culture_Breakdown$Culture_base[Geo_Culture_Breakdown$Event_Geo==input$Event_Geo & BU_Affiliate_Breakdown$Event_Culture==input$Event_Culture]))
    # }) 
    
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



  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      uid = if (is.null(hold)) NA else hold$uid,
      data = list(
        "Fiscal_Quarter"=input$Fiscal_Quarter,
        "Fiscal_Year"=input$Fiscal_Year,
        "Event_Name"=input$Event_Name,
        "Owner"=input$Owner,
        "Start_Date"=input$Start_Date,
        "End_Date"=input$End_Date,
        "Event_Type"=input$Event_Type,
        "BU_Info"=input$BU_Info,
        "Marketing_channel"=input$Marketing_channel,
        "BU_Sub_channel"=input$BU_Sub_channel,
        
        
        "Customer_Type"=input$Customer_Type,
        "Variation"=input$Variation,
        "Expiry_Timeline"=input$Expiry_Timeline,
        "Expiry_Date_Range"=input$Expiry_Date_Range,
        "Affiliate"=input$Affiliate,
        "Geo"=input$Geo,
        "Culture"=input$Culture,
        "Days_from_launch"=input$Days_from_launch,
        "Days_to_go"=input$Days_to_go,
        "NPrimary_KPI"=input$NPrimary_KPI,
        
        "NSecondary_KPI"=input$NSecondary_KPI,
        "NTertiry_KPI"=input$NTertiry_KPI,
        "Significance"=input$Significance,
        "Event_Status"=input$Event_Status,
        "Impact_BPS"=input$Impact_BPS,
        "Brief"=input$Brief,
        "Performance_Comments"=input$Performance_Comments,
        "Performance_Status"=input$Performance_Status,
        "ATP_BPS"=input$ATP_BPS,
        "Direct_BPS"=input$Direct_BPS,
        
        "Adobe_BPS"=input$Adobe_BPS,
        "Retention_BPS"=input$Retention_BPS,
        "Channel_Paid_BPS"=input$Channel_Paid_BPS,
        "Idea_Completion_Status"=input$Idea_Completion_Status,
        "Idea_Validation_Status"=input$Idea_Validation_Status
      )
    )

    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

    if (is.null(hold)) {
      # adding a new idea

      out$data$perf_manager_created_at <- time_now
      out$data$perf_manager_created_by <- reactiveValuesToList(auth_out)$user
    } else {
      # Editing existing car

      out$data$perf_manager_created_at <- as.character(hold$perf_manager_created_at)
      out$data$perf_manager_created_by <- hold$perf_manager_created_by
    }

    out$data$perf_manager_modified_at <- time_now
    out$data$perf_manager_modified_by <- reactiveValuesToList(auth_out)$user

    out
  })

  validate_edit <- eventReactive(input$submit, {
    dat <- edit_car_dat()

    # Logic to validate inputs...

    dat
  })

  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()

    tryCatch({

      if (is.na(dat$uid)) {
        # creating a new idea
        uid <- uuid::UUIDgenerate()

        dbExecute(
          conn,
          "INSERT INTO mtcars (uid,
Fiscal_Quarter,
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
         
perf_manager_created_at,
perf_manager_created_by,
perf_manager_modified_at,
perf_manager_modified_by) VALUES 
        ($20,$21,$2,$7,$29,$30,$1,$4,$6,$5,$28,$53,$44,$43,$12,$10,$11,$54,$55,$25,$26,$27,$56,$70,$33,$47,$79,$74,
                          $48,$49,$50,$51,$52,$71,$75,$126,$127,$128,$129,$130)",
          params = c(
            list(uid),
            unname(dat$data)
          )
        )
      } else {
        # editing an existing idea
        dbExecute(
          conn,
          "UPDATE mtcars SET 
         Fiscal_Quarter=$20,
Fiscal_Year=$21,          
Event_Name=$2,
Owner=$7,
Start_Date=$29,
End_Date=$30,          
Event_Type=$1,
BU_Info=$4,
Marketing_channel=$6,
BU_Sub_channel=$5,

Customer_Type=$28,
Variation=$53,
Expiry_Timeline=$44,
Expiry_Date_Range=$43,
Affiliate=$12,
Geo=$10,
Culture=$11,
Days_from_launch=$54,
Days_to_go=$55,        
NPrimary_KPI=$25,

NSecondary_KPI=$26,
NTertiry_KPI=$27,          
Significance=$56,
Event_Status=$70,
Impact_BPS=$33,
Brief=$47,
Performance_Comments=$79,
Performance_Status=$74,
ATP_BPS=$48,
Direct_BPS=$49,

Adobe_BPS=$50,
Retention_BPS=$51,
Channel_Paid_BPS=$52,
Idea_Completion_Status=$71,
Idea_Validation_Status=$75,
         
perf_manager_created_at=$126,
perf_manager_created_by=$127,
perf_manager_modified_at=$128,
perf_manager_modified_by=$129
         WHERE uid=$130",
          params = c(
            unname(dat$data),
            list(dat$uid)
          )
        )
      }

      session$userData$mtcars_trigger(session$userData$mtcars_trigger() + 1)
      showToast("success", paste0(modal_title, " Successs"))
    }, error = function(error) {

      msg <- paste0(modal_title, " Error")


      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
  })

}
