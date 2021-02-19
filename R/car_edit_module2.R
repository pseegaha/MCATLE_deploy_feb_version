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
car_edit_module2 <- function(input, output, session, modal_title, car_to_edit, modal_trigger) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        fluidRow(
          column(8, align="center", offset = 2,
                 textInput(ns("E_UID"), label="Event ID"),
                 #textInput(ns("E_UID"), label="Event ID","Input text",value=ifelse(is.null(hold),"", hold$E_UID)),
                 tags$style(type="text/css", "#E_UID { height: 50px; width: 100%; text-align:center; font-size: 30px;}")
          )
        ),
        fluidRow(
          column(8, align="center", offset = 2,
                 selectInput(ns('Event_Status'), 'Event Status',  choices = c('Creative', 'Dev','QA','Audience','Inflight','Pause','Complete','Relaunch'), selected = ifelse(is.null(hold), "", hold$Event_Status) ),
                 tags$style(type="text/css", "#Event_Status { height: 50px; width: 100%; text-align:center; font-size: 30px;}")
          )
        ),
        
        fluidRow( 
          #first col
          column(width=4,
                
              
                 
                # textInput(ns('E_UID'), 'Event Description',  value = ifelse(is.null(hold), "", hold$Event_Description) ),
                 #https://stackoverflow.com/questions/24175997/force-no-default-selection-in-selectinput
                 selectInput(ns('Event_Type'), 'Event Type',  choices = c('','Pricing', 'Outage', 'Release', 'Rollback','Breach'), selected = ifelse(is.null(hold), "", hold$Event_Type) ),
                 textInput(ns('Event_Name'), 'Event Name',  value = ifelse(is.null(hold), "", hold$Event_Name)),
                 textInput(ns('Event_Description'), 'Event Description',  value = ifelse(is.null(hold), "", hold$Event_Description) ),
                 selectInput(ns('BU_Sub_channel'), 'BU Sub channel',  choices = c('Social', 'Adobe','Paid Affliate','Paid Search','Paid Display','Organic'), selected = ifelse(is.null(hold), "", hold$BU_Sub_channel) ),
                  selectInput(
              ns('Marketing_channel'),
              'Marketing_channel',
              #choices=c('Cart','Landing Page','Store','NGM','Email','Store'),
              choices = NULL,
              selected = ifelse(is.null(hold), "", hold$Marketing_channel)
            ),
                 textInput(ns('Owner'), ' Owner',  value = ifelse(is.null(hold), "", hold$Owner) ),
            selectInput(ns('Geo'), 'Geo', choices = unique(Geo_Culture_Breakdown$Event_Geo), selected = ifelse(is.null(hold), "", hold$Geo) ),
            selectInput(ns('Culture'), 'Culture', choices=NULL, selected = ifelse(is.null(hold), "", hold$Culture) ),
            selectInput(ns('Affiliate'), 'Affiliate',  choices = NULL, selected = ifelse(is.null(hold), "", hold$Affiliate) ),
            
            selectInput(  ns('Fiscal_Quarter'), 'Fiscal Quarter', choices = 1:4,  selected = ifelse(is.null(hold), "", hold$Fiscal_Quarter) )
          ),
          
          #second col
          column(width=4,
                 selectInput(  ns('Fiscal_Year'), 'Fiscal Year', choices = 2014:as.numeric(format(Sys.Date(),"%Y")),  selected = ifelse(is.null(hold), "", hold$Fiscal_Year) ),
                 selectInput(ns('Customer_Type'), 'Customer_Type',  choices=NULL,  selected = ifelse(is.null(hold), "", hold$Customer_Type) ),
                 dateInput(ns('End_Date'), label = 'End Date: yyyy-mm-dd',  value = ifelse(is.null(hold), "", hold$End_Date), language = "en", min = NULL, max=NULL ),
                 dateInput(ns('Start_Date'), label = 'Start Date: yyyy-mm-dd',  value = ifelse(is.null(hold), "", hold$Start_Date), language = "en", min = NULL, max=NULL ),
                 numericInput(  ns('Impact_BPS'), 'Impact BPS', value = ifelse(is.null(hold), "", hold$Impact_BPS),  min=0, step=1 ),
                 numericInput(  ns('Impact_Revenue'), 'Impact Revenue', value = ifelse(is.null(hold), "", hold$Impact_Revenue),  min=0, step=1),
                 numericInput(  ns('Impact_Traffic'), 'Impact Traffic', value = ifelse(is.null(hold), "", hold$Impact_Traffic),  min=0, step=1),
                 selectInput(ns('Expiry_Date_Range'), 'Expiry Date Range',  choices = c('Pre-expiry', 'Post-expiry'), selected = ifelse(is.null(hold), "", hold$Expiry_Date_Range) ),
                 selectInput(ns('Expiry_Timeline'), 'Expiry Timeline',  choices = c('D30-D0', 'D30-D1'), selected = ifelse(is.null(hold), "", hold$Expiry_Timeline) ),
                 selectInput(ns('Existing_Segment'), 'Existing Segment', choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Existing_Segment) )
                 ),

          
          #third col
          column(width=4,
                 selectInput(ns('Existing_Campaign'), 'Existing Campaign',  choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Existing_Campaign) ),
                 selectInput(ns('Brief'), 'Brief',  choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Brief) ),
                 selectInput(ns('Event_Status'), 'Event Status',  choices = c('Creative', 'Dev','QA','Audience','Inflight','Pause','Complete','Relaunch'), selected = ifelse(is.null(hold), "", hold$Event_Status) ),
                 selectInput(ns('Idea_Completion_Status'), 'Idea Completion Status',  choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Idea_Completion_Status) ),
                 selectInput(ns('EManager_Status'), 'Event manager Status',  choices = c('Green Flag', 'Red Flag'), selected = ifelse(is.null(hold), "", hold$EManager_Status) ),
                 selectInput(ns('Idea_Validation_Status'), 'Idea Validation Status',  choices = c('yes', 'no'), selected = ifelse(is.null(hold), "", hold$Idea_Validation_Status) ),
                 selectInput(ns('Emanager_Comments'), 'Event Manager Comments',  choices = c('LP testing', 'Rollback Default'), selected = ifelse(is.null(hold), "", hold$Emanager_Comments) )
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

    # Observe event for "Event_Type" text input in Add/Edit Car Modal
    # `shinyFeedback`
#     observeEvent(input$Event_Type, {
#       if (input$Event_Type == "") {
#         shinyFeedback::showFeedbackDanger(
#           "Event_Type",
#           text = "Must enter Event_Type of car!"
#         )
#         shinyjs::disable('submit')
#       } else {
#         shinyFeedback::hideFeedback("Event_Type")
#         shinyjs::enable('submit')
#       }
#     })
# 
#   
#   observeEvent(input$Start_Date, {
#     if (is.null(input$Start_Date) ){
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
#   if (is.null(input$End_Date)) {
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
# 
      observe({

       # updateTextInput(session, "E_UID", value = paste0(substr(format(Sys.time(), "%H:%M:%S"), 4, 5),"_",substr(format(Sys.time(), "%H:%M:%S"), 7, 8)))

        updateTextInput(session, "E_UID", value = paste0(paste0("D",input$Journey_Start),paste0("M",-1*(input$Journey_Start)),"_",
                                                         paste0("D",input$Journey_End),paste0("M",-1*(input$Journey_End)),
                                                         input$BU_Info,"_",substr(input$Event_Name,1,3),"_",
                                                         input$Journey_Start,"_",
                                                         input$Journey_End,"_",
                                                         input$Marketing_channel,"_",
                                                         input$Geo,"_",
                                                         input$Culture))

      })

      
    
    toEUID <- reactive({
      list(input$Journey_Start,input$Journey_End,input$Marketing_channel,input$Geo,input$Culture)
    })
    
    # #Observe event for automatic update on the linked choices
    # observeEvent(toEUID(),{
    #   updateSelectInput(session,'E_UID',choices=input$E_UID)
    # }) 
    
    output$verb <- renderText({
                                paste0(

                                      input$BU_Info,"_",
                                      input$Event_Name,"_",
                                      input$Journey_Start,"_",
                                      input$Journey_End,"_",
                                      input$Marketing_channel,"_",
                                      input$Geo,"_",
                                      input$Culture,"_",
                                      round(runif(1, 1, 1000))
                                      )
      })
    
    toListen <- reactive({
      list(input$Start_Date,input$End_Date,input$Event_Type)
    })

observeEvent(toListen(), {
  if (!isTruthy(input$Start_Date) | !isTruthy(input$End_Date) | !isTruthy(input$Event_Type)){
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

observeEvent(input$Event_Type, {
  if (input$Event_Type == "") {
    shinyFeedback::showFeedbackDanger(
      "Event_Type",
      text = "Must enter Event_Type of Idea!"
    )
  } else {
    shinyFeedback::hideFeedback("Event_Type")
  }
})

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
        "Event_Type"=input$Event_Type,
        "Event_Name"=input$Event_Name,
        "Event_Description"=input$Event_Description,
        "BU_Sub_channel"=input$BU_Sub_channel,
        "Marketing_channel"=input$Marketing_channel,
        "Owner"=input$Owner,
        "Geo"=input$Geo,
        "Culture"=input$Culture,
        "Affiliate"=input$Affiliate,
        "Fiscal_Quarter"=input$Fiscal_Quarter,
        
        "Fiscal_Year"=input$Fiscal_Year,
        "Customer_Type"=input$Customer_Type,
        "Start_Date"=input$Start_Date,
        "End_Date"=input$End_Date,
        "Impact_BPS"=input$Impact_BPS,
        "Impact_Revenue"=input$Impact_Revenue,
        "Impact_Traffic"=input$Impact_Traffic,
        "Expiry_Date_Range"=input$Expiry_Date_Range,
        "Expiry_Timeline"=input$Expiry_Timeline,
        "Existing_Segment"=input$Existing_Segment,
        
        "Existing_Campaign"=input$Existing_Campaign,
        "Brief"=input$Brief,
        "Event_Status"=input$Event_Status,
        "Idea_Completion_Status"=input$Idea_Completion_Status,
        "EManager_Status"=input$EManager_Status,
        "Idea_Validation_Status"=input$Idea_Validation_Status,
        "Emanager_Comments"=input$Emanager_Comments,
        "E_UID"=input$E_UID
        
      )
    )

    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

    if (is.null(hold)) {
      # adding a new idea

      out$data$event_manager_created_at <- time_now
      out$data$Idea_created_by <- reactiveValuesToList(auth_out)$user
    } else {
      # Editing existing car

      out$data$event_manager_created_at <- as.character(hold$Idea_created_at)
      out$data$Idea_created_by <- hold$event_manager_created_by
    }

    out$data$event_manager_modified_at <- time_now
    out$data$event_manager_modified_by <- reactiveValuesToList(auth_out)$user

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
         Event_Type,
Event_Name,
Event_Description,
BU_Sub_channel,
Marketing_channel,
Owner,
Geo,
Culture,
Affiliate,
Fiscal_Quarter,

Fiscal_Year,
Customer_Type,
Start_Date,
End_Date,
Impact_BPS,
Impact_Revenue,
Impact_Traffic,
Expiry_Date_Range,
Expiry_Timeline,
Existing_Segment,

Existing_Campaign,
Brief,
Event_Status,
Idea_Completion_Status,
EManager_Status,
Idea_Validation_Status,
Emanager_Comments,
E_UID,

event_manager_created_at,
event_manager_created_by,
event_manager_modified_at,
event_manager_modified_by

        
        ) VALUES 
        ($1,$2,$3,$5,$6,$7,$10,$11,$12,$20,
        $21,$28,$29,$30,$33,$34,$35,$43,$44,$45,
        $46,$47,$70,$71,$73,$75,$78,$113,$123,$124,$125,
        $126,$130
        )",
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
          Event_Type=$1,
Event_Name=$2,
Event_Description=$3,
BU_Sub_channel=$5,
Marketing_channel=$6,
Owner=$7,
Geo=$10,
Culture=$11,
Affiliate=$12,
Fiscal_Quarter=$20,

Fiscal_Year=$21,
Customer_Type=$28,
Start_Date=$29,
End_Date=$30,
Impact_BPS=$33,
Impact_Revenue=$34,
Impact_Traffic=$35,
Expiry_Date_Range=$43,
Expiry_Timeline=$44,
Existing_Segment=$45,

Existing_Campaign=$46,
Brief=$47,
Event_Status=$70,
Idea_Completion_Status=$71,
EManager_Status=$73,
Idea_Validation_Status=$75,
Emanager_Comments=$78,
E_UID=$113,
event_manager_created_at=$123,
event_manager_created_by=$124,
event_manager_modified_at=$125,
event_manager_modified_by=$126
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
