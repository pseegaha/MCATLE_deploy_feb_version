server <- function(input, output, session) { 
  
  # user session$userData to store user data that will be needed throughout
  session$userData$mtcars_trigger <- reactiveVal(0)
  
  # Loader ----
  sever()
  Sys.sleep(1) 

  waiter_hide()

    # Call secure_server() with DB info
  auth_out <- secure_server(
    check_credentials = check_credentials(
      "credentials.sqlite",
      passphrase = "supersecret"
    ), timeout = 0,
    inputs_list = list(group = list(fun = "selectInput",
                                    args = list(choices = c("all", "restricted"),
                                                multiple = TRUE,
                                                selected = c("all", "restricted")
                                    )
    )
    )
  )
  
  observe({
    noty(text = paste0(reactiveValuesToList(auth_out)$user," is logged in successfully"), type = "info")
  }) 
  
  observe({
    toggleClass(condition = input$foo,
                class = "disabled",
                selector = ".navbar ul > li:nth-child(4)")
  })
  
  output$user <- renderUI({
    
    HTML(paste0(h4(reactiveValuesToList(auth_out)$user)))
  })

  
  isManager <- reactive({
    if (reactiveValuesToList(auth_out)$user == "manager"){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }) 
  

  # #tab3 Event Manager
  callModule(
    cars_table_module2,
    "cars_table2"
  )

  # #tab4 Performance Manager
  callModule(
    cars_table_module3,
    "cars_table3"
  )
  # #tab5 Repo Summary
  callModule(
    cars_table_module4,
    "cars_table4"
  )

}
