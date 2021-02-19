
require(readr)
require(rpivotTable)


source("R/cars_table_module2.R")
source("R/cars_table_module3.R")
source("R/cars_table_module4.R")

source("R/car_edit_module2.R")
source("R/car_edit_module3.R")

source("R/car_delete_module.R")
#source("R/EUID_v5.R")




# UI ----------------------------------------------------------------------
jscode <- '
shinyjs.init = function() {
$(".nav").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}'


ui <- fluidPage(
  
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  extendShinyjs(text = jscode, functions = "init"),
  use_sever(),
  useShinydashboard(),
  use_noty(),
  use_push(),
  use_notiflix_notify(),
  tagList(
    tags$head(HTML('<link rel="icon", href="large-logo-mcafee-dark.png", type="image/png" />)'),
              HTML("<title>MCATLE</title>")),
    
  ),
  tags$style(
    type = 'text/css',
    '.modal-dialog { width: fit-content !important; }'
  ),
  
  # 1.1.0 JUMBOTRON COMPONENT ----
  div(
    class = "container-fluid",
    style = "padding:0;",
    id = "jumbotron",
    
    div( # component
      class = "jumbotron",
      style = "background-image:url('large-logo-mcafee-dark.png');  background-repeat: no-repeat;
 background-size: 40% 100%;background-position: center center;margin-bottom:0;",
      
      div(
        class = "jumbotron-ui-box text-default bg-primary bg-default",
        style = "color:white; background-color:rgba(0,0,0,0.5); padding:20px;",
        h1("McAfee Consumer Automated Test and Learn Ecosystem
",align = 'center',
           style = "color: white;"),
      )
    )
  ),
  bsModal("modalExample", "Instructional Video", "button", size = "medium" ,
          p("Tutorial"),
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/pDQTAFnScxk" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  ),
  ##
tags$head(tags$style(HTML("
.navbar-nav {
  float: none;
}
.navbar ul > li:nth-child(4) {
  float: right;
}
.navbar ul > li:nth-child(4) {
  color: black !important;
}
                       "))),
  navbarPage( 
    title=div(img(src="large-logo-mcafee-dark.png", width = 30,height=30), ""),
    id = "tabs",
tabPanel("Event Manager",
         icon = icon("list-alt"),
         cars_table_module_ui2("cars_table2")
)
,
tabPanel("Performance Manager",
         icon = icon("list-alt"),
         cars_table_module_ui3("cars_table3")
)
,
tabPanel("Repository Summary",
         icon = icon("list-alt")
         ,cars_table_module_ui4("cars_table4")
)
,
tabPanel(tags$ul(class='nav navbar-nav',
                   style = "padding-left: 5px; float: right;", htmlOutput("user")))

  )
)


#inactivity js below will timeout authentication page after 5 seconds
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 5000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 5000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

#change auth ui background ?
ui <- secure_app(
  #head_auth = tags$script(inactivity),
  ui,
  use_sever(),
  use_waiter(),
  use_steward(),
  waiter_show_on_load(spin_hexdots()),

  # Choose a new theme
  theme = shinythemes::shinytheme("flatly"),


  background  =
    "url('https://upload.wikimedia.org/wikipedia/commons/5/59/McAfee_logo_%282017%29.svg')no-repeat top fixed;",
  enable_admin = TRUE,
  choose_language = TRUE,
  tags_top =
    tags$div(
      tags$h4("Consumer Marketing Project", style = "align:center"),
      tags$img(
        src = 'mcafee-7873.png', width = 100
      )
    ),
  # add information on bottom ?
  tags_bottom = tags$div(
    tags$p(
      "For any questions, please  contact ",
      tags$a(
        href = "mailto:pruthvi_narayanaswamy@mcafee.com?Subject=Shiny%20aManager",
        target="_top", "administrator"
      )
    )
  )


)


