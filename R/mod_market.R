#' Market UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @export
mod_market_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(10,dygraphs::dygraphOutput(ns("price_plot")))
  )
}

mod_market_input = function(id){
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("region"),"Region",choices=c("Kigoma")),
    shiny::selectInput(ns("district"),"District",
                choices=c("Kigoma","Kasulu","Buhigwe","Kakonko","Uvinza","Kibondo")),
    shiny::selectInput(ns("ward"),"Ward",choices=NULL),
    shiny::selectInput(ns("product"),"Product",choices=NULL),
  )
}
    
#' Market Server Functions
#' @importFrom dplyr filter
#' @noRd 
mod_market_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data = shiny::reactive(price_long())
    
    ward = reactive({
      dplyr::filter(data(),District==input$district)
    })
    
    ward_select = reactive({
      input$ward
    })
    
    district_select = reactive({
      input$district
    })
    
    product_select = reactive({
      input$product
    })
    
    observeEvent(ward(), {
      choices = unique(ward()$Ward)
      prod_choices = unique(data()$name)
      shiny::freezeReactiveValue(input,"ward")
      shiny::updateSelectInput(inputId = "ward", choices = choices) 
      shiny::updateSelectInput(inputId = "product", choices = prod_choices)
    })
    
    output$price_plot = dygraphs::renderDygraph({
      req(input$ward)
      
      price_dygraph(
        data(),
        dstrct = district_select(),
        wrd = ward_select(),
        product = product_select()
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_market_ui("Market_ui_1")
    
## To be copied in the server
# mod_market_server("Market_ui_1")

market_app = function(){
  
  ui = shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Market Information System"),
    shinydashboard::dashboardSidebar(
      mod_market_input("market")
    ),
    shinydashboard::dashboardBody(
      mod_market_ui("market") 
    )
  )
  
  server = function(input,output,session){
    mod_market_server("market")
  }
  
  shiny::shinyApp(ui, server)
}

