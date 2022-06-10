#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#'     
#' @import utils
#' @importFrom BiocManager install
#' @noRd
app_server <- function( input, output, session ) {
	# Your application server logic 
	mod_sets_server("sets_ui_1")
}
# @import shiny
