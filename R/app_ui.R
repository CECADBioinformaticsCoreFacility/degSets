#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#'     
#' 
#' @import bs4Dash
#' @importFrom shiny span tagList
#' 
#' @noRd
app_ui <- function(request) {
	tagList(
		# Leave this function for adding external resources
		golem_add_external_resources(),
		# Your application UI logic 
		bs4Dash::dashboardPage(
			dark = NULL,
			## Animation on app start ---- 
			# covers initial renderUI lag
			preloader = list(html = waiter::spin_1(), color = "#333e48"),
			header = bs4Dash::dashboardHeader(
				#title = h1("degSets") #: Set Interesctions
				title = shiny::h3("degSets") #: Set Interesctions
			),
			## Left Side bar ----- 
			# used for navigation
			sidebar = bs4Dash::dashboardSidebar(
				id = "sidebar",
				disable = TRUE,
				collapsed = TRUE
			),
			body = mod_sets_ui_body("sets_ui_1"),
			controlbar = mod_sets_ui_controlbar("sets_ui_1")
			
		)
	)
}
#@import shiny 

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'degSets'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}
# @import shiny
