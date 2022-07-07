#' sets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fluidRow uiOutput
#' @importFrom upsetjs upsetjsOutput
#' @importFrom bs4Dash dashboardBody box
mod_sets_ui_body <- function(id){
	ns <- NS(id)
	bs4Dash::dashboardBody(
		fluidRow(
			### UpSet plot box ----
			bs4Dash::box(
				width = 6,
				title = shiny::h3("UpSet plot"),
				solidHeader = FALSE,
				status = "primary",
				# h4(textOutput("upset_plot_selected")),
				#"Box body",
				id = ns("upset_plot_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				#actionButton("reset_selection", "Reset Selection"),
				upsetjs::upsetjsOutput(ns("upset_plot"))
			),
			### Venn plot box ----
			bs4Dash::box(
				width = 6,
				title = shiny::h3("Venn Diagram"),
				solidHeader = FALSE,
				status = "primary",
				#"Box body",
				id = ns("venn_diagram_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				upsetjs::upsetjsOutput(ns("venn_diagram"))
			)
		),
		### DGE results tabset output ----
		fluidRow(
			uiOutput(outputId = ns("dge_res"))
		)
	)
}

#' mod_sets_ui_controlbar
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList uiOutput fileInput textInput numericInput uiOutput verbatimTextOutput
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom bs4Dash bs4DashControlbar controlbarItem
mod_sets_ui_controlbar <- function(id) {
	ns <- NS(id)
	## Right sidebar ----
	bs4Dash::bs4DashControlbar(
		id = ns("controlbar"),
		width = 400,
		collapsed = FALSE,
		pinned = TRUE,
		overlay = FALSE,
		### UI for main Inputs ----
		bs4Dash::controlbarItem(
			title = "Select Comparisons",
			#textOutput("n"),
			#textOutput("venn_click_names"),
			#verbatimTextOutput("venn_click_names"),
			#textOutput("mode"),
			fileInput(
				ns("results_annotated_min_cov_grp"),
				"file", multiple = FALSE, accept = ".csv"
			),
			textInput(
				ns("species"), "Species", value = "Homo Sapiens",
				placeholder =  "Homo Sapiens"
			),
			numericInput(
				ns("padj"), "Adjusted p-value", value = 0.05,
				min = 0, max = 1, step = 0.001
			),
			shinyWidgets::radioGroupButtons(
				inputId = ns("directional"),
				label = "Direction of Change", 
				selected = "Either",
				choices = c("Up-regulated", "Either", "Down Regulated"),
				status = "primary"
			),
			uiOutput(ns("filtering_ui")),
			# numericInput(
			# 	"minlog2FoldChange", "log2(Fold Change)", value = -1,
			# 	min = min_lfc, max = max_lfc, step = 0.1
			# ),
			# numericInput(
			# 	"maxlog2FoldChange", "log2(Fold Change)", value = 1,
			# 	min = min_lfc, max = max_lfc, step = 0.1
			# ),
			# selectizeInput(
			# 	"comparisons", "Comparisons", multiple = TRUE,
			# 	selected = head(
			# 		all_comparisons,
			# 		2
			# 	),
			# 	choices = all_comparisons
			# ),
			uiOutput(ns("set_selector"))
			
		)
	)
}



#' sets Server Functions
#'
#' @noRd 
#' @importFrom dplyr %>% if_else group_keys group_split
#' @importFrom bs4Dash tabBox 
#' @importFrom rlang .data
#' @importFrom shiny reactive renderUI tagList numericInput selectizeInput
#' selectInput req reactiveValues renderText observeEvent observe moduleServer
#' @importFrom shinyWidgets pickerInput pickerOptions
mod_sets_server <- function(id) {
	moduleServer( id, function(input, output, session) {
		ns <- session$ns
		
		# Server ----
		# File processing ----
		options(shiny.maxRequestSize = Inf) # Do not limit file size
		results_annotated_min_cov_grp <- reactive({
			readr::read_csv(
				input$results_annotated_min_cov_grp$datapath,
				show_col_types = FALSE
			)
		})
		
		direction_filter <- reactive({
			switch(
				input$directional,
				"Up-regulated" = \(x) dplyr::filter(
					x, .data$log2FoldChange > 0
				),
				"Either" = \(x) x, 
				"Down Regulated" = \(x) dplyr::filter(
					x, .data$log2FoldChange < 0
				)
			)
		})
		
		# min_lfc <- reactive(
		# 	floor(min(results_annotated_min_cov_grp()$log2FoldChange))
		# )
		# max_lfc <- reactive(
		# 	ceiling(max(results_annotated_min_cov_grp()$log2FoldChange))
		# )
		# 
		# all_comparisons <- reactive(
		# 	unique(results_annotated_min_cov_grp()$comparison)
		# )
		
		output$filtering_ui <- renderUI({
			#req(input$results_annotated_min_cov_grp)
			req(input$results_annotated_min_cov_grp$datapath)
			
			min_lfc <- floor(
				min(results_annotated_min_cov_grp()$log2FoldChange)
			)
			max_lfc <- ceiling(
				max(results_annotated_min_cov_grp()$log2FoldChange)
			)
			
			all_comparisons <- unique(
				results_annotated_min_cov_grp()$comparison
			)
			
			tagList(
				numericInput(
					ns("minlog2FoldChange"),
					"Down-regulated threshold log2(Fold Change)", value = -1,
					#min = min_lfc(), max = max_lfc(), step = 0.1
					min = min_lfc, max = max_lfc, step = 0.1
				),
				numericInput(
					ns("maxlog2FoldChange"),
					"Up-regulated threshold log2(Fold Change)", value = 1,
					#min = min_lfc(), max = max_lfc(), step = 0.1
					min = min_lfc, max = max_lfc, step = 0.1
				),
				selectizeInput(
					ns("comparisons"), "Comparisons", multiple = TRUE,
					# selected = head(all_comparisons(), 2),
					# choices = all_comparisons()
					selected = head(all_comparisons, 2),
					choices = all_comparisons
				)
			)
		})
		
		## Dark/light Mode toggle state ----
		output$mode <- reactive(input$dark_mode)
		
		## p-value and LFC filtering ----
		significant_genes_by_comparison <- reactive({
			req(
				input$results_annotated_min_cov_grp,
				input$minlog2FoldChange,
				input$maxlog2FoldChange,
				input$comparisons
			)
			
			# all
			results_annotated_min_cov_grp() %>%
				dge_filters(
					direction_filter(), input$padj,
					input$minlog2FoldChange, input$maxlog2FoldChange
				) %>%
				dplyr::filter(.data$comparison %in% input$comparisons)
		})
		
		## Gene Names Set List ----
		significant_genes_by_comparison_lst <- reactive({
			significant_genes_by_comparison() %>%
				get_significant_genes_by_comparison_lst()
		})
		
		## Number of genes by set ----
		number_of_significant_genes_by_comparison <- reactive({
			significant_genes_by_comparison() %>% 
				dplyr::count()
		})
		
		significant_genes_by_comparison_set_matrix <- reactive({
			significant_genes_by_comparison_lst() %>%
				ComplexHeatmap::list_to_matrix()
		})
		
		intersection_selected_sets <- reactive({
			req(input$set_2_highlight)
			get_intersection_genes(
				significant_genes_by_comparison_set_matrix(),
				set_combinations()[[input$set_2_highlight]]
			)
		})
		
		## Selected Comparisons ----
		significant_genes_by_comparison_lst_subset <- reactive({
			req(input$comparisons)
			significant_genes_by_comparison_lst()[input$comparisons]
		})
		
		## set combinations ----
		set_combinations <- reactive({
			# req(input$results_annotated_min_cov_grp)
			# req(significant_genes_by_comparison_lst_subset())
			set_list_2_combinations(
				significant_genes_by_comparison_lst_subset()
			)
		})
		
		selected_comparison <- reactiveValues(comparison = NULL)
		observeEvent(c(input$upset_plot_click, input$venn_diagram_click), {
			#req(input$upset_plot_click, input$venn_diagram_click)
			selected_comparison$comparison <- set_selected_set_2_inputs(
					selected_comparison$comparison,
					input$upset_plot_click$name, input$venn_diagram_click$name
				)
		})
		
		###
		#debug
		 #output$venn_click_names <- renderText(paste(input$venn_diagram_click$name))
		###
		
		output$set_selector <- renderUI({
			#req(input$results_annotated_min_cov_grp)
			# selectInput(
			# 	selectize = TRUE,
			# 	ns("set_2_highlight"), "Set to highlight",
			# 	choices = names(set_combinations()),
			# 	#selected = input$upset_plot_click$name
			# 	selected = selected_comparison$comparison
			# )
			# 
			shinyWidgets::pickerInput(
				ns("set_2_highlight"), "Set to highlight",
				choices = names(set_combinations()),
				selected = selected_comparison$comparison,
				multiple = FALSE,
				options = shinyWidgets::pickerOptions(
					actionsBox = TRUE, liveSearch = TRUE, size = 5
				)
			)
			
			
		})
		
		## named list containing tibble of genes in selected comparisons 
		# named_gene_list_tibble <- reactive({
		# 	lst <- list(bind_cols_fill(
		# 		significant_genes_by_comparison_lst_subset()
		# 	))
		# 	names(lst) <- paste0(input$comparisons, collapse = "&") 
		# 	lst
		# })
		
		## number of selected comparisons ----
		n_comparisons_selected <- reactive({ length(input$comparisons) })
		
		too_many_comparisons <- reactive({
			n_comparisons_selected() > 5
		})
		
		## too many sets for venn diagram ----
		observe({
			req(input$set_2_highlight, input$comparisons)
			if(isTRUE(too_many_comparisons())) {
				updateBox(
					ns("venn_diagram_box"), action = c("update"),
					options = list(
						status = "warning",
						title = "Venn Diagram Cannot display more the 5 sets!"
					)
				)
			} else {
				updateBox(
					ns("venn_diagram_box"), action = c("update"),
					options = list(
						status = "primary",
						title = paste(
							"Venn Diagram",
							set_combinations()[[input$set_2_highlight]]
						)
					)
				)
			}
		})
		
		## UpSet Plot render ----
		output$upset_plot <- upsetjs::renderUpsetjs({
			req(input$set_2_highlight) #input$dark_mode
			gen_upset_plot(
				significant_genes_by_comparison_lst_subset(),
				set_combinations(),
				input$set_2_highlight
			)
		})
		
		## Venn diagram render ----
		output$venn_diagram <- upsetjs::renderUpsetjs({
			req(input$set_2_highlight) # input$dark_mode
			gen_venn(
				significant_genes_by_comparison_lst_subset(),
				set_combinations(),
				input$set_2_highlight
			)
		})
		
		## selected condition gene list render ----
		# output$gene_sets <- DT::renderDataTable(
		# 	DT::datatable(
		# 		significant_genes_by_comparison_lst() %>% 
		# 			purrr::map(~paste0(.x, collapse = ", ")) %>% 
		# 			as.data.frame() %>% 
		# 			t() %>% 
		# 			as.data.frame() %>% 
		# 			tibble::rownames_to_column(var = "comparison") %>%
		# 			tibble::as_tibble() %>% rename("gene symbols" = V1) %>%
		# 			dplyr::filter(comparison %in% input$comparisons)
		# 	)
		# )
		
		## DEG results tables ----
		
		### DEG results tables DT prep ----
		results_annotated_min_cov_grp_DTs <- reactive({
			req(input$species)
			gen_DT_lst(
				significant_genes_by_comparison(),
				intersection_selected_sets(), input$species
			)
		}) 
		
		### DEG results tables render ----
		output$dge_res <- renderUI({
			req(input$comparisons)
			# req(input$results_annotated_min_cov_grp)
			# req(results_annotated_min_cov_grp_DTs())
			lst_of_tbls <- purrr::map(input$comparisons, ~{tabPanel(
				title = .x, DT::renderDataTable(
					results_annotated_min_cov_grp_DTs()[[.x]], server = TRUE
				)
			)}) 
			tabBox(
				width = 12,
				id = "tabcard",
				title = shiny::h3("Differential Gene Expression Results"),
				maximizable = TRUE,
				solidHeader = FALSE,
				status = "primary",
				selected = tail(input$comparisons, 1),
				type = "tabs",
				.list = lst_of_tbls
			)
		})
	})
}
    
## To be copied in the UI
# mod_sets_ui("sets_ui_1")
    
## To be copied in the server
# mod_sets_server("sets_ui_1")
