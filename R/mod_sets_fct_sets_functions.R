#' format_results
#'
#' @param res_extra the extended results table from which to generate the new columns
#' @param organism_name latin name of the organism, used to make links to ensembl
#'
#' @return tibble
#' @export
#'
#' @importFrom stringr str_wrap
#' @importFrom dplyr mutate select everything
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
#' 
format_results <- function(res_extra, organism_name) {
	#res_extra_text_nona <- 
	res_extra %>% 
		dplyr::mutate(
			`-log10(p-value)` = -log10(.data$pvalue),
			#abslfc = abs(log2FoldChange),
			text = paste0(
				"Gene: <b>", .data$symbol, "</b> (",.data$gene_id,")\n",
				"Coords: ", .data$seqnames, ":",
				format(.data$start, big.mark = ","), "-",
				format(.data$end, big.mark = ","), "\n",
				"log2(Fold Change): <b>", sprintf("%.3f", .data$log2FoldChange),
				"</b>\n",
				"p-value: <b>", sprintf("%.3e", .data$pvalue), "</b> (",
				"-log10(p): ", sprintf("%.2f", .data$`-log10(p-value)`), ")\n",
				"type: ", .data$gene_biotype, "\n",
				"Description: ",
				stringr::str_wrap(.data$description, width = 40), "\n"
			),
			gene_id_link = paste0(
				# "href=https://wormbase.org/species/c_elegans/gene/",
				"<a target='_blank' href=",
				"https://www.ensembl.org/", gsub(" ", "_", organism_name),
				"/Gene/Summary?db=core;g=",
				.data$gene_id, ">", .data$gene_id, "</a>"
			)
		) %>%
		dplyr::select(.data$gene_id_link, dplyr::everything()) %>%
		tidyr::drop_na(.data$pvalue)
}

#' lfcpal
#'
#' generates a colour scale for log2 fold change values for a DT table
#'
#' @param x a numeric vector log2 Fold Changes
#'
#' @return DT::styleInterval
#' @export
#'
#' @importFrom colourScaleR universal_colour_scaler
#' @importFrom DT styleInterval
#'
lfcpal <- function(x) {
	pal <- colourScaleR::universal_colour_scaler(
		x, #log2FoldChange,
		#type = "scico", palette = "romaO",
		type = "viridis", palette = "viridis",
		mode = "palette", direction = -1, n_breaks = 9
	)
	DT::styleInterval(as.numeric(names(pal))[-1], pal)
}

#' pvalpal
#' 
#' generates a colour scale for p-values for a DT table
#' 
#' @param x a numeric vector of p-values
#'
#' @return DT::styleInterval
#' @export
#'
#' @importFrom colourScaleR universal_colour_scaler
#' @importFrom DT styleInterval
#' 
pvalpal <- function(x) {
	# pal <- colourScaleR::universal_colour_scaler(
	# 	x, #pvalue.
	# 	type = "scico", palette = "romaO",
	# 	mode = "palette", direction = -1, n_breaks = 9
	# )
	pal <- colourScaleR::universal_colour_scaler(
		x, #pvalue,
		type = "scico", palette = "hawaii", mode = "palette",
		direction = 1, n_breaks = 9#, verbose = TRUE
	)
	# pal
	DT::styleInterval(as.numeric(names(pal))[-1], pal)
}


# barplotformfun
# @param dt a DT data table 
# @param id the name of the column to colour by genotype
# @param df data frame for scale values
# barplotformfun <- function(dt, id, df, log2fc_range) {
# 	dt %>%
# 		DT::formatStyle(
# 			id, id,
# 			background = DT::styleColorBar(
# 				df[[id]],
# 				#'lightblue'
# 				lfcpal(log2fc_range)
# 			),
# 			backgroundSize = '98% 88%',
# 			backgroundRepeat = 'no-repeat',
# 			backgroundPosition = 'center'
# 		)
# }




#' results_table_DT
#'
#' @param data a tibble with the results from a differential gene expression
#' analysis (specifics analysis ...)
#' @param log2fc_range set the range for the scales of the log2fc colour scale
#' numeric vector length 2 Default: c(-5,5)
#' @param pvalue_range set the range for the scales of the p-value colour scale
#' numeric vector length 2 Default: c(0,1)
#' @param padj_range set the range for the scales of the adjusted p-value colour scale
#' numeric vector length 2 Default: c(0,1)
#'
#' @return DT::datatable
#' @export
#'
#' @importFrom DT datatable formatSignif formatStyle
#' @importFrom dplyr %>%
#' 
results_table_DT <- function(
	data,
	log2fc_range = c(-5, 5),
	pvalue_range = c(0, 1),
	padj_range = c(0, 1)
) {
	data %>%
		DT::datatable(
			filter = "top",  # allows filtering on each column
			extensions = c(
				"Buttons",  # add download buttons, etc
				#"Select",
				"Scroller"  # for scrolling down the rows rather than pagination
			),
			selection = 'none',
			rownames = FALSE,  # remove rownames
			style = "bootstrap",
			class = "compact",
			width = "100%",
			height = "100%",
			escape = FALSE,
			options = list(
				dom = "Blrtip",  # specify content (search box, etc)
				select = list(style = 'os', items = 'row'),
				deferRender = TRUE,
				scrollY = "960px",
				scrollX = "960px",
				scroller = TRUE,
				columnDefs = list(
					list(
						visible = FALSE,
						#targets = c(7,8,9,10,13,15,16,17,18)
						targets = c(1, c(7,8,9,10,13,15,16,17,18) + 1)
					)
				), 
				searchCols = c(
					vector(mode = "list", length = 6),
					# list(list(search = '0.000 ... 0.050')),
					vector(mode = "list", length = 12)
				),
				buttons = list(
					I("colvis"),  # turn columns on and off
					# "selectRows",
					#"selectNone",
					"csv",  # download as .csv
					"excel"  # download as .xlsx
				)
			)
		) %>%
		DT::formatSignif(
			c("baseMean", "pvalue", "padj", "log2FoldChange", "lfcSE"),
			digits = 4
		) %>%
		#barplotformfun("log2FoldChange", data, log2fc_range) %>%
		DT::formatStyle(
			"log2FoldChange",
			color = DT::styleInterval(0, c('#00000000', '#FFFFFFFF')),
			# backgroundColor = lfcpal(.$x$data$log2FoldChange)
			backgroundColor = lfcpal(log2fc_range)
		) %>%
		DT::formatStyle(
			"pvalue",
			# color = "#FFFFFF",
			color = DT::styleInterval(0.001, c('#FFFFFFFF', '#00000000')),
			# backgroundColor = pvalpal(.$x$data$pvalue)
			backgroundColor = pvalpal(pvalue_range)
		) %>%
		DT::formatStyle(
			"padj",
			# color = "#FFFFFF",
			color = DT::styleInterval(0.001, c('#FFFFFFFF', '#00000000')),
			# backgroundColor = pvalpal(.$x$data$padj)
			backgroundColor = pvalpal(padj_range)
		)
}


#' named_group_split
#'
#' @param .tbl a tibble
#' @param ... the groups to split
#'
#' @return a named list of tibbles
#' @export
#'
#' @importFrom dplyr group_by %>% group_keys
#' @importFrom rlang inject set_names
#' @importFrom rlang .data
#'
named_group_split <- function(.tbl, ...) {
	grouped <- dplyr::group_by(.tbl, ...)
	names <- rlang::inject(paste(!!!group_keys(grouped), sep = " / "))
	
	grouped %>% 
		group_split() %>% 
		rlang::set_names(names)
}

#' bind_cols_fill
#'
#' takes a list of vectors of potentilly differeing lengths and binds them 
#' together as columns in a tibble filling any empty values with NAs
#'
#' @param lst a list of vectors
#'
#' @return tibble
#' @export
#'
#' @importFrom purrr map_int imap_dfc
#' @importFrom tibble tibble
#'
bind_cols_fill <- function(lst) {
	rows_needed <- max(
		purrr::map_int(lst, ~max(length(.x)))
	)
	purrr::imap_dfc(lst, ~{
		vec <- rep(NA, rows_needed) #_character_
		vec[seq_along(.x)] <- .x
		tib <- tibble::tibble(vec)
		colnames(tib) <- .y 
		tib
	})
	
}

#' set_list_2_combinations
#'
#' note that set order in names is derived from size of the sets, biggest first
#' also a none set with an empty string as a value is added by default
#'
#' @param set_list a named list where the names are the sets and the values
#'  vectors of things in those sets
#' @param add_empty should an option to include an empty set be included?
#' if true (the default) adds an item called none with the value of an empty
#' string
#'
#' @return a list of combinations of sets
#' @export
#'
#' @importFrom purrr map_int map map_chr
#' @importFrom dplyr %>%
#'
set_list_2_combinations <- function(set_list, add_empty = TRUE) {
	set_list <- set_list[
		order(purrr::map_int(set_list, length), decreasing = TRUE)
	]
	
	set_names <- names(set_list)
	
	combs <- purrr::map(seq_along(set_names), ~{
		lst <- combn(set_names, .x) %>% as.data.frame() %>% as.list()
		names(lst) <- purrr::map_chr(lst, ~paste0(.x, collapse = "&"))
		lst
	}) %>% unlist(recursive = FALSE)
	
	if(add_empty) {
		return(c(list("none" = ""), combs))
	} else {
		return(combs)
	}
}
#set_list_2_combinations(set_list)

#' get_intersection_genes
#'
#' get the genes at the intersection of selected sets
#'
#' @param mat binary set membership matrix (ComplexHeatmap)
#' @param sets vector of names of sets for which you want the intersection
#'
#' @return character vector of gene names
#' @export
#'
get_intersection_genes <- function(mat, sets) {
	if(is.null(sets)) {
		return(rownames(mat))
	} else if(any(sets %in% c("none", "", NA))) {
		return(rownames(mat))
	} else {
		rownames(mat)[length(sets) == rowSums(mat[, sets, drop = FALSE])]
	}
}


#' set_selected_set_2_inputs
#'
#' deals with two upsets plots being able to provide inputs to the set
#' selection
#'
#' @param x current value of comparisons
#' @param upset names from selector 1
#' @param venn names from selector 2
#'
#' @return character
#' @export
#'
set_selected_set_2_inputs <- function(x, upset, venn){
	# x <- ""
	if(
		is.null(upset) && is.null(venn)
	) {
		#x <- NULL
		#return(NULL)
		return("")
	} else if(is.null(x)) {
		x <- ""
	}# else 
	
	if(
		(!is.null(upset)) || (!is.null(venn))
	) {
		y <- NULL
		if(!is.null(upset)) {
			if(x != upset) {
				y <- upset
			}
		}
		
		if(!is.null(venn)) {
			if(x != venn) {
				y <- venn
			}
		}
		x <- y
	}
	x
}

# 
# set_selected_set_2_inputs("",    NULL,  NULL )
# set_selected_set_2_inputs("",    NULL,  "A&B")
# set_selected_set_2_inputs(NULL,  NULL,  "A&B")
# set_selected_set_2_inputs(NULL,  "A&B", NULL )
# set_selected_set_2_inputs("",    "A&B", NULL )
# set_selected_set_2_inputs("",    "C&D", "A&B")
# set_selected_set_2_inputs("A&B", "C&D", "A&B")
# set_selected_set_2_inputs("",    "A&B", "C&D")
# set_selected_set_2_inputs("",    "A&B", "C&D")
# set_selected_set_2_inputs("C&D", "A&B", "C&D")
#' dge_filters
#'
#' applies filters for adjusted p-value, direction and log2 Fold Change to 
#' differential gene expression data
#'
#' @param data results_annotated_min_cov_grp
#' @param dir_filter dplyr function function for a particualt direction
#' @param padjt minimum adjusted p-value
#' @param lowerlfc lower log2 Fold Change cut-off
#' @param upperlfc upper log2 Fold Change cut-off
#'
#' @return filtered tibble
#' 
#' @export
#' 
#' @importFrom rlang .data
#' @importFrom dplyr filter group_by
#'
dge_filters <- function(data, dir_filter, padjt, lowerlfc, upperlfc) {
	data %>% 
		dplyr::group_by(.data$comparison) %>%
		dir_filter() %>%
		dplyr::filter(
			.data$padj <= padjt,
			.data$log2FoldChange <= lowerlfc | .data$log2FoldChange >= upperlfc
		)
}

#' get_significant_genes_by_comparison_lst
#'
#' Extracts gene set lists from filtered dge table
#' 
#' @param significant_genes_by_comparison significant_genes_by_comparison
#'
#' @return named list
#' @export
#' 
#' @importFrom rlang .data
#' @importFrom dplyr %>% select mutate pull
#' @importFrom tidyr nest
#' @importFrom purrr map set_names
#' 
get_significant_genes_by_comparison_lst <- function(
		significant_genes_by_comparison
){
	significant_genes_by_comparison %>%
		dplyr::select(.data$comparison, .data$symbol) %>%
		dplyr::group_by(.data$comparison) %>%
		tidyr::nest() %>%
		dplyr::mutate(
			data = .data$data %>% 
				purrr::set_names(.data$comparison) %>% 
				purrr::map(~.x$symbol)
		) %>%
		dplyr::pull(.data$data)
}

#' gen_upset_plot
#'
#' @param lst named list of items in sets
#' @param combinations possible set combinations
#' @param set_2_highlight the sets to highlight
#'
#' @return upsetjs plot
#' @export
#'
#' @importFrom upsetjs upsetjs fromList setSelection chartTheme interactiveChart
#' @importFrom dplyr %>%
#'
gen_upset_plot <- function(lst, combinations, set_2_highlight) {
	upsetjs::upsetjs() %>% 
		upsetjs::fromList(
			lst
		) %>% 
		upsetjs::setSelection(
			combinations[[set_2_highlight]]
		) %>% 
		upsetjs::chartTheme(
			#theme = if_else(input$dark_mode, "dark", "light"),
			selection.color = "#587792", hover.hint.color = "#8DB1AB"
		) %>%
		upsetjs::interactiveChart()
}

#' gen_venn
#'
#' @param lst named list of items in sets
#' @param combinations possible set combinations
#' @param set_2_highlight the sets to highlight
#'
#' @return upsetjs plot
#' @export
#' 
#' @importFrom upsetjs upsetjsVennDiagram fromList setSelection chartTheme interactiveChart
#' @importFrom dplyr %>%
#'
gen_venn <- function(lst, combinations, set_2_highlight) {
	upsetjs::upsetjsVennDiagram() %>% 
		upsetjs::fromList(
			lst
		) %>% 
		upsetjs::setSelection(
			combinations[[set_2_highlight]]
		) %>% 
		upsetjs::chartTheme(
			#theme = if_else(input$dark_mode, "dark", "light"),
			selection.color = "#587792", hover.hint.color = "#8DB1AB"
		) %>%
		upsetjs::interactiveChart()
}


#' gen_DT_lst
#'
#' @param significant_genes_by_comparison  significant_genes_by_comparison
#' @param intersection_selected_sets intersection_selected_sets 
#' @param species The latin binomial name of the species (used to make ensembl links)
#'
#' @return list of DT tables objects
#' @export
#'
gen_DT_lst <- function(significant_genes_by_comparison, intersection_selected_sets, species) {
	df <- significant_genes_by_comparison %>%
		dplyr::filter(.data$symbol %in% intersection_selected_sets)
	
	maxlog2FoldChange <- max(abs(df$log2FoldChange))
	
	df %>%
		dplyr::group_by(.data$comparison) %>%
		named_group_split(.data$comparison) %>%
		purrr::map(~{
			.x %>% 
				dplyr::select(-.data$comparison) %>%
				format_results(species) %>%
				results_table_DT(
					log2fc_range = c(
						-maxlog2FoldChange, maxlog2FoldChange
					),
					pvalue_range = c(0, quantile(
						df$pvalue, probs = c(0.75), na.rm = TRUE
					)),
					padj_range = c(0, quantile(
						df$padj, probs = c(0.75), na.rm = TRUE
					))
				)
		})
}
