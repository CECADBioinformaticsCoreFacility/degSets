library(dplyr)

# results_annotated_min_cov_grp <- readr::read_csv(
# 	'/tank/richardjacton/sciebo/cecad-bioinformatics/shared_results/AG_Rybniker/stheobald/out/dge_results/results_annotated_min_cov_grp.csv'
# )
# results_annotated_min_cov_grp
# colnames(results_annotated_min_cov_grp)
# testing_table <- results_annotated_min_cov_grp #%>% dplyr::group_by(comparison)

# testing_table <- tibble::tibble(
# 	gene_id = paste0("prefix", rep(c("001", "002", "003"), 3)),
# 	baseMean = 10,
# 	log2FoldChange = rep(c(1, -1, 0.02), 3),
# 	lfcSE = 0.5,
# 	pvalue = rep(c(0.01, 0.5, 1), 3),
# 	padj = rep(c(0.01, 0.5, 1), 3),
# 	seqnames = "chr1",
# 	start = 0,
# 	end = 1000,
# 	width = 1000,
# 	strand = "+",
# 	#gene_name = rep(c("abc1", "def2", "ghi3"), 3),
# 	gene_name = c("X","Y","Z", rep(c("abc1", "def2", "ghi3"), 2)),
# 	gene_biotype = "protein_coding",
# 	seq_coord_system = "chromosome",
# 	description = "a gene",
# 	symbol = c("X","Y","Z", rep(c("abc1", "def2", "ghi3"), 2)),
# 	entrezid = rep(c("001", "002", "003"), 3),
# 	comparison = rep(c("A_vs_B", "A_vs_C", "B_vs_C"), each = 3)
# ) %>%
# 	dplyr::group_by(.data$comparison)

get_significant_genes_by_comparison_lst(testing_table)

readr::write_csv(
	testing_table, "inst/extdata/testing_table.csv"
)

testing_table %>% 
	filter(log2FoldChange >= 1)


results_annotated_min_cov_grp <- readr::read_csv(
	"inst/extdata/testing_table.csv"
)


significant_genes_by_comparison <- dge_filters(
	results_annotated_min_cov_grp, 
	\(x){dplyr::filter(x, .data$log2FoldChange > 0)},
	0.05, -0.8, 0.8
) %>%
	filter(comparison %in% c("DC_24h_2_vs_M_24h_2", "DC_5h_2_vs_M_5h_2"))
significant_genes_by_comparison


significant_genes_by_comparison_lst <- 
	get_significant_genes_by_comparison_lst(significant_genes_by_comparison)
significant_genes_by_comparison_lst
purrr::map_int(significant_genes_by_comparison_lst,length)

significant_genes_by_comparison_set_matrix <-
significant_genes_by_comparison_lst %>%
	ComplexHeatmap::list_to_matrix()
significant_genes_by_comparison_set_matrix

set_combinations <- 
	set_list_2_combinations(
			significant_genes_by_comparison_lst[
				c("DC_24h_2_vs_M_24h_2", "DC_5h_2_vs_M_5h_2")
			]
			#["A_vs_B"]
	)

intersection_selected_sets <- 
get_intersection_genes(
	significant_genes_by_comparison_set_matrix,
	set_combinations#[["A_vs_B"]]
)

length(set_combinations)

rownames(significant_genes_by_comparison_set_matrix)[
	length(set_combinations[[2]]) == 
	rowSums(significant_genes_by_comparison_set_matrix[,set_combinations[[2]],drop=FALSE]) 
] %>% length()

rownames(significant_genes_by_comparison_set_matrix)[
	length(set_combinations[[4]]) == 
		rowSums(significant_genes_by_comparison_set_matrix[,set_combinations[[4]],drop=FALSE]) 
] %>% length()



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

gen_DT_lst(significant_genes_by_comparison, intersection_selected_sets, "Homo Sapiens")
