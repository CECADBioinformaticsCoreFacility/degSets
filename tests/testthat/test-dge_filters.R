testing_table <- tibble::tibble(
	gene_id = paste0("prefix", rep(c("001", "002", "003"), 3)),
	baseMean = 10,
	log2FoldChange = rep(c(1, -1, 0.02), 3),
	lfcSE = 0.5,
	pvalue = rep(c(0.01, 0.5, 1), 3),
	padj = rep(c(0.01, 0.5, 1), 3),
	seqnames = "chr1",
	start = 0,
	end = 1000,
	width = 1000,
	strand = "+",
	gene_name = rep(c("abc1", "def2", "ghi3"), 3),
	gene_biotype = "protein_coding",
	seq_coord_system = "chromosome",
	description = "a gene",
	symbol = rep(c("abc1", "def2", "ghi3"), 3),
	entrezid = rep(c("001", "002", "003"), 3),
	comparison = rep(c("A_vs_B", "A_vs_C", "B_vs_C"), each = 3)
) %>% 
	dplyr::group_by(.data$comparison)

all_prefix001 <- testing_table %>% 
	dplyr::filter(gene_id == "prefix001")

all_prefix002 <- testing_table %>% 
	dplyr::filter(gene_id == "prefix002")

test_that("filtering works", {
	expect_equal(
		dge_filters(
			testing_table, 
			\(x){dplyr::filter(x, .data$log2FoldChange > 0)},
			0.05, -0.8, 0.8
		),
		testing_table %>% 
			dplyr::filter(gene_id == "prefix001") %>% 
			dplyr::group_by(.data$comparison)
	)
	expect_equal(
		dge_filters(
			testing_table, \(x){x}, 1, -0.5, 5
		),
		all_prefix002
	)
	expect_equal(
		dge_filters(
			testing_table, 
			\(x){dplyr::filter(x, .data$log2FoldChange < 0)},
			1, -0.8, 2
		),
		all_prefix002
	)
	expect_equal(
		dge_filters(
			testing_table, \(x){x}, 0.05, -1.1, 0
		),
		all_prefix001
	)
	expect_equal(
		dge_filters(
			testing_table, \(x){x}, 1, -0, 0
		),
		testing_table
	)
	expect_equal(
		dge_filters(
			testing_table,
			\(x){dplyr::filter(x, .data$log2FoldChange > 0)},
			1, -0, 0
		),
		testing_table %>%
			dplyr::filter(gene_id %in% c("prefix001", "prefix003"))
	)
})

test_that("list conversion", {
	expect_equal(
		get_significant_genes_by_comparison_lst(testing_table),
		list(
			"A_vs_B" = c("abc1", "def2", "ghi3"),
			"A_vs_C" = c("abc1", "def2", "ghi3"),
			"B_vs_C" = c("abc1", "def2", "ghi3")
		)
	)
	expect_equal(
		get_significant_genes_by_comparison_lst(
			testing_table %>% dplyr::filter(symbol %in% c("abc1","def2"))
		),
		list(
			"A_vs_B" = c("abc1", "def2"),
			"A_vs_C" = c("abc1", "def2"),
			"B_vs_C" = c("abc1", "def2")
		)
	)
	expect_equal(
		get_significant_genes_by_comparison_lst(
			testing_table %>% 
				dplyr::filter(comparison %in% c("A_vs_B", "B_vs_C"))
		),
		list(
			"A_vs_B" = c("abc1", "def2", "ghi3"),
			"B_vs_C" = c("abc1", "def2", "ghi3")
		)
	)
	expect_equal(
		get_significant_genes_by_comparison_lst(
			testing_table %>% 
				dplyr::filter(
					comparison %in% c("A_vs_B", "B_vs_C"),
					symbol %in% c("abc1","def2")
				)
		),
		list(
			"A_vs_B" = c("abc1", "def2"),
			"B_vs_C" = c("abc1", "def2")
		)
	)
})

test_that("inner join genes from matrix", {
	expect_equal(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1", "def2"),
				"A_vs_C" = c("abc1", "def2"),
				"B_vs_C" = c("abc1", "def2")
			)),
			c("A_vs_B", "A_vs_C")
		),
		c("abc1", "def2")
	)
	expect_equal(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1", "def2", "x"),
				"A_vs_C" = c("abc1", "def2", "x"),
				"B_vs_C" = c("abc1", "def2", "z")
			)),
			c("A_vs_B", "A_vs_C")
		),
		c("abc1", "def2", "x")
	)
	expect_equal(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1", "def2", "x"),
				"A_vs_C" = c("abc1", "def2", "x"),
				"B_vs_C" = c("abc1", "def2", "z")
			)),
			c("A_vs_B", "B_vs_C")
		),
		c("abc1", "def2")
		
	)
	expect_equal(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1", "def2", "x"),
				"B_vs_C" = c("abc1", "def2", "z")
			)),
			NULL
		),
		c("abc1", "def2", "x", "z")
	)
	expect_equal(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1", "def2", "x"),
				"B_vs_C" = c("abc1", "def2", "z")
			)),
			"none"
		),
		c("abc1", "def2", "x", "z")
	)
	expect_equal(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1", "def2", "x"),
				"B_vs_C" = c("abc1", "def2", "z")
			)),
			NA
		),
		c("abc1", "def2", "x", "z")
	)
	expect_equal(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1", "def2", "x"),
				"B_vs_C" = c("abc1", "def2", "z")
			)),
			""
		),
		c("abc1", "def2", "x", "z")
	)
	expect_error(
		get_intersection_genes(
			ComplexHeatmap::list_to_matrix(list(
				"A_vs_B" = c("abc1")
			)),
			"q"
		)
	)
})

test_that("combinations from set list", {
	expect_equal(
		set_list_2_combinations(list(
			"A_vs_B" = c("x","y"), # longest first
			"A_vs_C" = c("x")
		), add_empty = FALSE),
		list(
			"A_vs_B" = "A_vs_B",
			"A_vs_C" = "A_vs_C",
			"A_vs_B&A_vs_C" = c("A_vs_B", "A_vs_C")
		)
	)
	expect_equal(
		set_list_2_combinations(list(
			"A_vs_B" = c("x"),
			"A_vs_C" = c("x","y")
		), add_empty = FALSE),
		list(
			"A_vs_C" = "A_vs_C",
			"A_vs_B" = "A_vs_B",
			"A_vs_C&A_vs_B" = c("A_vs_C", "A_vs_B")
		)
	)
	expect_equal(
		set_list_2_combinations(list(
			"A_vs_B" = c("x")
		), add_empty = TRUE),
		list("none" = "", "A_vs_B" = "A_vs_B")
	)
})



tibble::tibble(grp = c("a","a","b","b"), x = 1) %>% 
	named_group_split(grp)
