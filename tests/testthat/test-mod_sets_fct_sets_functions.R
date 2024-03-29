test_that("set name logic works", {
	expect_equal(set_selected_set_2_inputs("",    NULL,  NULL ),   "" )
	expect_equal(set_selected_set_2_inputs("",    NULL,  "A&B"), "A&B")
	expect_equal(set_selected_set_2_inputs(NULL,  NULL,  "A&B"), "A&B")
	expect_equal(set_selected_set_2_inputs(NULL,  "A&B", NULL ), "A&B")
	expect_equal(set_selected_set_2_inputs("",    "A&B", NULL ), "A&B")
	expect_equal(set_selected_set_2_inputs("",    "C&D", "A&B"), "A&B")
	expect_equal(set_selected_set_2_inputs("A&B", "C&D", "A&B"), "C&D")
	expect_equal(set_selected_set_2_inputs("",    "A&B", "C&D"), "C&D")
	expect_equal(set_selected_set_2_inputs("A&B", "A&B", "C&D"), "C&D")
	expect_equal(set_selected_set_2_inputs("C&D", "A&B", "C&D"), "A&B")
})
