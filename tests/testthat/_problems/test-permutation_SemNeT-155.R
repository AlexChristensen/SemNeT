# Extracted from test-permutation_SemNeT.R:155

# test -------------------------------------------------------------------------
low <- open.clean[which(open.group == "Low"), ]
high <- open.clean[which(open.group == "High"), ]
measures <- c("aspl", "cc", "q")
all_measures <- permutation_SemNeT(low, high, method = "tmfg", measure = measures, iter = 20, cores = 1, seed = 1)
expect_equal(row.names(all_measures$result), measures)
expect_named(all_measures$result, c("low", "high", "p_value", "alternative"))
expect_equal(dim(all_measures$differences), c(20, 3))
expect_equal(colnames(all_measures$differences), measures)
expect_named(all_measures$diagnostics, c(
    "aspl_1", "aspl_2", "cc_1", "cc_2", "q_1", "q_2", "nodes_1", "nodes_2", "edges_1", "edges_2"
  ))
for(name in measures){

    # Perform permutation test of measure
    single <- permutation_SemNeT(low, high, method = "tmfg", measure = name, iter = 20, cores = 1, seed = 1)

    # Check differences
    expect_equal(unname(all_measures$differences[, name]), single$differences)

    # Check result
    expect_equal(all_measures$result[name, ], single$result)

  }
single <- permutation_SemNeT(low, high, method = "tmfg", measure = "cc", iter = 20, cores = 1, seed = 1)
expect_null(dim(single$differences))
expect_named(single$diagnostics, c("value_1", "value_2", "nodes_1", "nodes_2", "edges_1", "edges_2"))
reused <- permutation_SemNeT(previous = all_measures, measure = c("aspl", "q"), cores = 1)
