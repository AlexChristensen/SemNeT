# Extracted from test-permutation_SemNeT.R:566

# test -------------------------------------------------------------------------
low <- open.clean[which(open.group == "Low"), ]
high <- open.clean[which(open.group == "High"), ]
boot <- suppressMessages(bootstrap_SemNeT(
    low = low, high = high, method = "tmfg", type = "case", minimum_cases = 2, iter = 4, seed = 42
  ))
perm <- suppressMessages(permutation_SemNeT(
    low, high, method = "tmfg", minimum_cases = 2, measure = c("aspl", "cc", "q"), iter = 4, seed = 42
  ))
observed <- boot$groups$low$observed$measures - boot$groups$high$observed$measures
expect_equal(unname(perm$differences[1, ]), unname(observed[colnames(perm$differences)]))
expect_false(isTRUE(all.equal(perm$differences[2, "q"], perm$differences[1, "q"])))
reused <- suppressMessages(permutation_SemNeT(
    sample_1 = low, sample_2 = high, method = "tmfg", measure = "q", previous = perm
  ))
