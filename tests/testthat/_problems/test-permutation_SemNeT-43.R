# Extracted from test-permutation_SemNeT.R:43

# test -------------------------------------------------------------------------
set.seed(3)
low <- open.clean[which(open.group == "Low"), ]
high <- open.clean[which(open.group == "High"), ]
first <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 10, cores = 1, keep_networks = TRUE))
second <- suppressMessages(permutation_SemNeT(previous = first, measure = "cc", cores = 1))
expect_identical(first$networks, second$networks)
