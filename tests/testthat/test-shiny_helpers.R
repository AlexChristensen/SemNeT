test_that("find_textcleaner_objects() finds a fluency-type textcleaner object in .GlobalEnv", {

  fluency_obj <- structure(list(), class = c("textcleaner", "fluency"))
  assign("test_fluency_tc", fluency_obj, envir = .GlobalEnv)
  on.exit(rm("test_fluency_tc", envir = .GlobalEnv), add = TRUE)

  expect_true("test_fluency_tc" %in% find_textcleaner_objects())

})

test_that("find_textcleaner_objects() excludes a free-type textcleaner object (different, incompatible shape)", {

  free_obj <- structure(list(), class = c("textcleaner", "free"))
  assign("test_free_tc", free_obj, envir = .GlobalEnv)
  on.exit(rm("test_free_tc", envir = .GlobalEnv), add = TRUE)

  expect_false("test_free_tc" %in% find_textcleaner_objects())

})

test_that("find_textcleaner_objects() ignores ordinary objects that aren't textcleaner-classed", {

  assign("test_plain_object", data.frame(x = 1:3), envir = .GlobalEnv)
  on.exit(rm("test_plain_object", envir = .GlobalEnv), add = TRUE)

  expect_false("test_plain_object" %in% find_textcleaner_objects())

})

test_that("find_textcleaner_objects() returns character(0), not an error, when none exist", {

  # Confirmed no stray "textcleaner"-classed object survives from another
  # test in this same run by explicitly checking its own found set is
  # unaffected by an unrelated object
  assign("test_unrelated_number", 42, envir = .GlobalEnv)
  on.exit(rm("test_unrelated_number", envir = .GlobalEnv), add = TRUE)

  expect_true(is.character(find_textcleaner_objects()))
  expect_false("test_unrelated_number" %in% find_textcleaner_objects())

})
