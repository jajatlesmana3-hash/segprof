test_that("seg_prof works with auto-segmentation", {
  data <- data.frame(
    customer_id = c(1,1,2,2,3,3),
    transaction_date = as.Date(c("2024-01-01","2024-02-01","2024-01-15",
                                 "2024-03-01","2024-02-10","2024-04-01")),
    amount = c(100,150,200,50,300,80)
  )
  result <- seg_prof(data, customer_id = customer_id, plot_3d = FALSE, verbose = FALSE)
  expect_true(is.data.frame(result$summary))
  expect_true(length(result$plots) >= 2)
})

test_that("seg_prof works with existing segment column", {
  data <- data.frame(
    segment = c("A","A","B","B"),
    num1 = rnorm(4),
    cat1 = c("X","X","Y","Y")
  )
  result <- seg_prof(data, segment_col = segment, plot_3d = FALSE)
  expect_true(is.data.frame(result$summary))
  expect_true(length(result$plots) >= 2)
})
