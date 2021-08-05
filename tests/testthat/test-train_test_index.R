context("Train/Test index consistency")

test_that("train_test_index returns same results using arg n or dates", {
    x <- data.frame(date = seq.Date(
        from = as.Date("2000-01-01"),
        to = as.Date("2004-12-01"),
        by = "quarter"
    ),
    val = 1:20)
    a <- train_test_index(x, n = 16, frequency = "quarter")
    b <- train_test_index(x, date_start = "2000-01-01",
                          date_end = "2003-10-01",
                          frequency = "quarter")
    expect_identical(a, b)
})
