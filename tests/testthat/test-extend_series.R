context("ARIMA extension consistency")

test_that("extend_series returns same results as a direct call to forecast::auto.arima.",
          {
              set.seed(123)
              x <- data.frame(a = runif(50),
                              b = runif(50, min = 0, max = 5),
                              c = 1:50)
              x_extended <-
                  x %>% extend_series(columns = c("a", "b"),
                                      n = c(1, 2),
                                      mode = "ARIMA")
              arima_a <- forecast::auto.arima(x$a[-50],
                                              max.p = 4,
                                              max.q = 4,
                                              max.d = 1)
              arima_b <- forecast::auto.arima(x$b[-c(49, 50)],
                                              max.p = 4,
                                              max.q = 4,
                                              max.d = 1)
              A <- c(x$a[-50], predict(arima_a, n.ahead = 1)$pred)
              B <-
                  c(x$b[-c(49, 50)], predict(arima_b, n.ahead = 2)$pred)
              expect_equal(A, x_extended$a)
              expect_equal(B, x_extended$b)
          })
