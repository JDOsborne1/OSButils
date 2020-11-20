test_that("multiplication works", {
        test_frame <- data.frame(
                      col1 = c("a", "b", "c")
                      , col2 = c(1, 1, 2)
                      )
               # This part should show TRUE since the test frame is unique on col1
expect_true(is.distinct(test_frame, on = col1))
               # This part should show FALSE since the test frame is NOT unique on col2
expect_false(is.distinct(test_frame, on = col2))
})
