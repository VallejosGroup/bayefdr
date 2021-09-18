context("plots")

test_that("cumplot works", {
    expect_is(cumplot(rnorm(1000)), "gg")
})

test_that("cumplot works", {
    expect_is(traceplot(rnorm(1000)), "gg")
})
