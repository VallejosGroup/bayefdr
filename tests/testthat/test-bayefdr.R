context("bayefdr")

test_that("Basic functionality", {
    probs <- runif(100)
    efdr <- efdr_search(probs, target_efdr = 0.1)
    expect_is(efdr, "data.frame")
    expect_is(efdr, "bayefdr")
    expect_is(plot(efdr), "gg")
})


test_that("error conditions/arguments", {
    probs <- runif(100)
    expect_error(
        efdr_search(probs),
        "argument \"target_efdr\" is missing, with no default"
    )
    expect_error(
        efdr_search(probs, target_efdr = -1),
        "target_efdr not greater than or equal to 0"
    )
    expect_error(
        efdr_search(probs, target_efdr = 2),
        "target_efdr not less than 1"
    )
    expect_error(
        efdr_search(probs, target_efdr = 0.1, min_threshold = -1),
        "min_threshold not greater than 0.5"
    )
    expect_error(
        efdr_search(probs, target_efdr = 0.1, min_threshold = 2),
        "min_threshold not less than 1"
    )
})


test_that("optimal", {
    set.seed(42)
    probs <- runif(100)
    e <- efdr_search(probs, target_efdr = 0.1)
    expect_equal(round(e[optimal(e), "threshold"], digits = 3), 0.768)
    expect_equal(round(e[optimal(e), "EFDR"], digits = 3), 0.099)
    expect_equal(round(e[optimal(e), "EFNR"], digits = 3), 0.399)
})

test_that("all ones", {
    expect_error(efdr_search(rep(1, 10), target_efdr=0.1), NA)
})
