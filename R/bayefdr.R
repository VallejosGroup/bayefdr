#' Bayesian EFDR optimisation.
#' 
#' Given a vector of probabilities, this function finds the probability 
#' threshold that matches a target expected false discovery rate as closely
#' as possible.
#' 
#' @param probs Vector of probabilities.
#' @param min_threshold Minimum probability threshold. If the optimal 
#'  probability threshold is below this number, it is rejected and 
#'  \code{min_threshold} is used instead.
#' @param target_efdr Numeric scalar specifying the expected false discovery 
#'  rate to match.
#' @param prob_thresholds Vector for probability thresholds to scan,
#'  with the aim of finding the threshold that matches the target EFDR.
#' @return An object of class "bayefdr" containing the probability thresholds 
#'  tested, the EFDR and EFNR at each probability threshold, and the optimal 
#'  threshold.
#' @examples
#'  probs <- runif(100)
#'  efdr <- efdr_search(probs, target_efdr = 0.1)
#'  plot(efdr)
#' @export
efdr_search <- function(
        probs,
        target_efdr,
        min_threshold = 0.7,
        prob_thresholds = seq(0.5, 0.9995, by = 0.00025)
    ) {

    assert_that(
        target_efdr >= 0,
        target_efdr < 1,
        min_threshold > 0.5,
        min_threshold < 1,
        all(prob_thresholds >= 0.5),
        all(prob_thresholds < 1)
    )

    efdr_grid <- scan(probs, type = "efdr", prob_thresholds = prob_thresholds)
    efnr_grid <- scan(probs, type = "efnr", prob_thresholds = prob_thresholds)

    if (all(is.na(efdr_grid))) {
        warning("EFDR estimation failed, returning specified min_threshold")
        return(
            bayefdr(
                which(prob_thresholds == min_threshold),
                prob_thresholds = prob_thresholds,
                efdr_grid = efdr_grid,
                efnr_grid = efnr_grid
            )
        )
    }

    abs_diff <- abs(efdr_grid - target_efdr)
    ind_opt <- which.min(abs_diff)
    efdr_opt <- efdr_grid[ind_opt]
    efnr_opt <- efnr_grid[ind_opt]
    optimal <- which(efdr_grid == efdr_opt & efnr_grid == efnr_opt)
    if (length(optimal) > 1) {
        optimal <- median(round(median(optimal)))
    }
    if (prob_thresholds[optimal] < min_threshold) {
        ## issue warning and fix to input
        optimal <- which(prob_thresholds == min_threshold)
        warning(
            "Unable to find a probability threshold that achieves the ",
            "desired EFDR +-0.025. ",
            "Returning specified min_threshold."
        )
    }
    optimal_threshold <- bayefdr(
        optimal,
        prob_thresholds = prob_thresholds,
        efdr_grid = efdr_grid,
        efnr_grid = efnr_grid
    )
}

#' Plot the EFDR, EFNR grids of a bayefdr object.
#' @param x An object of class bayefdr.
#' @param ... Unused.
#' @return A ggplot.
#' @export
plot.bayefdr <- function(x, ...) {
    mdf <- melt(x, measure.vars = c("EFDR", "EFNR"))
    ggplot(mdf, 
        aes_string(x = "threshold", y = "value", colour = "variable")
    ) +
        geom_line(na.rm = TRUE) +
        labs(x = "Probability threshold",
                      y = "Error rate") +
        ylim(0:1) +
        scale_colour_brewer(name = "", palette = "Set2") +
        geom_hline(
            aes(
                yintercept = x[optimal(x), "EFDR"],
                colour = "Selected EFDR"
            ),
            linetype = 2,
            na.rm = TRUE
        ) +
        geom_vline(
            aes(xintercept = x[optimal(x), "threshold"],
                colour = "Selected threshold"
            )
        )
}

#' EFDR and EFNR estimation
#' 
#' Calculate the Expected False Discovery Rate (EFDR) or Expected False 
#' Negative Rate (EFNR) in a vector of probabilities, given a specified
#' evidence threshold.
#' @param evidence_threshold Scalar value specifying the evidence threshold
#' at which the EFDR or EFNR should be evaluated.
#' @param probs Vector of probabilities.
#' @rdname efdr-efnr
#' @export
efdr <- function(evidence_threshold, probs) {
    sum((1 - probs) * (probs > evidence_threshold)) /
        sum(probs > evidence_threshold)
}
#' @rdname efdr-efnr
#' @export
efnr <- function(evidence_threshold, probs) {
    sum(probs * (evidence_threshold >= probs)) /
        sum(evidence_threshold >= probs)
}

scan <- function(
        probs,
        type = c("efdr", "efnr"),
        prob_thresholds
    ) {
    type <- match.arg(type)
    fun <- match.arg(type)
    grid <- vapply(
        prob_thresholds,
        FUN = fun,
        FUN.VALUE = numeric(1),
        probs = probs
    )
}

bayefdr <- function(optimal, prob_thresholds, efdr_grid, efnr_grid) {
    structure(
        data.frame(
            threshold = prob_thresholds,
            EFDR = efdr_grid,
            EFNR = efnr_grid
        ),
        optimal = optimal,
        class = c("bayefdr", "data.frame")
    )
}

#' Retrieve the index of the optimal probability threshold.
#' @param x An object of class "bayefdr".
#' @return The integer index of the optimal probability threshold.
#' @examples
#'  probs <- runif(100)
#'  e <- efdr_search(probs, target_efdr = 0.1)
#'  optimal(e)
#'  e[optimal(e), ]
#' @export
optimal <- function(x) attr(x, "optimal")
