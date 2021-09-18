#' Plot the cumulative median, mean, and 95\% high posterior density region.
#' @param x An vector of MCMC draws.
#' @param ylab An optional y-axis label.
#' @param burn Integer specifying the number of initial iterations to be
#' discarded.
#' @param thin Integer specifying the thinning factor to be used on
#' the MCMC steps.
#' @param hpd_level Floating point specifying the desired HPD level.
#' @return A ggplot showing the cumulative mean, median and HPD.
#' @examples
#' x <- rnorm(1000)
#' cumplot(x)
#' @export
cumplot <- function(
        x,
        ylab = NULL,
        burn = 0,
        thin = 1,
        hpd_level = 0.95
    ) {
    x <- x[burn:length(x)]
    x <- x[seq(1, length(x), by = thin)]
    cum_median <- .cumulative_median(x)
    cum_HPD <- .cumulative_HPD(x, hpd_level)
    cum_mean <- .cumulative_mean(x)
    ggplot() +
        aes(x = seq_along(cum_median)) +
        geom_line(aes(y = cum_median, color = "Posterior median")) +
        geom_line(aes(y = cum_mean, color = "Posterior mean")) +
        geom_line(aes(y = cum_HPD[1, ], color = "HPD (lower)")) +
        geom_line(aes(y = cum_HPD[2, ], color = "HPD (upper)")) +
        scale_color_brewer(palette = "Paired", name = NULL) +
        labs(x = "Iteration", y = ylab)
}

.cumulative_median <- function(y) {
    n <- length(y)
    out <- vapply(
        seq_len(n)[-1],
        function(x) median(y[seq_len(x)]),
        numeric(1)
    )
    return(out)
}

.cumulative_HPD <- function(y, level) {
    n <- length(y)
    out <- vapply(
        seq_len(n)[-1],
        function(x) .HPDinterval(y[seq_len(x)], level),
        numeric(2)
    )
    return(out)
}

.cumulative_mean <- function(y) {
    (cumsum(y) / seq_along(y))[-1]
}


.HPDinterval <- function(obj, prob = 0.95) {
    obj <- as.matrix(obj)
    vals <- apply(obj, 2, sort)
    if (!is.matrix(vals)) {
        stop("obj must have nsamp > 1")
    }
    nsamp <- nrow(vals)
    npar <- ncol(vals)
    gap <- max(1, min(nsamp - 1, round(nsamp * prob)))
    init <- seq_len(nsamp - gap)
    inds <- apply(
        vals[init + gap, , drop = FALSE] - vals[init, , drop = FALSE],
        2,
        which.min
    )
    ans <- cbind(
        vals[cbind(inds, seq_len(npar))],
        vals[cbind(inds + gap, seq_len(npar))]
    )
    dimnames(ans) <- list(colnames(obj), c("lower", "upper"))
    attr(ans, "Probability") <- gap / nsamp
    ans
}
