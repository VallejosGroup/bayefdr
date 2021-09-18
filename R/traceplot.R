
#' Trace, marginal density histogram, and autocorrelation plot of MCMC draws.
#' @param x A vector of MCMC draws.
#' @param ylab An optional y-axis label.
#' @param log Logical scalar controlling whether the y-axis should be logged.
#' @return A plot created using \code{\link[cowplot]{plot_grid}} showing
#' the trace, marginal density histogram, and autocorrelation function of the
#' MCMC draws in \code{x}.
#' @examples
#' x <- rnorm(1000)
#' traceplot(x)
#' @export
traceplot <- function(
        x,
        ylab = NULL,
        log = FALSE
    ) {

    df1 <- data.frame(
        Iteration = seq_along(x),
        Draws = x
    )
    # Code inspired by https://tinyurl.com/vezqqud
    acf <- stats::acf(x, plot = FALSE)
    df2 <- data.frame(lag = acf$lag, acf = acf$acf)

    p1 <- ggplot(df1) +
        geom_point(
            aes_string(x = "Iteration", y = "Draws"),
            alpha = 0
        ) + 
        geom_line(aes_string(x = "Iteration", y = "Draws")) +
        scale_y_continuous(trans = if (log) "log10" else "identity") +
        labs(
            x = "Iteration",
            y = ylab
        )
    p1 <- ggExtra::ggMarginal(p1, type = "histogram", margins = "y")
    p2 <- ggplot(df2, aes_string(x = "lag", y = "acf")) +
        geom_hline(aes(yintercept = 0)) +
        geom_segment(mapping = aes_string(xend = "lag", yend = 0))
    cowplot::plot_grid(p1, p2)
}
