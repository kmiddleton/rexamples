cutAndStack <- function(x, number = 6, overlap = 0.1, type = 'l',
                        xlab = "Time",
                        ylab = deparse(substitute(x)),
                        ...){
    stopifnot(is.ts(x))
    if (is.mts(x)) stop("mts not supported, use 'x[, 1]' etc")
    stopifnot(require(grid))
    stopifnot(require(lattice))
    tdf <-
        data.frame(.response = as.numeric(x),
                   .time = time(x),
                   .Time =
                   equal.count(as.numeric(time(x)),
                               number = number,
                               overlap = overlap))
    strip.ts <-
        function(which.given, which.panel, shingle.intervals,
                 bg = trellis.par.get("strip.background")$col[1],
                 ...)
        {
            pushViewport(viewport(xscale = range(tdf$.time),
                                  yscale = range(tdf$.response)))
            panel.fill(col = bg)
            current.interval <- shingle.intervals[which.panel[which.given], ]
            highlight <-
                cut(tdf$.time,
                    breaks =
                    c(min(shingle.intervals) - 1,
                      current.interval,
                      max(shingle.intervals) + 1))
            with(tdf, panel.xyplot(.time, .response,
                                   groups = highlight,
                                   subscripts = seq(length = nrow(tdf)),
                                   type = "l",
                                   col = c("grey", "red", "grey"),
                                   lwd = c(1, 2, 1)))
            upViewport()
        }
    xyplot(.response ~ .time | .Time,
           data = tdf,
           type = type,
           xlab = xlab, ylab = ylab,
           strip = strip.ts,
           default.scales =
           list(x = list(relation = "free"),
                y = list(relation = "free", rot = 0)),
           ...)
}

pdf("stack.pdf", height = 9, width = 9)

p <- cutAndStack(EuStockMarkets[, 1], aspect = "xy",
                scales = list(x = list(draw = FALSE)))

p

update(p[3], par.strip.text = list(lines = 3),
       scales = list(x = list(draw = TRUE, at = 1991:1999)))

dev.off()

