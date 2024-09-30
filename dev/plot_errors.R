## we could add some fancy marginal distribution to the panels
plot.errors <- function(errors) {
  nn <- c("epsilon", paste0("eta", 1:(ncol(errors) - 1)))
  dat <- as.data.frame(errors)
  colnames(dat) <- nn

  ## taken from ?pairs
  panel.cor <- function(x, y, digits = 2, prefix = "", ...) {
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    text(0.5, 0.5, txt)
  }

  ## based on ?panel.smooth
  panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                        cex = 1, col.lm = "red", ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      abline(lm(y ~ x), col = col.lm)
  }

  pairs(dat, gap = 0, pch = ".", cex = 3, upper.panel = panel.cor, lower.panel = panel.lm)
}
