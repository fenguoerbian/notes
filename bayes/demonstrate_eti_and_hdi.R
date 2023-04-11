x <- seq(from = 0, to = 1, by = 0.001)
a <- 18
b <- 4
y1 <- dbeta(x, a, b)

# HDI
tmp <- bayestestR::distribution_beta(n = 100000, shape1 = a, shape2 = b)
tmp2 <- bayestestR::ci(tmp, ci = 0.90, method = "HDI")

# Compare HDI and SDI
bayestestR::ci(tmp, ci = 0.90, method = "HDI")
str(bayestestR::hdi(tmp, ci = 0.9))
str(bayestestR::spi(tmp, ci = 0.9))    # SPI is a more robust HDI?


plot(x, y1, type = "l", ylab = "density", main = "ETI and HDI for Beta(18, 4)")
lines(x = c(qbeta(0.05, a, b, lower.tail = TRUE), 
            qbeta(0.05, a, b, lower.tail = TRUE)), 
      y = c(0, 
            dbeta(qbeta(0.05, a, b, lower.tail = TRUE), a, b)), 
      lty = 1, col = "blue")

lines(x = c(qbeta(0.05, a, b, lower.tail = FALSE), 
            qbeta(0.05, a, b, lower.tail = FALSE)), 
      y = c(0, 
            dbeta(qbeta(0.05, a, b, lower.tail = FALSE), a, b)), 
      lty = 1, col = "blue")

lines(x = c(-1, qbeta(0.05, a, b, lower.tail = TRUE)), 
      y = rep(dbeta(qbeta(0.05, a, b, lower.tail = TRUE), a, b), 2), 
      lty = 2, col = "blue")

lines(x = c(-1, qbeta(0.05, a, b, lower.tail = FALSE)), 
      y = rep(dbeta(qbeta(0.05, a, b, lower.tail = FALSE), a, b), 2), 
      lty = 2, col = "blue")

lines(x = c(tmp2$CI_low, tmp2$CI_low), 
      y = c(0, dbeta(tmp2$CI_low, a, b)), 
      lty = 1, col = "red")

lines(x = c(tmp2$CI_high, tmp2$CI_high), 
      y = c(0, dbeta(tmp2$CI_high, a, b)), 
      lty = 1, col = "red")
abline(h = dbeta(c(tmp2$CI_low, tmp2$CI_high), a, b), lty = 2, col = "red")



# ------ 
abline(v = c(qbeta(0.05, a, b, lower.tail = TRUE), 
             qbeta(0.05, a, b, lower.tail = FALSE)), 
       lty = 2, col = "blue")

abline(h = c(dbeta(qbeta(0.05, a, b, lower.tail = TRUE), a, b), 
             dbeta(qbeta(0.05, a, b, lower.tail = FALSE), a, b)), 
       lty = 2, col = "blue")

abline(v = c(tmp2$CI_low, tmp2$CI_high), lty = 2, col = "red")
abline(h = dbeta(c(tmp2$CI_low, tmp2$CI_high), a, b), lty = 2, col = "red")


