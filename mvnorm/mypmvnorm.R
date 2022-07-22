mypmvnorm <- function(a, b, rho, upper = 10000, stepsize = 0.001){
    z_vec <- seq(from = b, to = upper, by = stepsize)
    res <- sum(
        dnorm(z_vec) * pnorm((rho * z_vec - a) / sqrt(1 - rho ^ 2))) * stepsize
    return(res)
}

a <- -1
b <- 0
rho <- 0.25

mypmvnorm(a, b, rho)
pmvnorm(lower = c(a, b), 
        corr = rbind(c(1, rho), c(rho, 1)))
