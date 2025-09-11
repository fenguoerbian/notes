K <- 2
pi_vec <- runif(K)
pi_vec <- pi_vec / sum(pi_vec)
lam_vec <- runif(K, min = 0, max = 0.5)
beta <- -0.23

hrt <- function(t_val, lam_vec, pi_vec, beta){
  p11 <- sum(pi_vec * exp(- lam_vec * t_val))
  p12 <- sum(pi_vec * lam_vec * exp(-lam_vec * t_val))
  p1 <-  p11 / p12 
  
  p21 <- sum(pi_vec * lam_vec * exp(beta) * exp(-lam_vec * t_val * exp(beta)))
  p22 <- sum(pi_vec * exp(-lam_vec * t_val * exp(beta)))
  p2 <- p21 / p22
  
  res <- p1 * p2
}

hr <- function(t_vec, lam_vec, pi_vec, beta){
  sapply(t_vec, hrt, 
         lam_vec = lam_vec, 
         pi_vec = pi_vec, 
         beta = beta)
}

t_vec <- seq(0, 200, by = 0.01)
hr_vec <- hr(t_vec, lam_vec, pi_vec, beta)
plot(t_vec, hr_vec, type = "l")
abline(h = exp(beta), lty = 2)

