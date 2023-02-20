# normal approximation 
power1 <- function(n, p_trt, p_ctrl, alpha){
    cutoff <- qnorm(alpha / 2)
    mean_alt <- (p_trt - p_ctrl) / sqrt(p_ctrl * (1 - p_ctrl) / n)
    sd_alt <- sqrt((p_trt * (1 - p_trt)) / p_ctrl / (1 - p_ctrl))
    p1 <- pnorm(cutoff, mean = mean_alt, sd = sd_alt, lower.tail = TRUE)
    p2 <- pnorm(-cutoff, mean = mean_alt, sd = sd_alt, lower.tail = FALSE)
    return(p1 + p2)
}

uniroot(function(n, p_trt, p_ctrl, alpha, power){
    power1(n, p_trt, p_ctrl, alpha) - power
}, interval = c(10, 100), p_trt = 0.65, p_ctrl = 0.4, alpha = 0.05, power = 0.91)

power1(41, 0.65, 0.4, 0.05)

41 / 0.8

plot(10 : 100, power1(10 : 100, 0.65, 0.4, 0.05), type = "l")


# exact test
power2 <- function(n, p_trt, p_ctrl, alpha, alternative = "two.sided"){
    power <- 0
    for(i in (seq(n + 1) - 1)){
        prob <- dbinom(i, size = n, prob = p_trt)
        tmp <- binom.test(i, n, p = p_ctrl, 
                          alternative = alternative, 
                          conf.level = 1 - alpha)
        power <- power + (tmp$p.value < alpha) * prob
    }
    return(power)
}

# There are different logics to find the sample size that meets the power requirement
uniroot(function(n, p_trt, p_ctrl, alpha, power){
    n <- round(n)
    power2(n, p_trt, p_ctrl, alpha, "greater") - power
}, interval = c(10, 100), p_trt = 0.65, p_ctrl = 0.4, alpha = 0.025, power = 0.90)

# one-sided and two-sided results are somewhat different in exact test
power2(41, 0.65, 0.4, 0.05, "two.sided")
power2(46, 0.65, 0.4, 0.025, "greater")


# normal approximation 2?
power3 <- function(n, p_trt, p_ctrl, alpha, alternative = "two.sided"){
    power <- 0
    for(i in (seq(n + 1) - 1)){
        prob <- dbinom(i, size = n, prob = p_trt)
        tmp <- prop.test(i, n, p = p_ctrl, 
                          alternative = alternative, 
                          conf.level = 1 - alpha)
        power <- power + (tmp$p.value <= alpha) * prob
    }
    return(power)
}

uniroot(function(n, p_trt, p_ctrl, alpha, power){
    n <- round(n)
    power3(n, p_trt, p_ctrl, alpha) - power
}, interval = c(10, 100), p_trt = 0.65, p_ctrl = 0.4, alpha = 0.05, power = 0.90)

power3(43, 0.65, 0.4, 0.05)


