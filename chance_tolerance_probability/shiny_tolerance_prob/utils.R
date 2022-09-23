Get_Chance <- function(n = 6, a = 0.1, cv = 0.15, width_limit = 0.35){
    # 注意：这里的`width_limit`是对数变换后的CI区间的**半宽度**要求
    sigma2 <- log(1 + cv ^ 2)
    t_cut <- qt(1 - a / 2, df = n - 1)
    chisq_cut <- width_limit ^ 2 * n * (n - 1) / t_cut ^ 2 / sigma2
    chance <- pchisq(chisq_cut, df = n - 1)
    return(chance)
}

Get_Width <- function(n = 6, a = 0.1, cv = 0.15, chance = 0.7){
    # 注意：本函数计算的是log变换后尺度上的区间半宽度
    sigma2 <- log(1 + cv ^ 2)
    t_cut <- qt(1 - a / 2, df = n - 1)
    chisq_cut <- qchisq(chance, df = n - 1)
    width <- sqrt(chisq_cut * t_cut ^ 2 * sigma2 / n / (n - 1))
    return(width)
}

Case1_Compute <- function(cv_vec, chance_vec, n_vec, a){
    res <- expand.grid(cv = cv_vec, chance = chance_vec, n = n_vec, a = a)
    for(i in seq(nrow(res))){
        tmp <- Get_Width(n = res$n[i], a = res$a[i], 
                         cv = res$cv[i], chance = res$chance[i])
        res$lower[i] <- exp(-tmp)
        res$upper[i] <- exp(tmp)
        res$width_log[i] <- abs(tmp)    # log变换尺度上的区间半宽度
    }
    return(res)
}

Case2_Compute <- function(cv_vec, n_vec, width_limit, a){
    res <- expand.grid(cv = cv_vec, n = n_vec, width_limit = width_limit, a = a)
    for(i in seq(nrow(res))){
        tmp <- Get_Chance(n = res$n[i], a = res$a[i], 
                          cv = res$cv[i], width_limit = res$width_limit[i])
        res$chance[i] <- tmp
    }
    return(res)
}