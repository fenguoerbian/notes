## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## -----------------------------------------------------------------------------
library(ggplot2)

## ----fun_get_chance, echo = FALSE---------------------------------------------
Get_Chance <- function(n = 6, a = 0.1, cv = 0.15, width_limit = 0.35){
    # 注意：这里的`width_limit`是对数变换后的CI区间的**半宽度**要求
    sigma2 <- log(1 + cv ^ 2)
    t_cut <- qt(1 - a / 2, df = n - 1)
    chisq_cut <- width_limit ^ 2 * n * (n - 1) / t_cut ^ 2 / sigma2
    chance <- pchisq(chisq_cut, df = n - 1)
    return(chance)
}

## -----------------------------------------------------------------------------
a <- 0.1
cv <- 0.4
n <- 6
width_limit <- 0.35

# Get_Chance() 定义见后文
chance <- Get_Chance(n = n, a = a, cv = cv, width_limit = width_limit)
print(paste0("Chance is : ", chance))


## ---- echo = FALSE------------------------------------------------------------
width_vec <- seq(from = 0.1, to = 0.5, by = 0.025)
chance_vec <- Get_Chance(n = n, a = a, cv = cv, width_limit = width_vec)
data.frame(width = width_vec, chance = chance_vec) |>
    ggplot(aes(x = width, y = chance)) + 
    geom_line() + 
    geom_point() + 
    labs(x = "Half width (under log scale)", 
         y = "Chance")

## -----------------------------------------------------------------------------
Get_Width <- function(n = 6, a = 0.1, cv = 0.15, chance = 0.7){
    # 注意：本函数计算的是log变换后尺度上的区间半宽度
    sigma2 <- log(1 + cv ^ 2)
    t_cut <- qt(1 - a / 2, df = n - 1)
    chisq_cut <- qchisq(chance, df = n - 1)
    width <- sqrt(chisq_cut * t_cut ^ 2 * sigma2 / n / (n - 1))
    return(width)
}

## -----------------------------------------------------------------------------
cv_vec <- seq(from = 0.15, to = 0.4, by = 0.05)
chance_vec <- c(0.7, 0.8)
n_vec <- 6
a <- 0.1
res <- expand.grid(cv = cv_vec, chance = chance_vec, n = n_vec, a = a)
for(i in seq(nrow(res))){
    tmp <- Get_Width(n = res$n[i], a = res$a[i], 
                     cv = res$cv[i], chance = res$chance[i])
    res$lower[i] <- exp(-tmp)
    res$upper[i] <- exp(tmp)
    res$width_log[i] <- abs(tmp)    # log变换尺度上的区间半宽度
}
knitr::kable(res)

## ---- echo = FALSE------------------------------------------------------------
readr::write_csv(res, file = "case1.csv")

fig <- ggplot(res, aes(y = cv)) + 
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.01) + 
    facet_grid(rows = vars(chance), cols = vars(n), labeller = label_both) + 
    labs(x = "")
print(fig)

## ---- ref.label="fun_get_chance", eval = FALSE--------------------------------
#  Get_Chance <- function(n = 6, a = 0.1, cv = 0.15, width_limit = 0.35){
#      # 注意：这里的`width_limit`是对数变换后的CI区间的**半宽度**要求
#      sigma2 <- log(1 + cv ^ 2)
#      t_cut <- qt(1 - a / 2, df = n - 1)
#      chisq_cut <- width_limit ^ 2 * n * (n - 1) / t_cut ^ 2 / sigma2
#      chance <- pchisq(chisq_cut, df = n - 1)
#      return(chance)
#  }

## -----------------------------------------------------------------------------
cv_vec <- seq(from = 0.15, to = 0.4, by = 0.05)
width_limit <- 0.3576744    # 原始尺度上的(0.7, 1.43)对应对数尺度上的0.35的半宽度
n_vec <- 6
a <- 0.1

res <- expand.grid(cv = cv_vec, n = n_vec, width_limit = width_limit, a = a)
for(i in seq(nrow(res))){
    tmp <- Get_Chance(n = res$n[i], a = res$a[i], 
                      cv = res$cv[i], width_limit = res$width_limit[i])
    res$chance[i] <- tmp
}
knitr::kable(res[, c("cv", "n", "chance")])

## ---- echo = FALSE, fig.showtext=TRUE-----------------------------------------
readr::write_csv(res, file = "case2.csv")

fig <- res |>
    ggplot(aes(x = cv, y = chance)) + 
    geom_line(aes(color = factor(n))) + 
    geom_point(aes(color = factor(n))) + 
    labs(color = "sample size(n)", 
         title = paste0("alpha =", a, ", 对数变换后CI半宽度限制：", width_limit))

print(fig)

## -----------------------------------------------------------------------------
sessioninfo::session_info()

