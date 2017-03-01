# Categorical two bin example
chk_x2 <- runif(22)
chk_x2 <- chk_x2[order(chk_x2)]
chk_y2 <- as.factor(c("b", rep("a", 10), rep("b", 10), "a"))
chk_d2 <- data.frame(y = chk_y2, x = chk_x2)

# Categorical three bin example
chk_x3 <- runif(60)
chk_x3 <- chk_x3[order(chk_x3)]
chk_y3 <- as.factor(c(rep("a", 20), rep("b", 20), rep("a", 20)))
chk_d3 <- data.frame(y = chk_y3, x = chk_x3)

# Categorical four bin example
chk_x4 <- runif(80)
chk_x4 <- chk_x4[order(chk_x4)]
chk_y4 <- as.factor(c(rep("a", 20), rep("b", 20), rep("a", 20), rep("b", 20)))
chk_d4 <- data.frame(y = chk_y4, x = chk_x4)

# Continuous quadratic example
chk_xq <- 100*runif(1000)
chk_yq <- 100*chk_xq - chk_xq^2 + 10*rnorm(1000)
chk_dq <- data.frame(y = chk_yq, x = chk_xq)

# Continuous linear example
chk_xl <- runif(1000)
chk_yl <- chk_xl - 0.5 + 0.25*rnorm(1000)
chk_dl <- data.frame(y = chk_yl, x = chk_xl)

# Continuous sine wave example
chk_xs <- seq(-2*pi, 2*pi, length.out = 100)
chk_ys <- sin(chk_xs) + 0.2*rnorm(100)
chk_ds <- data.frame(y = chk_ys, x = chk_xs)

# Continuous random example
chk_xr <- 100*runif(1000)
chk_yr <- 100*runif(1000)
chk_dr <- data.frame(y = chk_yr, x = chk_xr)