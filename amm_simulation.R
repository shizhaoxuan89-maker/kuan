# AMM Simulation for Uniswap V2 and V3
# Initial pool: x = 10,000 tokens, y = 500 tokens
# Swap fee: 0.3%
# Liquidity provider owns 15% of pool shares

set.seed(123)

library(ggplot2)

simulate_uniswap_v2 <- function(nSteps, init_x, init_y, fee, lp_share) {
  pool_x <- numeric(nSteps)
  pool_y <- numeric(nSteps)
  price   <- numeric(nSteps)
  trade_volume <- numeric(nSteps)
  trade_dir    <- character(nSteps)
  lp_value <- numeric(nSteps)
  hodl_value <- numeric(nSteps)
  imp_loss <- numeric(nSteps)
  lp_fee_income <- numeric(nSteps)
  profitability <- numeric(nSteps)

  pool_x[1] <- init_x
  pool_y[1] <- init_y
  price[1]  <- pool_y[1] / pool_x[1]
  initial_lp_x <- init_x * lp_share
  initial_lp_y <- init_y * lp_share
  initial_value <- initial_lp_x * price[1] + initial_lp_y

  for (t in 2:nSteps) {
    direction <- sample(c("X_to_Y", "Y_to_X"), 1)
    trade_dir[t] <- direction
    volume <- runif(1, 1, 20)
    trade_volume[t] <- volume

    if (direction == "X_to_Y") {
      dx <- volume
      dx_net <- dx * (1 - fee)
      k <- pool_x[t-1] * pool_y[t-1]
      new_x <- pool_x[t-1] + dx
      new_y <- k / (pool_x[t-1] + dx_net)
      pool_x[t] <- new_x
      pool_y[t] <- new_y
      fee_value <- fee * dx * price[t-1]
    } else {
      dy <- volume
      dy_net <- dy * (1 - fee)
      k <- pool_x[t-1] * pool_y[t-1]
      new_y <- pool_y[t-1] + dy
      new_x <- k / (pool_y[t-1] + dy_net)
      pool_x[t] <- new_x
      pool_y[t] <- new_y
      fee_value <- fee * dy
    }
    price[t] <- pool_y[t] / pool_x[t]
    lp_fee_income[t] <- lp_fee_income[t-1] + lp_share * fee_value
    lp_value[t] <- lp_share * (pool_x[t] * price[t] + pool_y[t]) + lp_fee_income[t]
    hodl_value[t] <- initial_lp_x * price[t] + initial_lp_y
    imp_loss[t] <- lp_value[t] - hodl_value[t]
    profitability[t] <- lp_value[t] - initial_value
  }
  data.frame(step = 1:nSteps, pool_x = pool_x, pool_y = pool_y,
             price = price, trade_volume = trade_volume, trade_dir = trade_dir,
             lp_value = lp_value, hodl_value = hodl_value,
             imp_loss = imp_loss, lp_fee_income = lp_fee_income,
             profitability = profitability)
}

simulate_uniswap_v3 <- function(price_series, volume_series, fee, lp_share, p_lower, p_upper) {
  nSteps <- length(price_series)
  s_a <- sqrt(p_lower)
  s_b <- sqrt(p_upper)
  s_p0 <- sqrt(price_series[1])

  init_x <- 10000 * lp_share
  init_y <- 500 * lp_share
  L_x <- init_x * s_p0 * s_b / (s_b - s_p0)
  L_y <- init_y / (s_p0 - s_a)
  L <- min(L_x, L_y)
  deposit_x <- L * (s_b - s_p0) / (s_p0 * s_b)
  deposit_y <- L * (s_p0 - s_a)
  initial_value <- deposit_x * price_series[1] + deposit_y

  x_hold <- numeric(nSteps)
  y_hold <- numeric(nSteps)
  lp_fee_income <- numeric(nSteps)
  lp_value <- numeric(nSteps)
  hodl_value <- numeric(nSteps)
  imp_loss <- numeric(nSteps)
  profitability <- numeric(nSteps)

  for (t in 1:nSteps) {
    s_p <- sqrt(price_series[t])
    if (s_p <= s_a) {
      x_hold[t] <- L * (s_b - s_a) / (s_a * s_b)
      y_hold[t] <- 0
    } else if (s_p >= s_b) {
      x_hold[t] <- 0
      y_hold[t] <- L * (s_b - s_a)
    } else {
      x_hold[t] <- L * (s_b - s_p) / (s_p * s_b)
      y_hold[t] <- L * (s_p - s_a)
    }
    if (t > 1) lp_fee_income[t] <- lp_fee_income[t-1]
    if (s_p > s_a && s_p < s_b) {
      fee_value <- fee * volume_series[t] * price_series[t] * lp_share
      lp_fee_income[t] <- lp_fee_income[t] + fee_value
    }
    lp_value[t] <- x_hold[t] * price_series[t] + y_hold[t] + lp_fee_income[t]
    hodl_value[t] <- deposit_x * price_series[t] + deposit_y
    imp_loss[t] <- lp_value[t] - hodl_value[t]
    profitability[t] <- lp_value[t] - initial_value
  }
  data.frame(step = 1:nSteps, price = price_series, volume = volume_series,
             x_hold = x_hold, y_hold = y_hold, lp_fee_income = lp_fee_income,
             lp_value = lp_value, hodl_value = hodl_value,
             imp_loss = imp_loss, profitability = profitability)
}

nSteps <- 5000
fee <- 0.003
lp_share <- 0.15

v2 <- simulate_uniswap_v2(nSteps, 10000, 500, fee, lp_share)

p_lower <- 0.9 * v2$price[1]
p_upper <- 1.1 * v2$price[1]
v3 <- simulate_uniswap_v3(v2$price, v2$trade_volume, fee, lp_share, p_lower, p_upper)

# Impermanent loss comparison
il_df <- rbind(
  data.frame(step = v2$step, IL = v2$imp_loss, Pool = "Uniswap V2"),
  data.frame(step = v3$step, IL = v3$imp_loss, Pool = "Uniswap V3")
)

ggplot(il_df, aes(x = step, y = IL, color = Pool)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Impermanent Loss vs HODL", x = "Step", y = "Value (Token Y)") +
  theme_minimal()

ggsave("impermanent_loss.png", width = 8, height = 4)

# Fee income comparison
fee_df <- rbind(
  data.frame(step = v2$step, Fees = v2$lp_fee_income, Pool = "Uniswap V2"),
  data.frame(step = v3$step, Fees = v3$lp_fee_income, Pool = "Uniswap V3")
)

ggplot(fee_df, aes(x = step, y = Fees, color = Pool)) +
  geom_line() +
  labs(title = "Cumulative Fee Income", x = "Step", y = "Fees (Token Y)") +
  theme_minimal()

ggsave("fee_income.png", width = 8, height = 4)
