############################################################
##
## Code for pricing Snowball structured products 
##
############################################################

data <- read.csv("data.csv", stringsAsFactors = FALSE)
n <- nrow(data)
head(data)
stock <- data[, 2]
stock.r <- diff(log(stock))
stock.vol <- sd(stock.r) / sqrt(1/252)
acf(stock.r)
qqnorm(stock.r)
qqline(stock.r)

set.seed(10)                  # get reproducible results
d <- 100000                   # simulation trials
T <- 1                        # time until expiration (in years)
m <- T * 252                  # number of subintervals
delta.t <- T / m              # time per subinterval (in years)
r <- 0.05                     # interest rate
cupoun_r <- 0.312             # Cupoun rate
option <- 1000000             # Product price
s <- rep(0, m + 1)            # stock price
f <- rep(0, d)                # payoff
s[1] <- stock[n]              # initial stock price
K_out <- s[1] * 1.03          # Knock out price
K_in <- s[1] * 0.73           # Knock in price
obs_date = seq(1, m, 12) + 11 # Knock out observation date


for (j in 1:d) {              # begin new trial
  k.in = FALSE
  k.out = FALSE
  for (i in 2:(m + 1)) {      # simulate stock price path
    W <- sqrt(delta.t) * rnorm(1)
    ret  <- (r-(1/2) * stock.vol^2) * delta.t + stock.vol * W
    s[i] <- s[i - 1] * exp(ret)
    # If stock price excess knock out price on knock out observation date, exit.
    if (s[i]>=K_out & i %in% obs_date==TRUE) {    
      k.out = TRUE
      break
    }
    if (s[i]<=K_in) k.in = TRUE
  }
  
  if (k.in==TRUE & k.out==FALSE)  # Knock in but not Knock out, earn or loss price movement.
    f[j] <- exp(-r * T) * (s[i]/s[1] - 1) * option
  else                            # Other situation, earn mixed income.
    f[j] <- exp(-r * T) * cupoun_r * i/252 * option
  print(i)
}
cat("Product Price Estimate:", round(mean(f), 4), "\n")
cat("Standard Error:", round(sd(f) / sqrt(d), 4), "\n")






