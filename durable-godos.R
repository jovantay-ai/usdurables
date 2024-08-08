library(MacrobondAPI)
library(xts)

rm(list=ls())

# load data
## (1) real expenditures on durables, (2) nondurables
## (3) deflators on durables (4) deflators on nondurables
## (5) nominal consumer lending rate
tick <- c(expend_d = "usnaac0172",
          expend_nd = "usnaac0177",
          price_d = "usnaac0676",
          price_nd = "usnaac0677",
          credit = "usbank0108")

data_raw <- FetchTimeSeries(tick)
sapply(data_raw, getIsError)
names(data_raw) <- names(tick)
data <- rapply(data_raw, as.xts, how="replace")

# window to same time period
for(i in 1:length(data)){
  data[[i]] <- window(data[[i]], start=as.Date("2002-01-01"),
                      end=as.Date("2024-01-01"))
}

# making series stationary - QoQ
for (i in 1:(length(data))){
  data[[i]] <- diff(log(data[[i]])) * 100 
} 

# remove the first row bc of difference (2002 Q2 to 2024 Q1)
for (i in 1:length(data)){
  data[[i]] <- data[[i]][-1,]
}

# convert to mts object
xts_to_ts <- function(xts_obj) {
  start_year <- as.numeric(format(start(xts_obj), "%Y"))
  start_period <- as.numeric(format(start(xts_obj), "%m")) / 12 + 1
  freq <- 4  # Monthly data, so frequency is 12
  ts_obj <- ts(coredata(xts_obj), start = c(start_year, start_period), frequency = freq)
  return(ts_obj)
}
ts_list <- lapply(data, xts_to_ts)
data_mts <- do.call(ts.union, ts_list)


# decomposition
library(bsvarSIGNs)

sign_bl <- matrix(NA, 5,5)
sign_bl[,1] <- c(1,1,0,0,NA)
sign_bl[,2] <- c(1,-1,0,0,NA)
sign_bl[,3] <- c(NA,NA,1,1,1)
sign_bl[,4] <- c(NA,NA,1,-1,NA)
sign_bl[,5] <- c(1,NA,1,1,-1)

specs_bl = specify_bsvarSIGN$new(
  data=data_mts,
  p = 1L,
  sign_irf = sign_bl
)

posterior = estimate(specs_bl, S=1000)

irf = compute_impulse_responses(posterior, horizon=8)
plot(irf, probability=0.68)

decomp = compute_historical_decompositions(posterior)
plot(decomp)
