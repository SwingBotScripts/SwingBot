

#SwingBot Ticker

Packages <- c("dplyr", "lubridate", "purrr", "dbplyr", "RobinHood", "quantmod", "zoo", "roll", "ggplot2", "twitteR")

lapply(Packages, library, character.only = TRUE)

# Trading pipeline...

# Stock List

DOW_30 <- c("AXP",  "AMGN", "AAPL", "TSLA", "CAT",  "CSCO", "CVX",  
            "GS",   "HD",   "HON",  "IBM",  "INTC", "JNJ",  "KO",  
            "JPM",  "MCD",  "MMM",  "MRK",  "MSFT", "NKE",  "PG",  
            "TRV",  "UNH",  "CRM",  "VZ",   "SNAP", "WBA",  "WMT", 
            "DIS",  "DOW",  "SQ",   "NFLX", "FB",   "RH",   "GE",  
            "TWTR", "KR",   "AAL",  "CCL",  "COIN", "MRNA", "EAT",  
            "YUM",  "SBUX", "DPZ",  "WEN",  "SPCE", "BAC",  "NIO",  
            "PFE")

# RH Credentials
RH = RobinHood(username = "XXXXXXX@gmail.com", password = "XXXXXXX")

#Retrieve mark price repeatedly to build history for analysis

b <- 1

repeat{

a <- 1

df <- data.frame(matrix(ncol = length(DOW_30)+1, nrow = 0))

x <- c("Time", DOW_30[1:length(DOW_30)])
colnames(df) <- x

repeat{

i <- 1  

df[a,1] <- as.character(Sys.time())

repeat{

df[a,i+1] <- get_quote(RH, symbol= DOW_30[i])[2]

i = i +1

if (i > length(DOW_30)){
  break
}}

a = a+1

if (a > 10){
  break
}}

time <- Sys.time()

write.csv(df, paste(hour(time), minute(time), round(second(time),0), "data.csv", sep = "_"))

b = b+1
if (b > 500){
  break}}