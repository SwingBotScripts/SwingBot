
#Tool to generate all permutations of 1-4 letter combinations to extract all active tickers from Robinhood

Packages <- c("dplyr", "lubridate", "purrr", "dbplyr", "RobinHood", "quantmod", "zoo", "roll", "ggplot2", "twitteR", "gtools")

lapply(Packages, library, character.only = TRUE)

# describing the input vector

vec<- LETTERS[1:26]

# getting permutations on choose 1-4 with replacement
Tickers_1 <- as.data.frame(permutations(n=26,r=1,v=vec,repeats.allowed=T))
Tickers_1$Ticker <- Tickers_1$V1
Tickers_1 <- Tickers_1 %>% select(Ticker)

Tickers_2 <- as.data.frame(permutations(n=26,r=2,v=vec,repeats.allowed=T))
Tickers_2$Ticker <- paste(Tickers_2$V1, Tickers_2$V2, sep = "")
Tickers_2 <- Tickers_2 %>% select(Ticker)

Tickers_3 <- as.data.frame(permutations(n=26,r=3,v=vec,repeats.allowed=T))
Tickers_3$Ticker <- paste(Tickers_3$V1, Tickers_3$V2, Tickers_3$V3, sep = "")
Tickers_3 <- Tickers_3 %>% select(Ticker)

Tickers_4 <- as.data.frame(permutations(n=26,r=4,v=vec,repeats.allowed=T))
Tickers_4$Ticker <- paste(Tickers_4$V1, Tickers_4$V2, Tickers_4$V3, Tickers_4$V4, sep = "")
Tickers_4 <- Tickers_4 %>% select(Ticker)

Tickers <- rbind(Tickers_1, Tickers_2, Tickers_3, Tickers_4)

rm(Tickers_1, Tickers_2, Tickers_3, Tickers_4)

TICKER_LIST <- Tickers

# RH Credentials
RH = RobinHood(username = "email@email.com", password = "password")

a <- 1

df <- as.data.frame(TICKER_LIST)

repeat{
  
  i <- 1  
  
   repeat{
    
    df[i,2] <- try(get_quote(RH, symbol= TICKER_LIST[i])[2])
    
    i = i +1
    
    print(i)
    if (i > length(TICKER_LIST)){
      break
    }}
  
  a = a+1
  
  if (a > 1){
    break
  }}

#Filters (optional)
df1$last_trade_price <- as.numeric(df1$last_trade_price)
summary(df1$last_trade_price)
df2 <- df1 %>% filter(last_trade_price < 1000)
df2 <- df2 %>% filter(last_trade_price > 0.5)
df3 <- df2 %>% select(TICKER_LIST)

#Export list 
write.csv(df3, file = "RH_Ticker_List.csv")

#Test list for options contracts for further filtering

Tickers <- read.csv(file = "RH_Ticker_List.csv")

TICKER_LIST <- Tickers$TICKER_LIST

a <- 1

df <- as.data.frame(TICKER_LIST)

repeat{
  
  i <- 1  

  repeat{
    
    df[i,2] <- try(get_contracts(RH, type = "call", TICKER_LIST[i])[1,3])
    
    i = i +1
    
    print(i)
    if (i > length(TICKER_LIST)){
      break
    }}
  
  a = a+1
  
  if (a > 1){
    break
  }}

df2 <- dplyr::filter(df, !grepl('Error', V2))
write.csv(df2, file = "RH_Tickers.csv")