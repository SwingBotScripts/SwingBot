
# Load Packages

Packages <- c("dplyr", "lubridate", "purrr", "dbplyr", "RobinHood", "quantmod", "zoo", "roll", "ggplot2", "twitteR",
              "data.table", "gridExtra", "scales", "stringr", "webshot", "ggthemes", "png", "grid")

lapply(Packages, library, character.only = TRUE) 

# Twitter Credentials 

  consumer_key <- "*********"
  
  consumer_secret <- "*********"
  
  access_token <- "*********"
  
  access_secret <-  "*********"
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
  Sys.sleep(60*30)
  
  options(scipen = 999)
  ##########################

  PickList <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(PickList) <- c("Time", "Pick")
  
  z = 1
  
  repeat{
   
  df <-     try(list.files(pattern = "*data.csv") %>%     map_df(~fread(.), fill = TRUE))
 
  #Modified to include extra stocks 
  df <- df[,-c(1)]
  df <- as.data.frame(df)
  df$Time <- ymd_hms(df$Time, truncated = 2)
  df <- df %>% arrange(Time)
  df_cols <- ncol(df)
  
  #Define interval as the number of rows that equate to one minute of data. In my experience, it was 10 rows. 
  interval <- round(min(nrow(df)/120, 10),0)
  
  #The following loop is the buildup of technical analysis that is done on each stock. 
  #This strategy is attempting to flag "oversold" loans. Customize all you want. 
  i <- 2
  
  repeat {
    
  #60 minute SMA  
  df[ncol(df)+1] <- rollmean(df[i], k = 60*interval, fill= NA, align = "right")
  name <- paste(colnames(df[i]), "SMA", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #60 minute SD
  df[ncol(df)+1] <- roll_sd(as.numeric(df[,i]), width = 60*interval, complete_obs = FALSE)
  name <- paste(colnames(df[i]), "SD", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #Lower BB at 1.9 standard deviations
  df[ncol(df)+1] <- df[ncol(df)-1] - (df[ncol(df)] * 1.9)
  name <- paste(colnames(df[i]), "LBB", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #Oversold indicator based on price below lower BB
  df[ncol(df)+1] <-   ifelse(df[i] < df[ncol(df)], 1,0)
  name <- paste(colnames(df[i]), "OS", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - 52 period EMA (26)
  df[ncol(df)+1] <-   rollmean(df[i], k = 52*interval, fill= NA, align = "right", ema = TRUE)
  name <- paste(colnames(df[i]), "MACD26", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - 24 period EMA 
  df[ncol(df)+1] <-   rollmean(df[i], k = 24*interval, fill= NA, align = "right", ema = TRUE)
  name <- paste(colnames(df[i]), "MACD12", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - MACD
  df[ncol(df)+1] <-   df[ncol(df)] - df[ncol(df)-1]
  
  name <- paste(colnames(df[i]), "MACD", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - 18 period EMA - signal line
  df[ncol(df)+1] <-   rollmean(df[ncol(df)], k = 18*interval, fill= NA, align = "right", ema = TRUE)
  name <- paste(colnames(df[i]), "EMA9", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - Histogram
  df[ncol(df)+1] <-   df[ncol(df)-1] - df[ncol(df)]
  name <- paste(colnames(df[i]), "MACDHIST", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - 40 period MACDHIST avg 
  df[ncol(df)+1] <-   rollmean(df[ncol(df)], k = 40*interval, fill= NA, align = "right", ema = TRUE)
  name <- paste(colnames(df[i]), "MACDHIST_6", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - Histogram Change relative to average
  df[ncol(df)+1] <-   df[ncol(df)-1] - df[ncol(df)]
  name <- paste(colnames(df[i]), "MACDHIST_Change", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #MACD Component - Histogram Change Directionality
  df[ncol(df)+1] <-   ifelse(df[ncol(df)] > 0, "rising", "falling")
  name <- paste(colnames(df[i]), "MACDHIST_direction", sep = "_")
  colnames(df)[ncol(df)] <- c(name)
  
  #Profit
  df[ncol(df)+1] <-   (df[ncol(df)-10]*2.5 / as.numeric(df[,i])) * 3000
  name <- paste(colnames(df[i]), "Profit", sep = "_")
  colnames(df)[ncol(df)] <- c(name)

  #MACD Component - Oversold Override
  df[ncol(df)-9] <-   ifelse(df[ncol(df)-9] == 1 & df[ncol(df)-1] == "rising" & df[ncol(df)] >20, 1, 0)
  
 i = i+1
  
  if (i > df_cols){
    break
  }}
  
#Return column names of stocks where OS = 1. These are identified as "oversold" using the logic above. 

Picks <- names(df)[which(df[nrow(df),] == 1)] 
Picks <- str_sub(Picks,1,nchar(Picks)-3)

PickList[z,1] <- as.character(df$Time[nrow(df)])
Pick <- Picks[1]
Past_Picks <- unique(PickList$Pick)

#This is some logic I was using to avoid repeat alerts and hold out some tickers that were causing trouble. 
Twitter_Pick <- ifelse(Pick %in% Past_Picks, NA, 
                       ifelse(Pick == "CMG", NA, 
                              ifelse(Pick == "GE", NA,
                                     ifelse(Pick == "TSLA", NA, 
                                            ifelse(Pick == "SPCE", NA,
                                                   ifelse(Pick == "PCG", NA, 
                                                          ifelse(nchar(Pick) < 2, NA, Pick)))))))

PickList[z,2] <- Twitter_Pick


#Filter for current date only for cleaner plotting

df$Day <- day(df$Time)
DayNum <- day(Sys.Date())
df <- df %>% filter(Day == DayNum)

Target_Data <- if(!is.na(Pick)) {select(df,Time, contains(Pick))}

Target_Profit <- Target_Data[nrow(Target_Data), 4]*2.5
Target_Entry <- Target_Data[nrow(Target_Data), 2]

#Chart used in Tweet alerts. 

# 1. Open jpeg file

filename <- paste(z, "temp.jpeg", sep = "_")

bmp(filename, width = 1100, height = 628)

# 2. Create the plot
PLOT <- ggplot(data = Target_Data, aes(x = Time)) +   geom_line(aes(y = Target_Data[,2]), color = "black") +  
  geom_line(aes(y = Target_Data[,3]), color="steelblue") +
  geom_line(aes(y = Target_Data[,5]), color="red")  +
  geom_vline(xintercept = Target_Data$Time[nrow(Target_Data)], color = "forestgreen", size = 1.5, alpha = 0.5) + 
  theme_classic() + ggtitle(paste("SwingBot Pick: ", Twitter_Pick, " (", Sys.Date(), ")", sep ="")) +
  theme(plot.background = element_rect(fill = "lightgrey"), axis.title.x=element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(colour="grey20",size=15,face="bold"),
        axis.text.y = element_text(colour="grey20",size=15,face="bold"),
        plot.title = element_text(colour="grey20",size=18,face="bold", hjust = 0.5))+
  geom_hline(yintercept = Target_Data[nrow(Target_Data),2] + 2.5*Target_Data[nrow(Target_Data),4], color = "forestgreen", linetype = "dashed")  +
  geom_hline(yintercept = Target_Data[nrow(Target_Data),2] , color = "black", linetype = "dashed") +
  geom_hline(yintercept = Target_Data[nrow(Target_Data),2] - 3.5*Target_Data[nrow(Target_Data),4], color = "red", linetype = "dashed")+
  scale_y_continuous(position = "right") +
  geom_point(aes(x = Target_Data$Time[nrow(Target_Data)], y =Target_Data[nrow(Target_Data),2]),  
             shape = 21, colour = "black", fill = NA, size = 6, stroke = 2) +
  annotate("text", x=Target_Data$Time[nrow(Target_Data)],y=Target_Data[nrow(Target_Data),2],hjust=1.3,vjust=-1.0,
           label = paste("BUY!", sep = ""), alpha = 1, colour="black", face = "bold" ,size=5) +
  geom_point(aes(x = Target_Data$Time[nrow(Target_Data)], y =Target_Data[nrow(Target_Data),2] + 2.5*Target_Data[nrow(Target_Data),4]),  
             shape = 25, colour = "forestgreen", fill = "forestgreen", size = 3, stroke = 1) +
  annotate("text", x=Target_Data$Time[nrow(Target_Data)], 
           y=Target_Data[nrow(Target_Data),2] + 2*Target_Data[nrow(Target_Data),4],hjust=1.15,vjust=-.1,
         label = paste("Target Profit", sep = ""), alpha = 1, colour="black", face = "bold" ,size=5) +
  geom_point(aes(x = Target_Data$Time[nrow(Target_Data)], y =Target_Data[nrow(Target_Data),2] + -3.5*Target_Data[nrow(Target_Data),4]),  
             shape = 24, colour = "red", fill = "red", size = 3, stroke = 1) 
  
if(!is.na(Twitter_Pick)){print(PLOT)}

# 3. Close the file
dev.off()

tw <- if(!is.na(Twitter_Pick)) {updateStatus(paste("Now Buying: $", Twitter_Pick, " at ~" , dollar(Target_Entry), 
                                                   "\n\nExit Target: ", dollar(Target_Data[nrow(Target_Data), 4]*2.5 + Target_Data[nrow(Target_Data), 2]),
                                                   " (Profit: ", dollar(Target_Profit), " (", round((100*(Target_Profit/Target_Entry)),2),"%))",
                                                   "\n\nStop Loss: ", dollar(Target_Data[nrow(Target_Data), 2]- Target_Data[nrow(Target_Data), 4]*3.5), 
                                                   "\n\n#daytrading", sep = ""), 
                                             mediaPath = filename)}


#After hours analysis- can be used to simulate all of the day's trading decisions. Used for sharpen strategy after hours.  

# a <- 1
# 
# repeat {
# 
# Picks <- names(df)[which(df[a,] == 1)]
# Picks <- str_sub(Picks,1,nchar(Picks)-3)
# 
# PickList[a,1] <- as.character(df$Time[a])
# Pick <- Picks[1]
# Past_Picks <- unique(PickList$Pick)
# PickList[a,2] <- ifelse(Pick %in% Past_Picks, NA, Pick)
# 
# a = a+1
# 
# if (a > 4300){
#   break
# }}

 z = z+1
 
 time <- Sys.time()
 
 write.csv(PickList, paste(hour(time), minute(time), round(second(time),0), "picklist.csv", sep = "_"))
 
 Sys.sleep(45)

  if (z > 1500){
   break  }}