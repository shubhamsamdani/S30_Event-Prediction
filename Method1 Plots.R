
library(dplyr)
library(lubridate)
library(ggplot2)
setwd("E:\\Research\\BTP\\My BTP\\BM\\BM whole dataset")

p <- list.files(".",".csv")
#remove extra read names
#remove this for Jalli
p<- p[grep("2016*",p)]

firs <- gsub('.csv','',p[1])
firs <- gsub('[^a-zA-Z]','',firs)
print(firs)
firs <- "brexit"

new = data.frame(DATE=as.Date("2016-10-31"),Unique_Users=0, Unique_Tweets=0,
                 Total_tweet=0, pos =0)

for( i in 1:length(p)){
  a <- read.csv(p[i], stringsAsFactors = FALSE)
  uni_user <- length(unique(a$screenName))
  #fOR hOLI
  #date <- unique(as.Date(a$created, format= "%d-%m-%y"))
  #For others
  date <- unique(as.Date(a$created))
  Ttweet <- nrow(a)
  UTweets <- length(which(a$isRetweet== FALSE))
  new[i,] <- data.frame(DATE=date, Unique_Users=uni_user,Unique_Tweets=UTweets,
                        Total_tweet=Ttweet, pos=0)
}

  
  if(which.max(new$Unique_Users)==1){
    d <- new$DATE[which.max(new$Unique_Users)]
    new[i,] <- data.frame(DATE=d-1, Unique_Users=0,Unique_Tweets=0,
                          Total_tweet=0, pos=0)
    
  }
  new <- arrange(new, DATE)
  
  #new[6,1] <- as.Date("2017-01-17")
  #new[8,1] <- as.Date("2017-01-19")
  
  l <- ggplot(new, aes(DATE, Unique_Users)) + 
    geom_point() + geom_line(aes(group=1))
  l
  lok <- new
  po=as.character.Date(format(strftime(as.Date(new$DATE), "%d-%m-%Y")))
  new$DATE <- factor(po, levels = po)

  new$pos <- 1: nrow(new)
  max_pos <- which.max(new$Unique_Users)
  min_pos <- which.min(new$Unique_Users)
  
  x_p <- new$Unique_Users[max_pos]
  x_v <- new$Unique_Users[min_pos]



#Finding the minimas in data set
  # minima <- c()
  # h_pos_min <- c() #position of h in original dataset
  # k <- 1
  # for( i in 2:(length(new$Unique_Users)-1)){
  #   if(new$Unique_Users[i] - new$Unique_Users[i-1] <= 0 && 
  #      new$Unique_Users[i] - new$Unique_Users[i+1] <= 0){
  #     minima[k] <- new$Unique_Users[i]
  #     h_pos_min[k] <- new$pos[i]
  #     k <- k+1
  #   }
  # }
  # minima
  # h_pos_min
  
  L=75
  # mins=h_pos_min[h_pos_min < max_pos]
  # 
  # minless <- c()
  # l1 <- 1
  # if(length(mins)==0){
  #   xa=new$Unique_Users[1]
  # }else{
  #   for(i in length(mins):1){
  #     if(new$Unique_Users[mins[i]] <= (x_p)*L/100){
  #       minless <- c(minless, mins[i])
  #     }
  #   }
  #   xa <- 
  #   l1 <- minless[1]
  # }
  # 
  
  # maxs=h_pos_min[h_pos_min > max_pos]
  # 
  # l2 <- nrow(new)
  # maxgtr <- c()
  # if(length(maxs)==0){
  #   xb=new$Unique_Users[nrow(new)]
  # }else{
  #   for(i in 1:length(maxs)){
  #     if(new$Unique_Users[maxs[i]] <= (x_p)*L/100){
  #       maxgtr <- c(maxgtr, maxs[i])
  #     }
  #   }
  #   xb <- new$Unique_Users[new$pos == maxgtr[1]]
  #   l2 <- maxgtr[1]
  # }

  xa <- new$Unique_Users[new$DATE == "21-06-2016"]
  xb <- new$Unique_Users[new$DATE == "26-06-2016"]
  
  ttt<-61600
  df_L <- data.frame(x1=new$DATE[max_pos], y1=0, 
                     x2=new$DATE[max_pos], y2= (x_p)*L/100)
  
  g <-ggplot(new, aes(DATE, Unique_Users)) + ylab("No. of Unique Users") + 
    xlab("Date")+ geom_point() + geom_smooth() + #theme(axis.text = element_text(face="bold"))+
    geom_line(aes(group=1), size=1.0) + theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_hline(yintercept = x_p, color="black", linetype= "longdash") +
    annotate("text", x = new$DATE[(new$pos[1] + new$pos[nrow(new)])/2], y = x_p+ttt/2.5, 
             label = 'bold("Peak")', colour = "black", parse=TRUE)+
    geom_point(aes(x=new$DATE[max_pos],
                   y=new$Unique_Users[match(x_p, new$Unique_Users) ]), 
               shape=18, size=4)+
    annotate("text", x = new$DATE[max_pos], y = (x_p)+ttt/2, 
             label = 'x[p]', colour = "black", parse=TRUE)
  
  
  g <- g + geom_point(aes(x=new$DATE[match(xa, new$Unique_Users) ], 
                          y=new$Unique_Users[match(xa, new$Unique_Users) ]), 
                      shape=15, size=3)+ 
     annotate("text", x = new$DATE[match(xa, new$Unique_Users)],
              y = xa+ttt/2, label = 'x[a]', colour = "black", parse=TRUE)+
    geom_point(aes(x=new$DATE[match(xb, new$Unique_Users) ],
                   y=new$Unique_Users[match(xb, new$Unique_Users)]), 
               shape=17, size=3)+
    annotate("text", x = new$DATE[match(xb, new$Unique_Users)],
             y = xb + ttt/2, label = 'x[b]', colour = "black", parse=TRUE) +  
    geom_hline(yintercept = (x_p )*L/100 , color="black",
               linetype="dotted") + 
    annotate("text", x = new$DATE[(new$pos[1] + new$pos[nrow(new)])/2], y = (x_p*L/100)+ttt/2.5, 
             label = 'threshold', colour = "black", parse=TRUE)
  
  
  g <- g+annotate("rect", xmin = new$DATE[match(xa, new$Unique_Users)], 
                  xmax = new$DATE[match(xb, new$Unique_Users)],
                  ymin = 0, ymax = x_p,
                  alpha = .2) + theme(legend.position="none")
  g
  
  setwd("C:\\Users\\LOKESH TODWAL\\Desktop/")
  ggsave(g, filename =  paste("M12",firs,".png",sep=""))
  i <- i+1
#}
