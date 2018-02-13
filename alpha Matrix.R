library(xlsx)

fname <- choose.files()
fname
#see fname an set name
dname <- "BlackMoney"

setwd(paste("I:/twitter 2/",dname,"/",sep = ""))
#add participation matrix
data <- read.csv(fname,stringsAsFactors =F ,row.names = 1)

mat <- data
mat2 <- data
colnames(mat) 
#Define matrix for variable be slecting col ie time slice

#add to last row
cs <- colSums(mat)
#Check column and row count
c <- ncol(mat)
r <- nrow(mat)

#Calculate alpha as intersection<dot product> of 0 to i th time slice user
#divide by cardinality of previos time slice
#1st col and last row is empty thorought
#alpha+ and alpha- zero defined for just one time participation
alpha <- matrix(0,c,c)
alpha2 <- matrix(0,c,c)

##################
#Defining alpha as per users set contributing to alpha
#run loop last to first row
i <- 0
for(i in (c-1):0)
{
  j <- i+1
  #j defines cal of each rows
  for(j in (i+1):c)
  {
    #index till which prev time slice are considers
    num <- mat[,j]
    dem <- mat[,j]
    #run loop for prev timeslice cal. :Can Optimize
    if(i>0)
      for (k in 1:i) {
        num <- num*mat[,j-k]
        #dem same at 1st time. then multiply prev
        #dem <- dem|mat[,j-k]
       }
      alpha[i+1,j] <- sum(num)
          mat[which(num==1),] <- 0
  }  
    #for alpha 2
    for(j in 1:(c-i))
    {
      #index till which prev time slice are considers
      num2 <- mat2[,j]
      #run loop for prev timeslice cal. :Can Optimize
      if(i>0)
        for (k in 1:i) {
          num2 <- num2*mat2[,j+k]
          #dem same at 1st time. then multiply prev
          #dem <- dem|mat[,j-k]
        }
      alpha2[i+1,j] <- sum(num2)
      mat2[which(num2==1),] <- 0
      cat(sum(num2)," ",sum(dem)," :: ")
      cat("\n",alpha2[i+1,],"\n")
    }
    #mul current with dem.
    #Caution may have divide by zero
    #print alpha row
}

alpha
alpha2
#Plot alpha and beta
colnames(mat) <- format(as.Date(colnames(data),"X%Y.%m.%d"),"%d-%m-%Y")
colnames(alpha) <- as.character(colnames(mat))
colnames(alpha2) <- as.character(colnames(mat))

rownames(alpha) <-  paste(format(as.Date(colnames(mat)[as.numeric(which(alpha[1,]==max(alpha[1,])))],"%d-%m-%Y")+c(0:-(c-1))
                                 ,"%d-%m-%Y"),' (B_alpha',c(0:(c-1)),")",sep = "")
rownames(alpha2) <- paste(format(as.Date(colnames(mat)[as.numeric(which(alpha[1,]==max(alpha[1,])))],"%d-%m-%Y")+c(0:(c-1))
                                 ,"%d-%m-%Y"),' (F_alpha',c(0:(c-1)),")",sep = "")
head(alpha)


write.csv(alpha,paste(dname,"alpha backward bucketing.csv"))
write.csv(alpha2,paste(dname,"alpha2 forward bucketing.csv"))

#till here alpha bucketing 
##################
# use i for row nos
for(i in 1:(c-1))
{
  #j defines cal of each rows
  for(j in (i+1):c)
  {
    #index till which prev time slice are considers
    num <- mat[,j]
    dem <- mat[,j]
    #run loop for prev timeslice cal. :Can Optimize
    for (k in 1:i) {
      num <- num*mat[,j-k]
      #dem same at 1st time. then multiply prev
      #dem <- dem|mat[,j-k]
    }
    #mul current with dem.
    #Caution may have divide by zero
    alpha[i,j] <- sum(num)/sum(dem)
    cat(sum(num)," ",sum(dem)," :: ")
  }
  #print alpha row
  cat("\n",alpha[i,],"\n")
}

#Try: for 2nd row
# for (i in 3:c) {
#   alpha[2,i] <- sum(mat[,i]*mat[,i-1]*mat[,i-2])/sum(mat[,i-1]*mat[,i-2])
# }

alpha

#Plot alpha and beta
colnames(mat) <- as.Date(colnames(data),"X%Y.%m.%d")

colnames(alpha) <- colnames(mat)
rownames(alpha) <- paste('alpha',c(0:c),sep = "")
head(alpha)

#col sum of mat: user participation
#cs <- colSums(mat)
#take max of all col
#mcs <- max(cs)
#initz factor as col sum/max of col 
#fcs <- cs/mcs

beta <- 1-alpha
rownames(beta) <- paste('beta',c(1:c),sep = "")
head(beta)

#use tranv. of transv.*factor
#alphaNorm <- t(t(alpha) *fcs)
write.xlsx(alpha,"alpha Matrix bucketing.xlsx")

write.xlsx(beta,"Beta Matrix single Denominator.xlsx")

rs  <- rowSums(mat)
urc <- c(1:max(rs))
rc  <- sapply(urc, function(x){length(which(rs==x))})
write.csv(cbind(urc,rc),paste(dname,"participation count.csv"))
