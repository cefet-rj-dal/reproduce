w <- data.frame(k_f$tempbulboseco, k_f$atraso)
u<-data.frame(k_f$tempbulboseco_chegada, k_f$atraso)
colnames(w)<-c("x","atraso")
colnames(u)<-c("x","atraso")
t<-rbind(w,u)

tt<-subset(t,!is.na(x))

ttt <- remove_outliers(tt)


z<-data.frame(0,0)


k <- quantile(ttt$x, c(0.0, 1))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[1,1]<-mean((b-a)/2)        
z[1,2]<-2

k <- quantile(ttt$x, c(0.0,0.5, 1))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[2,1]<-mean((b-a)/2)        
z[2,2]<-3

k <- quantile(ttt$x, c(0.0,0.33,0.67, 1))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[3,1]<-mean((b-a)/2)        
z[3,2]<-4


k <- quantile(ttt$x, c(0.0, 0.25, 0.5, 0.75, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[4,1]<-mean((b-a)/2)        
z[4,2]<-5

k <- quantile(ttt$x, c(0.0, 0.20, 0.40, 0.60, 0.80,1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[5,1]<-mean((b-a)/2)        
z[5,2]<-6

k <- quantile(ttt$x, c(0.0, 0.17, 0.33, 0.5, 0.67, 0.84, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[6,1]<-mean((b-a)/2)        
z[6,2]<-7       

k <- quantile(ttt$x, c(0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[7,1]<-mean((b-a)/2)        
z[7,2]<-8

k <- quantile(ttt$x, c(0.0, 0.11, 0.22, 0.33, 0.44, 0.55, 0.66, 0.77, 0.88, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[8,1]<-mean((b-a)/2)        
z[8,2]<-9

k <- quantile(ttt$x, c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
a <- c(k[1:(length(k)-1)])
b <- c(k[2:(length(k))])
z[9,1]<-mean((b-a)/2)        
z[9,2]<-10

colnames(z)<-c("media","num")
