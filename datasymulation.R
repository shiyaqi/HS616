setwd("/Users/Anna/Documents/HS616/Projects/Datasimulation/")
#install.packages('hash')
#library(hash)
#################################################### Part1 Data Simulation ####################################################
set.seed(123)
logistic <- function(t) 1 / (1 + exp(-t))

generate_dataset <- function(N){
  temp <- sample(c("Complication", "NoComplication"), N, replace=TRUE, prob=c(0.595, 0.415))
  GA_mean <- c(Complication= 15.3, NoComplication = 13.3)
  GA_sd <- c(Complication = 1.6, NoComplication = 1.1)
  GA <- rnorm(N, mean = GA_mean[temp], sd = GA_sd[temp])
  
  Hb_temp_mean <- c(Complication = 5.9, NoComplication = 5.6)
  Hb_temp_sd <- c(Complication = 0.3, NoComplication = 0.4)
  Hb_temp <- rnorm(N, mean = Hb_temp_mean[temp], sd = Hb_temp_sd[temp])
  HbA1C <- 0.245 * (GA-mean(GA)) + mean(Hb_temp) + rnorm(N, sd = sd(Hb_temp))
  
  Hypo_glc <- runif(N) < (13/42)*2*logistic(GA-mean(GA))
  Resp_dis <- runif(N) < (11/42)*2*logistic(GA-mean(GA))
  Hypo_cal <- runif(N) < (9/42)*2*logistic(GA-mean(GA))
  Hype_vol <- sample(c(TRUE, FALSE), N,replace=TRUE, prob=c(4/42, 1-4/42))
  Hype_bil <- sample(c(TRUE, FALSE), N,replace=TRUE, prob=c(6/42, 1-6/42))
  Hype_myocar <- runif(N) < (17/42)*2*logistic(GA-mean(GA))
  Large_for_date <- runif(N) < (8/42)*2*logistic(GA-mean(GA))
  df <- data.frame(Hypo_glc,Resp_dis,Hypo_cal,Hype_vol,Hype_bil,Hype_myocar,Large_for_date)
  df$complication <- ifelse(apply(df[,c(1:7)],1,sum) != 0, TRUE, FALSE)
  df$GA <- GA
  df$HbA1C <- HbA1C
  return(df)
}
N <- 420
mydata <- generate_dataset(N)

#################################################### Part2 Data Exploration ####################################################
library(ggplot2)

# Multiple plot function

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


Hb1 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = Hypo_glc), binwidth = 0.05, alpha = 0.5, position = "identity" )+ggtitle('HbA1C~Hypo_glc') 
Hb2 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = Resp_dis), binwidth = 0.05, alpha = 0.5, position = "identity")+ggtitle('HbA1C~Resp_dis')
Hb3 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = Hypo_cal), binwidth = 0.05, alpha = 0.5, position = "identity")+ggtitle('HbA1C~Hypo_cal')
Hb4 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = Hype_vol), binwidth = 0.05, alpha = 0.5, position = "identity")+ggtitle('HbA1C~Hype_vol')
Hb5 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = Hype_bil), binwidth = 0.05, alpha = 0.5, position = "identity")+ggtitle('HbA1C~Hype_bil')
Hb6 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = Hype_myocar), binwidth = 0.05, alpha = 0.5, position = "identity")+ggtitle('HbA1C~Hype_myocar')
Hb7 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = Large_for_date), binwidth = 0.05, alpha = 0.5, position = "identity")+ggtitle('HbA1C~Large_for_date')
Hb8 <- ggplot(data = mydata) + geom_histogram(aes(x = HbA1C, fill = factor(complication)), binwidth = 0.05, alpha = 0.5, position = "identity")+ggtitle('HbA1C~complication')

multiplot(Hb1,Hb2,Hb3,Hb4,Hb5,Hb6,Hb7,Hb8, cols = 2)

GA1 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = Hypo_glc), binwidth = 0.3, alpha = 0.5, position = "identity" )+ggtitle('GA~Hypo_glc')
GA2 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = Resp_dis), binwidth = 0.3, alpha = 0.5, position = "identity")+ggtitle('GA~Resp_dis')
GA3 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = Hypo_cal), binwidth = 0.3, alpha = 0.5, position = "identity")+ggtitle('GA~Hypo_cal')
GA4 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = Hype_vol), binwidth = 0.3, alpha = 0.5, position = "identity")+ggtitle('GA~Hype_vol')
GA5 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = Hype_bil), binwidth = 0.3, alpha = 0.5, position = "identity")+ggtitle('GA~Hype_bil')
GA6 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = Hype_myocar), binwidth = 0.3, alpha = 0.5, position = "identity")+ggtitle('GA~Hype_myocar')
GA7 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = Large_for_date), binwidth = 0.3, alpha = 0.5, position = "identity")+ggtitle('GA~Large_for_date')
GA8 <- ggplot(data = mydata) + geom_histogram(aes(x = GA, fill = complication), binwidth = 0.3, alpha = 0.5, position = "identity")+ggtitle('GA~complication')
multiplot(GA1,GA2,GA3,GA4,GA5,GA6,GA7,GA8, cols = 2)

T_test <- function(data){
  for (col in names(data)[c(1:8)]){
    print(sprintf("==========================================================%s==========================================================",col))
    print (t.test(data$GA~data[[col]]))
    print (t.test(data$HbA1C~data[[col]]))
  }
}

T_test(mydata)
sum(mydata$complication)


cor(mydata[,c(9,10)])
fit_Hb <- lm(HbA1C~GA,data = mydata)
pred_HB <- predict(fit_Hb)
summary(fit_Hb) #Hb = 0.235*GA + 2.38  R^2 = 0.5742
plot(fit_Hb,1)
ggplot(data=mydata)+geom_point(aes(x=GA,y=HbA1C,color = complication))+ geom_smooth(aes(x=GA,y=HbA1C))+geom_point(aes(x=GA,y=pred_HB))


#################################################### Part3 Data modeling ####################################################
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

mydata1 <- data.frame(apply(mydata,2,as.numeric))   # change the T/F to 1/0

fit_Com <- glm(complication~GA,family=binomial, data = mydata1)
plot(complication~GA, data = mydata1)
pred_com <- predict(fit_Com,type="response")
summary(fit_Com)
ggplot(data=mydata1)+geom_point(aes(x=GA,y=complication,color = complication))+
  geom_point(aes(x=GA,y= pred_com))


# 3D Scatterplot
#install.packages("scatterplot3d")
library(scatterplot3d)
s3d <-scatterplot3d( mydata$GA[mydata$complicatio==1],mydata$HbA1C[mydata$complicatio==1],mydata$complication[mydata$complicatio==1],
                    color = "blue",
                    xlab= "GA",ylab=  "HbA1C",zlab="complication", 
                    main="GA VS HbA1C VS complication")
s3d$points3d(mydata$GA[mydata$complicatio==0],mydata$HbA1C[mydata$complicatio==0],mydata$complication[mydata$complicatio==0], col = "red")












