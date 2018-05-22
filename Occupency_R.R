
###############################   Data investigation /  Data pre processing  ############################################

#Occ.data <- read.csv(file.choose(), header=T)  
Oc.test=read.table("datatest.txt",sep = ",")
Oc.train=read.table("datatraining.txt",sep=",")
Occ.data=rbind(Oc.train,Oc.test)

head(Occ.data)    # VISUALIZE FEW ELEMENTS OF THE DATA SET
dim(Occ.data)     # DIMENSION
str(Occ.data)     # Display the Structure of of an object
summary(Occ.data)   
sum(is.na(Occ.data))  ## No missing values
sum(duplicated(Occ.data))  ## test for duplicated values
Occ.data_backup<-Occ.data 
Occ.data=Occ.data[,-1]   ## remove the variable "date" from the table for further analysis


#-------------------------------------------------------------------------------------
#
#    DATA VISUALIZATION
#
#-------------------------------------------------------------------------------------

library(ggplot2)

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor*r )
}


##   plots

hist(Occ.data,col="blue")
boxplot(Occ.data)
pairs(Occ.data,lower.panel = panel.smooth,diag.panel = panel.hist, upper.panel = panel.cor)



#---------------------------------------------------------------------------------------------
#
#
#      Neural networks 
#
#---------------------------------------------------------------------------------------------
library(neuralnet)


##   Normalization, one could also use the z-normalization 
max = apply(Occ.data , 2 , max)
min = apply(Occ.data, 2 , min)
scaled = as.data.frame(scale(Occ.data, center = min, scale = max - min))

#-------------  splitting---------
set.seed(1) 
n=nrow(Occ.data)    # 
Train.index=sample(1:n,2*n/3)    ## 2/3 of the data for training 
Test.index=sample(1:n,1*n/3)     ## 1/3 of the data for testing
Train=scaled[Train.index,]       
Test=scaled[Test.index,]


## Define the formula to be used in 
name.Oc = names(Occ.data)   # extract the names 
formula.Oc = as.formula(paste("Occupancy ~", paste(name.Oc[!name.Oc %in% "Occupancy"], collapse = "+")))

#
# Fit the model with 1 layer and 4 neurons
#
nnet_1_layer = neuralnet(formula = formula.Oc, data = Train, hidden = 4, threshold = 0.01,
                            linear.output=F)
plot(nnet_1_layer)

#
# Fit the model with 2 layers
#
nnet_2_layers = neuralnet(formula = formula.Oc, data = Train, hidden = c(4,3), threshold = 0.1,
                      linear.output=F)
plot(nnet_2_layers)

#
# Fit the model with 3 layers
#
nnet_3_layers = neuralnet(formula = formula.Oc, data = Train, hidden = c(4,2), threshold = 0.1,
                       act.fct = "logistic", linear.output=F)
plot(nnet_3_layers, rep="best")



## do this for (logistic)regression or 
## prediction and scaling back before.
#pred.nnet_1_layer <- compute(nnet_1_layer,Test)
# pred.nnet_1_layer. <- pred.nnet_1_layer$net.result*(max(Occ.data$Occupancy)-min(Occ.data$Occupancy))+min(Occ.data$Occupancy)
# Test.r <- (Test$Occupancy)*(max(Occ.data$Occupancy)-min(Occ.data$Occupancy))+min(Occ.data$Occupancy)
#MSE.ocnn <- mean((Test$Occupancy - pred.nnet_1_layer)^2)
#MSE_ocnn




