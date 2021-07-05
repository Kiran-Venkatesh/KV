
minmax_norm<-function(x){
  (((x-max(x))*(0.1-0.9))/(min(x)-max(x)))+0.9
}
library("wavelets")
data <- read.csv("D:\\8th Semester books\\Soft_Computing\\oct18-24.csv")
data=data[complete.cases(data), ]

pb_wt = dwt(data[,3],filter="d4")
mcp_wt = dwt(data[,9],filter="d4")


pb_iwt=idwt(pb_wt,fast=TRUE)
mcp_iwt=idwt(mcp_wt,fast=TRUE)
pb_iwt=minmax_norm(idwt(pb_wt,fast=TRUE))
mcp_iwt=minmax_norm(idwt(mcp_wt,fast=TRUE))
df1=data.frame(date=data[,1],hours=data[,2],pb=pb_iwt,mcp=mcp_iwt)
df2=data.frame(date=data[,1],hours=data[,2],pb=minmax_norm(data[,3]),mcp=minmax_norm(data[,9]))
write.csv(df1,"D:\\8th Semester books\\Soft_Computing\\oct18-24_wavelet_norm.csv",row.names=FALSE)
write.csv(df2,"D:\\8th Semester books\\Soft_Computing\\oct18-24_norm.csv",row.names=FALSE)

par(mfrow=c(2,1))
plot(data[,9],type="o")
plot(mcp_iwt,type="o")

par(mfrow=c(2,1))
plot(data[,3],type="o")
plot(pb_iwt,type="o")

#df <- read.csv("D:\\8th Semester books\\Soft_Computing\\oct18-24_Preprocessed.csv")
#dfw <- read.csv("D:\\8th Semester books\\Soft_Computing\\oct18-24_wavelet_Preprocessed.csv")

df<-df2
library("neuralnet")
trainDFx<-df[c(1:72),c("pb","mcp")]
trainDFy<-df[c(25:96),c("mcp")]
train<-cbind(trainDFx,trainDFy)
testDFx<-df[c(73:144),c("pb","mcp")]
testDFy<-df[c(97:168),c("mcp")]
test<-cbind(testDFx,testDFy)
f<-as.formula(trainDFy ~ pb + mcp)
#f <- as.formula(paste(paste(paste(colnames(trainDFy),collapse = " + "),"~"),paste(colnames(trainDFx),collapse = " + ")))
NN<-neuralnet(f, data = train, hidden = 4, threshold =0.0001,
              stepmax = 1e+05, rep = 5, startweights = NULL,
              learningrate.limit = NULL, learningrate.factor = list(minus = 0.5,
                                                                    plus = 1.2), learningrate = 0.1, lifesign = "full",
              lifesign.step =24, algorithm = "backprop", err.fct = "sse",
              act.fct = "logistic", linear.output = TRUE, exclude = NULL,
              constant.weights = NULL, likelihood = FALSE)
no_of_trials<-5
AMSE_train<-2*(sum(NN$result.matrix[1,]/nrow(trainDF))/no_of_trials)
AMSE_test<-sum((testDFy - compute(NN,testDFx)$net.result[,1])^2)/nrow(test)

rsquare_train<-1-(sum((trainDFy - compute(NN,trainDFx)$net.result[,1])^2)/sum((trainDFy-sum(trainDFy)/nrow(train))^2))
rsquare_test<-1-(sum((testDFy - compute(NN,testDFx)$net.result[,1])^2)/sum((testDFy-sum(testDFy)/nrow(test))^2))

MAPE_test<-sum(abs((testDFy - compute(NN,testDFx)$net.result[,1])/testDFy))/nrow(test)
MAPE_train<-sum(abs((trainDFy - compute(NN,trainDFx)$net.result[,1])/trainDFy))/nrow(train)

delta_test<-sum((testDFy - mean(testDFy))^2)/(nrow(test)-1)
delta_train<-sum((trainDFy - mean(trainDFy))^2)/(nrow(train)-1)

NMSE_test<-sum((testDFy - compute(NN,testDFx)$net.result[,1])^2)/(nrow(test)*(delta_test^2))
NMSE_train<-sum((trainDFy - compute(NN,trainDFx)$net.result[,1])^2)/(nrow(train)*(delta_train^2))

EV_test<-sum((abs((testDFy - compute(NN,testDFx)$net.result[,1])/testDFy)-MAPE_test)^2)/nrow(test)
EV_train<-sum((abs((trainDFy - compute(NN,trainDFx)$net.result[,1])/trainDFy)-MAPE_train)^2)/nrow(train)

plot(NN)
par(mfrow=c(2,1))
#plot(compute(NN,testDFx)$net.result,type="o")
#plot(testDFy,type="o")
plot(compute(NN,testDFx)$net.result,type="l",col="green",xlab="Time(in hours)",ylab="MCP(INR)",main="title",bty="l")
lines(testDFy,col="blue")
#par(xpd=TRUE)
legend("topright",
       c("actual_mcp","predicted_mcp"),
       fill=c("blue","green")
)
print(NN$weights)

print("PSO")
w<-0.7
c1<-0.2
c2<-0.6
E<-100
oper<-10
CP<-list()
Global<<-list()
n<-length(do.call(c, unlist(NN$weights, recursive=FALSE)))
r1<-runif(n)
r2<-runif(n)

V<-list()
for(i in c(1:oper)){
  V[[i]]<-runif(n,min=-2,max=2)
#  CP[[i]]<-runif(n,min=-500,max=500)
  
  CP[[i]]<-do.call(c, unlist(NN$weights, recursive=FALSE))
#  print(CP[[i]])
}
sum_t<-c()
iter<-c()
pbest<-CP
pbest_fit<-c()
fitness_function<- function(n,s) 
{
    print(n)
  
    p<-1
    for(i in c(1:5)){
      for(k in c(1:4)){
        for(j in c(1:3)){
          NN$weights[i][[1]][[1]][j,k]<-n[p]
          print(p)
          p<-p+1
        }
      }
      for(t in c(1:5)){
        NN$weights[t][[1]][[2]][t,1]<-n[p]
        print(p)
        p<-p+1
      }
    } 
    print(NN$weights)
    Global<<-NN
    print((1/(sum((testDFy - compute(NN,testDFx)$net.result[,1])^2)/nrow(test))))
    return(1/(sum((testDFy - compute(NN,testDFx)$net.result[,1])^2)/nrow(test)))
    
  
  
}  
temp_cp<-list()

for(i in c(1:E)){
  r1<-runif(n)
  r2<-runif(n)
  #print(r1,r2)
  F_x<-c()
  for(j in c(1:oper)){
    if(length(CP[[j]])==0){
      next
    }
    F_x<-c(F_x,fitness_function(CP[[j]],'MIN'))
    if(i==1){
      flag<-1
      pbest[[j]]<-CP[[j]]
      pbest_fit[j]<-F_x[j]
   #   print(pbest_fit)
      
      next
    }
  #  print(pbest_fit)
    if(F_x[j]>pbest_fit[j]){
      pbest[[j]]<-CP[[j]]
      pbest_fit[j]<-F_x[j]
    }
    
  }
  print('Iteration')
  print(i)
  gbest_ind<-which.max(F_x)
#  print(gbest_ind)
#  print(F_x)
  if(is.null(F_x)){
    next
  }
  gbest_pos<-CP[[gbest_ind]]
  gbest_fit<-F_x[gbest_ind]
  print(gbest_fit)
  temp_cp<-CP
  for(j in c(1:oper)){
    V[[j]]<-w*V[[j]]+c1*r1[j]*(pbest[[j]]-CP[[j]])+c2*r2[j]*(gbest_pos-CP[[j]])
    CP[[j]]<-CP[[j]]+V[[j]]
 #   print(V[[j]])
#    print(CP[[j]])
    
  }
  
  iter<-c(iter,i)
  sum_t<-c(sum_t,gbest_fit)
#  print(sum(gbest_pos))
#  print(gbest_fit)
}
plot(iter,sum_t)
fitness_function(gbest_pos,'MIN')
print(sum((testDFy - compute(Global,testDFx)$net.result[,1])^2)/nrow(test))
plot(Global)
par(mfrow=c(2,1))
plot(compute(Global,testDFx)$net.result,type="l",col="green",xlab="Time(in hours)",ylab="MCP(INR)",bty="l")
lines(testDFy,col="blue")
#par(xpd=TRUE)
legend("topright",
       c("actual_mcp","predicted_mcp"),
       fill=c("blue","green")
)

plot(compute(Global,testDFx)$net.result,type="o")
plot(testDFy,type="o")
print(NN$weights)
AMSE_train<-2*(sum(Global$result.matrix[1,]/nrow(trainDF))/no_of_trials)
AMSE_test<-sum((testDFy - compute(Global,testDFx)$net.result[,1])^2)/nrow(test)
MAPE_test<-sum(abs((testDFy - compute(Global,testDFx)$net.result[,1])/testDFy))/nrow(test)
MAPE_train<-sum(abs((trainDFy - compute(Global,trainDFx)$net.result[,1])/trainDFy))/nrow(train)

delta_test<-sum((testDFy - mean(testDFy))^2)/(nrow(test)-1)
delta_train<-sum((trainDFy - mean(trainDFy))^2)/(nrow(train)-1)

NMSE_test<-sum((testDFy - compute(Global,testDFx)$net.result[,1])^2)/(nrow(test)*(delta_test^2))
NMSE_train<-sum((trainDFy - compute(Global,trainDFx)$net.result[,1])^2)/(nrow(train)*(delta_train^2))

EV_test<-sum((abs((testDFy - compute(Global,testDFx)$net.result[,1])/testDFy)-MAPE_test)^2)/nrow(test)
EV_train<-sum((abs((trainDFy - compute(Global,trainDFx)$net.result[,1])/trainDFy)-MAPE_train)^2)/nrow(train)
