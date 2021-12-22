
# import data 
data<-read.csv("C:\\Users\\kevin\\OneDrive\\桌面\\統計\\期末報告\\data2.csv")
data<-data[colnames(data)[2:18]]
str(data)
summary(data)

#transform the type of data
unique(data$learning_stage) 
newdata <- data[data$difficulty!="unset",]
newdata$learning_stage<-as.numeric(factor(newdata$learning_stage))
newdata$difficulty<-factor(newdata$difficulty,labels = c("easy", "normal", "hard"))
colnames(newdata)

# set model
library(VGAM)
model1<-vglm(difficulty~upid+is_correct+level1+level4+total_sec_taken+used_hint_cnt+repeat_correct_pct+grade_top+learning_stage+points+badges_cnt,family=multinomial,data=newdata)
summary(model1)

model_final<-step4(model1,direction="both")
summary(model_final)

# coefficient
round(exp(coef(model_final)),5)

# prediction
predict(model_final, newdata[1,1:16], type="response")
index<-apply(predict(model_final, newdata[,1:16],type="response"),1,FUN = which.max)
labels<-c("easy","normal",'hard')
predictions<-factor(sapply(index,function(x)labels[x]),labels = c("easy", "normal", "hard"))
predictions
mean(predictions==newdata$difficulty)

#畫ROC曲線

library(pROC)
#AUC
multiclass.roc(index,as.numeric(newdata$difficulty))
#Multi-class area under the curve: 0.6617

#confusion matrix
table(predictions,newdata$difficulty)


###################################
#不同模型
model2<-vglm(difficulty~uuid+level4+upid+is_correct+level1+total_attempt_cnt+learning_stage,family=multinomial,data=newdata)
summary(model2)

#交互作用模型
model_double<-vglm(difficulty ~ (upid+is_correct+points+level1+repeat_correct_pct+grade_top+learning_stage)^2
            ,family=multinomial,data=newdata)
summary(model_double)
model_double_best<-step4(model,direction="backward")
summary(model_double_best)
