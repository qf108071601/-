library(magrittr)
library(ggplot2)
library(dplyr)
data = read.csv("D:/GLTD_project/a.csv",header = T)


#6
levels(as.factor(data$CaseSize))
data$CaseSize_rep = data$CaseSize 

data$CaseSize_rep <- gsub(pattern = ' 1: 0 or Blank', replacement = '05: 0 or Blank', x = data$CaseSize_rep,fixed = T)

data$CaseSize_rep <- gsub(pattern = ' 2: 1 - 99', replacement = '01: 1 - 1000', x = data$CaseSize_rep,fixed = T)
data$CaseSize_rep <- gsub(pattern = ' 3: 100 - 249', replacement = '01: 1 - 1000', x = data$CaseSize_rep,fixed = T)
data$CaseSize_rep <- gsub(pattern = ' 4: 250 - 999', replacement = '01: 1 - 1000', x = data$CaseSize_rep,fixed = T)


data$CaseSize_rep <- gsub(pattern = ' 5: 1000 - 4999', replacement = '02: 1000 - 4999', x = data$CaseSize_rep,fixed = T)
data$CaseSize_rep <- gsub(pattern = ' 6: 5000 - 9999', replacement = '03: 5000 - 9999', x = data$CaseSize_rep,fixed = T)
data$CaseSize_rep <- gsub(pattern = ' 7: 10000 +', replacement = '04: 10000 +', x = data$CaseSize_rep,fixed = T)

data$CaseSize_rep = as.factor(data$CaseSize_rep)
levels(as.factor(data$CaseSize_rep))


#5
levels(as.factor(data$ratiogroup))
data$ratiogroup_rep = data$ratiogroup 

data$ratiogroup_rep <- gsub(pattern = '01: Less than 40%', replacement = '01: <= 40%', x = data$ratiogroup_rep,fixed = T)
data$ratiogroup_rep <- gsub(pattern = '02: 40%', replacement = '01: <= 40%', x = data$ratiogroup_rep,fixed = T)

data$ratiogroup_rep <- gsub(pattern = '03: GT 40% and LT 50%', replacement = '02: 40< X <=50', x = data$ratiogroup_rep,fixed = T)
data$ratiogroup_rep <- gsub(pattern = '04: 50%', replacement = '02: 40< X <=50', x = data$ratiogroup_rep,fixed = T)

data$ratiogroup_rep <- gsub(pattern = '05: GT 50% and LT 60%', replacement = '03: 50< X <=60', x = data$ratiogroup_rep,fixed = T)
data$ratiogroup_rep <- gsub(pattern = '06: 60%', replacement = '03: 50< X <=60', x = data$ratiogroup_rep,fixed = T)

data$ratiogroup_rep <- gsub(pattern = '05: GT 50% and LT 60%', replacement = '04: 50< X <=60', x = data$ratiogroup_rep,fixed = T)
data$ratiogroup_rep <- gsub(pattern = '05: GT 50% and LT 60%', replacement = '04: 50< X <=60', x = data$ratiogroup_rep,fixed = T)

data$ratiogroup_rep <- gsub(pattern = '07: GT 60% and LT 66%', replacement = '05: 60< X <70', x = data$ratiogroup_rep,fixed = T)
data$ratiogroup_rep <- gsub(pattern = '08: GE 66% and LT 68%', replacement = '05: 60< X <70', x = data$ratiogroup_rep,fixed = T)
data$ratiogroup_rep <- gsub(pattern = '09: GE 68% and LT 70%', replacement = '05: 60< X <70', x = data$ratiogroup_rep,fixed = T)

data$ratiogroup_rep <- gsub(pattern = '10: 70%', replacement = '06: X >=70', x = data$ratiogroup_rep,fixed = T)
data$ratiogroup_rep <- gsub(pattern = '11: GT 70% ', replacement = '06: X >=70', x = data$ratiogroup_rep,fixed = T)

data$ratiogroup_rep = as.factor(data$ratiogroup_rep)
levels(as.factor(data$ratiogroup_rep))


#4 
levels(as.factor(data$Gross_Indexed_Benefit_Amount))
data$Gross_Indexed_Benefit_Amount_rep = data$Gross_Indexed_Benefit_Amount 

data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '1: $ < 1000', replacement = '01: < $2,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)
data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '2: $1,000 - $1,999', replacement = '01: < $2,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)

data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '3: $2,000 - $2,999', replacement = '02: $2,000 - $4,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)
data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '4: $3,000 - $3,999', replacement = '02: $2,000 - $4,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)

data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '5: $4,000 - $4,999', replacement = '03: $4,000 - $10,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)
data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '6: $5,000 - $9,999', replacement = '03: $4,000 - $10,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)

data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '7:$10,000 - $14,999', replacement = '04: > $20,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)
data$Gross_Indexed_Benefit_Amount_rep <- gsub(pattern = '8:$15,000 - $19,999', replacement = '04: > $20,000', x = data$Gross_Indexed_Benefit_Amount_rep,fixed = T)
levels(as.factor(data$Gross_Indexed_Benefit_Amount_rep))
data$Gross_Indexed_Benefit_Amount_rep = as.factor(data$Gross_Indexed_Benefit_Amount_rep)

#3 Limited_Own_Occupation_Period
levels(as.factor(data$Limited_Own_Occupation_Period))

data$Limited_Own_Occupation_Period_rep = data$Limited_Own_Occupation_Period 

data$Limited_Own_Occupation_Period_rep <- gsub(pattern = '^02: 1 - 18 Months', replacement = '01: < 30 Month', x = data$Limited_Own_Occupation_Period_rep)
data$Limited_Own_Occupation_Period_rep <- gsub(pattern = '^03: 19 - 30 Months', replacement = '01: < 30 Month', x = data$Limited_Own_Occupation_Period_rep)

data$Limited_Own_Occupation_Period_rep <- gsub(pattern = '^04: 31 - 42 Months', replacement = '02: > 30 Month', x = data$Limited_Own_Occupation_Period_rep)
data$Limited_Own_Occupation_Period_rep <- gsub(pattern = '05: 43+ Months', replacement = '02: > 30 Month', x = data$Limited_Own_Occupation_Period_rep,fixed = T)
levels(as.factor(data$Limited_Own_Occupation_Period_rep))

data$Limited_Own_Occupation_Period_rep = as.factor(data$Limited_Own_Occupation_Period_rep)

#2
levels(as.factor(data$Age_at_Disability))

data$Age_at_Disability_rep = data$Age_at_Disability 
levels(as.factor(data$Age_at_Disability_rep))

data$Age_at_Disability_rep <- gsub(pattern = '^01: < 20', replacement = '01: < 30', x = data$Age_at_Disability_rep)
data$Age_at_Disability_rep <- gsub(pattern = '^02:  20-24', replacement = '01: < 30', x = data$Age_at_Disability_rep)
data$Age_at_Disability_rep <- gsub(pattern = '^03: 25-29', replacement = '01: < 30', x = data$Age_at_Disability_rep)

data$Age_at_Disability_rep <- gsub(pattern = '^04: 30-34', replacement = '02: 30-39', x = data$Age_at_Disability_rep)
data$Age_at_Disability_rep <- gsub(pattern = '^05: 35-39', replacement = '02: 30-39', x = data$Age_at_Disability_rep)

data$Age_at_Disability_rep <- gsub(pattern = '^06: 40-44', replacement = '03: 40-49', x = data$Age_at_Disability_rep)
data$Age_at_Disability_rep <- gsub(pattern = '^07: 45-49', replacement = '03: 40-49', x = data$Age_at_Disability_rep)

data$Age_at_Disability_rep <- gsub(pattern = '^08: 50-54', replacement = '04: 50-59', x = data$Age_at_Disability_rep)
data$Age_at_Disability_rep <- gsub(pattern = '^09: 55-59', replacement = '04: 50-59', x = data$Age_at_Disability_rep)

data$Age_at_Disability_rep <- gsub(pattern = '^10: 60-64', replacement = '05: 60+', x = data$Age_at_Disability_rep)
data$Age_at_Disability_rep <- gsub(pattern = '^11: 65-69', replacement = '05: 60+', x = data$Age_at_Disability_rep)
data$Age_at_Disability_rep <- gsub(pattern = "12: 70+", replacement = '05: 60+', x = data$Age_at_Disability_rep,fixed = TRUE)

data$Age_at_Disability_rep = as.factor(data$Age_at_Disability_rep)
levels(as.factor(data$Age_at_Disability_rep))


#1 改Duration
data$Annual_Duration_rep <- data$Annual_Duration
data$Annual_Duration_rep <- gsub(pattern = '^Year: 10', replacement = 'Year: 10+', x = data$Annual_Duration_rep)
data$Annual_Duration_rep <- gsub(pattern = '^Over 10 Years', replacement = 'Year: 10+', x = data$Annual_Duration_rep)
levels(as.factor(data$Annual_Duration_rep))
data$Annual_Duration_rep = as.factor(data$Annual_Duration_rep)

char = data[,38:43]
attach(data)
char = cbind(char,
             Gender,
             Disability_Category,
             Elimination_Period,
             OwnOccToAnyTransition,
             Integration_with_STD,
             COLA_Indicator,
             Region)

char.t <- model.matrix(~.-1, data=char)
char.t = as.data.frame(char.t)

char = cbind(char.t,Actual_Recoveries=data$Actual_Recoveries/100)
#write.csv(char,"D:/GLTD_project/aa.csv")

char = read.csv("D:/GLTD_project/aa.csv",header = T)
set.seed(12345)
sampid0 <- sample(0:1, dim(char)[1], replace=T, prob=c(0.8, 0.2))      
sampid <- which(sampid0 == 0)

train=char[sampid,-1]
test=char[-sampid,-1]

raw.train= data[sampid,]
raw.test = data[-sampid,]

#######################
library(xgboost)
dtrain = xgb.DMatrix(data = as.matrix(train[,-60]),label = train$Actual_Recoveries)

dtest = xgb.DMatrix(data = as.matrix(test[,-60]),label = test$Actual_Recoveries)


#跑模型------ 
#2. 設定xgb.params，也就是 xgboost 裡面的參數
xgb.params = list(
  #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  colsample_bytree = 0.4,                    
  # row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  subsample = 0.4,                      
  booster = "gblinear",
  # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
  max_depth = 5,           
  # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
  eta = 0.01,
  # 或用'mae'也可以
  eval_metric = "rmse",                      
  objective = "reg:squarederror",
  # 越大，模型會越保守，相對的模型複雜度比較低
  gamma = 0)        


cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,     # 10-fold cv
  nrounds=500,   # 測試1-500，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 20 # 每20個單位才顯示一次結果，
) 

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

# 獲得 best nround
best.nrounds = cv.model$best_iteration 
best.nrounds

# 4. 用xgb.train()建立模型
xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 

# 預測
xgb_y = predict(xgb.model, dtest)

raw.test$AE_ratio = test$Actual_Recoveries/xgb_y


raw.test %>% 
  group_by(Calendar_Year,Disability_Category) %>%
  summarise(AE_ratio = mean(AE_ratio))%>%
  ggplot(aes(x=Calendar_Year, y=AE_ratio,
             group=Disability_Category)) + 
  geom_line(aes(linetype=Disability_Category,
                color=Disability_Category),size=1.2)+ 
  geom_point(aes(color=Disability_Category),size=2.2)+
  theme(axis.text.x = element_text(angle = 15,vjust=0.5,size=14),
        legend.text=element_text(size=16),legend.title=element_text(size=16))

raw.test %>% 
  group_by(Calendar_Year,Region) %>%
  summarise(AE_ratio = mean(AE_ratio))%>%
  ggplot(aes(x=Calendar_Year, y=AE_ratio,
             group=Region)) + 
  geom_line(aes(linetype=Region,
                color=Region),size=1.2)+ 
  geom_point(aes(color=Region),size=2.2)+
  theme(axis.text.x = element_text(angle = 15,vjust=0.5,size=14),
        legend.text=element_text(size=16),legend.title=element_text(size=16))

raw.test %>% 
  group_by(ratiogroup_rep,Disability_Category) %>%
  summarise(AE_ratio = mean(AE_ratio))%>%
  ggplot(aes(x=ratiogroup_rep, y=AE_ratio,
             group=Disability_Category)) + 
  geom_line(aes(linetype=Disability_Category,
                color=Disability_Category),size=1.2)+ 
  geom_point(aes(color=Disability_Category),size=2.2)+
  theme(axis.text.x = element_text(angle = 15,vjust=0.5,size=14),
        legend.text=element_text(size=16),legend.title=element_text(size=16))



#模型變數討論----
imp = xgb.importance(names(train),model=xgb.model)
xgb.plot.importance(imp[1:20])

summary(raw.test$AE_ratio)
mean(raw.test$AE_ratio)
sqrt(var(raw.test$AE_ratio))
plot(raw.test$AE_ratio)




