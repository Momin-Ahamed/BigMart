##load package

library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(xgboost)
library(cowplot)
library(glmnet)
library(e1071)
#library(ggplot2, plotrix, scatterplot3d)
#read datasets

#train = fread.csv("Train_UWu5bXk.csv")
train = fread("Train_UWu5bXk.csv")
test  =fread("Test_u94Q5KV.csv")
#submission = fread("SampleSubmission_TmnO39y.csv")

## train data column names
names(train)

##test data column names
names(test)

##Dimension Of Data
dim(train);dim(test)
##structure of train data
str(train)
str(test)
show(test)
test <- as.data.table(test)
test[,Item_Outlet_Sales := NA]## add item_outlet_sales to test data
combi = rbind(train , test) ##combining train and test datasets
dim(combi)
combi



Export_ITEM = combi$Item_Visibility
Export_ITEMr

Export_MRP = combi$Item_MRP
Export_MRP
Food_fat=combi$Item_Fat_Content
Food_fat

dim(Food_fat)
#------------Graph 1 *****Histogram*******
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales) ,binwidth = 50, fill = "purple") + xlab("Item_Outlet_Sales")
#######ggplot(data = diamonds,carat,price,colour=clarity,fac)
#dev.off()
#------------Graph 2
png(file = "Histogram.jpg")
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p1
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "orange")
p2
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
p3
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package
dev.off()

#-------Graph 3 bar graph
bp <- ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
bp
#pie chart 3d and 2d
pie3D(Food_fat, labels = Export_ITEM, main = "An exploded 3D pie chart", explode=0.1, radius=.9, labelcex = .9,  start=0.7)
#pie(Money_IN_US, labels = Export_Date, main = 'dkd')
bp+ coord_polar("y", start=0)



#-------Graph 4 bar graph
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "purple")

#-----Graph 5  ***bar chart****
png(file = "line_chart_5_lines.jpg")
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "purple") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")
p4
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "black") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)
dev.off()

#-------Graph 6*****barchart
png(file = "outletEst.jpg")
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))
p7
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))
plot_grid(p7, p8, ncol = 2)
dev.off()

#Bivariate Analysis  graph 7  ***scatter plot****
png(file = "scatter.jpg")
train = combi[1:nrow(train)]
p9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "black", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
p9
p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)
dev.off()

#------Graph 8 *****violin Plot*****
png(file = "outletsalesv1.jpg")
p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
p14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)
dev.off()

#---------Graph 9  ***violin plot****
png(file = "outletsalesV.jpg")
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")
plot_grid(p15, p16, ncol = 1)
dev.off()

#-*****************Missing Value Treatment*********************
colSums(is.na(combi))
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
  
}
zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
  
}


#Feature Engineering
# create a new feature 'Item_Type_new' 
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",
                               ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]
#table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

#Based on the above table we can create a new feature. Let's call it Item_category.


#png(file = "line_chart_7_lines.jpg")

#add Item Category with two letters
combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"


# years of operation of outlets
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]
#dev.off()


#Creating a new variable Item_MRP_clusters
Item_MRP_clusters = kmeans(combi$Item_MRP, centers = 4)
table(Item_MRP_clusters$cluster) # display no. of observations in each cluster

#png(file = "line_chart_10_lines.jpg")
combi$Item_MRP_clusters = as.factor(Item_MRP_clusters$cluster)
#dev.off()

##***********************Label encoding for the categorical variables
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                                 ifelse(Outlet_Size == "Medium", 1, 2))]
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
# removing categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]


#*************************One hot encoding for the categorical variable************

ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)


#*******************PreProcessing Data******************************************
#timeDate::skewness(combi$Item_Visibility); skewness(combi$price_per_unit_wt)

skewness(combi$Item_Visibility);

skewness(combi$price_per_unit_wt)
??skewness()
#197 line dewa chilo but aida follow korle code run hoy na..194 line tai create kora hoyce.
#skewness(combi$Item_Visibility); skewness(combi$price_per_unit_wt)

combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]


#*****************************Scaling numeric predictors*********************

num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars
num_vars_names = names(num_vars)
num_vars_names

combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
combi_numeric
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
prep_num 

##uses preprocess Number
?preProcess

combi_numeric_norm = predict(prep_num, combi_numeric)


combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

##Splitting the Data Back to train set

train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA

#png(file = "line_chart_10_lines.jpg")
cor_train = cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)
#dev.off()

#Modeling

#amar random forerst and xgboast use korbo..
    #Linear Regression
   # RandomForest
    # XGBoost
#******************************Linear Regression

set.seed(1234)
my_control = trainControl(method="cv", number=5)
linear_reg_mod = train(x = train[,-c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,
                       method='glmnet', trControl= my_control)
print("5- fold cross validation scores:")

print(round(linear_reg_mod$resample$RMSE, 2))

#linear_reg_mod=lm(Item_Outlet_Sales ~ .,data = train[, -c("Item_Identifier")])
#summary(linear_reg_mod)
#submission$Item_Outlet_Sales = (predict(linear_reg_mod, test[,-c("Item_Identifier")]))^2 
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")])
write.csv(submission, "Linear_Reg_submit_2_21_Apr_18.csv", row.names = F)

#***************RandomForest
set.seed(1237)
my_control = trainControl(method="cv", number=5)

tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)

rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
               y = train$Item_Outlet_Sales,
               method='ranger', 
               trControl= my_control, 
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")

#mean validation score
mean(rf_mod$resample$RMSE)


#plot displaying RMSE scores for different turing parameters

png(file = "randomForest.jpg")
plot(rf_mod)

#plot variable
plot(varImp(rf_mod))
dev.off()

#*****************XGBoost Model********************
?
param_list = list(
  
  objective = "reg:linear",
  eta=0.01,
  gamma = 1,
  max_depth=6,
  subsample=0.8,
  colsample_bytree=0.5
)
###??xgb.DMatrix() **amara function somporke jante pari
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))
set.seed(112)
xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)
xgb_model


#*********Graph 11********final output************
png(file = "xgboast.jpg")
var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model)
xgb.plot.importance(var_imp)
dev.off()


