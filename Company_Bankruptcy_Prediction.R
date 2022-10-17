TZ = 'GMT+3'
Sys.getlocale()
setwd()


#    Imports
# ============================================

install.packages('class')
library(class)
install.packages('gmodels')
library(gmodels)


input_data <- read.csv('data.csv', 
                       stringsAsFactors = FALSE)
str(input_data)

#    Check Nan rows and normalization
# ============================================

summary(input_data)
count.fields('data.csv')
sum(is.na(input_data))
# нормализация не нужна

#    Logistic regression 
# ============================================

input_data[c(1), c(16,18,19,37,42,46,50,57,74,81)]
input_data[c(1), c(1,17,19,20,38,43,47,51,58,75,82)]
df <- input_data[,c(1,17,19,20,38,43,47,51,58,75,82)]
df[1,]

str(df)

df$Bankrupt. <- factor(df$Bankrupt., 
                             levels = c(1,0),
                             labels = c(1,0))

str(df)


#    Correlation 
# ============================================

need_data <- df[,c(2,3,4,5,6,7,8,9,11)]
str(need_data)

cor_data = cor(need_data)
print(cor_data) # correlation matrix

summary(df)


#    Extraction of principal components (PC)
# ============================================

xpca <- prcomp(df[,-1], center=TRUE, scale=TRUE)
summary(xpca)
xpca$rotation

df$PC1 =0.487344684*df$Net.Value.Per.Share..B.
  +0.487183641*df$Net.Value.Per.Share..C.
  +0.487561847*df$Persistent.EPS.in.the.Last.Four.Seasons
  -0.184836735*df$Debt.ratio..
  +0.436723317*df$Operating.profit.Paid.in.capital
  -0.016669603*df$Accounts.Receivable.Turnover
  -0.009101715*df$Net.Worth.Turnover.Rate..times.
  +0.211251362*df$Cash.Total.Assets
  -0.069297348*df$Cash.Turnover.Rate
  +0.112910815*df$Cash.Flow.to.Liability

df$PC2 =-0.09362332*df$Net.Value.Per.Share..B.
  -0.09381222*df$Net.Value.Per.Share..C.
  -0.15512177*df$Persistent.EPS.in.the.Last.Four.Seasons
  -0.53889493*df$Debt.ratio..
  -0.20309386*df$Operating.profit.Paid.in.capital
  +0.02560597*df$Accounts.Receivable.Turnover
  -0.45631225*df$Net.Worth.Turnover.Rate..times.
  +0.44814781*df$Cash.Total.Assets
  -0.36928841*df$Cash.Turnover.Rate
  +0.28395315*df$Cash.Flow.to.Liability

df$PC3 =-0.16369781*df$Net.Value.Per.Share..B.
  -0.16405193*df$Net.Value.Per.Share..C.
  +0.02221135*df$Persistent.EPS.in.the.Last.Four.Seasons
  +0.33053051*df$Debt.ratio..
  +0.12197723*df$Operating.profit.Paid.in.capital
  +0.02352823*df$Accounts.Receivable.Turnover
  +0.56420956*df$Net.Worth.Turnover.Rate..times.
  +0.40141584*df$Cash.Total.Assets
  -0.29677697*df$Cash.Turnover.Rate
  +0.50355788*df$Cash.Flow.to.Liability
  
df$PC4 =0.029914209*df$Net.Value.Per.Share..B.
  +0.028781364*df$Net.Value.Per.Share..C.
  +0.014695681*df$Persistent.EPS.in.the.Last.Four.Seasons
  +0.073410309*df$Debt.ratio..
  +0.006644783*df$Operating.profit.Paid.in.capital
  +0.989072506*df$Accounts.Receivable.Turnover
  -0.070382737*df$Net.Worth.Turnover.Rate..times.
  -0.078450654*df$Cash.Total.Assets
  -0.040987917*df$Cash.Turnover.Rate
  +0.039685231*df$Cash.Flow.to.Liability

str(df)
df_pc <- df[,c(1,12,13,14,15)]
df_pc[1,]


#    Train test split
# ============================================

train = df_pc[1:5455,] # 80% 
test_targ = df_pc[5456:6819,] # 20%
test = test_targ[-1]

str(test)



#    Model predict
# ============================================


fit <- glm(Bankrupt. ~ PC1+PC2+PC3+PC4,
             family=binomial, data=train)
predict <- predict(fit, new_data=test,
                   type='response',
                   se.fit=FALSE,
                   dispersion=NULL, 
                   terms=NULL,
                   na.action=na.pass)
str(predict)


#    K-Neighbors classification
# ============================================

df_kn <- input_data
df_kn$Bankrupt. <- factor(df$Bankrupt., 
                               levels=c(1,0),
                               labels=c(1,0))

df_kn_train <- df_kn[1:5455,]
df_kn_test <- df_kn[5456:6730,]
df_kn_train_labels <- df_kn[1:5455,1]
df_kn_test_labels <- df_kn[5456:6730,1]


predict_test <- knn(train=df_kn_train,
                    test=df_kn_test,
                    cl=df_kn_train_labels,
                    k=5)

CrossTable(x=df_kn_test_labels,
            y=predict_test,
            prop.chisq=TRUE)











