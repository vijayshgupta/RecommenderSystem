# 读入csv文件 进行探索性数据分析

# 输入数据
kBreakDate <- as.Date('2014-7-15')
kUrl <- "G:\\DataMining\\ali_user_data\\data_desc.csv"  # csv file url
data.user <- read.csv(kUrl, header = TRUE, sep = ',', 
                      colClasses = c('character', 'character', 'character', 'Date'))


# 将数据分为两部分
data.train <- data.user[data.user$visit_datetime < kBreakDate, ]
data.test <- data.user[data.user$visit_datetime >= kBreakDate, ]

# 绘制用户行为图形
par(mfrow = c(2, 2))
hist(by(data.user, data.user$user_id, function(m) sum(m[,3]=='0') / 4),
     ylab = '频率', xlab = '点击次数', 
     col = 'cyan', freq = FALSE, main = '用户月均点击直方图')
lines(density(by(data.user, data.user$user_id, function(m) sum(m[,3]=='0') / 4)))

hist(by(data.user, data.user$user_id, function(m) sum(m[,3]=='1') / 4),
     ylab = '频率', xlab = '购买次数',
     col = 'cyan3', freq = FALSE, main = '用户月均购买直方图')
hist(by(data.user, data.user$user_id, function(m) sum(m[,3]=='2') / 4), 
     ylab = '频率', xlab =  '收藏次数',
     col = 'cyan4', freq = FALSE, main = '用户月均收藏直方图')
hist(by(data.user, data.user$user_id, function(m) sum(m[,3]=='3') / 4),
     ylab = '频率', xlab = '购物车使用次数',
     col ='blue', freq = FALSE , main = '用户月均使用购物车直方图')
par(mfrow = c(1, 1))
# 商品的基本信息
summary(by(data.user, data.user$brand_id, function(m) sum(m[,3] == '0')))
# result
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    1.00    4.00   18.31   12.00 3196.00 

summary(by(data.user, data.user$brand_id, function(m) sum(m[,3] == '1')))
# result
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.0000   0.0000   0.0000   0.7328   0.0000 124.0000 

summary(by(data.user, data.user$brand_id, function(m) sum(m[,3] == '2')))
# result
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.1263  0.0000 36.0000 

summary(by(data.user, data.user$brand_id, function(m) sum(m[,3] == '3')))
# result
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.00000 0.01605 0.00000 8.00000 


