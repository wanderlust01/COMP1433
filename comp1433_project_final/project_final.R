# Part 1 数据的导入和总览

#install
install.packages("randomForest")
install.packages("e1071")
install.packages("RColorBrewer")
install.packages("tidyverse")
library("RColorBrewer")
library("ggplot2")
library("tidyverse")
library("e1071")
library("randomForest")
#data 
dir=getwd()

dir1<-paste(dir,"tmdb_5000_movies_new_new.csv",sep="/")

tmdb_data<-read.csv(dir1)
summary(tmdb_data)
tmdb_data<-tmdb_data[c(1:2000),]#取数据的前2000行，缺失值比较少，更加可靠
tmdb_data<-tmdb_data[,c(1:28)]
tmdb_data[,1]<-as.numeric(tmdb_data[,1])#将character类型的数字转换成数值，下同
tmdb_data[,9]<-as.numeric(tmdb_data[,9])
tmdb_data[,13]<-as.numeric(tmdb_data[,13])
tmdb_data[,14]<-as.numeric(tmdb_data[,14])
tmdb_data[,19]<-as.numeric(tmdb_data[,19])
tmdb_data[,23]<-factor(tmdb_data[,23])#将数据类型传换成因子


summary(tmdb_data)
#missing data
missing<-which(colSums(is.na(tmdb_data))>0)
missing#发现对于目前数值型的数据，没有缺失值，但是字符串型的数据可能有缺失


#check whether the value follows the normal distribution
qqnorm(tmdb_data[,13])
qqline(tmdb_data[,13])
plot(density(tmdb_data[,13]),main = "cdf",lwd=3)
ggplot(data.frame(tmdb_data),aes(tmdb_data[,13]))+
  geom_histogram(bins=30)#票房的数据成比较陡峭的分布，近似为正态分布，但是与正态分布有差距，之后完善使之更符合正态分布





#Part 2 几个因素对电影票房是否有影响的分析


#duration
movie_revenue_duration<-tmdb_data[,c(13,14)]
movie_revenue_duration[,1]<-as.numeric(movie_revenue_duration[,1])
movie_revenue_duration[,1]<-log(movie_revenue_duration[,1])

plot(x=movie_revenue_duration[,1],
     y=movie_revenue_duration[,2],
     xlab='log.revenue',
     ylab='duration',
     ylim=c(0,300),
     main='log.revenue_duration')
#可以看出电影的时常与票房关系不大，这也符合常理


#budget
movie_revenue_budget<-tmdb_data[,c(13,1)]

budget_mean<-mean(movie_revenue_budget[,2])

for(i in 1:2000)
{
  if(tmdb_data[i,1]<10)
  {
    tmdb_data[i,1]= budget_mean
  }
}
movie_revenue_budget<-tmdb_data[,c(13,1)]
summary(movie_revenue_budget)

plot(x=movie_revenue_budget[,2],
     y=movie_revenue_budget[,1],
     xlab='budget',
     ylab='revenue',
     main='revenue_budget')
#可以看出预算与票房有一定的正相关性，可以被看作影响票房的因素

#popularity###############################
movie_revenue_popular<-tmdb_data[,c(9,1)]
popular_mean<-mean(movie_revenue_popular[,2])
for(i in 1:2000)
{
  if(tmdb_data[i,1]<10)
  {
    tmdb_data[i,1]= popular_mean
  }
}
movie_revenue_popular<-tmdb_data[,c(9,1)]
summary(movie_revenue_popular)

plot(x=movie_revenue_popular[,2],
     y=movie_revenue_popular[,1],
     xlab='popularity',
     ylab='budget',
     main='movie_revenue_popular'
)

#points
movie_revenue_vote<-tmdb_data[,c(19,1)]
vote_mean<-mean(movie_revenue_vote[,2])
for(i in 1:2000)
{
  if(tmdb_data[i,1]<10)
  {
    tmdb_data[i,1]= vote_mean
  }
}
movie_revenue_vote<-tmdb_data[,c(9,1)]
summary(movie_revenue_vote)
movie_revenue_vote[,2]
plot(x=movie_revenue_vote[,1],
     y=movie_revenue_vote[,2],
     xlab='votey',
     ylab='budget',
     main='movie_revenue_vote'
)

#genre

ifdif <-function(x,y)
{
  result<-vector()
  a<-substring(x,1:nchar(x),1:nchar(x))
  for(i in 1:length(y))
  {
    result[i]<- ifelse(all(a%in%substring(y[i],1:nchar(y[i]),1:nchar(y[i]))),
                       T,F)
  }
  return(result)
}
movie_revenue_genre<-tmdb_data[,c(13,21)]
movie_revenue_number<-as.numeric(movie_revenue_genre[,1])

movie_genre_table= data.frame(
  genre=c("Comedy","Mystery","Crime",
          "Drama","Romance","Thriller",
          "Sci-Fi","Animation","Horror",
          "Adventure","Action","Family"),
  avg_value=c(0,0,0,0,0,0,0,0,0,0,0,0)
)


for(i in 1:12){
  num = 0
  for(j in 1:2000){
    if(ifdif(movie_genre_table[i,1],movie_revenue_genre[j,2])){
      movie_genre_table[i,2] = movie_genre_table[i,2] + movie_revenue_number[j]/10000
      num= num+1
    }
  }
  movie_genre_table[i,2] = 10000*(movie_genre_table[i,2]/num )
}
order(movie_genre_table$avg_value)
genre_ordered<-movie_genre_table[order(movie_genre_table$avg_value),]
barplot(genre_ordered[,2],names.arg = genre_ordered[,1],col=rainbow(12))

#可以看出不同类型的电影的平均票房差距很大，电影类型可以看作影响票房的因素


#country
movie_revenue_country<-tmdb_data[,c(13,23)]
movie_revenue_number<-as.numeric(movie_revenue_country[,1])
movie_revenue_country[,2]<-as.character(movie_revenue_country[,2])
movie_country_table= data.frame(
  genre=c("USA","Canada","Spain",
          "Turkey","UK","France",
          "Chinese","Australia","Germany",
          "Sweden","South Korea","India"),
  avg_value=c(0,0,0,0,0,0,0,0,0,0,0,0)
)

for(i in 1:12){
  num = 0
  for(j in 1:2000){
    if(ifdif(movie_country_table[i,1],movie_revenue_country[j,2])){
      movie_country_table[i,2] = movie_country_table[i,2] + movie_revenue_number[j]/10000
      num= num+1
    }
  }
  movie_country_table[i,2] = 10000*(movie_country_table[i,2]/num )
}

order(movie_country_table$avg_value)
country_ordered<-movie_country_table[order(movie_country_table$avg_value),]
barplot(country_ordered[,2],names.arg = country_ordered[,1],col=rainbow(6),ylim=c(0,400000000))
#电影国别与电影票房也有关系



#language
movie_revenue_language<-tmdb_data[,c(13,22)]

movie_language_table= data.frame(
  country=c("English","Danish","Spanish",
            "Turkish","Korean","French",
            "German","Japanese","German",
            "Russian","Italian"),
  avg_value=c(0,0,0,0,0,0,0,0,0,0,0)
)
for(i in 1:11){
  num = 0
  for(j in 1:419){
    if(ifdif(movie_language_table[i,1],movie_revenue_language[j,2])){
      movie_language_table[i,2] = movie_language_table[i,2] +  movie_revenue_number[j]/10000
      num= num+1
    }
  }
  movie_language_table[i,2] = 10000*movie_language_table[i,2]/num
}
order(movie_language_table$avg_value)
language_ordered<-movie_language_table[order(movie_language_table$avg_value),]
barplot(language_ordered[,2],names.arg = language_ordered[,1],col=rainbow(4),ylim=c(0,800000000))
#电影语言与票房有一定关系，考虑到语言与国别的相关性，这里只取国别一个因素




#product_company  (the for loop in here may cost some time, please wait)
movie_revenue_production_company<-tmdb_data[,c(13,24,1)]
movie_revenue_number<-as.numeric(movie_revenue_country[,1])

for(i in 1:419)
{
  movie_revenue_production_company[i,3]=0
}
movie_production_company_table<-data.frame(
  company=c(1:100),
  avg_value=c(1:100)
)

company_count = 1

for(i in 1:1900)
{
  num = 0
  for(j in 1:1900)
  {
    if(movie_revenue_production_company[i,2]%in%movie_revenue_production_company[j,2]&&movie_revenue_production_company[i,2]!="#N/A")
    {
      movie_revenue_production_company[i,3] = movie_revenue_production_company[i,3] +
        movie_revenue_number[j]
      num = num+1
    }
  }
  movie_revenue_production_company[i,3] = movie_revenue_production_company[i,3]/num
  judge=F
  for(k in 1:100)
  {
    if(ifdif(movie_revenue_production_company[i,2],movie_production_company_table[k,1]))
    {
      judge=T
    }
  }
  if(num>3&&company_count<100&&!judge)
  {
    movie_production_company_table[company_count,1] = movie_revenue_production_company[i,2]
    movie_production_company_table[company_count,2] = movie_revenue_production_company[i,3]
    company_count = company_count +1
  }
}
order(movie_production_company_table$avg_value)
production_company_ordered<-movie_production_company_table[order(movie_production_company_table$avg_value),]
production_company_ordered_unique<-unique.data.frame(production_company_ordered)
production_company_ordered_unique2<-production_company_ordered_unique[c(95:100),]
par(las=2)
ggplot(production_company_ordered_unique2)+
  geom_bar(aes(x=avg_value,y=company),stat='identity')
#选取了出现次数比较多，而且平均票房较高的几个电影公司,可以看出不同电影公司所取得的平均票房成绩不同，
#因此电影公司可以看成影响票房的因素

library(readr)
library(tidyverse)
#df <- read_csv("E:/1433_project(1) (2) (2)/tmdb_5000_movies.csv")
dir=getwd()
dir1<-paste(dir,"tmdb_5000_movies_new_new.csv",sep="/")
df<-read.csv(dir1)
# top_10_director
top_10_director <- df %>%
  slice(1:2000) %>%
  drop_na(new_director) %>%
  filter(new_director != "#N/A") %>%
  group_by(new_director) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  slice(1:10)



# top_10_director_avg_budget

top_10_director_avg_budget <- df %>%
  slice(1:2000) %>%
  filter(new_director %in% top_10_director$new_director) %>%
  group_by(new_director) %>%
  summarise(ave_budget = mean(budget)) %>%
  arrange(desc(ave_budget))

top_10_director_avg_budget
director_ordered<-data.frame(top_10_director_avg_budget)
director_ordered
ggplot(data = top_10_director_avg_budget, 
       aes(x = reorder(new_director, ave_budget), y = ave_budget)) +
  geom_col() +
  coord_flip() +
  labs(x = "Director")




# top 10 actor
actors <- unlist(str_split(df$new_actor[1:2000], ","))
actors <- actors[actors != "#N/A"]
top_10_actor <- sort(table(actors), 
                     decreasing = T)[1:10]

top_10_actor <- as_tibble(top_10_actor)




# top_10_actor_ave_budget
rows <- c()
actors <- c()
for (i in 1:10) {
  numbers <- c(1:2000)[str_detect(df$new_actor[1:2000], top_10_actor$actors[i])]
  rows <- append(rows, numbers)
  actors <- append(actors, rep(top_10_actor$actors[i], length(numbers)))
}
new <- data.frame(rows, actors)
new <- new[order(new$rows), ]
new$budgets <- df$budget[new$rows]
top_10_actor_ave_budget <- new %>%
  group_by(actors) %>%
  summarise(ave_budget = mean(budgets)) %>%
  arrange(desc(ave_budget))


actor_ordered<-data.frame(top_10_actor_ave_budget)

ggplot(data = top_10_actor_ave_budget, aes(x = reorder(actors, ave_budget), y = ave_budget)) +
  geom_col() +
  coord_flip() +
  labs(x = "actors")



#Part 3 预测模型的使用和优化

#update some parameters to decrease the freedoms
#在电影类型，国家，电影公司这几个因素中，由于变量太多不能全部放入模型（如随机森林模型最多只支持53个变量）
#并且变量过多可能导致数据不能很好拟合，因此只取出现次数多，影响较大的变量，其余变量变更为“normal”或“others”变量，
#代表这些变量没有突出特点，我们放在一起考虑
updated_prodcution_company<-tmdb_data[,24]
for(i in 1:2000)
{
  judge=F
  for(j in 1:6)
  {
    if(ifdif(updated_prodcution_company[i],production_company_ordered_unique2[j,1]))
    {
      judge=T
    }
  }
  if(!judge)
  {
    updated_prodcution_company[i]="normal"
  }
}




updated_country<-tmdb_data[,23]
updated_country<-as.character(updated_country)
for(i in 1:2000)
{
  judge=F
  for(j in 1:12)
  {
    if(ifdif(country_ordered[j,1],updated_country[i]))
    {
      updated_country[i]=country_ordered[j,1]
      judge=T
    }
  }
  if(!judge)
  {
    updated_country[i]="normal"
  }
}


updated_genre<-tmdb_data[,21]
updated_genre
for(i in 1:2000)
{
  judge=F
  for(j in 1:12)
  {
    if(ifdif(genre_ordered[j,1],updated_genre[i]))
    {
      updated_genre[i]=genre_ordered[j,1]
      judge=T
    }
  }
  if(!judge)
  {
    updated_genre[i]="others"
  }
}


updated_actor<-tmdb_data[,28]

actor_ordered
for(i in 1:2000)
{
  judge=F
  for(j in 1:10)
  {
    if(ifdif(actor_ordered[j,1],updated_actor[i]))
    {
      updated_actor[i]=actor_ordered[j,1]
      judge=T
    }
  }
  if(!judge)
  {
    updated_actor[i]="other actors"
  }
}



updated_director<-tmdb_data[,27]

for(i in 1:2000)
{
  judge=F
  for(j in 1:5)
  {
    if(ifdif(director_ordered[j,1],updated_director[i]))
    {
      updated_director[i]=director_ordered[j,1]
      judge=T
    }
  }
  if(!judge)
  {
    updated_director[i]="other directors"
  }
}




#model
tmdb_data[,13]
train_data<-data.frame(
  revenue=log(as.numeric(tmdb_data[,13])),
  budget=log(as.numeric(tmdb_data[,1])),
  popularity=as.numeric(tmdb_data[,9]),
  vote=as.numeric(tmdb_data[,19]),
  production_company=factor(updated_prodcution_company),
  country = factor(updated_country),
  genre=factor(updated_genre),
  actor=factor(updated_actor),
  director=factor(updated_director)
)
summary(train_data[,])
#linear regression model
output.lm<-lm(revenue~budget+popularity+vote+production_company+country+genre+actor+director,data=train_data)
p<-lm(revenue~budget+popularity+vote+production_company+country+genre+actor+director,data=train_data)
summary(output.lm)#多元线性回归模型的结果
plot(fitted(p)/train_data$revenue)


#random forest model
output.forest<-randomForest(revenue~budget+popularity+vote+production_company+country+genre+actor+director,data=train_data,
                            importance=T,ntree=1000)
print(output.forest)#随机森林模型的结果
importance_data<-importance(output.forest)#重要性分析
varImpPlot(output.forest)


#create train and test data
#分配训练集和测试集
train<-train_data[1:1500,]
test <- train_data[1501:2000,]


#svm
svm_model<-svm(revenue~budget+popularity+vote+production_company+country+genre+actor+director,
               data=train,cost=3,kernel="linear")
svm_pred<-predict(svm_model,newdata=test)
summary(svm_model)
mean<-mean(train$revenue)
sd<-sd(train$revenue)
error_svm<-test$revenue-(sd*svm_pred+mean)
rmse<-function(error)
{
  sqrt(mean(error^2))
}
mae<-function(error)
{
  mean(abs(error))
}
rmse(error_svm)


#采用了多元线性回归，随机森林，SVM三个模型来分析，判断了SVM模型的预测结果


