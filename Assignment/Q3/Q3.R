# Q1. load the data into the R system and put it into a data frame for further analyses
sal_1 <- read.csv('Salaries.csv',sep=',',header=TRUE)



# Q2. Adding an additional column with name ID
sal_1$ID <- seq(1, 35)
write.csv(sal_1, 'newSalaries.csv')



# Q3. output the data as schedule
SAL=sal_1[,1]
SAL=gsub("[$]","",SAL)
SAL=gsub("[,]","",SAL)
SAL=as.numeric(SAL)
#To find out every value
qujian=mean(SAL)
pingjun=sd(SAL)
zhongzhi=median(SAL,na.rm=TRUE)
zuixiao=min(SAL)
zuida=max(SAL)
#print the table
cat(" The statistics for salaries are: ","\n","mean value=",qujian,"\n","standard deviation=",pingjun,"\n","median value=",zhongzhi,"\n","minimum value=",zuixiao,"\n","maximum value=",zuida)



#Q4:
par(mfrow=c(1,2))
duck=sal_1[,3]
boxplot(SAL~a2,main="Salaries and MBA",col=c("yellow","purple"),xlab="MBA",ylab="Salaries")

zuck=sal_1[,2]
plot(zuck,SAL,type="p",main="Salaries and Ages",col=c("brown","blue"),xlab="Ages",ylab="Salaries")




#Q5:
#Compare with the people who has the MBA or Don't has the MBA,and finally make it visual
#suppose the list of who has/hasn't
has <- subset(sal_1,MBA=="Yes")     
hasl=c()
hasnot <- subset(sal_1,MBA=="No")
hasnotl=c()
#I will set different age group as fellow
thrity=c()
forty=c()
fifty=c()
sixty=c()

for(i in 1:35){
        if (sal_1$MBA[i]=="Yes"){
                hasnol=append(hasnotl,salary[i])
        }
        else{
                hasl=append(hasl,salary[i])
        }
}
cout = 0
while(cout < 36){
        if (s$Age[i]<30){
                thrity=append(thrity,salary[cout])
                cout = cout + 1
        }
        if(s$Age[i]>=30 & s$Age<40){
                forty=append(forty,salary[cout])
                cout = cout + 1
        }
        
        if(s$Age[i]>=40 & s$Age<50){
                fifty=append(fifty,salary[cout])
                cout = cout + 1
        }
        
        if (s$Age[i]>50){
                sixty=append(sixty,salary[cout])
                cout = cout + 1
        }
}
meanvalue=c(mean(thrity),mean(forty),mean(fifty),mean(sixty))
barplot(meanl2,col=c("black"),names=c("Age<30","Age[30,40)","Age[40,50)","age>50"))