
#The function "scratchKmeans" implement the algorithm of k-means
scratchKmeans<-function(data,k){
  rows_num<-nrow(data)
  continue_flag=TRUE
  initialPoints<-data.frame(V1=c(40,100,0),V2=c(40,0,100)) #initialization
  Points_before<-initialPoints
  Points_iteration<-matrix(0,nrow = k,ncol = ncol(data))
  
  #Calculate the Euclidean distance between two points
  Eudistance<-function(a,b){
    dist<-sqrt(sum((a-b)^2))
    return (dist)
  }
  
  #Record the distance from each point to each cluster
  matrix_error<-matrix(0,nrow=rows_num,ncol=k)
  while(continue_flag){
    #Record which cluster each point belongs to
    matrix_cluster<-matrix(0,nrow=rows_num,ncol=k)
    i=1
    while(i<rows_num+1){
      j=1
      while(j<k+1){
        matrix_error[i,j]<-Eudistance(data[i,],Points_before[j,])
        j=j+1
      }
      i=i+1
    }

    #Calculate the cluster to which each point belongs
    i=1
    while(i<rows_num+1){
      matrix_cluster[i,which.min(matrix_error[i,])]<-1
      i=i+1
    }
    #Update the new center of clusters
    i=1
    while(i<k+1){
      Points_iteration[i,]<-apply(data[which(matrix_cluster[,i] == 1),],2,"mean")
      i=i+1
    }
    true_vector<-c()
    #Determine if the center point has remained the same
    i=1
    while(i<k+1){
      if(all(Points_before[i,] == Points_iteration[i,]) == T){
        true_vector[i]<-TRUE
      }
      i=i+1
    }

    Points_before = Points_iteration
    continue_flag=ifelse(all(true_vector) == T,F,T)
  }
  colnames(Points_iteration)<-colnames(data)
  resul=list()
  resul[["centers"]]<-Points_iteration
  resul[["distance"]]<-matrix_error
  
  
  resul[["cluster"]]<-rep(1,rows_num)
  i=1
  
  while(i<rows_num+1){
    resul[["cluster"]][i]<-which(matrix_cluster[i,] == 1)
    i=i+1
  }
  #Returns the result
  return(resul)
}


#Load in the dataset "Points"
#In order to load the dataset, use the setwd() function to 
#set the work path to the path where the data is located
data<-read.csv("Points.csv", header = FALSE)

#Implement k-means algorithm to the data and calculate the result
result<-scratchKmeans(data, 3)
cluster<-result$cluster

#Plot the result
result_dataset<-cbind(data, cluster)
plot(result_dataset$V1, result_dataset$V2, 
     col=c("red","green", "blue")[result_dataset$cluster], 
     xlab="x-axis",ylab="y-axis", main="Plot of k-means")
#legend("topright", legend=unique(result_dataset$cluster), 
#       col=c("red","blue","green"), pch=1)












