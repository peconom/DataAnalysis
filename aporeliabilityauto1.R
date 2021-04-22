library(psych)

#get the data
datafilename="http://personality-project.org/R/datasets/extraversion.items.txt" 
mydata=read.table(datafilename,header=TRUE)  
str(mydata)

alpha<-psych::alpha(mydata[,2:6],check.keys=TRUE)
alpha



#επιπρόσθετη ανάλυση

mydata.keys.list <- list(Extraversion=c(1, -2, 3, -4,5))
mydata.keys <- make.keys(mydata[,2:6],mydata.keys.list,item.labels=colnames(mydata))
mydata.scored <-scoreItems(mydata.keys, mydata[,2:6], impute="NULL", min=1, max=6, digits=3)

mydata.scored$alpha
mydata.scored$av.r
datascores<-mydata.scored$scores
datascores
str(datascores)
print(mydata.scored,short=FALSE)
describe(datascores)























