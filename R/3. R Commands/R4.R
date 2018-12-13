#datafrmaes
df=mtcars

#all columns except wt and mpg
df[,!names(df) %in% c("wt", "mpg")]


#order
df[order(df$vs,df$wt),]

#decending  order
df[order(df$vs,-df$wt),]

#Joins

#install dplyr : Tools => Install Packages => Install Dependencies
library("dplyr")

df1 = data.frame(CustomerId = c(1:6),product =c(rep("Toaster",3), rep("Radio",3)))
df2 = data.frame(CustomerId = c(3,4,5,6), state =c (rep("Alabama", 2), rep("Ohio", 2)))
df1
df2
inner = inner_join(df1,df2,by="CustomerId")
left = left_join(df1,df2,by="CustomerId")
right = right_join(df1,df2,by="CustomerId")
full = full_join(df1,df2,by="CustomerId")
semi= semi_join(df1,df2,by="CustomerId")
anti = anti_join(df1,df2,by="CustomerId")

full
semi
anti



#for loop
x=sample(1:100,10)
x
for (i in 1:10) {
  print(x[i]**2)
}

for(j in which(x%%2==0)){
  print(x[j]**2)
}

for(j in which(x%%2!=0)){
  print(x[j]**2)
}


#functions
myfunc=function(x){
  range = max(x)-min(x)
  return (range)
}
myfunc(1:10)
myfunc("a":"z")


myfunc2=function(x){
  if(class(x) %in% c("numeric","integer")){
    range = max(x)-min(x)
    return (range)  
  }else{
    print("Vector is not of supported type")
  }
}
myfunc2(1:10)

myfunc2(c("a","b"))

#return multiple values
mysummary=function(x){
  s1=min(x)
  s2=max(x)
  s3=mean(x)
  s4=sd(x)
  s5=median(x)
  summary=list(min=s1,max=s2,mean=s3,sd=s4,median=s5)
  return (summary)
}

mysummary(2:100)




# optimization of R
# use vectors

# google r standards
#https://google.github.io/styleguide/Rguide.xml


#other functions

sapply(), vapply(), lapply()
sort()
print()
identical()

abs()
round()
sum()
mean()

--
seq() #create sequence, start, end, by	
rep()   #repeat , replicate

str
is.*()
as.*()

grep(),grepl()
sub(),gsub()


