#sequence generation
#by = gap
x=seq(1,5,by=0.3)
x

x=seq(1,by=.5,length.out = 10)
x

y=seq(1,50,length.out = 35)
y


#repetation
z=rep(2,10)
z

rep(x,y[1])

#here each parameter does not come in intellicence. But you can go to definition of function and find out
#111222333
a=rep(1:3,each=3)
a


#1a,2a,3a....10a

x=1:10
y=rep("a",10)

z=paste(x,y, sep = "")
z

#other way
p=paste(x,"b",sep = "")
p

#collapse
#1a+2a+3a+4a+5a...+10a
q = paste(p,collapse = "+")
q

#tabulate : need to find out
# x=c("a","b","c","a","a","a","c")
table(x)

#match:  Find vector in other vector
#it gives exact postion of the element in other vector
x=c("k","j","1","$","f")
y=letters

match(x,y)  #11 10 NA NA  6

# %in% operator = it gives whather element is present in other vector.
x%in%y #TRUE  TRUE FALSE FALSE  TRUE

x[x%in%y] # "k" "j" "f"

letters[match(x,y)] #"k" "j" NA  NA  "f"


length(y)
y[27]="a"
y

y[28]="k"

match(x,y)#11 10 NA NA  6

letters[match(x,y)] #"k" "j" NA  NA  "f"

#which gives index
x=2:99
which(x%%2!=0)


#sorting
sort(x)
sort(x,decreasing = TRUE)

y=c("abc","bcd","pqr","tuv")
sort(y)

#rev


#sampling
x=1:100
sample(x,3)

set.seed(1) #
sample(x,3)

set.seed(1) #
sample(letters,3)


set.seed(2) #
sample(letters,3)


#it uses resets algoritm

?set.seed

#taking samples more than total items. You can do it by replacing
#cannot take a sample larger than the population when 'replace = FALSE'
sample(c("a","b","c"),10)

sample(c("a","b","c"),10,replace = T)

x=sample(c("a","b"),100,replace = T, prob = c(0.3,0.7))
table(x)

letters[sample(1:10,5)]

#unique
x=sample(1:10,20,replace=T)
unique(x)


#List
x=1:10
y=rep(c("a","b"),each=6)
z=4.56

list1=list(x,y,z)
list1

  list1[[1]]
list1[[1]][3]
p=list1[[1]][3]
p


#list inside list
list2=list(list1,x,y)

list2
list2[[1]][[1]][3]

#you can give names to elements and then access it using $
list3=list(num=x,let=y,single=z)
list3$num
list3$let


#vector and dataframe are most important 
#mtcars comes from base package
df=mtcars
df
#select cell
df[2,3]
df[4,1]
df[1,1]

#select range
df[c(1:5),c(2:3)]

#select range, skip the rows/columns 
df[-4,-7]

#rownames 
rownames(df)
rownames(df)[2]

#column  names
colnames(df)
colnames(df)[3]


# datacamp.com/promo/meetup
#
#Learn => Datascience with R
#	introduction to r
#	intermediate r
# 	1 month 23 courses

#matrices
m = matrix(c(3:14), nrow = 4, byrow = TRUE)
m

n = matrix(c(3:14), nrow = 4, byrow = FALSE)
n

m[1,1]
n[3,2]

m1 = matrix(c(1,2,3,4,5,6), nrow = 2)
m2 = matrix(c(10,20,30,40,50,60), nrow =2)
m1+m2
m2-m1



#factors?


