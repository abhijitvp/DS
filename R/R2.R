#TRUE is reserved word
x=TRUE

#Not allowed
x=True

#type
x="bunny"
y=F
z=4.5
class(x) #character
class(y) #character
class(z) #numeric


#character
v1="23.45"
class(v1)
v1

#type casting
v2=as.numeric(v1)
class(v2)
v2

#can not convert anything to anytype.
#If not possible the gives warning "NAs introduced by coercion "
v1="abcd"
v2=as.numeric(v1) #NAs introduced by coercion 


sum(1:5)

#To get help for function
#finds the functions with exact name
?sum
#finds the functions in all the packages and also on internet if connected
??sum

#one more way press F1



sin(90)
tan(10)
log10
log10(100)
log1p

#rounding function. Rounds to closest integer value
round(23.45)#23
round(23.55)#24


exp(1)

#factorial
factorial(5)
factorial(6)/factorial(5)

#string concatenation
x="sachin"
y="Tendulakar"
z="cricket"

paste(x,y,z)
paste(x,y,z,sep = "")

paste(x,y,z,sep = "$")

#sachin#tendulakr:cricket using paste
paste(paste(x,y,sep = "#"),z,sep = ":")

#substitution
address="address-A1/3, Morya-Resi, Pune-411005"

#substitute 1st instance -sub
sub("-",":",address)

#substitute all occurances -gsub
gsub("-",":",address)


#substring
substring
part=sub

#fix functions
fix(sum)

#
ip="192.0.0.111:8080"
substr(ip,9,20)

#length of string - its not length of vector
nchar(ip)

#substring
substr(ip,nchar(ip)-3,nchar(ip))


#Logic	al operators
x=7
y=9
x>y
y>x
x==y
x!=y

#logical and &
x=10
x>=1 & x<=19

#logical or |
y="sachin"
y=="Sachin"|y=="SACHIN"

#vector declaration
x=c(2,4,6,8)
is.vector(x)
class(x)
x[3]

#length of vector
length(x)
x[length(x)+1]=10
x[5]

#copy 1st,3rd and 4th element
z=x[c(1,3,4)]
z[2]

#copy 1 to 3 elements
q=x[c(1:3)]
q[3]

#copy 1 to 3 and 5th element
p=x[c(1:3,5)]
p[4]

#new vector with 5 elements but using x vector
m=x[c(1,1,2,3,3)]
m

#negative subscript. All elements except 2nd element
m[-2]

#all elements except 2nd and 3rd. two ways to do
m[-c(2,3)]
m[c(-2,-3)]

#you can have conficting selction = not mix +ve and -ve

#all the elemets which meets the condition
n=1:10
n
n=n>3
n
n[n>2]  
n


#vector extends itself to length of other.. rest elements are repeated

a=1:5
b=1:10
a+b
#2  4  6  8 10  7  9 11 13 15
b+a
b=1:9
a+b


# Vectors (one dimensional array): can hold numeric, character or logical values. 
#                                 The elements in a vector all have the same data type.
# Matrices (two dimensional array): can hold numeric, character or logical values. 
#                                 The elements in a matrix all have the same data type.
# Data frames (two-dimensional objects): can hold numeric, character or logical values. 
#                                 Within a column all elements have the same data type, but different columns can be of different data type.


read.table()
