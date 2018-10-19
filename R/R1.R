#Comments are written with # or ctrl + shift + C

# variable delcaration
A=3.5
A
print (A)

#not allowed. variable name must start from char
1a=3.5
1a

#its allowed. 
a1=3.6
a1

#not allowed
1.mod = 11.2

#allowed
mod_3=3.6
mod_3

#hidden variables
.MOD=6
.MOD
a

#all are vectors
# index starts from 1
A[1]
is.vector(A)

#new item can be directly added
A[2]=44
A[2]
A

#to decalre vector with n elements
A=c(3,4,5,6,7)
A

#if you add elements with differnent datatypes
A=c(3,4,5,6,7,"a")
A


B=3
B*B
B^10

A^3

#My Name is Abhijit


num = 3.5
cha= "A"
#to find type of variable
class(num)
num=3
class(num)
num=3L
class(num)


vect_num = c(1,2,3,4,5)
vect_char=c("a","b","c","d","e")

class(vect_num)
class(vect_char)

is.vector(vect_num)
is.vector(vect_char)


#Numeric Operations
C=4;
D=5

C+D
C-D
C*D
C/D
C %% D
D %% C

#String operation
g="Sachin Tendulkar"
h="Sachin"
i="Tendulkar"
h+i
h&i
h&&i

#string concatenation.. Default separator is " " 
paste(h,i)
paste(i,h)

#with _ as separator
paste(h,i,sep = "_")
paste(h,i,sep = "")

k=paste(h,i,sep = "")
k

substr(h,0,8)
x=g
x
x="abcd"
x
g

substr(g,7,8)
substr(g,1,8)

log(10)
log10(200)


log10(10)

abs(80)
abs(-90)
sqrt(36)
sqrt(38)
ls()
rm(a)
ls()
rm(B)
ls()
