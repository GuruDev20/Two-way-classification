n<-as.integer(readline("Enter the number of elements:"))
a<-numeric(n)
b<-numeric(n)
c<-numeric(n)
sum1<-0;sum2<-0;sum3<-0;sum4<-0;sum5<-0;sum6<-0;sum7<-0;df1<-0;df2<-0;df3<-0;df4<-0;sum8<-0;sum9<-0
print("Enter array elements:")
for(i in 1:n){
    a[i]<-as.integer(readline())
    sum1<-sum1+a[i]
}
print("x1")
print(paste(a))
print("----------------------")
for(i in 1:n){
    b[i]<-as.integer(readline())
    sum2<-sum2+b[i]
}
print("x2")
print(paste(b))
print("----------------------")
for(i in 1:n){
    c[i]<-as.integer(readline())
    sum3<-sum3+c[i]
}
print("x3")
print(paste(c))
print("----------------------")
total<-numeric(n)
for(i in 1:n){
    total[i]<-a[i]+b[i]+c[i]
    sum5<-sum5+total[i]
}
print("total")
print(paste(total))
print("----------------------")
for(i in 1:n){
    a[i]<-a[i]*a[i]
    sum6<-sum6+a[i]
    b[i]<-b[i]*b[i]
    sum7<-sum7+b[i]
    c[i]<-c[i]*c[i]
    sum8<-sum8+c[i]
}
print("x1 square")
print(paste(a))
print("----------------------")
print("x2 square")
print(paste(b))
print("----------------------")
print("x3 square")
print(paste(c))
print("----------------------")
N<-length(a)+length(b)+length(c)
Tsquare<-(sum5*sum5)
CF<-Tsquare/N
print(paste("Correction factor",CF))
print("----------------------")
TSS<-sum6+sum7+sum8-CF
print(paste("Total sum of square",TSS))
print("----------------------")
SSC<-((sum1*sum1)/length(a))+((sum2*sum2)/length(b))+((sum3*sum3)/length(c))-CF
print(paste("Column sum of square",SSC))
print("----------------------")
sum10<-0
for(i in 1:n){
    Z<-length(a[i])+length(b[i])+length(c[i])
    sum10=sum10+(total[i]*total[i])/Z
    SSR=sum10-CF
}
print(paste("Row sum of square",SSR))
print("----------------------")
SSE<-TSS-SSC-SSR
print(paste("Error sum of square",SSE))
print("----------------------")
print("Degrees of freedom")
c=Z-1;
print(paste("Column",c))
r=length(total)-1
print(paste("Row",r))
e=c*r 
print(paste("Error",e))
t=(Z*length(total))-1 
print(paste("Total",t))
print("----------------------")
MSC=SSC/c 
print(paste("MSC",MSC))
MSR=SSR/r 
print(paste("MSR",MSR))
MSE=SSE/e 
print(paste("MSE",MSE))
print("----------------------")
if(MSC>MSE){
    F_column<-(MSC/MSE)
    print(paste("F_column",F_column))
}else{
    F_column<-(MSE/MSC)
    print(paste("F_column",F_column))
}
print("----------------------")
if(MSR>MSE){
    F_row<-(MSR/MSE)
    print(paste("F_column",F_row))
}else{
    F_row<-(MSE/MSR)
    print(paste("F_row",F_row))
}
alpha<-as.integer(readline("enter the alpha value "))/100
print(paste(alpha))
if(MSE>MSC){
    df1<-e
    df2<-c
}else{
    df1<-c
    df2<-e
}
print(paste(df1))
print(paste(df2))
if(MSE>MSR){
    df3<-e
    df4<-r
}else{
    df3<-r
    df4<-e
}
print(paste(df3))
print(paste(df4))
F_col<-qf(1-alpha,df1,df2)
print(paste(F_col))
print(paste("Conclusion"))
if(F_column<F_col){
    print(paste("Column"))
    print(paste("Accept Ho"))
}else{
    print(paste("Column"))
    print(paste("Reject Ho"))
}
print("----------------------")
F_ro<-qf(1-alpha,df3,df4)
print(paste(F_ro))
if(F_row<F_ro){
    print(paste("Row"))
    print(paste("Accept Ho"))
}else{
    print(paste("Row"))
    print(paste("Reject Ho"))
}
print("----------------------")