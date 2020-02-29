#d1=read.table("Copy of student-mat.csv",sep=";",header=TRUE)
#d2=read.table("Copy of student-por.csv",sep=";",header=TRUE)

#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#print(nrow(d3)) # 382 students

#opened up the data manually and ran the code below to get the full set. 

FullSet <- merge(Copy_of_student_mat, Copy_of_student_por, by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(FullSet))

summary(FullSet)
require(CAR)
car::vif(FullSet)

age_reg <- lm(age ~ freetime.y, data = FullSet)
summary(age_reg)