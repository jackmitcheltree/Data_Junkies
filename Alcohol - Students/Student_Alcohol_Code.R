# URL = https://data.world/data-society/student-alcohol-consumption
# The data were obtained in a survey of students math and portuguese language courses in secondary school. It contains a lot of interesting social, gender and study information about students. 

# School: 1 = Gabriel Pereira and 0 = Mousinho da Silveira
# Sex: 1 = Male and 0 = Female
# Age: numeric (15-22)
# Location: 1 = Urban and 0 = Rural
# Fam_Size: 1 = less or equal to 3 and 0 = greater than 3
# Parent_Status: 1 = living together and 0 = living apart
# M_Edu: mother's education (0 =none, 1 = 4th grade, 2 = 5th to 9th grade, 3 = secondary, 4 = higher education)
# F_Edu: father's education (0 =none, 1 = 4th grade, 2 = 5th to 9th grade, 3 = secondary, 4 = higher education)
# M_Job: mother's job 
# F_Job: Father's job 
# Reason: reason to choose this school (close to 'home', school 'reputation', 'course' preference or 'other')
# Guardian: student's guardian ('mother', 'father' or 'other')
# Travel_Time: home to school travel time (1 = <15 min., 2 = 15 to 30 min., 3 = 30 min. to 1 hour, or 4 = >1 hour)
# Study_Time: weekly study time (1 = <2 hours, 2 = 2 to 5 hours, 3 = 5 to 10 hours, or 4 = >10 hours)
# Failures: number of past class failures
# Schl_Suport: extra educational support (1 = yes and 0 = no)
# Fam_Support: family educational support (1 = yes and 0 = no)
# Activities: extra-curricular activities (1 = yes and 0 = no)
# Nursery: attended nursery school (1 = yes and 0 = no)
# Higher: wants to take higher education (1 = yes and 0 = no)
# Internet: Internet access at home (1 = yes and 0 = no)
# Romantic: with a romantic relationship (1 = yes and 0 = no)
# Fam_Rel: quality of family relationships (from 1 = very bad to 5 = excellent)
# Freetime: free time after school (from 1 = very low to 5 = very high)
# Going_Out: going out with friends (from 1 = very low to 5 = very high)
# Weekday_Alc: workday alcohol consumption (from 1 = very low to 5 = very high)
# Weekend_Alc: weekend alcohol consumption (from 1 = very low to 5 = very high)
# Health: current health status (from 1 = very bad to 5 = very good)
# Absences: number of school absences (0 to 93)
# G1 - first period math grade (0 to 20)
# G2 - second period math grade (0 to 20)
# G3 - third period math grade (0 to 20)



#--------------------------------------------------------------------------------------



math <- read.table("math_data.csv", sep=",", header=TRUE)
math$M_Job <- as.factor(math$M_Job)   
math$F_Job <- as.factor(math$F_Job)
math$Reason <- as.factor(math$Reason)
math$Guardian <- as.factor(math$Guardian)


model <- lm(G1 ~ School + Sex + Age + Location + Fam_Size + Parent_Status + M_Edu + F_Edu +
              Reason + Guardian + Travel_Time + Study_Time + Failure + Schl_Suport + 
              Fam_Support + Activities + Nursery + Higher + Internet +Romantic + 
              Fam_Rel + Freetime + Going_Out + Weekday_Alc + Weekend_Alc + 
              Absences, data = math)


# Removed "Health" and "M_job" and "F_job from full model"
both <- step(model, direction='both')
car::vif(both)
summary(both)


model2 <- lm(Freetime ~ Age, data=math)
summary(model2)












