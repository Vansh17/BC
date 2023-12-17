# ********************************EXP 1
library('LearnBayes')
data(studentdata)
print(studentdata[1:10,])

table(studentdata$Drink)

table(studentdata$Height)

table(studentdata$Drink)

barplot(table(studentdata$Drink),xlab="Drink",ylab="Count")

hours.of.sleep = studentdata$WakeUp - studentdata$ToSleep
summary(hours.of.sleep)

hist(hours.of.sleep,main="")

boxplot(hours.of.sleep~studentdata$Gender,ylab="Hours of Sleep")

female.Haircut=studentdata$Haircut[studentdata$Gender=="female"]
summary(female.Haircut)

male.Haircut=studentdata$Haircut[studentdata$Gender=="male"]
summary(male.Haircut)

hist(studentdata$Dvds)
print(summary(studentdata$Dvds))

print(table(studentdata$Dvds))
barplot(table(studentdata$Dvds))

boxplot(studentdata$Height~studentdata$Gender)