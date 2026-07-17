library(car)
library(lmtest)

data=read.csv("C:/Users/panos/Downloads/Industrial_Process_Performance.csv", stringsAsFactors = TRUE)

str(data)
summary(data)

sapply(data[c("Training_Hours", "Previous_Quality_Score", "Rest_Hours_Before_Shift",
              "QC_Simulations_Completed", "Production_Performance_Index")], sd)

table(data$Lean_Program)
table(data$Shift)
prop.table(table(data$Lean_Program))
prop.table(table(data$Shift))

par(mfrow=c(2, 3))
hist(data$Training_Hours, main="Training_Hours", xlab="Ώρες εκπαίδευσης", col="lightblue")
hist(data$Previous_Quality_Score, main="Previous_Quality_Score", xlab="Βαθμολογία ποιότητας", col="lightblue")
hist(data$Rest_Hours_Before_Shift, main="Rest_Hours_Before_Shift", xlab="Ώρες ξεκούρασης", col="lightblue")
hist(data$QC_Simulations_Completed, main="QC_Simulations_Completed", xlab="Αρ. προσομοιώσεων", col="lightblue")
hist(data$Production_Performance_Index, main="Production_Performance_Index", xlab="Δείκτης απόδοσης", col="lightblue")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
barplot(table(data$Lean_Program), main="Lean_Program",col="orange")
barplot(table(data$Shift), main="Shift", col="orange")
par(mfrow=c(1, 1))


aggregate(Production_Performance_Index~Shift, data=data,
          FUN=function(x) c(n=length(x),mean=mean(x),median=median(x),sd=sd(x)))

morning=subset(data, Shift=="Morning")$Production_Performance_Index
evening=subset(data, Shift=="Evening")$Production_Performance_Index

shapiro.test(morning)
shapiro.test(evening)

par(mfrow=c(1, 2))
qqnorm(evening,main="Q-Q Plot: Evening"); qqline(evening, col="red")
qqnorm(morning, main="Q-Q Plot: Morning"); qqline(morning, col="red")
par(mfrow=c(1, 1))

leveneTest(Production_Performance_Index~Shift, data=data)
var.test(Production_Performance_Index~Shift, data=data)

boxplot(Production_Performance_Index~Shift, data=data,
        main="Δείκτης Παραγωγικής Απόδοσης ανά Βάρδια",
        xlab="Βάρδια", ylab = "Production_Performance_Index",
        col=c("skyblue", "salmon"), notch=TRUE)

plot(density(evening), col="blue", lwd=2, main="Κατανομή PPI ανά βάρδια",
     xlab = "Production_Performance_Index")
lines(density(morning), col="red", lwd=2)
legend("topright", legend=c("Evening", "Morning"), col=c("blue", "red"), lwd= 2)

t.test(Production_Performance_Index~Shift, data=data)

n1=length(morning)
n2=length(evening)
sp=sqrt(((n1-1)*var(morning)+(n2-1)*var(evening))/(n1+n2-2))
cohend_b=(mean(morning)-mean(evening))/sp
cohend_b


t.test(Training_Hours~Shift,data=data, alternative="less")
aggregate(Training_Hours~Shift,data=data,mean)

boxplot(Training_Hours~Shift,data=data,
        main="Ώρες Τεχνικής Εκπαίδευσης ανά Βάρδια",
        xlab="Βάρδια", ylab = "Training_Hours", col=c("skyblue", "yellow"))


cor.test(data$Previous_Quality_Score, data$Production_Performance_Index, method="pearson")

plot(data$Previous_Quality_Score, data$Production_Performance_Index,
     main="Προηγούμενη Βαθμολογία Ποιότητας vs Δείκτης Παραγωγικής Απόδοσης",
     xlab="Previous_Quality_Score", ylab="Production_Performance_Index",
     pch=19, col=rgb(0, 0, 1, 0.3))
abline(lm(Production_Performance_Index ~ Previous_Quality_Score, data = data), col = "red", lwd = 2)


table(data$Shift, data$Lean_Program)
chisq.test(table(data$Shift, data$Lean_Program))


t.test(Production_Performance_Index~Lean_Program, data=data)

morning_data=subset(data, Shift=="Morning")
evening_data=subset(data, Shift=="Evening")

t.test(Production_Performance_Index~Lean_Program, data=morning_data)
t.test(Production_Performance_Index~Lean_Program, data=evening_data)

par(mfrow=c(1, 2))
boxplot(Production_Performance_Index~Lean_Program, data=morning_data,
        main="Πρωινή Βάρδια", xlab="Lean_Program", ylab="PPI", col=c("green", "gold"))
boxplot(Production_Performance_Index~Lean_Program, data=evening_data,
        main="Απογευματινή Βάρδια", xlab="Lean_Program", ylab="PPI", col=c("green", "gold"))
par(mfrow=c(1, 1))


model=lm(Production_Performance_Index~Training_Hours+Previous_Quality_Score+
              Lean_Program+Rest_Hours_Before_Shift+QC_Simulations_Completed+Shift,
            data=data)
summary(model)


par(mfrow=c(2, 2))
plot(model)
par(mfrow=c(1, 1))

shapiro.test(residuals(model))
bptest(model)
dwtest(model)
vif(model)

