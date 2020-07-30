BerkeleyPlots = function(){

library("Math160UPS")
data(UCBerkeley)

male = subset(UCBerkeley, Gender == "Male")
female = subset(UCBerkeley, Gender == "Female")

ProgramA = subset(UCBerkeley, Program == "Program A")
ProgramF = subset(UCBerkeley, Program == "Program F")

table1 = table(UCBerkeley$Accepted,UCBerkeley$Gender)
table1


table2 = table(UCBerkeley$Gender,UCBerkeley$Program,UCBerkeley$Accepted)
table2

mosaicplot(table(UCBerkeley$GenderProgram,UCBerkeley$Accepted), main = "The whole picture")
barplot(table(UCBerkeley$Accepted,UCBerkeley$GenderProgram), beside = TRUE, legend.text = TRUE, main = "Proportion Accepted by Gender and Program")
barplot(prop.table(table(ProgramF$Accepted,ProgramF$Gender),2), legend.text = TRUE, main = "Proportion Accepted by Gender (Program F)")
barplot(prop.table(table(ProgramA$Accepted,ProgramA$Gender),2), legend.text = TRUE, main = "Proportion Accepted by Gender (Program A)")
mosaicplot(t(table1), main = "Number of Students Admitted by Gender")
barplot(prop.table(table1,2), legend.text = TRUE, main = "Proportion of Applicants Accepted by Gender")
barplot(table1,beside = TRUE, legend.text = TRUE, main = "Number Admitted to Graduate Programs by Gender")
}
