#R version 4.2.1 (2022-06-23 ucrt)


library(readxl)
library(data.table)
library(multcomp)


#read file
data <- read_excel("path", 
                   sheet = "Sheet1",
                   range = "A1:A1")


#transform file suitable for anova
#the data was assumed to being in this form:
#Condition1 |	Condition2
#Value1     | Value2
#Value3     | Value4

data_melted <- melt(data, id.vars = NULL)
setnames(data_melted, c("variable", "value"), c("Conditions", "Value"))
data_melted$Conditions <- as.factor(data_melted$Conditions)


#One Way Anova
model_data_melted <- aov(Value ~ Conditions, data = data_melted)
summary(model_data_melted)


#Tukey's HSD
TukeyHSD_model_data_melted <- glht(model_data_melted,
                                   linfct = mcp(Conditions = "Tukey"))
summary(TukeyHSD_model_data_melted)


#Export tables
capture.output(summary(model_data_melted),file="data_anova.doc")
capture.output(summary(TukeyHSD_model_data_melted),file="data_tukey.doc")
