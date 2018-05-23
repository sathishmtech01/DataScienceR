library(dplyr)
library(magrittr)
# Data Gathering
data_gathered = read.csv("/home/sathish/Sathish/Sathish/Learning/DataScience/CR Details for PBI/CR_Month_Effort.csv")

# Data Understanding
# Summary of data
data_summary = summary(data_gathered)
data_summary

# Coloumn1 - CR.No.
dplyr::filter(data_gathered,CR.No.=="NPTR-1320")
# Findings the data is not proper, since their should be only one unique CR.No.

# Coloumn2 - Application
data.frame(data_gathered["Application"])

application_ud = data_gathered %>% select(Application,Total.Effort) %>% group_by(Application) %>% summarise(total = n(),total_effort = sum(Total.Effort)) %>% arrange(desc(total)) 
plot(application_ud$total,application_ud$total_effort)

linear_model = lm(total_effort~total,data = application_ud)  
summary(linear_model)


data_gathered$Application %<>% factor

factor(data_gathered$Application)
dplyr::filter(data_gathered,CR.No.=="NPTR-1320")
# Findings the data is not proper, since their should be only one unique CR.No.


# Data Preprocessing