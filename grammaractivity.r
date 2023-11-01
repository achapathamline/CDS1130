library(ggplot2)
library(dplyr)


df <- read.csv("grammaractivity.csv")

plot1 <- ggplot(data = df, aes(x = HEIGHT, y = FEV, col = SEX, shape = SEX)) +
  geom_point() +
  ggtitle("plot1")

print(plot1)

df_summary_sex <- df %>%
  group_by(SMOKE, SEX) %>%
  summarise(Total_FEV = sum(FEV))


plot2 <- ggplot(data = df_summary_sex, aes(x = factor(SMOKE), y = Total_FEV, fill = SEX)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("plot2")

print(plot2)

plot3 <- ggplot(data = df_summary_sex, aes(x = factor(SMOKE), y = Total_FEV, fill = SEX)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("plot3")

print(plot3)

plot4 <- ggplot(data = df_summary_sex, aes(x = factor(SEX), y = Total_FEV, fill = SEX)) +
  geom_bar(stat = "identity") +
  ggtitle("plot4")

print(plot4)

plot5 <- ggplot(data = df_summary_sex, aes(x = factor(SMOKE), y = Total_FEV, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle("plot5")

print(plot5)

plot6 <- ggplot(data = df_summary_sex, aes(x = factor(SMOKE), y = Total_FEV, col = SEX, shape = SEX)) +
  geom_point() +
  ggtitle("plot6")

print(plot6)