#' ---
#' title: "Week 4 Data Visualization Lab"
#' author: "Nicholas Thiphakhinkeo A17686679"
#' date: "10/15/2024"
#' ---

library("ggplot2")
ggplot(data=cars) +
  aes(x=speed, y=dist) +
  geom_point()
p <- ggplot(data=cars) +
  aes(x=speed, y=dist) +
  geom_point()
#adding scatter line
p + geom_line()
#adding fit line
p + geom_smooth()
#adding linear line 
p + geom_smooth(method="lm")

#-----------------------------
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)
#Plot and Adding Color
g <- ggplot(data=genes) +
  aes(x=Condition1, y=Condition2,
      col=State) +
  geom_point()
#Fraction of Total Genes Up-Reg(%)
table(genes$State)/nrow(genes) * 100
#Rounding Value (last purple value sig fig amounts)
round(table(genes$State)/nrow(genes) * 100,3)
#Changing Color
g + scale_color_manual(values=c("brown", "gray","purple")) +
  labs(title="Gene Expression Changes Upon Drug Treatment",
       x="Control(no drug)", y="Drug Administered") +
  theme_bw()

