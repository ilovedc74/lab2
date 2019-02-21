setwd("~/Documents/Panc/Research/Research skills/lab2/My file")
l2 = read.csv("Lab2_data.csv")

install.packages("data.table")
library(data.table)
l2 = as.data.table(l2)
colnames(l2) = c('id', 'gender', 'age', 'con', 'score')

View(l3)
l3 = na.omit(l2)

# t test
fit1 = t.test(l3$score~l3$con, var.equal=FALSE)
fit2 = t.test(lg$score~lg$gender, var.equal=FALSE)

lg = l3[gender!=3,]
l3[,`:=`(percent=(score/36), chance = (rep(0.5,length(l3$score))), fail = 36-score),]
View(l3)
l3[,mean(percent),by="con"]




l4 = l3[, list(mean_score = mean(score), 
               mean_age = mean(age), 
               max_age = max(age),
               min_age = min(age),
               mean_percent = mean(percent)), by = "gender"
        ]

l5 = l3[, list(mean_score = mean(score), 
               mean_age = mean(age), 
               max_age = max(age),
               min_age = min(age),
               mean_percent = mean(percent)),
        ]



t.test(l3[con == 1,score,], mu=18) # non-stretched VS chance level
t.test(l3[con == 2,score,], mu=18) # stretched VS chance level

l3[, std:=sd(l3$score), by = c("gender","con")]
l3[, mean:=mean(l3$score), by = c("gender","con")]

l6 = l3[, `:=`(mean=mean(score), std=sd(score)),by = "con"]
l6 = l3[, mean_p:=mean(percent),by = "con"]

View(l6)


#------------------plots--------------------#
l3$gender[l3$gender == '1'] = 'female'
l3$gender[l3$gender == '2'] = 'male'
l3$gender[l3$gender == '3'] = 'other'

l3$con[l3$con == '1'] = 'normal'
l3$con[l3$con == '2'] = 'stretched'

install.packages("gglot2")
library(ggplot2)
ggplot(l3, aes(x = con, y = percent, color = gender)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent)+
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    plot.title = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(1.5)),
    axis.title.y = element_text(size = rel(1.5), angle = 90))+
  labs(
    title = "Correctness in different conditions",
    x = "Condition",
    y = "Percentage"
  )


