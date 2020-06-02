# Import the dataset

library(readxl)
myResults <- read_excel("C:/Users/Krisi/Downloads/myForm_71852.xlsx")
colnames(myResults) <- paste("Question", 1:ncol(myResults), sep = "_")
str(myResults)
View(myResults)

#tibble [130 x 11] (S3: tbl_df/tbl/data.frame)
#$ Question_1 : chr [1:130] "15 - 25 години" "15 - 25 години" "15 - 25 години" "Над 50 години" ...
#$ Question_2 : chr [1:130] "Да, имам." "Да, имам." "Да, имам." "Да, имам." ...
#$ Question_3 : num [1:130] 5 5 9 8 6 5 1 6 1 2 ...
#$ Question_4 : chr [1:130] "Водя си бележки на телефона., Правя снимки., По - голяма част от времето си прекарвам в социалните мрежи., За о"| __truncated__ "Общувам със семейството и приятелите си., Провеждам бизнес разговори., Правя снимки., Слушам музика., За образователни цели." "Общувам със семейството и приятелите си., За образователни цели." "Общувам със семейството и приятелите си., Провеждам бизнес разговори., Чета книги онлайн., Правя снимки., Слуша"| __truncated__ ...
#$ Question_5 : chr [1:130] "Facebook, Instagram, YouTube, Messenger" "Facebook, Instagram, YouTube, Messenger" "Instagram, Messenger" "Facebook, Instagram, YouTube, Viber, Messenger, WhatsApp" ...
#$ Question_6 : chr [1:130] "Снимки" "Снимки" "Не споделям нищо" "Снимки, Коментари по определени теми" ...
#$ Question_7 : chr [1:130] "Ще видя съобщението, защото също може да е нещо важно, но ще отговоря по - късно." "Ще видя съобщението, защото също може да е нещо важно, но ще отговоря по - късно." "Ще видя съобщението, защото също може да е нещо важно, но ще отговоря по - късно." "Ще видя съобщението, защото също може да е нещо важно, но ще отговоря по - късно." ...
#$ Question_8 : num [1:130] 10 50 60 70 40 50 0 50 40 30 ...
#$ Question_9 : chr [1:130] "Не" "Не" "Донякъде" "Не" ...
#$ Question_10: num [1:130] 24 5 24 3 24 24 24 12 23 24 ...
#$ Question_11: chr [1:130] "12" "12" "12" "13" ...

# Част 1: Анализ на едномерна променлива.

# Въпрос 1: На колко години си? - Категорийна променлива

table_q1 <- table(myResults$Question_1) # използваме table(), за да представим данните в таблица
table_q1
#15 - 25 години 25 - 35 години 35 - 50 години  7 - 15 години  Над 50 години
#79              6             22             10             13

# Графично представяне
barplot(round(prop.table(table_q1)*100, 2), col = "darkblue", main = "На колко години си?", xlab = "Възрастов интервал",
        ylab = "Проценти", ylim = c(0, 100))

#Тест за нормално разпределение
shapiro.test(table_q1)
#Shapiro-Wilk normality test

#data:  table_q1
#W = 0.72425, p-value = 0.01689

#Въпрос 2: Имаш ли мобилен телефон, таблет или друго уствойство, което използваш за комуникация и за връзка с Интернет?
table_q2 <- table(myResults$Question_2)
factor(table_q2)


#Въпрос 3: По колко часа на ден в интервала 0 - 24 използваш телефона си? - Непрекъсната променлива
active_hours_q3 <- c(5,5,9,8,6,5,1,6,1,2,4,6,6,5,6,24,8,3,3,3,2,6,3,5,3,5,5,2,5,10,1,8,8,3,16,2,3,3,10,
                     3,4,3,12,8,8,7,3,16,15,10,5,10,10,4,5,6,7,4,1,4,5,12,1,10,24,3,4,5,10,8,5,10,4,4,12,10,
                     8,3,3,2,2,7,4,4,5,6,7,10,10,3,15,8,5,6,6,3,4,10,3,3,2,22,12,1,8,5,4,18,0,3,5,2,11,9,8,
                     1,6,6,6,8,3,7,3,6,12,8,5,4,8,6)
length(active_hours_q3)
#[1] 130

mean(active_hours_q3) # средна стойност
# [1] 6.467213
median(active_hours_q3) # медиана
# [1] 5
table_q3 <- table(active_hours_q3)        # мода - най - често срещана стойност
names(table_q3)[table_q3 == max(table_q3)]
# [1] "3"

# Следващите две функции описват центъра на разпределение
summary(active_hours_q3)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.000   3.000   5.000   6.467   8.000  24.000
quantile(active_hours_q3, prob = seq(0.1, 0.9, by = 0.1))
# 10%  20%  30%  40%  50%  60%  70%  80%  90%
# 2.1  3.0  4.0  5.0  5.0  6.0  8.0 10.0 11.8

# Вариация (дисперсия) на разпределението
range(active_hours_q3)   # range - показва най - голямата и най - малката стойност
# [1]  0 24
var(active_hours_q3)     # дисперсия
# [1] 19.1403719.40801
sd(active_hours_q3)      # стандартно отклонение
# [1] 4.3749714.405452
fivenum(active_hours_q3)
# [1]  0  3  5  8 24

# Графично представяне
hist(table_q3, main = "По колко часа на ден в интервала 0 - 24 използваш телефона си?", xlab = "Часове /Нормално разпределение/",
     ylab = "Брой хора /Честота/", col = "purple")

# Проверка за нормално разпределение
d1 <- rnorm(n = 10^2, mean = mean(active_hours_q3), sd = sd(active_hours_q3))
qqplot(active_hours_q3, d1, main = "Проверка за нормално разпределение")
abline(a = 0, b = 1) # чертае линия

#Тест за нормално разпределение
shapiro.test(table_q3)

#Shapiro-Wilk normality test

#data:  table_q3
#W = 0.85482, p-value = 0.0101

# Въпрос 4: С каква цел най - често използваш мобилното си устройство?  - Категорийна променлива

answer_1 <- "Общувам със семейството \nи приятелите си."
answer_2 <- "Провеждам бизнес разговори."
answer_3 <- "Играя игри."
answer_4 <- "Гледам филми."
answer_5 <- "Чета книги онлайн."
answer_6 <- "Водя си бележки на телефона."
answer_7 <- "Правя снимки."
answer_8 <- "Слушам музика."
answer_9 <- "По - голяма част от\nвремето си прекарвам в социалните мрежи."
answer_10 <- "За образователни цели."

# функция rep() - обединява всички отговори и тяхното процентно разпределение
question_4 <- c(rep(answer_1, 88), rep(answer_2, 22), rep(answer_3, 22), rep(answer_4, 12),
                rep(answer_5, 12), rep(answer_6, 25), rep(answer_7, 44), rep(answer_8, 44),
                rep(answer_9, 42), rep(answer_10, 51))
table_q4 <- table(question_4) # правим таблица с оотговорите
str(table_q4)
#'table' int [1:10(1d)] 25 12 51 22 88 42 44 22 44 12
#- attr(*, "dimnames")=List of 1
#..$ question_4: chr [1:10] "Водя си бележки на телефона." "Гледам филми." "За образователни цели." "Играя игри." ...

# процентно разпределение
str(round(prop.table(table_q4)*100, 2))
#'table' num [1:10(1d)] 6.91 3.31 14.09 6.08 24.31 ...
#- attr(*, "dimnames")=List of 1
#..$ question_4: chr [1:10] "Option_1" "Option_2" "Option_3" "Option_4" ...

# Графично представяне
barplot(round(prop.table(table_q4)*100, 2), col = "pink", main = "С каква цел най - често използваш \nмобилното си устройство? ",
        ylim = c(0, 100), ylab = "Проценти")
row.names(table_q4) <- paste("Option", 1:nrow(table_q4), sep = "_")

# Легенда
legend(x = "topright", legend = c("Option_1 - Водя си бележки на телефона.",
        "Option_2 - Гледам филми.","Option_3 - За образователни цели", "Option_4 - Провеждам бизнес разговори.",
        "Option_5 - Общувам със семейството и приятелите си.", "Option_6 - По - голяма част отвремето си
        прекарвам в социалните мрежи.","Option_7 - Правя снимки.","Option_8 - Играя игри..", "Option_9 - Слушам музика.",
        "Option_10 - Чета книги онлайн."), cex = 0.7, text.width = 5)

#Тест за нормално разпределение
shapiro.test(table_q4)
#Shapiro-Wilk normality test

#data:  table_q4
#W = 0.87528, p-value = 0.1151

# Въпрос 5: От изброените приложения и социални мрежи, отбележи тези, които най - често използваш. - Категорийна променлива
question_5 <- c(rep("Facebook", 73.1), rep("Instagram", 66.2), rep("Twitter", 0.0), rep("YouTube", 67.7),
                rep("Viber", 31.5), rep("Messenger", 82.3), rep("WhatsApp", 6.2), rep("Spotify", 10.8),
                rep("Amazon Kindle", 1.5), rep("None", 1.5))

table_q5 <- table(question_5)
str(table_q5)
#'table' int [1:9(1d)] 1 73 66 82 1 10 31 6 67
#- attr(*, "dimnames")=List of 1
#..$ question_5: chr [1:9] "Amazon Kindle" "Facebook" "Instagram" "Messenger" ...

str(round(prop.table(table_q5)*100, 2))
#'table' num [1:9(1d)] 0.3 21.7 19.6 24.3 0.3 ...
#- attr(*, "dimnames")=List of 1
#..$ question_5: chr [1:9] "Amazon Kindle" "Facebook" "Instagram" "Messenger" ...

table_q5
#question_5
#Amazon Kindle      Facebook     Instagram     Messenger          None       Spotify         Viber
#1                       73            66            82             1            10            31
#WhatsApp       YouTube
#6                   67

# Графично представяне
barplot(round(prop.table(table_q5)*100, 2), col = "firebrick1", main = "Кои приложения се използват най - много?", ylim = c(0,100), ylab = "Проценти")

#Тест за нормално разпределение
shapiro.test(table_q5)

#Shapiro-Wilk normality test

#data:  table_q5
#W = 0.8341, p-value = 0.04964


# Въпрос 6: Какво най - често споделяш в социалните мрежи? - Категорийна променлива
question_6 <- c(rep("Снимки", 55), rep("Видео", 5), rep("Коментари", 15), rep("Нищо", 25))
table_q6 <- table(question_6)
str(table_q6)
table_q6

# Графично представяне
barplot(round(prop.table(table_q6)*100, 2), col = "deepskyblue1", main = "Какво най - често споделяш в социалните мрежи?",
        ylim = c(0, 100), ylab = "Проценти")

#Тест за нормално разпределение
shapiro.test(table_q6)
#Shapiro-Wilk normality test

#data:  table_q6
#W = 0.92708, p-value = 0.5774

# Въпрос 7: Представи си, че си на важна среща, или имаш неотложен ангажимент, който трябва да свършиш.
# В същия момент получаваш съобщение. Какво ще направиш? - Категорийна променлива

question_7 <- c(rep("Ще отговоря веднага.", 3), rep("\nЩе видя съобщението, \nзащото също може да е нещо важно,
                но ще отговоря по - късно.", 64), rep("Изобщо няма да му обърна \nвнимание в момента.", 33))

table_q7 <- table(question_7)
round(prop.table(table_q7)*100, 2)
#question_7
#Изобщо няма да му обърна внимание в момента.
#33
#Ще видя съобщението, защото също може да е нещо важно, но ще отговоря по - късно.
#64
#Ще отговоря веднага.
#3

# Графично представяне
barplot(round(prop.table(table_q7)*100, 2), col = "greenyellow", main = "Какво ще направиш, ако получиш съобщение в неподходяща ситуация?",
        ylab = "Проценти", ylim = c(0, 100))

#Тест за нормално разпределение
shapiro.test(table_q7)
#Shapiro-Wilk normality test

#data:  table_q7
#W = 0.99991, p-value = 0.9819

# Въпрос 8: В рамките на интервала 0  - 100 колко процента считаш, че си ,,зависим" от телефона си? - Непрекъсната променлива
depend_percent_q8 <- c(10,50,60,70,40,50,0,50,40,30,30,60,50,50,80,79,30,50,25,50,50,40,80,40,50,30,50,
                       0,80,60,20,50,70,75,16,15,20,80,30,30,65,60,70,70,75,80,70,50,50,70,60,50,50,50,5,
                       80,70,30,50,0,50,25,50,40,75,100,70,30,90,30,50,0,50,50,90,50,80,70,30,40,10,20,
                       70,20,20,50,60,99,70,70,50,70,50,40,25,70,85,70,35,65,80,70,60,20,80,80,100,80,
                       50,50,50,100,25,0,50,0,50,50,50,50,25,50,25,50,100,50,75,50,75,50)
length(depend_percent_q8)
# [1] 130

mean(depend_percent_q8)  #средна стойност
# [1] 51.22308
median(depend_percent_q8) #медиана
# [1] 50
table_q8 <- table(depend_percent_q8)        #мода
names(table_q8)[table_q8 == max(table_q8)]
# [1] "50"

# Център на разпределението
summary(depend_percent_q8)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   31.25   50.00   51.22   70.00  100.00
quantile(depend_percent_q8, prob = seq(0.1, 1.0, by = 0.1))
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
# 20   30   40   50   50   50   70   70   80  100

# Вариация на  разпределението
range(depend_percent_q8)   #обхват
# [1]  0 100
var(depend_percent_q8)     #дисперсия
# [1] 579.2909
sd(depend_percent_q8)      #стандартно отклонение
# [1] 24.06846
fivenum(depend_percent_q8)
# [1]  0  30  50  70 100

# Графично представяне
hist(depend_percent_q8, main = "Колко процента считаш, че си зависим от телефона си?\n/Вероятностно разпределение/",
     xlab = "Нормално разпределение", ylab = "Брой хора /Честота/", col = "coral")

# Проверка за нормално разпределение
d2 <- rnorm(n = 10^2, mean = mean(depend_percent_q8), sd = sd(depend_percent_q8))
qqplot(depend_percent_q8, d2, main = "Проверка за нормално разпределение")
abline(a = 0, b = 1) # чертае линия

#Тест за нормално разпределение
shapiro.test(depend_percent_q8)
#Shapiro-Wilk normality test

#data:  depend_percent_q8
#W = 0.96358, p-value = 0.001469

# Въпрос 9: Смяташ ли, че мобилните устройства те отдалечават от приятелите и семейството ти? - Категорийна променлива
question_9 <- c(rep("Да", 10), rep("Не", 79), rep("Донякъде", 33))

table_q9 <- table(question_9)
# брой
table_q9
#question_9
#Да Донякъде       Не
#10       33       57

# проценти
round(prop.table(table_q9)*100, 2)
#question_9
#Да Донякъде       Не
#8.20    27.05    64.75

# Графично представяне
barplot(round(prop.table(table_q9)*100, 2), col = "Orange", main = "Смяташ ли, че мобилните устройства \nте отдалечават от близките ти?",
        ylab = "Проценти", ylim = c(0, 100))

#Тест за нормално разпределение
shapiro.test(table_q9)
#Shapiro-Wilk normality test

#data:  table_q9
#W = 0.96429, p-value = 0.6369

# Въпрос 10: Колко време - 0 - 24 часа, би могъл/ла да издържиш без да използваш мобилния си телефон? - Непрекъсната променлива
no_mobiles_q10 <- c(24,5,24,3,24,24,24,12,23,24,12,23,24,10,24,5,20,10,24,24,24,12,0,24,12,24,24,24,10,21,12,18,24,
                    24,24,12,12,1,24,6,24,17,10,5,10,24,5,0,12,12,23,24,12,24,1,24,24,24,24,20,10,24,3,5,2,4,24,24,16,
                    12,24,10,20,5,12,12,10,24,24,24,24,24,24,24,12,4,24,10,10,6,24,9,12,24,24,24,24,24,24,11,10,24,24,
                    23,3,10,10,12,24,4,24,24,24,24,24,24,4,24,10,24,24,24,20,22,24,24,24,15,4,4)

length(no_mobiles_q10)
# [1] 130

mean(no_mobiles_q10) #средна стойност
# [1] 16.92308
median(no_mobiles_q10) #медиана
# [1] 23
table_q10 <- table(no_mobiles_q10)        #мода
names(table_q10)[table_q10 == max(table_q10)]
# [1] "24"

# Център на разпределението
summary(no_mobiles_q10)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   10.00   23.00   16.92   24.00   24.00
quantile(no_mobiles_q10, prob = seq(0.1, 1.0, by = 0.1))
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
#   4   10   12   12   23   24   24   24   24   24

# Вариация на разпределението
range(no_mobiles_q10)   #обхват
# [1]  0 24
var(no_mobiles_q10)     #дисперсия
# [1] 65.68396
sd(no_mobiles_q10)      #стандартно отклонение
# [1] 8.104564
fivenum(no_mobiles_q10)
# [1]  0 10 23 24 24

# Графично представяне
hist(no_mobiles_q10, main = "Колко часа, би могъл/ла да издържиш \nбез да използваш мобилния си телефон?
      /Вероятностно разпределение/",xlab = "Часове /Нормално разпределение/",
      ylab = "Брой хора /Честота/", col = "cyan3")

# Проверка за нормално разпределение
d3 <- rnorm(n = 100, mean = mean(no_mobiles_q10), sd = sd(no_mobiles_q10))
qqplot(no_mobiles_q10, d2, main = "Проверка за нормално разпределение")
abline(a = 0, b = 1)

#Тест за нормално разпределение
shapiro.test(no_mobiles_q10)
#Shapiro-Wilk normality test

#data:  no_mobiles_q10
#W = 0.78822, p-value = 2.027e-12

# Въпрос 11: Каква месечна такса заплащаш месечно, за да използваш услугите на мобилиния си оператор?
monthly_fee_q11 <- c(15,13,56,35,20,20,10,14,25,30,15,12,15,19,36,25,20,20,25,25,25,0,20,20,25,25,30,20,25,24,30,
                     20,26,30,15,25,35,30,12,52,40,20,50,30,32,24,25,25,20,12,26,22,15,13,25,25,29,20,18,30,30,36,26,
                     20,26,25,25,25,50,50,15,20,25,17,25,28,25,12,30,20,17,17,12,40,20,12,12,20,35,25,40,30,16,25,30,30,25,30,
                     20,16,23,23,17,40,40,25,20,29,30,20,16,15,20,20,60,20,0,18,24,20,23,24,30,20,20,15,13,31,15,35)

length(monthly_fee_q11)
# [1] 130

mean(monthly_fee_q11) #средна стойност
# [1] 24.21538
median(monthly_fee_q11) #медиана
# [1] 24
table_q11 <- table(monthly_fee_q11)        #мода
names(table_q11)[table_q11 == max(table_q11)]
# [1] "20"

# Център на разпределението
summary(monthly_fee_q11)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   19.25   24.00   24.22   30.00   60.00
quantile(monthly_fee_q11, prob = seq(0.1, 1.0, by = 0.1))
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
# 15.0 18.0 20.0 20.0 25.0 25.0 27.8 30.0 35.3 60.0

# Вариация на разпределението
range(monthly_fee_q11)   #обхват
# [1]  0 60
var(monthly_fee_q11)     #дисперсия
# [1] 95.93775
sd(monthly_fee_q11)      #стандартно отклонение
# [1] 9.794782
fivenum(monthly_fee_q11)
# [1] 0 19 24 30 60

# Графично представяне
hist(monthly_fee_q11, main = "Каква месечна такса заплащаш?\n/Вероятностно разпределение/",
     xlab = "Такса /Нормално разпределение/", ylab = "Брой хора /Честота/", col = "deeppink")

# Проверка за нормално разпределение
d3 <- rnorm(n = 108, mean = mean(monthly_fee_q11), sd = sd(monthly_fee_q11))
qqplot(monthly_fee_q11, d2, main = "Проверка за нормално разпределение")
abline(a = 0, b = 1)

#Тест за нормално разпределение
shapiro.test(monthly_fee_q11)
#Shapiro-Wilk normality test

#data:  monthly_fee_q11
#W = 0.92202, p-value = 1.384e-06

#Част 2: Анализ на многомерна променлива

#Категорийна vs Категорийна
#1. Въпрос 4 - Въпрос 6
sample_q4 <- sample(x = question_4, size = 100, replace = TRUE)
table(sample_q4, question_6)
prop.table(x = table(sample_q4, question_6), margin = 1) # Показва ни в коя група, колко процента от данните попадат.
# Графично представяне
barplot(prop.table(x = table(sample_q4, question_6), margin = 1), beside = TRUE)


#Категорийна vs Числова
#1. Въпрос 3 - Въпрос 9
sample_q3 <- sample(x = active_hours_q3, size = 122)
tt <- boxplot(sample_q3~question_9)

#Числова vs Категорийна
#2. Въпрос 8 и Въпрос 1
wilcox.test(x = depend_percent_q8, y = table_q1, alternative = "greater", conf.int = TRUE)

# Числова vs Числова
# Въпрос 3 и Въпрос 8
# Корелационен анализ
rho <- round(cor(active_hours_q3, depend_percent_q8), 3) #коефициент на корелация
par(mfrow = c(1, 1))
plot(active_hours_q3, depend_percent_q8, main = "Correlation analysis")
abline(a = 4, b = 3, col = "red", lwd = 2)
cor(active_hours_q3, depend_percent_q8)
# [1] 0.2333105

cor.test(active_hours_q3, depend_percent_q8, method = "spearman")

# Въпрос 3 и Въпрос 11
# Линейна регресия
DF <- data.frame(active_hours_q3, monthly_fee_q11)
model <- lm(active_hours_q3~monthly_fee_q11)
model
#Call:
#  lm(formula = active_hours_q3 ~ monthly_fee_q11)

#Coefficients:
#  (Intercept)  monthly_fee_q11
#5.40803         0.03842

summary(model)

#Call:
#  lm(formula = active_hours_q3 ~ monthly_fee_q11)

#Residuals:
#  Min      1Q  Median      3Q     Max
#-6.5607 -3.1765 -0.9844  1.6218 17.6314

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)      5.40803    1.02689   5.266 5.71e-07 ***
#  monthly_fee_q11  0.03842    0.03933   0.977     0.33
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.376 on 128 degrees of freedom
#Multiple R-squared:  0.0074,	Adjusted R-squared:  -0.0003548
#F-statistic: 0.9542 on 1 and 128 DF,  p-value: 0.3305

resid(lm(active_hours_q3~monthly_fee_q11))

par(mfrow = c(1, 1))
plot(lm(active_hours_q3~monthly_fee_q11))

# Липса на автокорелация на грешките
install.packages("lmtest")
library(lmtest)
dwtest(model)
#Durbin-Watson test

#data:  model
#DW = 1.9754, p-value = 0.4392
#alternative hypothesis: true autocorrelation is greater than 0
