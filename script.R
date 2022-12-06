# Installing necessary libraries
# Libraries are commented to avoid reinstalling after rerunning the code.

#install.packages("coin")
install.packages("skedastic")

# Importing necessary libraries

library(haven) #necessary for read_sav
library(ggplot2)
library(coin)
library(dplyr)
library(car)
library(skedastic)

# Removing scientific notation

options(scipen = 99999)

# Importing the dataset

data <- read_sav('GenderEqualityIndex.sav')

# First question

# Get mean, median, sd, min and max

desc.stat <- function(x) {
  paste('Mean =', mean(x), 
        'SD =', sd(x),
        'Max =', max(x),
        'Min =', min(x),
        'Median =', median(x))
}

desc.stat(data$Participation)
desc.stat(data$Segregationandqualityofwork)


# Indicator with the smaller variance is:
print(paste('Variance of Participation is:', 
            var(data$Participation),
            'Variance of Segregation and quality of work is:', 
            var(data$Segregationandqualityofwork)))

# Median of Parititipation compared to Segregation and quality of work

if (median(data$Participation) > median(data$Segregationandqualityofwork)) {
  print('Median of Partitipation is higher than median of Segregation and quality of work')
}

# Range of Segregation and quality of work

print(paste('Range of Segregation and quality of work is:', 
            max(data$Segregationandqualityofwork) 
           -min(data$Segregationandqualityofwork)))

# Interquartile range of Participation

print(paste('Interquartile range of Participation is:',
            IQR(data$Participation)))


# Question number two

table(data$Time)

# Question number three

# Testing the normality of distribution

shapiro.test(data$Financialresources)

# Testing for H0

aov.fin.pow <- aov(data$Financialresources ~ data$Power)
summary(aov.fin.pow)

# Question number four

data_4 <- filter(data, Work == 2 | Work == 3)

shapiro.test(data_4$Notatriskofpoverty)

data_4$Work <- as.factor(data_4$Work)

t.test(data_4$Notatriskofpoverty~data_4$Work)

ggplot(data_4, aes(x=Work, y=Notatriskofpoverty)) + 
  geom_boxplot()

# Question number five

shapiro.test(data$Economicsituation)

shapiro.test(data$Segregation)

cor.test(data$Economicsituation, data$Segregation)

ggplot(data, aes(x=Economicsituation, y=Segregation)) +
  geom_point()

# Question number six

table(data$Power, data$Money)

class(data$Money)
class(data$Power)

data$Money <- as.factor(data$Money)
data$Power <- as.factor(data$Power)

chisq.test(data$Money, data$Power)

# Question number seven

data_7 <- filter(data, Knowledge == 1 | Knowledge == 2)

class(data_7$Knowledge)
data_7$Knowledge <- as.factor(data_7$Knowledge)

shapiro.test(data_7$Attainmentandparticipation)

leveneTest(Attainmentandparticipation ~ Knowledge, data = data_7)

ggplot(data_7, aes(Attainmentandparticipation, fill = Knowledge)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

t.test(data_7$Attainmentandparticipation~data_7$Knowledge)

ggplot(data_7, aes(x=Knowledge, y=Attainmentandparticipation)) + 
  geom_boxplot()

# Question number eight

shapiro.test(data$Careactivities)

t.test(data$Careactivities, mu = 75)

# Question number nine

model9 <- lm(data$Attainmentandparticipation ~ data$Graduatesoftertiaryeducation)

summary(model9)

ggplot(data, aes(x=Graduatesoftertiaryeducation, y=Attainmentandparticipation)) +
  geom_point()

# Question number ten
# Changed from Health variable to Time variable

shapiro.test(data$Socialactivities)

ggplot(data, aes(x=Socialactivities)) +
  geom_histogram()

model10 <- aov(Socialactivities ~ Time, data = data)
summary(model10)

ggplot(data, aes(x=Health, y=Socialactivities)) + 
  geom_boxplot()

class(data$Time)
data$Time <- as.factor(data$Time)

ggplot(data, aes(x=Time, y=Socialactivities)) + 
  geom_boxplot()

# Question number eleven

model11 <- lm(data$Economic ~ data$Social + 
                data$Access + data$Political)

summary(model11)

# Part G) from question eleven
# Is there a problem with multicolinearity in the model?

car::vif(model11)

# Part H) from question eleven
#Is there a problem of autocorrelation in the model?

car::durbinWatsonTest(model11) 

skedastic::glejser(model11)
