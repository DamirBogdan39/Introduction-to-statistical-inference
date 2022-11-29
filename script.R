# Importing dataset

library(haven) #necessary for read_sav
data <- read_sav('GenderEqualityIndex.sav')

#First question
#Get mean, median, sd, min and max

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
print(paste('Variance of Participation is:', var(data$Participation),
            'Variance of Segregation and quality of work is:', var(data$Segregationandqualityofwork)))

# Median of Parititipation compared to Segregation and quality of work
if (median(data$Participation) > median(data$Segregationandqualityofwork)) {
  print('Median of Parititipation is higher than median of Segregation and quality of work')
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

#Question number three

# Testing the normality of distribution
shapiro.test(data$Financialresources)

# Testing for H0

aov.fin.pow <- aov(data$Financialresources ~ data$Power)
summary(aov.fin.pow)
