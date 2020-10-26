### PROJECT 1 - INTRO TO BUSINESS ANALYTICS ###
# IMPORTING DATASET - UNEMPLOYMENT AND RELATED FACTORS BY STATE
df<-read.csv('/Users/madonnaconnors/Desktop/IBA\ PROJECT\ 1/IBA\ Project\ I\ Dataset.csv')

# First will perform preliminary exploratory analysis before determining cleaning tactics and generating visualizations
# After importing the data, will use the view function to see the full data sheet/ data frame that was imported
View(df)

#Using the summary function to get the "six number summary" statistics
summary(df)

# Checking the object class of the relevant parts of the dataset
class(df)
class(df$AVG..UNEMPLOYMENT.RATE)
class(df$STATE)
class(df$AVG..PER.CAPITA.PERSONAL.INCOME)
class(df$REAL.TOTAL.GDP)
class(df$POPULATION.SIZE..Thousands.of.Persons.)
class(df$BACHELOR.S.DEGREE.OR.HIGHER)

# Finding levels of the categorical variables in the dataset
levels(df$STATE)
levels(df$YEAR)

# Number of levels
length(levels(df$state))
length(levels(df$YEAR))

# Computing summary statistics
mean(df$AVG..UNEMPLOYMENT.RATE)
mean(df$AVG..PER.CAPITA.PERSONAL.INCOME)
mean(df$REAL.TOTAL.GDP)
sd(df$AVG..UNEMPLOYMENT.RATE)
var(df$POPULATION.SIZE..Thousands.of.Persons.)

# All returned NA
# Generating preliminary histograms for the numeric variables of the dataset

hist(df$AVG..UNEMPLOYMENT.RATE)
hist(df$BACHELOR.S.DEGREE.OR.HIGHER)
hist(df$REAL.TOTAL.GDP)
hist(df$POPULATION.SIZE..Thousands.of.Persons.)

### CLEANING ###
# Deleting the 'Postal Abbr.' and 'Code' variables/columns because they are not relevant to our analysis
View(df)
df1 <- data.frame(df)
View(df1)
df1$POSTAL.ABBR. <- NULL 
df1$CODE <- NULL
dim(df)

# Editing column name 
colnames(df1) # Viewing column names
## renaming these columns so it is easier/clearer when we input in the code for later, especially for visualizations
colnames(df1)[3] <- "AVG.UNEMPLOYMENT.RATE"
colnames(df1)[4] <- "AVG.PER.CAPITA.PERSONAL.INCOME"
colnames(df1)[5] <- "REAL.TOTAL.GDP"
colnames(df1)[6] <- "AVG.RESIDENT.POPULATION" 
colnames(df1)[7] <- "BACHELOR.S.DEGREE.OR.HIGHER"
colnames(df1) # View new names


### ANALYSIS ###
# Now wthat the data is fully clean, we can begin the analysis
## loading ggplot2 library

library(ggplot2)

# Creating scatter plots using the point geometry
ggplot(df1, aes(x = BACHELOR.S.DEGREE.OR.HIGHER, y = AVG.UNEMPLOYMENT.RATE)) # without data rendered
ggplot(df1, aes(x = BACHELOR.S.DEGREE.OR.HIGHER, y = AVG.UNEMPLOYMENT.RATE)) + 
  geom_point() #with data rendered
ggplot(df1, aes(AVG.RESIDENT.POPULATION, AVG.UNEMPLOYMENT.RATE)) + geom_point()
ggplot(df1, aes(REAL.TOTAL.GDP, AVG.UNEMPLOYMENT.RATE)) + geom_point()
ggplot(df1, aes(AVG.PER.CAPITA.PERSONAL.INCOME, AVG.UNEMPLOYMENT.RATE)) + geom_point()

#smoothing the plots
ggplot(df1, aes(BACHELOR.S.DEGREE.OR.HIGHER, AVG.UNEMPLOYMENT.RATE)) +
  geom_point() +
  geom_smooth()
ggplot(df1, aes(AVG.RESIDENT.POPULATION, AVG.UNEMPLOYMENT.RATE)) +
  geom_point() +
  geom_smooth()
ggplot(df1, aes(REAL.TOTAL.GDP, AVG.UNEMPLOYMENT.RATE)) +
  geom_point() +
  geom_smooth()
ggplot(df1, aes(AVG.UNEMPLOYMENT.RATE, AVG.PER.CAPITA.PERSONAL.INCOME)) +
  geom_point() +
  geom_smooth()

# linear regression
ggplot(df1, aes(BACHELOR.S.DEGREE.OR.HIGHER, AVG.UNEMPLOYMENT.RATE)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df1, aes(BACHELOR.S.DEGREE.OR.HIGHER, AVG.UNEMPLOYMENT.RATE)) + geom_jitter()

ggplot(df1, aes(BACHELOR.S.DEGREE.OR.HIGHER, AVG.UNEMPLOYMENT.RATE)) + geom_boxplot()
ggplot(df1, aes(BACHELOR.S.DEGREE.OR.HIGHER, AVG.UNEMPLOYMENT.RATE)) + geom_violin()
ggplot(df1, aes(BACHELOR.S.DEGREE.OR.HIGHER, AVG.PER.CAPITA.PERSONAL.INCOME )) + geom_violin()

ggplot(df1, aes(AVG.UNEMPLOYMENT.RATE)) + geom_histogram()
ggplot(df1, aes(BACHELOR.S.DEGREE.OR.HIGHER)) + geom_histogram()
ggplot(df1, aes(AVG.RESIDENT.POPULATION)) + geom_histogram()
ggplot(df1, aes(REAL.TOTAL.GDP)) + geom_histogram()
ggplot(df1, aes(AVG.PER.CAPITA.PERSONAL.INCOME)) + geom_histogram()

x <- "BACHELOR.S.DEGREE.OR.HIGHER"
y <- "AVG.UNEMPLOYMENT.RATE"

cor(df1$BACHELOR.S.DEGREE.OR.HIGHER, df1$AVG.UNEMPLOYMENT.RATE)
cor.test(x,y)

## normality test
shapiro.test(df1$BACHELOR.S.DEGREE.OR.HIGHER) #p-value = 0.6957
shapiro.test(df1$AVG.UNEMPLOYMENT.RATE) #p-value = 0.6383
# both not significantly different from normal distribution

res <- cor.test(df1$BACHELOR.S.DEGREE.OR.HIGHER, df1$AVG.UNEMPLOYMENT.RATE, 
                method = "pearson")
res ## The p-value is 0.4614, which is greater than the alpha level if 0.05 - fail to reject - corr. coeff. is -0.87 - thus, no correlation/ negative correlation

res <- cor.test(df1$AVG.UNEMPLOYMENT.RATE, df1$AVG.PER.CAPITA.PERSONAL.INCOME, 
                method = "pearson")
res ## The p-value is 0.2549 > 0.05 - fail to reject - corr. coeff. is -0.16 - thus, no correlation (closer to 0)

res <- cor.test(df1$REAL.TOTAL.GDP, df1$AVG.UNEMPLOYMENT.RATE, 
                method = "pearson")
res ## The p-value is 0.04578 < 0.05 - reject - corr. coeff. is 0.2838237 - thus, slightly positive correlation

res <- cor.test(df1$AVG.RESIDENT.POPULATION, df1$AVG.UNEMPLOYMENT.RATE, 
                method = "pearson")
res ## The p-value is 0.01007 < 0.05 - reject - corr. coeff. is 0.360721 - thus, strong positive correlation

## LINEAR REGRESSION MODEL ##
## Model: AVG.UNEMPLOYMENT.RATE = B0 + B1*BACHELOR.S.DEGREE.OR.HIGHER+u
## AVG.PER.CAPITA.PERSONAL.INCOME = B0 + B1*AVG.UNEMPLOYMENT.RATE+u
## AVG.UNEMPLOYMENT.RATE = B0 + B1*REAL.TOTAL.GDP+u
## AVG.RESIDENT.POPULATION = B0 + B1*AVG.UNEMPLOYMENT.RATE+u

## Ran final again
