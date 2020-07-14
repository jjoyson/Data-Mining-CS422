
library('dplyr')
setwd(getwd())
college.df <- read.csv("college.csv", sep=",", header=T)
college.df %>% head

private <- college.df %>% group_by(Private) %>% summarize(Colleges = n())
private
str <- paste("There are",private[[2]][1],"public schools and",private[[2]][2],"private schools.")
str

college.private.df <- filter(college.df, Private == "Yes")
hist(college.private.df$PhD, prob = TRUE, main = "PhD holders in Private Colleges", xlab = "Private Colleges", col = terrain.colors(10))
lines(density(college.private.df$PhD), col = "red", lwd = 3)

college.public.df <- filter(college.df, Private == "No")
hist(college.public.df$PhD, prob = TRUE, main = "PhD holders in Public Colleges", xlab = "Public Colleges", col = topo.colors(8))
lines(density(college.public.df$PhD), col = "red", lwd = 3)

college.sortedGrad.Rate <- arrange(college.df, Grad.Rate)
select(college.sortedGrad.Rate, Name, Grad.Rate) %>% head(n=5)

summary(college.df)

pairs(college.df[,1:10], main = "First 10 columns of college.df", col = terrain.colors(nrow(college.df)))

boxplot(perc.alumni~Private , data = college.df, col = heat.colors(2),  main = "Donation in Colleges", xlab = "Private College", ylab = "Percent of Alumni")

str <- paste("Seen by the data, Private Colleges on average have higher percent of donating alumni")
str

boxplot(PhD~Private , data = college.df, col = topo.colors(2),  main = "Employed PhDs", xlab = "Private College", ylab = "PhDs")

str <- paste("Seen by the data, Public Colleges on average employee more PhDs")
str

Elite <- rep("No", nrow(college.df))
Elite[college.df$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college.df <- data.frame(college.df, Elite)
summ <- summary(college.df)
elite.colleges.str <- summ[,20][2]
elite.colleges.str <- strsplit(elite.colleges.str, ":")
elite.colleges <- elite.colleges.str[[1]][2]
str <- paste0("There are", elite.colleges, "elite colleges")
str

par(mfrow=c(3,2))
hist(college.df$Top25perc, breaks = 2, prob = TRUE, main = "Top 25 percent 2 Bins", col = rainbow(2), xlab = "Top 25 percent")
hist(college.df$Top25perc, breaks = 4, prob = TRUE, main = "Top 25 percent 4 Bins", col = terrain.colors(4), xlab = "Top 25 percent")
hist(college.df$Top25perc, breaks = 6, prob = TRUE, main = "Top 25 percent 6 Bins", col = heat.colors(6), xlab = "Top 25 percent")
hist(college.df$Top25perc, breaks = 8, prob = TRUE, main = "Top 25 percent 8 Bins", col = topo.colors(8), xlab = "Top 25 percent")
hist(college.df$Top25perc, breaks = 10, prob = TRUE, main = "Top 25 percent 10 Bins", col = cm.colors(10), xlab = "Top 25 percent")
hist(college.df$Top25perc, breaks = 12, prob = TRUE, main = "Top 25 percent 12 Bins", col = rainbow(12), xlab = "Top 25 percent")

par(mfrow=c(2,3))
hist(college.df$Books, breaks = 2, prob = TRUE, main = "Books 2 Bins", col = cm.colors(2), xlab = "Books")
hist(college.df$Books, breaks = 4, prob = TRUE, main = "Books 4 Bins", col = rainbow(4), xlab = "Books")
hist(college.df$Books, breaks = 6, prob = TRUE, main = "Books 6 Bins", col = terrain.colors(6), xlab = "Books")
hist(college.df$Books, breaks = 8, prob = TRUE, main = "Books 8 Bins", col = heat.colors(8), xlab = "Books")
hist(college.df$Books, breaks = 10, prob = TRUE, main = "Books 10 Bins", col = topo.colors(10), xlab = "Books")
hist(college.df$Books, breaks = 12, prob = TRUE, main = "Books 12 Bins", col = cm.colors(12), xlab = "Books")

par(mfrow=c(2,2))
hist(college.df$Terminal, breaks = 4, prob = TRUE, main = "Terminal 4 Bins", col = heat.colors(4), xlab = "Terminals")
hist(college.df$Terminal, breaks = 8, prob = TRUE, main = "Terminal 8 Bins", col = topo.colors(8), xlab = "Terminals")
hist(college.df$Terminal, breaks = 10, prob = TRUE, main = "Terminal 12 Bins", col = cm.colors(12), xlab = "Terminals")
hist(college.df$Terminal, breaks = 16, prob = TRUE, main = "Terminal 16 Bins", col = rainbow(16), xlab = "Terminals")

par(mfrow=c(2,2))
pub_schools <- filter(college.df, Private == "No", Elite == "No")
pri_schools <- filter(college.df, Private == "Yes", Elite == "No")
pub_elite_schools <- filter(college.df, Elite == "Yes", Private == "No")
pri_elite_schools <- filter(college.df, Elite == "Yes", Private == "Yes")
hist(pub_elite_schools$Accept,prob = TRUE, main = "Acceptance",col = terrain.colors(7), xlab = "Elite Public College")
hist(pub_schools$Accept,prob = TRUE, main = "Acceptance",col = cm.colors(5), xlab = "Not Elite Public College")
hist(pri_elite_schools$Accept,prob = TRUE, main = "Acceptance",col = terrain.colors(8), xlab = "Elite Private College")
hist(pri_schools$Accept,prob = TRUE, main = "Acceptance",col = cm.colors(6), xlab = "Not Elite Private College")

install.packages('ISLR')

library('ISLR')

data('Auto')

lm.fit = lm(mpg~horsepower, data = Auto)
summary(lm.fit)

f <- summary(lm.fit)$fstatistic
p <- pf(f[1],f[2],f[3],lower.tail=F)
p

paste("Varriability: ",summary(lm.fit)$r.squared * 100, " %")
RSE <- summary(lm.fit)$sigma
mean <- getElement(summary(Auto$mpg), 'Mean')
paste("Error: ",RSE/mean * 100, "%")

paste("Coefficient of horsepower: ",summary(lm.fit)$coefficients[2])

print("Confidence Interval")
predict(lm.fit, data.frame(horsepower = 98), interval = "confidence")
print("Prediction Interval")
predict(lm.fit, data.frame(horsepower = 98), interval = "prediction")

plot(Auto$horsepower, Auto$mpg, main = "Least squares regression")
abline(lm.fit, lwd = 3, col = 'red')

par(mfrow = c(2,2))
plot(lm.fit, lwd = 3, col = 'blue') 

library('psych')
pairs(Auto, col = terrain.colors(nrow(Auto)))
pairs.panels(Auto, col = terrain.colors(nrow(Auto)))

cor(Auto[1:8])

lm.fit <- lm(mpg ~ . - name, data = Auto)
summary(lm.fit)

f <- summary(lm.fit)$fstatistic
p <- pf(f[1],f[2],f[3],lower.tail=F)
p

summary(lm.fit)[["coefficients"]][, "t value"]

summary(lm.fit)[["coefficients"]][, "Estimate"]

par(mfrow = c(2, 2))
plot(lm.fit, lwd = 3, col = 'blue') 
