# CS422 Data Mining Section 01
# Jithin Joyson
# Illinois Institute of Technology

# Problem 2

# 2.1

# 1-A
attach(cars)
print(cars)

# 1-B
plot(cars, main = "The Cars Dataset")

# 1-C
summary(cars)

# 1-D
print('The maximum speed is 25.0')
print('The minimum distance is 2.00')

# 2.2

# 2-A
df <- read.csv("student.csv", sep=";", header=T)

temp <- data.frame(name = 'Brad Pitt', id = 40051, gpa = 2.21, grade = 'C')
df <- rbind(df,temp)
df