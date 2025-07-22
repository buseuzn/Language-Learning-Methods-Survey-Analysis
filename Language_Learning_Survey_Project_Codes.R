data <- read.csv("language.csv")

str(data)

#changing names of the columns

colnames(data) <- c("Date","Age","Gender","Class","Department","Language","Method","App Name","Course Name","Starting Level",
                    "Current Level","Total_Time","Weekly_Time","Efficiency","Skill","Alphabet")
head(data)

#new_Method column

data[,"new_Method"] <- ifelse(data[,"Method"]=="via Apps (i.e. Duolingo, Busuu, Cake, Memrise...)", "App","Course")
data


#new_Levels


vec <- vector()
vec

for (i in 1:length(data[,"Starting Level"])) {
  if(data[,"Starting Level"][i] == "Beginner"){
    vec[i] <- 1
  } else if(data[,"Starting Level"][i] == "Elementary"){
    vec[i] <- 2
  }  else if(data[,"Starting Level"][i] == "Pre-intermediate"){
    vec[i] <- 3
  }  else if(data[,"Starting Level"][i] == "Intermediate"){
    vec[i] <- 4
  }  else if(data[,"Starting Level"][i] == "Upper-intermediate"){
    vec[i] <- 5
  }   else if(data[,"Starting Level"][i] == "Advanced"){
    vec[i] <- 6
  }   else{
    vec[i] <- 7
  } 
}
vec
data[,"New_Starting"] <- vec
head(data)

#current_Levels

vec2 <- vector()
vec2
for (i in 1:length(data[,"Current Level"])) {
  if(data[,"Current Level"][i] == "Beginner"){
    vec2[i] <- 1
    }
  else if(data[,"Current Level"][i] == "Elementary"){
    vec2[i] <- 2
    }
  else if(data[,"Current Level"][i] == "Pre-intermediate"){
    vec2[i] <- 3
    }
  else if(data[,"Current Level"][i] == "Intermediate"){
    vec2[i] <- 4
    }
  else if(data[,"Current Level"][i] == "Upper-intermediate"){
    vec2[i] <- 5
    }
  else if(data[,"Current Level"][i] == "Advanced"){
    vec2[i] <- 6
    }
  else{
    vec2[i] <- 7
  } 
}
vec2
data[,"New_Current"] <- vec2
head(data)

#Progress Score

vec3 <- vector()

for (i in 1:length(data[,"New_Current"])){
  vec3[i] <- data[,"New_Current"][i] - data[,"New_Starting"][i]  
}
vec3

data[,"Progress_Score"] <- vec3
head(data)

str(data)

####################################################################

#alphabet 0/1
# 
# data[,"new_Alphabet"] <- ifelse(data[,"Alphabet"]=="Yes", 1, 0)
# data

#DUMMIES

# data[,"Method_dum"] <- ifelse(data[,"new_Method"]=="Course", 0, 1)
# head(data)
# 
# #DUMMIES for Total Time
# 
# #Install the required package
# install.packages("fastDummies")
# 
# #Load the library
# library(fastDummies)
# 
# #Create dummy variable
# data <- dummy_cols(data, 
#                    select_columns = "Total Time")
# 
# #Print
# print(data)
# 
# data <- data[,-23]
# data
# 
# #DUMMIES for Weekly Time
# 
# #Create dummy variable
# data <- dummy_cols(data, 
#                    select_columns = "Weekly Time")
# 
# #Print
# print(data)
# 
# data <- data[,-29]
# data

########################################################################

#Factorization of variables

Total_Time_levels <- factor(data$Total_Time, levels = c("0-3months","4-7 months","8-11 months","12-15 months",
                                                        "16-19 months", "20-23 months", "2+ years"))

levels(Total_Time_levels)

Weekly_Time_levels <- factor(data$Weekly_Time, levels = c("0-1 hour", "2-3 hours",
                                                          "4-5 hours", "6-7 hours", "8+ hours"))
levels(Weekly_Time_levels)

new_Method_factor <- factor(data$new_Method)


#LINEAR REGRESSION 

a <- lm(Progress_Score~Alphabet+Total_Time+Weekly_Time+new_Method, data=data)
summary(a)


data$Alphabet[which(data$Alphabet == "Don't know")] <- NA

par(mfrow = c(2,2))
plot(a)

par(mfrow = c(2,2))


#GGPLOTS

library(ggplot2)

ggplot(data, aes(x = new_Method_factor, y = Progress_Score)) +
  geom_point() +
  geom_smooth(method = "glm", 
              se = FALSE,
              method.args = list(family = "poisson")) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Method", y = "Progress Score") 

ggplot(data, aes(x = factor(Alphabet), y = Progress_Score)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              col = "#C42126", se = FALSE, linewidth = 1) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Alphabet", y = "Progress Score")

ggplot(data, aes(x = Weekly_Time_levels, y = Progress_Score)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              col = "#C42126", se = FALSE, linewidth = 1) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Weekly Time", y = "Progress Score")

ggplot(data, aes(x = Total_Time_levels, y = Progress_Score)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson),
              col = "#C42126", se = FALSE, linewidth = 1) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Total Time", y = "Progress Score")
#cramer wi



boxplot(Progress_Score~new_Method, data = data, xlab = "Method", ylab = "Progress Score")
boxplot(Progress_Score~factor(Alphabet), data, xlab = "Learning Alphabet", ylab = "Progress Score")
boxplot(Progress_Score~Weekly_Time_levels, data, xlab = "Weekly Time", ylab = "Progress Score")
boxplot(Progress_Score~Total_Time_levels, data, xlab = "Total Time", ylab = "Progress Score")





#HYPOTHESIS TESTING 


var(data$Progress_Score[data$new_Method=="Course"])
var(data$Progress_Score[data$new_Method=="App"])

mean(data$Progress_Score[data$new_Method=="Course"])
mean(data$Progress_Score[data$new_Method=="App"])

t.test(data$Progress_Score~new_Method_factor, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

#GLM W/ POI

summary(m1 <- glm(Progress_Score ~Alphabet+Total_Time+Weekly_Time+new_Method, family="poisson", data=data))
#quasi poisson
summary(m2 <- glm(Progress_Score ~Alphabet+Total_Time_levels+Weekly_Time_levels+factor(new_Method), family="poisson", data=data))


class(data$Total_Time)

#over dispersion problem 



table()

round(exp(summary(m2)$coefficients[,1]),4)

