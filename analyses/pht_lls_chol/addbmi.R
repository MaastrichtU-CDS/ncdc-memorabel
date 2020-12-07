#Add bmi

data <- read.csv("C:\\Users\\inigo.bermejo\\Documents\\Data\\Memorabel\\LLS2.csv")

data$bmi <- round(data$weight / (data$height/100)^2, 2)

write.csv(data, "C:\\Users\\inigo.bermejo\\Documents\\Data\\Memorabel\\LLS2.csv")
