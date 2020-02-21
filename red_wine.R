red <- read.table(file="~/Documents/Wine/data_red.csv", sep = ",", header = TRUE)
names(red) <- c("Acid_Fixed", "Acid_Volatile", "Acid_Citric", "Sugar", "Cholrides", "Sulfur_Free", "Sulfur_Total", "Density", "PH", "Sulphates", "Alchohol", "Quality")

model <- lm(Quality ~ Acid_Fixed + Acid_Volatile + Acid_Citric + Sugar + Cholrides 
      + Sulfur_Free + Sulfur_Total + Density + PH + Sulphates + Alchohol, data = red)
summary(model)

# Backwards Model ------------------------------
backwards_model <- step(model, direction="backward")
summary(backwards_model)

vifs <- car::vif(backwards_model)
print(vifs)

# Forwards Model ------------------------------
forwards_model <- step(model, direction="forward")
summary(forwards_model)

vifs <- car::vif(forwards_model)
print(vifs)

# Both Model ------------------------------
both_model <- step(model, direction="both")
summary(both_model)

vifs <- car::vif(both_model)
print(vifs)

# Backwards and Both model are same
# New Model taking into consideration VIFS (didn't change anything) -------------
# https://www.statisticshowto.datasciencecentral.com/variance-inflation-factor
model <- lm(Quality ~ Acid_Volatile + Cholrides + PH + Sulphates + Alchohol, data = red)
summary(model)

vifs <- car::vif(model)
print(vifs)

cov_matrix <- cor(red)
cov_df <- as.data.frame(cov_matrix)

write.csv(round(cov_df, 2),"~/Documents/Wine/cov_matrix_red.csv", row.names = TRUE)


cor(red)

model <- lm(Quality ~ Alchohol, data=red)
summary(model)





