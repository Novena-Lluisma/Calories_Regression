# Analisis Regresi Linear Berganda

# 1. Load Data
# Import Data
data <- read.csv("calories.csv")

# 2. Data Understanding
#Lihat Data Pertama
head(data)

#Struktur Data
str(data)

# Deskripsi Statistik
summary(data)

# 3. Data Preprocessing
# cek missing value
colSums(is.na(data))

# cek data duplikat
sum(duplicated(data))

# Mengubah gender jadi faktor karena data kategori
data$Gender <- as.factor(data$Gender)

# cek struktur data (ulang)
str(data)

# 4. EDA
# Histogram Calories
hist(data$Calories,
     main = "Histogram Calories",
     xlab = "Calories",
     col = "lightblue",
     border = "black")

# Boxplot calories
boxplot(data$Calories,
        main = "Boxplot Calories",
        ylab = "Calories",
        col = "lightgreen")

# Scatter plot durasi dan kalori
plot(data$Duration, data$Calories,
     main = "Duration vs Calories",
     xlab = "Duration (menit)",
     ylab = "Calories",
     pch = 16,
     col = "blue")

# Scatter plot berat badan dan kalori
plot(data$Weight, data$Calories,
     main = "Weight vs Calories",
     xlab = "Weight (kg)",
     ylab = "Calories",
     pch = 16,
     col = "red")

# Scatter plot heart rate dan kalori
plot(data$Heart_Rate, data$Calories,
     main = "Heart Rate vs Calories",
     xlab = "Heart Rate",
     ylab = "Calories",
     pch = 16,
     col = "darkgreen")

# Korelasi antar variabel numerik
data_numeric <- data[, sapply(data, is.numeric)]
cor(data_numeric)

# 5. Regresi Linear Berganda
# Regresi linear berganda
model <- lm(Calories ~ Gender + Age + Height + Weight +
              Duration + Heart_Rate + Body_Temp,
            data = data)

summary(model)

# 6. Uji Multikolinearitas
#Install package (cukup sekali)
install.packages("car")
options(timeout = 600)
install.packages("car", dependencies = TRUE)
library(car)

vif(model)

# 7. Uji Normalitas Residual
# Plot diagnostik
par(mfrow = c(2,2))
plot(model)


# Uji Heteroskedastisitas (Breusch-Pagan)

install.packages("lmtest")
library(lmtest)
bptest(model)


# 8. Prediksi Model
prediksi <- predict(model)
head(prediksi)


# RMSE
rmse <- sqrt(mean((data$Calories - prediksi)^2))
rmse

# MAE
mae <- mean(abs(data$Calories - prediksi))
mae

# Grafik Prediksi vs Aktual
plot(data$Calories, prediksi,
     xlab = "Nilai Aktual",
     ylab = "Nilai Prediksi",
     main = "Aktual vs Prediksi",
     pch = 16,
     col = "blue")
abline(0, 1, col = "red", lwd = 2)
