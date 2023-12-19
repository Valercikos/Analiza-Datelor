library(readxl)

data <- read_excel('2023 September data.xlsx')

# Eliminarea rândurilor cu valori lipsă
cleaned_data <- na.omit(data)

# Afișarea datelor curățate
print(cleaned_data)

# Conversia coloanei "Price" în tip numeric
data$Price <- as.numeric(data$Price)

# Verificarea dacă coloana "Price" este numerică
if (is.numeric(data$Price)) {
  # Calcularea mediei
  medie_pret <- mean(data$Price, na.rm = TRUE)
  print(paste("Media din categoria 'Price' este:", medie_pret))
} else {
  print("Coloana 'Price' nu este numerică.")
}
library(ggplot2)

# Crearea diagramă de distribuție a tipurilor de vânzători
ggplot(data, aes(x = `Seller Type`, fill = `Seller Type`)) +
  geom_bar() +
  labs(title = "Distribuția Tipurilor de Vânzători",
       x = "Tipul de Vânzător",
       y = "Număr de Mașini")

# Crearea unei palete de culori mai intense
culoare_intensa <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")

# Crearea diagramă de bare empilate cu culori intense și contururi mai groase
ggplot(data, aes(x = Transmission, fill = Transmission)) +
  geom_bar(stat = "count", position = "stack", color = "black", size = 1.5) +
  scale_fill_manual(values = culoare_intensa) +
  labs(title = "Distribuția Tipurilor de Transmisie",
       x = "Tipul de Transmisie",
       y = "Număr de Mașini") +
  theme_minimal()

# Crearea unui grafic liniar (bar chart) pentru variabila Condition și Warranty
ggplot(data, aes(x = Condition, fill = Warranty)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Distribuția între Conditia masinii și Garantia",
       x = "Condition",
       y = "Count",
       fill = "Warranty") +
  theme_minimal()

# Crearea unui scatterplot pentru relația dintre Car Make și Price
ggplot(data, aes(x = `Car Make`, y = Price)) +
  geom_point() +
  labs(title = "Relația dintre Car Make și Price",
       x = "Car Make",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Ajustarea unui model de regresie liniară
model <- lm(Price ~ `Car Make` + Condition + `Year Manufactured` + Transmission + `Engine Capacity` + Warranty + Mileage, data = data)

# Calcularea eroarei standard reziduale (RSE)
residuals <- resid(model)
rse <- sqrt(sum(residuals^2) / (length(residuals) - ncol(model$X)))

# Calcularea R-squared
rsquared <- summary(model)$r.squared

# Afișarea rezultatelor
cat("Eroarea Standard Reziduală (RSE):", round(rse, 3), "\n")
cat("R-squared (R²):", round(rsquared, 3), "\n")

# Încărcarea pachetului glmnet
library(glmnet)

# Verificarea tipului de date al coloanei Price
str(data$Price)

# Conversia coloanei Price în format numeric și gestionarea valorilor nevalide
data$Price <- as.numeric(gsub("[^0-9.]", "", data$Price))
if (any(is.na(data$Price))) {
  stop("Există valori nevalide în coloana Price.")
}


# Definirea matricelor X și Y
X <- as.matrix(data[, -1])  # Excludem coloana Price
Y <- data$Price

# Normalizarea datelor (standardizare)
X <- scale(X, center = TRUE, scale = TRUE)


# Setarea unei secvențe de valori lambda pentru Ridge și Lasso
lambda_seq <- 10^seq(10, -2, length = 100)

# Regresie Ridge
ridge_model <- cv.glmnet(X, Y, alpha = 0, lambda = lambda_seq)
plot(ridge_model)

# Regresie Lasso
lasso_model <- cv.glmnet(X, Y, alpha = 1, lambda = lambda_seq)
plot(lasso_model)



