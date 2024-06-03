# Definirea functiei de calculare volumului torului circular
calculate_torus_volume <- function(R, r) {
  return(2 * pi^2 * R * r^2)
}

# Definirea functiei de estimare a volumului torului circular folosind metoda Monte Carlo
monte_carlo_torus_volume <- function(R, r, n) {
  # Generarea de puncte aleatorii în interiorul cubului care încadrează torul
  x1 <- runif(n, min = -R - r, max = R + r)
  x2 <- runif(n, min = -R - r, max = R + r)
  x3 <- runif(n, min = -r, max = r)
  
  # Calcularea numărului de puncte care cad în interiorul torului
  points_inside <- sum((x3^2 + (sqrt(x1^2 + x2^2) - R)^2) < r^2)
  
  # Calcularea volumului cubului care încadrează torul
  cube_volume <- (2 * (R + r))^2 * (2 * r)
  
  # Estimarea volumului torului
  torus_volume_estimate <- points_inside / n * cube_volume
  
  return(torus_volume_estimate)
}

# Parametrii
R <- 10
r <- 3
sample_sizes <- c(10000, 20000, 50000)

# Calcularea volumului torului folosind formula exactă
exact_volume <- calculate_torus_volume(R, r)

# Estimarea volumului torului folosind metoda Monte Carlo pentru diferite dimensiuni de eșantion
for (n in sample_sizes) {
  volume_estimate <- monte_carlo_torus_volume(R, r, n)
  relative_error <- abs((volume_estimate - exact_volume) / exact_volume) * 100
  print(paste("Dimensiune eșantion:", n))
  print(paste("Estimare volum:", volume_estimate))
  print(paste("Eroare relativă (%):", relative_error))
}
# Determinarea intervalului pentru x
x_min <- 0  # valoarea minimă pentru x
x_max <- 2  # valoarea maximă pentru x (intersecția între y = 2x și y = 6 - 3x)

# Determinarea intervalului pentru y
y_min <- 0  # valoarea minimă pentru y
y_max <- 6 - 3 * x_min  # valoarea maximă pentru y (intersecția între y = 2x și y = 6 - 3x)

# Calculul ariei zonei rectangulară care include toate punctele interioare ale triunghiului
area_rectangle <- (x_max - x_min) * (y_max - y_min)

# Funcția pentru verificarea dacă un punct este în interiorul triunghiului
is_inside_triangle <- function(x, y) {
  return(y > 0 & y <= 2 * x & y < 6 - 3 * x)
}

# Funcția pentru estimarea ariei triunghiului folosind metoda Monte Carlo
monte_carlo_triangle_area <- function(n, x_min, x_max, y_min, y_max) {
  # Generarea de puncte aleatoare uniform distribuite în interiorul zonei rectangulare
  x <- runif(n, min = x_min, max = x_max)
  y <- runif(n, min = y_min, max = y_max)
  
  # Calcularea numărului de puncte care cad în interiorul triunghiului
  points_inside <- sum(is_inside_triangle(x, y))
  
  # Estimarea ariei triunghiului
  triangle_area_estimate <- points_inside / n * (x_max - x_min) * (y_max - y_min)
  
  return(triangle_area_estimate)
}

# Parametrii pentru metoda Monte Carlo
n <- 20000  # dimensiunea esantionului

# Estimarea ariei triunghiului folosind metoda Monte Carlo
triangle_area_estimate <- monte_carlo_triangle_area(n, x_min, x_max, y_min, y_max)

# Afișarea rezultatului
print(paste("Aria triunghiului estimată cu metoda Monte Carlo:", triangle_area_estimate))
# Definirea funcțiilor pentru integrale
f1 <- function(x) {
  return((2 * x - 1) / (x^2 - x - 6))
}

f2 <- function(x) {
  return((x + 4) / (abs(x - 3)^(1/3)))
}

f3 <- function(x) {
  return(x * exp(-x^2))
}

# Calculul integralelor folosind metoda numerică
integral_result1 <- integrate(f1, lower = -1, upper = 1)$value
integral_result2 <- integrate(f2, lower = 3, upper = 11)$value
integral_result3 <- integrate(f3, lower = 0, upper = Inf)$value

# Afișarea rezultatelor
print(paste("Integrala a) estimată:", integral_result1))
print(paste("Integrala a) exactă (ln(3) - ln(2)):", log(3) - log(2)))
print(paste("Integrala b) estimată:", integral_result2))
print(paste("Integrala b) exactă (61.2):", 61.2))
print(paste("Integrala c) estimată:", integral_result3))
print(paste("Integrala c) exactă (1/2):", 1/2))
# Parametrii
n <- 1000 #Numarul de evenimente posibile
p <- 0.25 #Probabilitatea de succes
q <- 0.01 #Probabilitatea de esec
target_users <- 15000
years <- 40 + 10/12  # 40 ani și 10 luni

# (a) Estimarea numărului mediu de ani necesari pentru a ajunge la cel puțin 15000 de utilizatori
mean_years <- (target_users - 10000) / (n * p)
print(paste("Numărul mediu de ani necesari:", mean_years))

# (b) Estimarea probabilității de a avea cel puțin 15000 de utilizatori după 40 de ani și 10 luni
prob_15000_users <- pbinom(target_users - 1, size = round(years * n), prob = p)
print(paste("Probabilitatea de a avea cel puțin 15000 de utilizatori:", prob_15000_users))

# (c) Estimarea probabilității cu o eroare de maxim 0.01 și o probabilitate de încredere de 0.99
library(binom)
prob_interval <- binom.wilson(target_users, years * n, conf.level = 0.99, width = 0.01)
print(paste("Probabilitatea cu eroarea maximă de 0.01:", prob_interval$conf.int))


