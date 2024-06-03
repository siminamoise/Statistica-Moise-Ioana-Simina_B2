
# Citirea datelor din fișierul probabilitati.csv
probabilitati <- read.csv("probabilitati.csv")

# Calculul dimensiunii eșantionului
n <- length(probabilitati$note)

# Calculul mediei și deviației standard
mean_prob <- mean(probabilitati$note)
sd_prob <- sqrt(92.16)

# Determinarea cuantilelor pentru nivelurile de semnificație 0.05 și 0.01
t_95 <- qt(0.975, df = n - 1)
t_99 <- qt(0.995, df = n - 1)

# Calculul intervalului de încredere pentru media la nivelul de semnificație 95%
interval_95 <- c(mean_prob - t_95 * (sd_prob / sqrt(n)), mean_prob + t_95 * (sd_prob / sqrt(n)))

# Calculul intervalului de încredere pentru media la nivelul de semnificație 99%
interval_99 <- c(mean_prob - t_99 * (sd_prob / sqrt(n)), mean_prob + t_99 * (sd_prob / sqrt(n)))

# Afișarea rezultatelor
print(paste("Intervalul de încredere de 95% pentru media la Probabilități:", interval_95))
print(paste("Intervalul de încredere de 99% pentru media la Probabilități:", interval_99))
# Citirea datelor din fișierul statistica.csv
probabilitati <- read.csv("statistica.csv")

# Determinarea cuantilelor pentru nivelurile de semnificație 0.05 și 0.01
Z_95 <- qnorm(0.975)
Z_99 <- qnorm(0.995)

# Calculul intervalului de încredere pentru media la nivelul de semnificație 95%
interval_95_stat <- c(mean_prob - Z_95 * (sd_stat / sqrt(n)), mean_prob + Z_95 * (sd_stat / sqrt(n)))

# Calculul intervalului de încredere pentru media la nivelul de semnificație 99%
interval_99_stat <- c(mean_prob - Z_99 * (sd_stat / sqrt(n)), mean_prob + Z_99 * (sd_stat / sqrt(n)))

# Afișarea rezultatelor
print(paste("Intervalul de încredere de 95% pentru media la Statistică:", interval_95_stat))
print(paste("Intervalul de încredere de 99% pentru media la Statistică:", interval_99_stat))
# Numărul total de studenți și numărul studenților care nu pot rezolva temele după schimbare
n_total <- 100
n_failed <- 14

# Media și deviația standard a eșantionului
x_bar <- n_total - n_failed
s <- sqrt(x_bar * (1 - 0.85))

# Calculul valorii t pentru nivelul de semnificație 1%
t_1 <- qt(0.995, df = n_total - 1)

# Calculul valorii t pentru nivelul de semnificație 5%
t_5 <- qt(0.975, df = n_total - 1)

# Calculul intervalului de încredere la nivelul de semnificație 1%
interval_1 <- c(x_bar - t_1 * (s / sqrt(n_total)), x_bar + t_1 * (s / sqrt(n_total)))

# Calculul intervalului de încredere la nivelul de semnificație 5%
interval_5 <- c(x_bar - t_5 * (s / sqrt(n_total)), x_bar + t_5 * (s / sqrt(n_total)))

# Afișarea rezultatelor
print(paste("Intervalul de încredere de 1% pentru procentul de studenți care nu pot rezolva temele:", interval_1))
print(paste("Intervalul de încredere de 5% pentru procentul de studenți care nu pot rezolva temele:", interval_5))


