library(pacman)
p_load(tidyverse)

Universo <- tribble(
  ~"id", ~"nombre", ~"gasto",
      1, "Juan", 30000,
      2, "Catalina", 300000,
      3, "Ricardo", 42000,
      4, "Paula", 150000,
      5, "Stiven", 35000,
      6, "Sebastian", 37000,
      7, "Carolina", 355000,
      8, "Andrea", 180000,
      9, "Duban", 20000,
      10, "Owen", 0,
      11, "Juand", 500,
      12, "Wiston", 22000,
      13, "David", 45000,
      14, "Luisa", 10000,
      15, "Johan", 65000,
)

ty <- sum(Universo$gasto)
ybu <- mean(Universo$gasto) 

N <- nrow(Universo)
n <- 5

(Vmas <- (N^2/n)*(1 - n/N)*var(Universo$gasto)) #Varianza del total

(Vmas_yb <- (1/n)*(1 - n/N)*var(Universo$gasto)) #Varianza de la muestra


df <- data.frame(combn(1:N, n) |> t())



omega <- df |>
  left_join(Universo |> rename(n1 = nombre, y1 = gasto),
            by = c("X1" = "id")) |>
  left_join(Universo |> rename(n2 = nombre, y2 = gasto),
            by = c("X2" = "id")) |>
  left_join(Universo |> rename(n3 = nombre, y3 = gasto),
            by = c("X3" = "id")) |>
  left_join(Universo |> rename(n4 = nombre, y4 = gasto),
            by = c("X4" = "id")) |>
  left_join(Universo |> rename(n5 = nombre, y5 = gasto),
            by = c("X5" = "id")) |>
  relocate(starts_with("X"), starts_with("n"), starts_with("y")) |>
  mutate(ps = 1/3003,
         typ = (N/n)*(y1 + y2 + y3 + y4 + y5),
         yb = typ/N,
         LI_t = typ - 1.96*sqrt(Vmas),
         LS_t = typ + 1.96*sqrt(Vmas),
         LI_yb = yb - 1.96*sqrt(Vmas_yb),
         LS_yb = yb + 1.96*sqrt(Vmas_yb),
         ) |>
  rowwise() |> mutate(In_t = ifelse(between(ty, LI_t, LS_t),1,0),
         In_yb = ifelse(between(ybu, LI_yb, LS_yb),1,0),
         ve_mas_t = (N^2/n)*((1 - n/N)*var(c(y1, y2, y3, y4, y5))),
         LI_e_t = typ - 1.96*sqrt(ve_mas_t),
         LS_e_t = typ - 1.96*sqrt(ve_mas_t),
         IN_e_t = ifelse(between(ty, LI_e_t, LS_e_t), 1, 0)
         )

conf_t <- sum(omega$ps*omega$In_t)
conf_yb <- sum(omega$ps*omega$In_yb)
conf_t <- sum(omega$ps)