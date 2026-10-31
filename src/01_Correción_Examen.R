library(pacman)
p_load(tidyverse, survey, srvyr, gt)

datos <- read_csv("data/RedesSociales.csv") |> 
  mutate(Sexo = factor(sexo, 1:2, c("Femenino", "Masculino")),
         Edad = factor(grupo_edad, 1:3, c("De 18 a 30 años", "De 31 a 45 años",
                                           "Mayores de 45 años")),
         N = 160,
         pk = 1/150,
         pi = 0.07)

# 1) M.A.S.(160, 12) -----------------------------------------------------------
dis_mas <- datos |> as_survey_design(ids = 1, fpc = N)

dominio_sexo_edad_mas <- dis_mas |> 
  group_by(Sexo, Edad) |> 
  summarise(
    Total = survey_total(horas, vartype = "cv")
  )|> 
  mutate(cve = round(Total_cv * 100, 2))
dominio_sexo_edad_mas

dominio_edad_mas <- dis_mas |> 
  group_by(Edad) |> 
  summarise(
    Total = survey_total(horas, vartype = "cv")
  )|> 
  mutate(cve = round(Total_cv * 100, 2))

dominio_edad_mas

dominio_global_mas <- dis_mas |> 
  summarise(Total = survey_total(horas, vartype = "cv"))|> 
  mutate(cve = round(Total_cv * 100, 2))

dominio_global_mas


# 2) MCR: peso = 1/(m * pk) = 12.5 ----------------------------------------------
datos$fex_mcr <- 1 / (nrow(datos) * datos$pk)
dis_mcr <- datos |> as_survey_design(ids = 1, weights = fex_mcr)   # sin fpc

dominio_sexo_edad_mcr <- dis_mcr |> 
  group_by(Sexo, Edad) |> 
  summarise(Total = survey_total(horas, vartype = "cv"))|> 
  mutate(cve = round(Total_cv * 100, 2))

dominio_sexo_edad_mcr

dominio_edad_mcr <- dis_mcr |> 
  group_by(Edad) |> 
  summarise(Total =survey_total(horas, vartype = "cv")) |> 
  mutate(cve = round(Total_cv * 100, 2))

dominio_edad_mcr

dominio_global_mcr <- dis_mcr |> summarise(Total = survey_total(horas,vartype = "cv"))

dominio_global_mcr

# 3) Bernoulli con pi = 0.07 ----------------------------------------------------
dis_ber <- svydesign(ids = ~1, probs = ~pi, pps = poisson_sampling(datos$pi),
                     data = datos)

dominio_sexo_edad_ber <- svyby(~horas, ~Sexo + Edad, design = dis_ber,
                               FUN = svytotal, vartype = "cv")
dominio_sexo_edad_ber

dominio_edad_ber <- svyby(~horas, ~Edad, design = dis_ber,
                          FUN = svytotal, vartype = "cv")
dominio_edad_ber

dominio_global_ber <- svytotal(~horas, design = dis_ber)
100 * cv(dominio_edad_ber)

# ==============================================================================
# MAQUILLAJE
# ==============================================================================

# 1. TABLA M.A.S. --------------------------------------------------------------------
cuadro_mas <- bind_rows(
  dominio_sexo_edad_mas |> ungroup() |>
    mutate(across(c(Sexo, Edad), as.character)),
  dominio_edad_mas |> ungroup() |>
    mutate(Edad = as.character(Edad), Sexo = "Total"),
  dominio_global_mas |>
    mutate(Sexo = "Gran total", Edad = "")
) |>
  transmute(Sexo, Edad, MAS__total = Total, MAS__cve = 100 * Total_cv)

# 2. TABLA MCR -----------------------------------------------------------------------
cuadro_mcr <- bind_rows(
  dominio_sexo_edad_mcr |> ungroup() |>
    mutate(across(c(Sexo, Edad), as.character)),
  dominio_edad_mcr |> ungroup() |>
    mutate(Edad = as.character(Edad), Sexo = "Total"),
  dominio_global_mcr |>
    mutate(Sexo = "Gran total", Edad = "")
) |>
  transmute(Sexo, Edad, MCR__total = Total, MCR__cve = 100 * Total_cv)

# 3. TABLA Bernoulli -----------------------------------------------------------------
cuadro_ber <- bind_rows(
  dominio_sexo_edad_ber |> as.data.frame() |>
    mutate(across(c(Sexo, Edad), as.character)),
  dominio_edad_ber |> as.data.frame() |>
    mutate(Edad = as.character(Edad), Sexo = "Total"),
  tibble(Sexo = "Gran total", Edad = "",
         horas = as.numeric(coef(dominio_global_ber)),
         cv    = as.numeric(cv(dominio_global_ber)))
) |>
  transmute(Sexo, Edad, BER__total = horas, BER__cve = 100 * cv)

# 4. Unir los tres diseños en un solo cuadro ------------------------------------
cuadro <- cuadro_mas |>
  left_join(cuadro_mcr, by = c("Sexo", "Edad")) |>
  left_join(cuadro_ber, by = c("Sexo", "Edad"))

cuadro   # versión numérica

# 5. Tabla con gt ---------------------------------------------------------------
tabla_salida <- cuadro |>
  gt() |>
  tab_header(
    title    = md("**Cuadro 1. Total de horas en redes sociales según sexo y grupo de edad**"),
    subtitle = "Totales estimados y coeficientes de variación estimados (cve)"
  ) |>
  tab_spanner(label = "M.A.S.(160, 12)",
              columns = c(MAS__total, MAS__cve), id = "sp_mas") |>
  tab_spanner(label = "MCR (pk = 1/150, m = 12)",
              columns = c(MCR__total, MCR__cve), id = "sp_mcr") |>
  tab_spanner(label = "Bernoulli (π = 0.07)",
              columns = c(BER__total, BER__cve), id = "sp_ber") |>
  cols_label(Edad = "Grupo de edad",
             MAS__total = "Total estimado", MAS__cve = "cve (%)",
             MCR__total = "Total estimado", MCR__cve = "cve (%)",
             BER__total = "Total estimado", BER__cve = "cve (%)") |>
  fmt_number(columns = ends_with("__total"), decimals = 2) |>
  fmt_number(columns = ends_with("__cve"),   decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Sexo == "Gran total")) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.align = "left",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "solid",
    table.border.bottom.style = "solid"
  )

tabla_salida

