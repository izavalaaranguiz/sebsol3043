# ==============================================================
# Cambio y estabilidad de valoraciones hacia políticas indígenas
# en Chile, 2016-2023 (ELRI, olas 1 a 4)
#
# Ignacio Zavala Aránguiz - SOL3043 Sociología del Estado de Bienestar
#
#
# Estructura de carpetas esperada:
#   01_input/   BBDD_ELRI_LONG_4.0.dta
#   02_scripts/ este script
#   03_output/  figura y tablas que genera este script
#
# Correr con el directorio de trabajo en 02_scripts/, por ejemplo:
# setwd(".../seb3043/elri_seb/02_scripts")
# ==============================================================

library(tidyverse)
library(haven)
library(lavaan)

carpeta_input  <- "../01_input"
carpeta_output <- "../03_output"
dir.create(carpeta_output, showWarnings = FALSE)


# ==============================================================
# 1. DATOS
# ==============================================================

elri <- read_dta(file.path(carpeta_input, "BBDD_ELRI_LONG_4.0.dta"))

# Módulo E: "Existe una serie de políticas que se han discutido para
# abordar el tema indígena. Según usted, ¿cuán importante es...?"
# (1 = Muy poco, 5 = Mucho)
elri <- elri %>%
  select(folio, ano,
         e4_auton_terri    = e4_2,  # Que puedan administrar autónomamente sus territorios
         e4_escanos        = e4_5,  # Establecer asientos especiales en el congreso
         e4_devolver_terri = e4_4,  # Restituirles o devolverles sus tierras
         e4_mejor_educ     = e4_3,  # Mejorar su educación
         e4_empleo         = e4_1)  # Políticas de generación de empleo

# Orden de los ítems en figuras y tablas: primero las tres políticas
# discutidas en la Convención, luego las dos socioeconómicas
items <- c("e4_auton_terri", "e4_escanos", "e4_devolver_terri",
           "e4_mejor_educ", "e4_empleo")
etiquetas <- c("Autonomía territorial", "Escaños reservados",
               "Restitución de tierras", "Mejorar la educación",
               "Generación de empleo")

# Códigos de no respuesta -> NA
elri <- elri %>%
  mutate(ano = as.numeric(zap_labels(ano))) %>%
  mutate(across(all_of(items), ~ as.numeric(zap_labels(.x)))) %>%
  mutate(across(all_of(items), ~ if_else(.x %in% c(88, 99, 8888, 9999), NA_real_, .x)))

# Tres folios aparecen duplicados en 2023: se eliminan sus filas de esa ola
duplicados <- elri %>% count(folio, ano) %>% filter(n > 1)
elri <- elri %>% filter(!(folio %in% duplicados$folio & ano == 2023))

# Panel balanceado: personas entrevistadas en las cuatro olas
folios_panel <- elri %>%
  group_by(folio) %>%
  summarise(n_olas = n_distinct(ano), .groups = "drop") %>%
  filter(n_olas == 4) %>%
  pull(folio)

elri_panel <- elri %>% filter(folio %in% folios_panel)
stopifnot(n_distinct(elri_panel$folio) == 1589)

# Formato ancho: una fila por persona, una columna por ítem y ola
# (p. ej. e4_escanos_2016, e4_escanos_2018, ...)
elri_ancho <- elri_panel %>%
  pivot_wider(id_cols = folio, names_from = ano,
              values_from = all_of(items), names_sep = "_")


# ==============================================================
# 2. FIGURA 1: MEDIA OBSERVADA POR OLA (IC 95%)
# ==============================================================

medias <- elri_panel %>%
  pivot_longer(all_of(items), names_to = "item", values_to = "y") %>%
  group_by(item, ano) %>%
  summarise(media = mean(y, na.rm = TRUE),
            ee    = sd(y, na.rm = TRUE) / sqrt(sum(!is.na(y))),
            .groups = "drop") %>%
  mutate(ic_inf = media - 1.96 * ee,
         ic_sup = media + 1.96 * ee,
         item   = factor(item, levels = items, labels = etiquetas),
         ola    = factor(ano))

figura_medias <- ggplot(medias, aes(ola, media, group = item)) +
  geom_line(color = "#4A90C2", linewidth = 0.6) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup),
                  color = "#1B3A5C", size = 0.4) +
  facet_wrap(~ item, nrow = 1) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01, decimal.mark = ",")) +
  labs(x = NULL, y = "Media observada (IC 95%)") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

figura_medias
ggsave(file.path(carpeta_output, "fig_medias_por_ola.png"), figura_medias,
       width = 11, height = 3.8, dpi = 300)


# ==============================================================
# 3. ESPECIFICACIÓN DE LOS MODELOS
#    y1, y2, y3, y4 = respuesta en 2016, 2018, 2021 y 2023
# ==============================================================

# --- SDM libre: disposición fija U, intercepto libre por ola ----
sdm_libre <- "
  U =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
  U ~~ U
  y1 ~~ v1*y1
  y2 ~~ v2*y2
  y3 ~~ v3*y3
  y4 ~~ v4*y4
  y1 ~ 1
  y2 ~ 1
  y3 ~ 1
  y4 ~ 1
"

# --- SDM puro: disposición fija U, intercepto constante ---------
sdm_puro <- "
  U =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
  U ~~ U
  y1 ~~ v1*y1
  y2 ~~ v2*y2
  y3 ~~ v3*y3
  y4 ~~ v4*y4
  y1 ~ int*1
  y2 ~ int*1
  y3 ~ int*1
  y4 ~ int*1
"

# --- AUM constante: persistencia e intercepto iguales entre olas -
aum_constante <- "
  y2 ~ alpha*1 + rho*y1
  y3 ~ alpha*1 + rho*y2
  y4 ~ alpha*1 + rho*y3
  y2 ~~ v2*y2
  y3 ~~ v3*y3
  y4 ~~ v4*y4
"

# --- AUM libre: persistencia e intercepto libres por ola --------
aum_libre <- "
  y2 ~ 1 + y1
  y3 ~ 1 + y2
  y4 ~ 1 + y3
  y2 ~~ v2*y2
  y3 ~~ v3*y3
  y4 ~~ v4*y4
"

# --- QSM (Wiley y Wiley 1970): true score eta con error igual ----
# Medición:    y_t   = eta_t + e_t           (var(e_t) igual entre olas)
# Estructural: eta_t = alpha_t + b * eta_t-1 + zeta_t
# Se definen además el nivel de cada ola (mu), sus diferencias, la
# varianza del true score (vt) y la correlación entre olas (r).
qsm <- "
  eta1 =~ 1*y1
  eta2 =~ 1*y2
  eta3 =~ 1*y3
  eta4 =~ 1*y4
  eta2 ~ b21*eta1
  eta3 ~ b32*eta2
  eta4 ~ b43*eta3
  eta1 ~~ z1*eta1
  eta2 ~~ z2*eta2
  eta3 ~~ z3*eta3
  eta4 ~~ z4*eta4
  y1 ~~ e*y1
  y2 ~~ e*y2
  y3 ~~ e*y3
  y4 ~~ e*y4
  y1 ~ 0*1
  y2 ~ 0*1
  y3 ~ 0*1
  y4 ~ 0*1
  eta1 ~ a1*1
  eta2 ~ a2*1
  eta3 ~ a3*1
  eta4 ~ a4*1

  mu_2016 := a1
  mu_2018 := a2 + b21*mu_2016
  mu_2021 := a3 + b32*mu_2018
  mu_2023 := a4 + b43*mu_2021
  dif_2021_2018 := mu_2021 - mu_2018
  dif_2023_2021 := mu_2023 - mu_2021
  dif_2023_2018 := mu_2023 - mu_2018
  dif_2023_2016 := mu_2023 - mu_2016

  vt1 := z1
  vt2 := b21^2*vt1 + z2
  vt3 := b32^2*vt2 + z3
  vt4 := b43^2*vt3 + z4
  r_2016_18 := b21*sqrt(vt1/vt2)
  r_2018_21 := b32*sqrt(vt2/vt3)
  r_2021_23 := b43*sqrt(vt3/vt4)
"

# QSM con la varianza de zeta en 2018 fijada en 0. Se usa solo si el
# QSM libre entrega esa varianza negativa (caso Heywood: ocurre en
# generación de empleo, var(zeta_2018) = -0,001).
qsm_z2_cero <- qsm %>%
  str_replace("eta2 ~~ z2\\*eta2", "eta2 ~~ 0*eta2") %>%
  str_replace("vt2 := b21\\^2\\*vt1 \\+ z2", "vt2 := b21^2*vt1")

modelos <- list("SDM libre" = sdm_libre, "SDM puro" = sdm_puro,
                "AUM constante" = aum_constante, "AUM libre" = aum_libre,
                "QSM" = qsm)


# ==============================================================
# 4. ESTIMACIÓN: 5 MODELOS x 5 ÍTEMS
#    MLR con FIML. fixed.x = FALSE hace que y1 se modele también en
#    los AUM, de modo que los BIC sean comparables entre modelos.
#    Esta función fue generada con ayuda de Claude para correr todos 
#    los modelos a la vez
# ==============================================================

tabla_ajuste <- tibble()
tabla_qsm    <- tibble()

for (i in seq_along(items)) {

  # Datos del ítem: y1 a y4
  datos <- elri_ancho %>% select(starts_with(paste0(items[i], "_20")))
  names(datos) <- c("y1", "y2", "y3", "y4")

  for (m in names(modelos)) {

    ajuste <- sem(modelos[[m]], data = datos, estimator = "MLR",
                  missing = "fiml", meanstructure = TRUE, fixed.x = FALSE)

    # Caso Heywood en el QSM: si var(zeta_2018) < 0, se fija en 0.
    # (El aviso de lavaan "some estimated lv variances are negative"
    # corresponde a este QSM libre de empleo, que luego se reemplaza.)
    if (m == "QSM") {
      pe <- parameterEstimates(ajuste)
      var_zeta <- pe$est[pe$label %in% c("z1", "z2", "z3", "z4")]
      print(paste(etiquetas[i], "- varianzas de zeta del QSM libre:",
                  paste(round(var_zeta, 3), collapse = " / ")))
      if (any(var_zeta < 0)) {
        stopifnot(which(var_zeta < 0) == 2)  # solo previsto para 2018
        ajuste <- sem(qsm_z2_cero, data = datos, estimator = "MLR",
                      missing = "fiml", meanstructure = TRUE, fixed.x = FALSE)
      }
    }

    # Ajuste de cada modelo
    fm <- fitMeasures(ajuste, c("df", "bic", "cfi", "tli", "rmsea", "srmr"))
    tabla_ajuste <- bind_rows(tabla_ajuste, tibble(
      item = etiquetas[i], modelo = m,
      gl = fm[["df"]], bic = fm[["bic"]], cfi = fm[["cfi"]],
      tli = fm[["tli"]], rmsea = fm[["rmsea"]], srmr = fm[["srmr"]]))

    # Coeficientes del QSM: beta (b21, b32, b43) y parámetros definidos
    if (m == "QSM") {
      pe <- parameterEstimates(ajuste)
      coef_qsm <- pe %>%
        filter(label %in% c("b21", "b32", "b43") | op == ":=") %>%
        filter(!lhs %in% c("vt1", "vt2", "vt3", "vt4")) %>%
        transmute(item = etiquetas[i],
                  parametro = if_else(op == ":=", lhs, label),
                  estimacion = est, ee = se)
      tabla_qsm <- bind_rows(tabla_qsm, coef_qsm)
    }
  }
}


# ==============================================================
# 5. TABLAS DE RESULTADOS
# ==============================================================

# --- Tabla de ajuste: menor BIC por ítem marcado -----------------
tabla_ajuste <- tabla_ajuste %>%
  group_by(item) %>%
  ungroup() %>%
  mutate(across(c(cfi, tli, rmsea, srmr), ~ round(.x, 3)),
         bic = round(bic, 1))

print(tabla_ajuste, n = Inf)
write_csv(tabla_ajuste, file.path(carpeta_output, "tabla_ajuste_modelos.csv"))

# --- Tabla QSM: parámetros en filas, políticas en columnas -------
# Formato "estimación (error estándar)", tres decimales. Para r se
# reporta solo la estimación. En empleo, r_2016_18 = 1 es un valor
# de frontera producto de fijar var(zeta_2018) = 0.
orden_parametros <- c("b21", "b32", "b43",
                      "r_2016_18", "r_2018_21", "r_2021_23",
                      "mu_2016", "mu_2018", "mu_2021", "mu_2023",
                      "dif_2021_2018", "dif_2023_2021",
                      "dif_2023_2018", "dif_2023_2016")

tabla_qsm_ancha <- tabla_qsm %>%
  mutate(valor = if_else(str_starts(parametro, "r_"),
                         sprintf("%.3f", estimacion),
                         sprintf("%.3f (%.3f)", estimacion, ee)),
         valor = if_else(str_starts(parametro, "dif_") & abs(estimacion / ee) > 1.96,
                         paste0(valor, "*"), valor),
         parametro = factor(parametro, levels = orden_parametros),
         item = factor(item, levels = etiquetas)) %>%
  select(parametro, item, valor) %>%
  pivot_wider(names_from = item, values_from = valor) %>%
  arrange(parametro)

print(tabla_qsm_ancha, width = Inf)
write_csv(tabla_qsm_ancha, file.path(carpeta_output, "tabla_qsm.csv"))
