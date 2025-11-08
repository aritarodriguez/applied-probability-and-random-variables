# SCRIPT R: FUNDAMENTOS DE PROBABILIDAD Y VARIABLES ALEATORIAS
# Certificación en Data Science Aplicado a la Gestión Empresarial

# =============================================================================
# 1. CONFIGURACIÓN INICIAL
# =============================================================================

# Limpiar entorno de trabajo
rm(list = ls())

# Instalar paquetes si no están instalados
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gridExtra")) install.packages("gridExtra")

# Cargar librerías
library(ggplot2)
library(dplyr)
library(gridExtra)

# Configurar semilla para reproducibilidad
set.seed(123)

# =============================================================================
# 2. EXPERIMENTOS ALEATORIOS - LANZAMIENTO DE DADO
# =============================================================================

cat("=== EXPERIMENTO ALEATORIO: LANZAMIENTO DE DADO ===\n\n")


# Definir espacio muestral
espacio_muestral <- 1:6
cat("Espacio muestral E =", espacio_muestral, "\n")

# Simular 1000 lanzamientos de dado
lanzamientos <- sample(espacio_muestral, 1000, replace = TRUE)
lanzamientos
# Frecuencias absolutas
frecuencias <- table(lanzamientos)
cat("\nFrecuencias absolutas de 1000 lanzamientos:\n")
print(frecuencias)

# Probabilidades empíricas
prob_empirica <- frecuencias / length(lanzamientos)
cat("\nProbabilidades empíricas:\n")
print(round(prob_empirica, 3))

# Probabilidades teóricas (dado justo)
prob_teorica <- rep(1/6, 6)
cat("\nProbabilidades teóricas (dado justo):\n")
print(round(prob_teorica, 3))



# Simular 10,000 lanzamientos de dado
lanzamientos <- sample(espacio_muestral, 10000, replace = TRUE)
lanzamientos
# Frecuencias absolutas
frecuencias <- table(lanzamientos)
cat("\nFrecuencias absolutas de 10,000 lanzamientos:\n")
print(frecuencias)

# Probabilidades empíricas
prob_empirica <- frecuencias / length(lanzamientos)
cat("\nProbabilidades empíricas:\n")
print(round(prob_empirica, 3))

# Probabilidades teóricas (dado justo)
prob_teorica <- rep(1/6, 6)
cat("\nProbabilidades teóricas (dado justo):\n")
print(round(prob_teorica, 3))


# =============================================================================
# 3. SUCESOS ALEATORIOS - OPERACIONES CON CONJUNTOS
# =============================================================================

cat("\n=== SUCESOS ALEATORIOS Y OPERACIONES ===\n\n")

# Definir sucesos
A <- c(2, 4, 6)  # Números pares
B <- c(1, 2, 3)  # Números menores o iguales a 3
C <- c(5, 6)     # Números mayores que 4

cat("Suceso A (pares):", A, "\n")
cat("Suceso B (≤ 3):", B, "\n")
cat("Suceso C (> 4):", C, "\n")

# Operaciones con sucesos
union_AB <- union(A, B)
interseccion_AB <- intersect(A, B)
complemento_A <- setdiff(espacio_muestral, A)
diferencia_AB <- setdiff(A, B)

cat("\n--- Operaciones con sucesos ---\n")
cat("A ∪ B:", union_AB, "\n")
cat("A ∩ B:", interseccion_AB, "\n")
cat("Complemento de A (Ā):", complemento_A, "\n")
cat("Diferencia A - B:", diferencia_AB, "\n")

# Sucesos especiales
cat("\n--- Sucesos especiales ---\n")
cat("Suceso seguro E:", espacio_muestral, "\n")
cat("Suceso imposible ∅:", numeric(0), "\n")

# =============================================================================
# 4. CÁLCULO DE PROBABILIDADES
# =============================================================================

cat("\n=== CÁLCULO DE PROBABILIDADES ===\n\n")

# Función para calcular probabilidad de un suceso
calcular_probabilidad <- function(suceso, espacio) {
  return(length(suceso) / length(espacio))
}

# Probabilidades teóricas
cat("Probabilidades teóricas:\n")
cat("P(A) = P(pares) =", calcular_probabilidad(A, espacio_muestral), "\n")
cat("P(B) = P(≤3) =", calcular_probabilidad(B, espacio_muestral), "\n")
cat("P(C) = P(>4) =", calcular_probabilidad(C, espacio_muestral), "\n")
cat("P(A ∩ B) =", calcular_probabilidad(interseccion_AB, espacio_muestral), "\n")
cat("P(A ∪ B) =", calcular_probabilidad(union_AB, espacio_muestral), "\n")
cat("P(Ā) =", calcular_probabilidad(complemento_A, espacio_muestral), "\n")

# Verificación axiomas de probabilidad
cat("\n--- Verificación axiomas de probabilidad ---\n")
cat("P(E) =", calcular_probabilidad(espacio_muestral, espacio_muestral), "(debe ser 1)\n")
cat("P(∅) =", calcular_probabilidad(numeric(0), espacio_muestral), "(debe ser 0)\n")

# =============================================================================
# 5. VARIABLES ALEATORIAS DISCRETAS
# =============================================================================

cat("\n=== VARIABLES ALEATORIAS DISCRETAS ===\n\n")

# Variable aleatoria: Ganancia en juego de dados
# Si sale par: ganas 10€, si sale impar: pierdes 5€

ganancia <- ifelse(lanzamientos %% 2 == 0, 10, -5)

# Distribución de probabilidad
valores_ganancia <- c(-5, 10)
prob_ganancia <- c(
  sum(lanzamientos %% 2 == 1) / length(lanzamientos),
  sum(lanzamientos %% 2 == 0) / length(lanzamientos)
)

distribucion_ganancia <- data.frame(
  Ganancia = valores_ganancia,
  Probabilidad = prob_ganancia
)

cat("Distribución de probabilidad - Ganancia:\n")
print(distribucion_ganancia)

# Valor esperado (esperanza matemática)
valor_esperado <- sum(valores_ganancia * prob_ganancia)
cat("\nValor esperado E[X] =", round(valor_esperado, 2), "€\n")

# Varianza y desviación estándar
varianza <- sum((valores_ganancia - valor_esperado)^2 * prob_ganancia)
desviacion_estandar <- sqrt(varianza)
cat("Varianza Var[X] =", round(varianza, 2), "\n")
cat("Desviación estándar σ =", round(desviacion_estandar, 2), "\n")

# =============================================================================
# 6. SIMULACIÓN DE EXPERIMENTOS DEL MUNDO REAL
# =============================================================================

cat("\n=== SIMULACIÓN DE EXPERIMENTOS DEL MUNDO REAL ===\n\n")

# Simulación 1: Control de calidad en producción
cat("1. CONTROL DE CALIDAD EN PRODUCCIÓN\n")

# Supongamos que el 5% de las piezas son defectuosas
piezas_producidas <- 1000
defectuosas <- rbinom(1, piezas_producidas, 0.05)
cat("Piezas producidas:", piezas_producidas, "\n")
cat("Piezas defectuosas:", defectuosas, "\n")
cat("Tasa de defectos:", round(defectuosas/piezas_producidas, 3), "\n")

# Simulación 2: Demanda diaria de un producto
cat("\n2. DEMANDA DIARIA DE UN PRODUCTO\n")

# Demanda sigue distribución Poisson con lambda = 50
demanda_diaria <- rpois(30, lambda = 50)  # 30 días
cat("Demanda diaria (30 días):\n")
print(demanda_diaria)
cat("Demanda promedio:", round(mean(demanda_diaria), 1), "\n")

# Simulación 3: Intención de voto
cat("\n3. INTENCIÓN DE VOTO\n")

# Tres partidos: A, B, C con probabilidades 0.4, 0.35, 0.25
partidos <- c("A", "B", "C")
prob_voto <- c(0.4, 0.35, 0.25)
encuesta <- sample(partidos, 1000, replace = TRUE, prob = prob_voto)

cat("Resultados encuesta (1000 personas):\n")
print(table(encuesta))
cat("Proporciones:\n")
print(round(table(encuesta)/1000, 3))

# =============================================================================
# 7. VISUALIZACIONES
# =============================================================================

# Gráfico 1: Distribución de lanzamientos de dado
g1 <- ggplot(data.frame(Lanzamientos = lanzamientos[1:100]), 
             aes(x = factor(Lanzamientos))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribución de 100 Lanzamientos de Dado",
       x = "Resultado", y = "Frecuencia") +
  theme_minimal()

# Gráfico 2: Distribución de ganancias
g2 <- ggplot(data.frame(Ganancia = ganancia), aes(x = factor(Ganancia))) +
  geom_bar(fill = "darkorange", alpha = 0.7) +
  labs(title = "Distribución de Ganancias en Juego de Dados",
       x = "Ganancia (€)", y = "Frecuencia") +
  theme_minimal()

# Gráfico 3: Demanda diaria
g3 <- ggplot(data.frame(Dia = 1:30, Demanda = demanda_diaria), 
             aes(x = Dia, y = Demanda)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  geom_hline(yintercept = mean(demanda_diaria), linetype = "dashed", color = "red") +
  labs(title = "Demanda Diaria de Producto (30 días)",
       x = "Día", y = "Demanda") +
  theme_minimal()

# Mostrar gráficos
grid.arrange(g1, g2, g3, ncol = 2)

# =============================================================================
# 8. EJERCICIOS PRÁCTICOS Y APLICACIONES
# =============================================================================

cat("\n=== EJERCICIOS PRÁCTICOS ===\n\n")

# Ejercicio 1: Probabilidad condicional
cat("Ejercicio 1: Probabilidad condicional\n")
cat("P(A|B) = P(A ∩ B) / P(B) =", 
    round(calcular_probabilidad(interseccion_AB, espacio_muestral) / 
            calcular_probabilidad(B, espacio_muestral), 3), "\n")

# Ejercicio 2: Independencia de sucesos
cat("\nEjercicio 2: Independencia de sucesos\n")
independientes <- (calcular_probabilidad(interseccion_AB, espacio_muestral) == 
                     calcular_probabilidad(A, espacio_muestral) * 
                     calcular_probabilidad(B, espacio_muestral))
cat("¿Son A y B independientes?", independientes, "\n")

# Ejercicio 3: Ley de los grandes números
cat("\nEjercicio 3: Ley de los grandes números\n")
prob_empirica_10 <- table(sample(espacio_muestral, 10, replace = TRUE)) / 10
prob_empirica_1000 <- table(sample(espacio_muestral, 1000, replace = TRUE)) / 1000

cat("Probabilidad con 10 lanzamientos:\n")
print(round(prob_empirica_10, 2))
cat("Probabilidad con 1000 lanzamientos:\n")
print(round(prob_empirica_1000, 3))
cat("Las probabilidades empíricas se acercan a las teóricas (0.167) con más lanzamientos\n")

# =============================================================================
# 9. RESUMEN CONCEPTUAL
# =============================================================================

cat("\n=== RESUMEN CONCEPTUAL ===\n\n")
cat("✓ Espacio muestral: Conjunto de todos los resultados posibles\n")
cat("✓ Suceso: Cualquier subconjunto del espacio muestral\n")
cat("✓ Probabilidad: Medida de la incertidumbre de un suceso\n")
cat("✓ Variable aleatoria: Función que asigna valores numéricos a los resultados\n")
cat("✓ Valor esperado: Promedio ponderado de los valores posibles\n")
cat("✓ Axiomas de probabilidad: P(A) ≥ 0, P(E) = 1, P(A∪B) = P(A) + P(B) si A∩B=∅\n")

cat("\n=== SIMULACIONES COMPLETADAS ===\n")
cat("Se han practicado los conceptos de:\n")
cat("- Experimentos aleatorios y espacio muestral\n")
cat("- Sucesos y operaciones entre conjuntos\n")
cat("- Cálculo de probabilidades teóricas y empíricas\n")
cat("- Variables aleatorias discretas\n")
cat("- Simulación de escenarios del mundo real\n")
cat("- Visualización de distribuciones de probabilidad\n")

