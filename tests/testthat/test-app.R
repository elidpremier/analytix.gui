library(testthat)

test_that("Normalisation des noms de colonnes fonctionne", {
  # Simuler la logique de nettoyage de mod_import.R
  clean_cols <- function(names) {
    names <- gsub("[^a-zA-Z0-9_]", "_", names)
    names <- gsub("_+", "_", names)
    names <- gsub("^_|_$", "", names)
    names <- tolower(names)
    names
  }

  expect_equal(clean_cols("Age (Years)"), "age_years")
  expect_equal(clean_cols("Sexe / Genre"), "sexe_genre")
  expect_equal(clean_cols("__Test__"), "test")
})

test_that("Diagnostic des valeurs manquantes est correct", {
  df <- data.frame(
    A = c(1, 2, NA),
    B = c(NA, NA, NA),
    C = c(1, 2, 3)
  )

  na_counts <- sapply(df, function(x) sum(is.na(x)))
  na_pct <- round((na_counts / nrow(df)) * 100, 1)

  expect_equal(unname(na_counts), c(1, 3, 0))
  expect_equal(unname(na_pct), c(33.3, 100.0, 0.0))
})

test_that("Détection des outliers (IQR fallback) est robuste", {
  vec <- c(1, 10, 11, 12, 13, 14, 15, 50, NA)

  q1 <- quantile(vec, 0.25, na.rm = TRUE)
  q3 <- quantile(vec, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  outliers <- vec[vec < (q1 - 1.5 * iqr) | vec > (q3 + 1.5 * iqr)]
  outliers <- na.omit(outliers)

  expect_true(1 %in% outliers)
  expect_true(50 %in% outliers)
  expect_false(12 %in% outliers)
})
