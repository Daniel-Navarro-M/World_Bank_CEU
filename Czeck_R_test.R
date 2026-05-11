rm(list = ls())
library(survey)

load("data/processed_data/master/master_processed.RData")
dat <- master_processed

# PI alternative scale: keep item values as 0/1/2 and sum (max 36)
pi_vars <- sprintf("ASBH01%s", LETTERS[1:18])
pi_mat <- as.matrix(dat[, pi_vars])
pi_mat[pi_mat %in% c(998, 999)] <- NA
dat$PI_index_36 <- rowSums(pi_mat, na.rm = TRUE)
all_pi_na <- rowSums(!is.na(pi_mat)) == 0
dat$PI_index_36[all_pi_na] <- NA

years <- c(2019, 2023)

for (yr in years) {
  cat("\n==============================\n")
  cat("YEAR:", yr, "\n")
  cat("==============================\n")

  d <- dat[dat$year == yr & dat$CountryName != "Netherlands", ]
  d$IDCNTRY <- as.numeric(d$IDCNTRY)

  njk <- max(d$JKZONE, na.rm = TRUE)
  for (i in 1:njk) {
    d[[paste0("rwgt_", i)]] <- ifelse(d$JKZONE == i & d$JKREP == 1, 2 * d$TOTWGT,
      ifelse(d$JKZONE == i & d$JKREP == 0, 0, d$TOTWGT))
  }
  rwgt_vars <- grep("^rwgt_[0-9]+$", names(d), value = TRUE)
  ok_design <- complete.cases(d[, c("TOTWGT", "JKZONE", "JKREP", rwgt_vars[1])])
  design <- svrepdesign(
    weights = ~TOTWGT,
    repweights = d[ok_design, rwgt_vars],
    type = "JK2",
    combined.weights = TRUE,
    data = d[ok_design, ]
  )

  countries <- sort(unique(as.character(design$variables$CountryName)))
  countries <- countries[!is.na(countries) & nzchar(countries)]

  out <- data.frame(
    Country = character(0),
    Beta = numeric(0),
    SE = numeric(0),
    p_value = numeric(0),
    stringsAsFactors = FALSE
  )

  for (cn in countries) {
    ds <- design[design$variables$CountryName == cn & !is.na(design$variables$CountryName), ]
    v <- ds$variables
    ok <- complete.cases(v[, c("PI_index_36", "parent_edu_binary")])
    ok <- ok & !(v$PI_index_36 %in% c(998, 999)) & !(v$parent_edu_binary %in% c(998, 999))
    ds2 <- ds[ok, ]
    if (nrow(ds2$variables) < 20) next

    fit <- try(suppressWarnings(svyglm(PI_index_36 ~ parent_edu_binary, design = ds2)), silent = TRUE)
    if (inherits(fit, "try-error")) next

    sm <- summary(fit)$coefficients
    if (!"parent_edu_binary" %in% rownames(sm)) next
    b <- sm["parent_edu_binary", "Estimate"]
    se <- sm["parent_edu_binary", "Std. Error"]
    p <- 2 * pnorm(-abs(b / se))

    out <- rbind(out, data.frame(Country = cn, Beta = b, SE = se, p_value = p, stringsAsFactors = FALSE))
  }

  out <- out[order(out$Country), ]
  print(out, row.names = FALSE)
  cat("\nCountries estimated:", nrow(out), "\n")

  cat("\n--- Czech Republic direct check (by parent_edu_binary) ---\n")
  cz <- d[d$CountryName == "Czech Republic", ]
  cz <- cz[!is.na(cz$parent_edu_binary) & !cz$parent_edu_binary %in% c(998, 999), ]

  grp_vals <- sort(unique(cz$parent_edu_binary))
  for (g in grp_vals) {
    s <- cz[cz$parent_edu_binary == g, ]
    n_all <- nrow(s)

    miss_pi_index <- mean(is.na(s$PI_index) | s$PI_index %in% c(998, 999))
    miss_pi_factor <- mean(is.na(s$PI_factor) | s$PI_factor %in% c(998, 999))
    miss_pi36 <- mean(is.na(s$PI_index_36))

    n_pi_index <- sum(!(is.na(s$PI_index) | s$PI_index %in% c(998, 999)))
    n_pi_factor <- sum(!(is.na(s$PI_factor) | s$PI_factor %in% c(998, 999)))
    n_pi36 <- sum(!is.na(s$PI_index_36))

    m_pi_index <- mean(ifelse(s$PI_index %in% c(998, 999), NA, s$PI_index), na.rm = TRUE)
    m_pi_factor <- mean(ifelse(s$PI_factor %in% c(998, 999), NA, s$PI_factor), na.rm = TRUE)
    m_pi36 <- mean(s$PI_index_36, na.rm = TRUE)

    cat("\nGroup parent_edu_binary =", g, "\n")
    cat("n total =", n_all,
        "| PI_index: n =", n_pi_index, " miss% =", round(100 * miss_pi_index, 2), " mean =", round(m_pi_index, 3),
        "| PI_factor: n =", n_pi_factor, " miss% =", round(100 * miss_pi_factor, 2), " mean =", round(m_pi_factor, 3),
        "| PI_index_36: n =", n_pi36, " miss% =", round(100 * miss_pi36, 2), " mean =", round(m_pi36, 3), "\n")
  }
}

