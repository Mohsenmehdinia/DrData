# interaction_helpers.R
# Helper functions for professional interaction effects analysis

# ── 1. Detect Interaction Terms ──────────────────────────────────────────────
detect_interaction_terms <- function(model) {
  coefs     <- summary(model)$coefficients
  all_terms <- rownames(coefs)
  int_terms <- grep(":", all_terms, value = TRUE)
  
  if (length(int_terms) == 0) return(NULL)
  
  df <- data.frame(
    interaction_term     = int_terms,
    estimate             = coefs[int_terms, "Estimate"],
    std_error            = coefs[int_terms, "Std. Error"],
    t_value              = coefs[int_terms, "t value"],
    p_value              = coefs[int_terms, "Pr(>|t|)"],
    stringsAsFactors     = FALSE,
    row.names            = NULL
  )
  df$significance <- ifelse(df$p_value < 0.001, "***",
                            ifelse(df$p_value < 0.01,  "**",
                                   ifelse(df$p_value < 0.05,  "*",
                                          ifelse(df$p_value < 0.1,   ".", ""))))
  df$order <- vapply(df$interaction_term, function(t) length(strsplit(t, ":")[[1]]), integer(1L))
  df
}

# ── 2. Build Base Model (main effects only) ───────────────────────────────────
build_base_model <- function(model, data) {
  tt       <- terms(model)
  response <- as.character(formula(model)[[2]])
  # فقط main effects (order == 1)
  main_terms <- attr(tt, "term.labels")[attr(tt, "order") == 1]
  if (length(main_terms) == 0) return(NULL)
  base_f <- as.formula(paste(response, "~", paste(main_terms, collapse = " + ")))
  lm(base_f, data = data)
}

# ── 3. Compare Models ─────────────────────────────────────────────────────────
compare_interaction_models <- function(base_model, full_model) {
  if (is.null(base_model) || is.null(full_model)) return(NULL)
  
  s_base <- summary(base_model)
  s_full <- summary(full_model)
  
  r2_base <- s_base$r.squared
  r2_full <- s_full$r.squared
  
  anova_res <- tryCatch(anova(base_model, full_model), error = function(e) NULL)
  f_stat <- if (!is.null(anova_res)) anova_res$F[2]      else NA
  p_val  <- if (!is.null(anova_res)) anova_res$`Pr(>F)`[2] else NA
  
  data.frame(
    Model        = c("Base (Main Effects)", "Full (With Interactions)"),
    R2           = round(c(r2_base, r2_full), 4),
    Adj_R2       = round(c(s_base$adj.r.squared, s_full$adj.r.squared), 4),
    AIC          = round(c(AIC(base_model), AIC(full_model)), 2),
    BIC          = round(c(BIC(base_model), BIC(full_model)), 2),
    Delta_R2     = c(NA, round(r2_full - r2_base, 4)),
    F_change     = c(NA, round(f_stat, 3)),
    p_F_change   = c(NA, round(p_val, 4)),
    stringsAsFactors = FALSE
  )
}

# ── 4. Simple Slopes Analysis ─────────────────────────────────────────────────
simple_slopes_analysis <- function(model, predictor, moderator, data) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    return(data.frame(Note = "Package 'emmeans' required. Run: install.packages('emmeans')"))
  }
  
  tryCatch({
    m_mod  <- mean(data[[moderator]], na.rm = TRUE)
    sd_mod <- sd(data[[moderator]],   na.rm = TRUE)
    
    mod_levels <- c(m_mod - sd_mod, m_mod, m_mod + sd_mod)
    mod_labels <- c("Low (-1 SD)", "Mean", "High (+1 SD)")
    
    results <- lapply(seq_along(mod_levels), function(i) {
      # داده با moderator ثابت
      d_tmp           <- data
      d_tmp[[moderator]] <- mod_levels[i]
      
      # slope = تغییر predicted به ازای تغییر واحد predictor
      d_hi            <- d_tmp
      d_lo            <- d_tmp
      d_hi[[predictor]] <- d_tmp[[predictor]] + 1
      d_lo[[predictor]] <- d_tmp[[predictor]]
      
      pred_hi <- predict(model, newdata = d_hi)
      pred_lo <- predict(model, newdata = d_lo)
      slope   <- mean(pred_hi - pred_lo, na.rm = TRUE)
      
      # SE از coef matrix
      coef_name <- paste0(predictor, ":", moderator)
      coef_alt  <- paste0(moderator, ":", predictor)
      coefs_mat <- summary(model)$coefficients
      
      b1  <- if (predictor %in% rownames(coefs_mat)) coefs_mat[predictor, "Estimate"] else 0
      b3  <- if (coef_name %in% rownames(coefs_mat)) coefs_mat[coef_name, "Estimate"]
      else if (coef_alt %in% rownames(coefs_mat)) coefs_mat[coef_alt, "Estimate"]
      else 0
      
      analytic_slope <- b1 + b3 * mod_levels[i]
      
      data.frame(
        moderator_level = mod_labels[i],
        mod_value       = round(mod_levels[i], 3),
        slope           = round(analytic_slope, 4),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, results)
    
  }, error = function(e) {
    data.frame(Note = paste("Error:", e$message), stringsAsFactors = FALSE)
  })
}

# ── 5. Marginal Effects ───────────────────────────────────────────────────────
compute_marginal_effects <- function(model, predictor, moderator, data, n_points = 50) {
  tryCatch({
    mod_range <- seq(
      min(data[[moderator]], na.rm = TRUE),
      max(data[[moderator]], na.rm = TRUE),
      length.out = n_points
    )
    
    coefs_mat <- summary(model)$coefficients
    b1  <- if (predictor %in% rownames(coefs_mat)) coefs_mat[predictor, "Estimate"] else 0
    
    coef_name <- paste0(predictor, ":", moderator)
    coef_alt  <- paste0(moderator, ":", predictor)
    b3  <- if (coef_name %in% rownames(coefs_mat)) coefs_mat[coef_name, "Estimate"]
    else if (coef_alt %in% rownames(coefs_mat)) coefs_mat[coef_alt, "Estimate"]
    else 0
    
    # dy/dx1 = b1 + b3 * x2
    me <- b1 + b3 * mod_range
    
    # CI با delta method (تقریبی)
    vcov_mat <- vcov(model)
    se_b1 <- if (predictor %in% rownames(vcov_mat)) sqrt(vcov_mat[predictor, predictor]) else 0
    se_b3 <- if (coef_name %in% rownames(vcov_mat)) sqrt(vcov_mat[coef_name, coef_name])
    else if (coef_alt %in% rownames(vcov_mat)) sqrt(vcov_mat[coef_alt, coef_alt])
    else 0
    
    cov_b1b3 <- tryCatch({
      if (coef_name %in% rownames(vcov_mat) && predictor %in% rownames(vcov_mat))
        vcov_mat[predictor, coef_name]
      else if (coef_alt %in% rownames(vcov_mat) && predictor %in% rownames(vcov_mat))
        vcov_mat[predictor, coef_alt]
      else 0
    }, error = function(e) 0)
    
    se_me <- sqrt(se_b1^2 + (mod_range^2) * se_b3^2 + 2 * mod_range * cov_b1b3)
    
    data.frame(
      moderator_value = round(mod_range, 4),
      marginal_effect = round(me, 4),
      lower_ci        = round(me - 1.96 * se_me, 4),
      upper_ci        = round(me + 1.96 * se_me, 4)
    )
  }, error = function(e) {
    data.frame(Note = paste("Error:", e$message), stringsAsFactors = FALSE)
  })
}

# ── 6. Smart Interaction Plot ─────────────────────────────────────────────────
plot_interaction_smart <- function(model, v1, v2, data, target) {
  tryCatch({
    is_factor_v1 <- is.factor(data[[v1]]) || is.character(data[[v1]])
    is_factor_v2 <- is.factor(data[[v2]]) || is.character(data[[v2]])
    
    if (!is_factor_v1 && !is_factor_v2) {
      # continuous × continuous: moderator plot با 3 سطح
      m2  <- mean(data[[v2]], na.rm = TRUE)
      sd2 <- sd(data[[v2]],   na.rm = TRUE)
      levels_v2 <- c(m2 - sd2, m2, m2 + sd2)
      labels_v2 <- c("Low (-1SD)", "Mean", "High (+1SD)")
      
      v1_range <- seq(min(data[[v1]], na.rm = TRUE),
                      max(data[[v1]], na.rm = TRUE), length.out = 100)
      
      plot_df <- do.call(rbind, lapply(seq_along(levels_v2), function(j) {
        # یک نقطه نماینده برای بقیه متغیرها
        new_d <- data[rep(1, 100), , drop = FALSE]
        # مقادیر عددی رو به mean تنظیم کن
        num_cols <- vapply(new_d, is.numeric, logical(1L))
        for (col in names(new_d)[num_cols]) {
          new_d[[col]] <- mean(data[[col]], na.rm = TRUE)
        }
        new_d[[v1]] <- v1_range
        new_d[[v2]] <- levels_v2[j]
        new_d$pred  <- predict(model, newdata = new_d)
        new_d$level <- labels_v2[j]
        new_d[, c(v1, "pred", "level")]
      }))
      
      p <- ggplot(plot_df, aes(x = .data[[v1]], y = .data[["pred"]], color = .data[["level"]], fill = .data[["level"]])) +
        geom_line(size = 1) +
        geom_ribbon(aes(ymin = .data[["pred"]] - 0.1, ymax = .data[["pred"]] + 0.1), alpha = 0.1) +
        scale_color_manual(values = c("#e74c3c", "#3498db", "#2ecc71")) +
        scale_fill_manual(values  = c("#e74c3c", "#3498db", "#2ecc71")) +
        theme_minimal(base_size = 13) +
        labs(title   = paste("Interaction:", v1, "×", v2),
             x       = v1,
             y       = paste("Predicted", target),
             color   = paste(v2, "(levels)"),
             fill    = paste(v2, "(levels)"),
             caption = "Lines show predicted values at Mean ± 1SD of moderator")
      
    } else if (!is_factor_v1 && is_factor_v2) {
      # continuous × categorical
      v1_range  <- seq(min(data[[v1]], na.rm = TRUE),
                       max(data[[v1]], na.rm = TRUE), length.out = 100)
      v2_levels <- levels(factor(data[[v2]]))
      
      plot_df <- do.call(rbind, lapply(v2_levels, function(lv) {
        new_d <- data[rep(1, 100), , drop = FALSE]
        num_cols <- vapply(new_d, is.numeric, logical(1L))
        for (col in names(new_d)[num_cols]) new_d[[col]] <- mean(data[[col]], na.rm = TRUE)
        new_d[[v1]] <- v1_range
        new_d[[v2]] <- factor(lv, levels = v2_levels)
        new_d$pred  <- tryCatch(predict(model, newdata = new_d), error = function(e) NA)
        new_d$level <- lv
        new_d[, c(v1, "pred", "level")]
      }))
      
      p <- ggplot(plot_df, aes(x = .data[[v1]], y = .data[["pred"]], color = .data[["level"]])) +
        geom_line(size = 1.2) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Interaction:", v1, "×", v2),
             x = v1, y = paste("Predicted", target), color = v2)
      
    } else {
      # categorical × categorical: bar plot
      v1_levels <- levels(factor(data[[v1]]))
      v2_levels <- levels(factor(data[[v2]]))
      
      grid <- expand.grid(v1 = v1_levels, v2 = v2_levels, stringsAsFactors = FALSE)
      names(grid) <- c(v1, v2)
      
      new_d <- data[rep(1, nrow(grid)), , drop = FALSE]
      num_cols <- vapply(new_d, is.numeric, logical(1L))
      for (col in names(new_d)[num_cols]) new_d[[col]] <- mean(data[[col]], na.rm = TRUE)
      new_d[[v1]] <- factor(grid[[v1]], levels = v1_levels)
      new_d[[v2]] <- factor(grid[[v2]], levels = v2_levels)
      new_d$pred  <- tryCatch(predict(model, newdata = new_d), error = function(e) NA)
      
      p <- ggplot(new_d, aes(x = .data[[v1]], y = .data[["pred"]], fill = .data[[v2]])) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal(base_size = 13) +
        labs(title = paste("Interaction:", v1, "×", v2),
             x = v1, y = paste("Predicted", target), fill = v2)
    }
    
    p
    
  }, error = function(e) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = paste("Plot error:", e$message), size = 5) +
      theme_void()
  })
}

# ── 7. Auto Interpretation ────────────────────────────────────────────────────
interpret_interaction <- function(int_df, model_data) {
  if (is.null(int_df) || nrow(int_df) == 0) return("No interaction terms found in the model.")
  
  lines <- vapply(seq_len(nrow(int_df)), function(i) {
    row   <- int_df[i, ]
    parts <- strsplit(row$interaction_term, ":")[[1]]
    sig   <- row$p_value < 0.05
    dir   <- if (row$estimate > 0) "positive" else "negative"
    order_label <- if (row$order == 2) "two-way" else paste0(row$order, "-way")
    
    if (sig) {
      sprintf(
        "• %s interaction between %s is statistically significant (β = %.3f, p = %.4f%s). The %s effect of %s on the outcome strengthens/weakens depending on %s.",
        order_label, paste(parts, collapse = " and "),
        row$estimate, row$p_value, row$significance,
        dir, parts[1], parts[2]
      )
    } else {
      sprintf(
        "• %s interaction between %s is NOT significant (β = %.3f, p = %.4f). No meaningful moderation detected.",
        order_label, paste(parts, collapse = " and "),
        row$estimate, row$p_value
      )
    }
  })
  
  paste(lines, collapse = "\n\n")
}
