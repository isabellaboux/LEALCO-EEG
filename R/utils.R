save_model_to_html_VOTinv <- function(model, exp_name) {
  
  # ---- label dictionary ----
  var_labels <- c(
    VOTinv = "Naming Latencies (inverted)",
    partner_type = "Partner Type",
    block = "Block",
    c_SUBTLEX_frequency_log = "Frequency (log)",
    c_FASQUEL_image_agreement = "Image Agreement",
    c_FASQUEL_concreteness = "Concreteness"
  )
  
  # ---- file name (prefix) ----
  file <- paste0("output_stats/", exp_name, "_votINV_lmm.html")
  
  # ---- DV label ----
  dv_name <- as.character(formula(model)[[2]])
  dv_label <- var_labels[dv_name]
  if (is.na(dv_label)) dv_label <- dv_name
  
  # ---- fixed effects ----
  fe_names <- names(lme4::fixef(model))
  
  # ---- relabel + strip suffix ----
  relabel_component <- function(x) {
    if (x == "(Intercept)") return(x)
    
    keys <- names(var_labels)[order(nchar(names(var_labels)), decreasing = TRUE)]
    
    for (k in keys) {
      if (startsWith(x, k)) {
        label <- var_labels[k]
        if (is.na(label)) label <- k
        return(label)  # strip suffix
      }
    }
    x
  }
  
  # ---- build interaction labels ----
  relabel_term <- function(term) {
    if (term == "(Intercept)") return("(Intercept)")
    
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    parts <- vapply(parts, relabel_component, character(1))
    paste(parts, collapse = " × ")
  }
  
  pred_labels <- vapply(fe_names, relabel_term, character(1))
  
  # ---- create table ----
  sjPlot::tab_model(
    model,
    file = file,
    show.ci = FALSE,
    show.se = TRUE,
    string.est = "β",
    string.se = "SE",
    dv.labels = dv_label,
    pred.labels = pred_labels,
    digits = 6,
    digits.re = 10
  )
}


save_model_to_html_acc <- function(model, exp_name) {
  
  # ---- label dictionary ----
  var_labels <- c(
    correct = "Label Maintenance",
    partner_type = "Partner Type",
    block = "Block",
    c_SUBTLEX_frequency_log = "Frequency (log)",
    c_FASQUEL_image_agreement = "Image Agreement",
    c_FASQUEL_concreteness = "Concreteness"
  )
  
  # ---- file name ----
  file <- paste0("output_stats/", exp_name, "_acc_lmm.html")
  
  # ---- DV label ----
  dv_name <- as.character(formula(model)[[2]])
  dv_label <- var_labels[dv_name]
  if (is.na(dv_label)) dv_label <- dv_name
  
  # ---- fixed effects ----
  fe_names <- names(lme4::fixef(model))
  
  # ---- relabel + strip suffix ----
  relabel_component <- function(x) {
    if (x == "(Intercept)") return(x)
    
    keys <- names(var_labels)[order(nchar(names(var_labels)), decreasing = TRUE)]
    
    for (k in keys) {
      if (startsWith(x, k)) {
        label <- var_labels[k]
        if (is.na(label)) label <- k
        return(label)
      }
    }
    x
  }
  
  # ---- build interaction labels ----
  relabel_term <- function(term) {
    if (term == "(Intercept)") return("(Intercept)")
    
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    parts <- vapply(parts, relabel_component, character(1))
    paste(parts, collapse = " × ")
  }
  
  pred_labels <- vapply(fe_names, relabel_term, character(1))
  
  # ---- create table ----
  sjPlot::tab_model(
    model,
    file = file,
    show.ci = FALSE,
    show.se = TRUE,
    string.est = "b",   # 👈 changed here
    string.se = "SE",
    dv.labels = dv_label,
    pred.labels = pred_labels,
    digits = 3,
    digits.re = 3
  )
}