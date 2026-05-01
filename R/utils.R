save_model_to_html <- function(model, decimals, filename) {
  
  # ---- label dictionary ----
  var_labels <- c(
    correct = "Label Maintenance",
    VOTinv = "Naming Latency (inverted, scaled)",
    partner_type = "Test Partner",
    block = "Block",
    c_SUBTLEX_frequency_log = "Frequency (log)",
    c_FASQUEL_image_agreement = "Image Agreement",
    c_FASQUEL_concreteness = "Concreteness",
    entrainment_VOTinv = "Partner-Specific Naming Latency",
    entrainment_VOT = "Partner-Specific Naming Latency",
    c_entrainment_ACC = "Partner-Specific Label Maintenance",
    test_partner = "Test Partner",
    c_norm = "Naming Agreement",
    training_order = "Training Order",
    produced_label = "Produced Label",
    target_label = "Item",
    subjID = "Subject"
  )
  
  # ---- file name ----
  file <- file.path("output_stats", filename)
  
  # ---- DV label ----
  dv_name <- as.character(formula(model)[[2]])
  dv_label <- var_labels[dv_name]
  if (is.na(dv_label)) dv_label <- dv_name
  
  # ---- relabel single model component ----
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
  
  # ---- relabel interaction term ----
  relabel_term <- function(term) {
    if (term == "(Intercept)") return("(Intercept)")
    
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    parts <- vapply(parts, relabel_component, character(1))
    paste(parts, collapse = " × ")
  }
  
  # ---- relabel random-effect labels from sjPlot HTML ----
  relabel_random_effect_label <- function(x) {
    
    if (!grepl(".", x, fixed = TRUE)) {
      return(relabel_component(x))
    }
    
    dot_pos <- regexpr(".", x, fixed = TRUE)[1]
    
    group_part <- substr(x, 1, dot_pos - 1)
    slope_part <- substr(x, dot_pos + 1, nchar(x))
    
    group_label <- relabel_component(group_part)
    slope_label <- relabel_term(slope_part)
    
    paste0(group_label, ": ", slope_label)
  }
  
  # ---- fixed effects ----
  fe_names <- names(lme4::fixef(model))
  pred_labels <- vapply(fe_names, relabel_term, character(1))
  
  # ---- create table ----
  tab <- sjPlot::tab_model(
    model,
    file = file,
    show.ci = FALSE,
    show.se = TRUE,
    transform = NULL,
    string.est = "b",
    string.se = "SE",
    dv.labels = dv_label,
    pred.labels = pred_labels,
    digits = decimals,
    digits.re = decimals
  )
  
  print(tab)
  
  # ---- check that the file was created ----
  if (!file.exists(file)) {
    stop("tab_model() did not create the HTML file: ", file)
  }
  
  # ---- post-process random effects labels in HTML ----
  html <- readLines(file, warn = FALSE)
  
  # Find all <sub>...</sub> labels and relabel their contents
  sub_pattern <- "<sub>([^<]+)</sub>"
  matches <- gregexpr(sub_pattern, html, perl = TRUE)
  
  for (i in seq_along(html)) {
    m <- matches[[i]]
    
    if (m[1] != -1) {
      matched_text <- regmatches(html[i], list(m))[[1]]
      
      replaced_text <- vapply(
        matched_text,
        function(mt) {
          inner <- sub("^<sub>([^<]+)</sub>$", "\\1", mt, perl = TRUE)
          inner_new <- relabel_random_effect_label(inner)
          paste0("<sub>", inner_new, "</sub>")
        },
        character(1)
      )
      
      html[i] <- Reduce(
        function(line, pair) {
          sub(pair[1], pair[2], line, fixed = TRUE)
        },
        Map(c, matched_text, replaced_text),
        init = html[i]
      )
    }
  }
  
  writeLines(html, file)
}