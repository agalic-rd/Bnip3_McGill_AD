####╔════    ════╗####
####💠 Data Viz 💠####
####╚════    ════╝####

#--------------------#
####🔺Correlation ####
#--------------------#

corr_matrix_plot <- function(dat, vars, title = "") {
  return(
    dat
    |> mutate(
      across(where(is.character), factor),
      across(where(is.factor), label_encoding)
    )
    |> correlation(select = vars, include_factors = TRUE, redundant = TRUE, method = "auto")
    |> rename(R = matches("^r$|^rho$"))
    |> mutate(across(matches("Parameter[1-2]"), \(x) factor(x, levels = vars)))
    |> ggplot(aes(x = Parameter1, y = Parameter2))
      + geom_tile(aes(fill = R), colour = "white", linewidth = 1.2, stat = "identity")
      + geom_text(aes(label = round(R, 2), colour = abs(R) > 0.5), size = rel(4.5))
      + scale_color_manual(values = c("black", "white"))
      + scale_fill_gradient2(na.value = "white", breaks = seq(-1, 1, 0.2), limits = c(-1, 1))
      + scale_x_discrete(position = "top")
      + scale_y_discrete(limits = rev)
      + guides(fill = guide_colourbar(title = "R", barheight = rel(17), title.hjust = 0.15), colour = "none")
      + labs(title = title)
      + theme(
        plot.title = element_markdown(hjust = 0.5)
        , axis.title.x = element_blank()
        , axis.title.y = element_blank()
        , axis.text.x = element_text(face = "bold", angle = 30, hjust = 0, size = 8)
        , axis.text.y = element_text(face = "bold", angle = 45, hjust = 1, size = 8)
      )
  )
}

#-----------------#
####🔺Boxplots ####
#-----------------#

## Generating a boxplot for an individual gene, showing the main effect of a predictor (using the model fitted to this gene's data as input)
make_signif_boxplot <- function(
    mod, xaxis = "condition", facet = NULL, cluster = "rat", add_cluster_averages = TRUE, subtitle = NULL, caption = NULL, 
    scale = "link", adjust = "none", method = "pairwise", resp_name = NULL, max_points = 50, ncol = 2, print_eqs = FALSE
) {
  
  get_n_units <- function(df) {
    if(!is.null(cluster) && cluster %in% colnames(df)) return(length(unique(df[[cluster]])))
    else return(dplyr::tally(df))
  }
  
  dat <- insight::get_data(mod)
  
  if (!is.null(cluster) && cluster %ni% colnames(dat)) {
    cluster <- NULL
    add_cluster_averages <- FALSE
  }
  
  if (!is.null(cluster) 
      && cluster %in% colnames(dat) 
      && dat |> group_by(across(any_of(c(xaxis, facet, cluster)))) |> count() |> filter(n > 1) |> nrow() == 0
  ) {
    cluster <- NULL
    add_cluster_averages <- FALSE
  }
  
  resp <- insight::find_response(mod)
  if (is.null(resp_name)) resp_name <- get_response_name(resp)
  
  ## Making sure the variables of interest are contrasts for emmeans
  dat <- dat |> mutate(across(c(any_of(c(xaxis, facet)) & where(\(c) !is.factor(c))), as.factor))
  
  extra_dat <- dat |> group_by(across(any_of(c(xaxis, facet)))) |> summarize(N = str_glue("N = {get_n_units(pick(everything()))}")) |> ungroup()
  
  max <- max(dat[[resp]])
  min <- min(dat[[resp]])
  amp <- abs(max - min)
  
  if(adjust == "none") correction <- "(uncorrected)"
  else correction <- str_glue("({adjust} corrected)")
  
  # -----------[ Contrasts ]----------- #
  
  specs <- paste0(" ~ ", xaxis)
  if(!is.null(facet)) specs <- paste0(specs, " | ", facet)
  specs <- as.formula(specs)
  
  emms <- emmeans::emmeans(mod, specs = specs, type = "response", data = insight::get_data(mod))
  if (tolower(scale) %in% c("response", "resp")) emm <- regrid(emm, transform = "response")
  
  contrasts <- emmeans::contrast(emms, method = method, adjust = adjust, infer = TRUE) |> 
    as_tibble() |> 
    rename(Contrast = contrast) |> 
    tidyr::extract(col = Contrast, into = c("X1", "X2"), regex = "(.*) [- | /] (.*)", remove = FALSE)
  
  p_data_contrasts <- (
    contrasts
    |> group_by(across(any_of(c(facet))))
    |> mutate(
      x1 = match(X1, levels(dat[[xaxis]])),
      x2 = match(X2, levels(dat[[xaxis]])),
      p.signif = label_pval(p.value)
    ) 
    |> arrange(x.diff := abs(x2 - x1))
    |> mutate(
      step = 1:n(),
      pos.x = (x2 + x1) * 0.5,
      pos.y = max + step * 0.1 * (max - min)
    ) 
    |> ungroup()
  )
  
  x_title <- str_c(
    ifelse(xaxis == "condition", "Genotype", stringr::str_to_title(xaxis)), 
    " across ", 
    facet
  )
  
  # -----------[ Plot ]----------- #
  
  plot <- (
    ggplot(dat, aes(x = .data[[xaxis]], y = .data[[resp]], color = .data[[xaxis]], fill = .data[[xaxis]]))
    + geom_boxplot(outlier.alpha = 0, size = 1.1, fill = NA)
    + stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)), width = 0.75, linewidth = 1.1, linetype = "dotted")
    + { if (!is.null(cluster)) geom_jitter(
      data = \(x) x |> group_by(across(any_of(c(xaxis, facet)))) |> group_modify(\(d, g) slice_sample(d, n = min(nrow(d), max_points))) |> ungroup(), 
      size = 1.5, width = 0.1, alpha = 0.3
    )
      else geom_jitter(
        data = \(x) x |> group_by(across(any_of(c(xaxis, facet)))) |> group_modify(\(d, g) slice_sample(d, n = min(nrow(d), max_points))) |> ungroup(), 
        mapping = aes(fill = .data[[xaxis]]), shape = 23, color = color_text, size = 3, width = 0.1, alpha = 0.9
      )
    }
    + {if (add_cluster_averages) stat_summary(
      aes(group = .data[[cluster]], fill = .data[[xaxis]]), geom = "point", fun = mean, 
      size = ifelse(is.null(facet), 4, 3), shape = 23, color = color_text, alpha = 0.9, position = position_dodge(0.2)
    )}
    #+ ggrepel::geom_text_repel(aes(label = rat), color = "black")
    + geom_errorbarh(
      data = p_data_contrasts, aes(xmin = x1, xmax = x2, y = pos.y), inherit.aes = FALSE, 
      color = "black", height = 0.03 * amp, linewidth = 0.5
    )
    + geom_text(
      data = p_data_contrasts, aes(x = pos.x, y = pos.y, label = p.signif), inherit.aes = FALSE,
      size = 5, color = "black", fontface = "bold", vjust = 0, hjust = 0.5, position = position_nudge(y = 0.02 * amp),
      family = "serif"
    )
    + geom_label(
      aes(y = min - 0.05 * amp, fontface = "bold", label = N, color = .data[[xaxis]]),
      data = extra_dat, fill = NA, size = 6, alpha = 0.7,
      family = "serif"
    )
    # + scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
    + theme(
      text = element_text(family = "serif"),
      legend.position = "none", 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, face = "plain"),
      plot.caption = element_text(hjust = 0.5, face = "plain", size = 13),
      axis.text.x = ggplot2::element_text(size = 15),
      axis.text.y = ggplot2::element_text(size = 15)
    )
    + labs(y = resp_name, x = x_title)
    + {if(!is.null(subtitle)) labs(subtitle = subtitle)}
    + {if(!is.null(caption)) labs(caption = caption)}
    + {if (!is.null(facet)) facet_wrap( ~ .data[[facet]], ncol = ncol)}
    #+ {if (add_cluster_averages) labs(caption = str_glue("Small round points are individual measurements\n Diamonds represent {cluster}-averages"))}
  )

    # -----------[ Formatted results ]----------- #
  
  if (print_eqs) {
    # print(contrasts)
    contrasts_eqs <- contrasts |> rowwise() |> mutate(
      contrast_name = pick(everything()) |> colnames() |> str_subset("^estimate|risk|odds|^ratio|^difference"),
      crit_val_name = pick(everything()) |> colnames() |> str_subset("^(z|t|F)"),
      Equation = glue::glue(
        "$<<str_extract({{crit_val_name}}, '^(z|t)')>>(<<df>>) = <<round(.data[[crit_val_name]], 3)>>; " %s+%
          "p = <<scales::pvalue(p.value)>>; " %s+%
          "<<str_to_sentence({{contrast_name}})>> = <<round(.data[[contrast_name]], 3)>>; " %s+%
          "CI_{95} = [<<round(asymp.LCL, 3)>>, <<round(asymp.UCL, 3)>>];$",
        .open = "<<", .close = ">>"
      )) |> select(Contrast, any_of(facet), Equation)
    
    print(contrasts_eqs)
  }
  
  return(plot)
}

## Generating a boxplot for an individual gene, showing interaction effects between two predictors (using the model fitted to this gene's data as input)
make_signif_boxplot_inter <- function(
    mod, pred1 = "condition", pred2, facet = NULL, cluster = NULL, add_cluster_averages = FALSE, stage = NULL,
    scale = "link", adjust = "none", resp_name = NULL, max_points = 50, ncol = 2, print_eqs = FALSE
) {
  
  get_n_units <- function(df) {
    if(!is.null(cluster) && cluster %in% colnames(df)) return(length(unique(df[[cluster]])))
    else return(dplyr::tally(df))
  }
  
  dat <- insight::get_data(mod)
  resp <- insight::find_response(mod)
  
  if (is.null(resp_name)) {
    resp_name <- get_response_name(resp)
  }
  
  ## Making sure the variables of interest are factors for emmeans
  dat <- dat |> mutate(across(c(any_of(c(pred1, pred2)) & where(\(c) !is.factor(c))), as.factor))
  
  extra_dat <- dat |> 
    group_by(across(any_of(c(pred1, pred2, facet))), .drop = TRUE) |> 
    summarize(N = str_glue("N = {get_n_units(pick(everything()))}")) |> 
    ungroup()
  
  # TODO: should be computed by facet
  max <- max(dat[[resp]])
  min <- min(dat[[resp]])
  amp <- abs(max - min)
  
  # -----------[ Contrasts ]----------- #
  
  specs <- paste0(" ~ ", pred1)
  if (!is.null(pred2)) specs <- paste0(specs, " | ", pred2)
  specs <- as.formula(specs)
  
  emmeans <- emmeans::emmeans(
    mod, 
    specs = specs, 
    type = "response", 
    by = facet, 
    data = mutate(insight::get_data(mod), across(any_of(c(facet)), as.factor))
  )
  if (tolower(scale) %in% c("response", "resp")) emmeans <- regrid(emmeans, transform = "response")
  
  contrasts <- emmeans::contrast(emmeans, method = "pairwise", adjust = adjust, infer = TRUE) |> 
    as.data.frame() |> 
    rename(Contrast = contrast) |> 
    tidyr::extract(col = Contrast, into = c("X1", "X2"), regex = "(.*) [- | /] (.*)", remove = FALSE)
  
  p_data_contrasts <- contrasts |>
    group_by(across(any_of(c(pred2, facet)))) |>
    mutate(
      x1 = (match(.data[[pred2]], levels(dat[[pred2]])) - 1) * length(unique(dat[[pred1]])) + match(X1, levels(dat[[pred1]])),
      x2 = (match(.data[[pred2]], levels(dat[[pred2]])) - 1) * length(unique(dat[[pred1]])) + match(X2, levels(dat[[pred1]])),
      p.signif = label_pval(p.value)
    ) |>
    arrange(x.diff := abs(x2 - x1)) |>
    mutate(
      step = 1:n(),
      pos.x = (x2 + x1) * 0.5,
      pos.y = max + step * 0.1 * (max - min)
    ) |>
    ungroup()
  
  contrasts_interactions <- emmeans::contrast(emmeans, interaction = c("pairwise"), by = facet, adjust = "none", infer = TRUE) |> 
    as.data.frame() |> 
    tidyr::extract(col = paste0(pred1, "_pairwise"), into = c("pred1_1", "pred1_2"), regex = "(.*) [- | /] (.*)", remove = FALSE) |> 
    tidyr::extract(col = paste0(pred2, "_pairwise"), into = c("pred2_1", "pred2_2"), regex = "(.*) [- | /] (.*)", remove = FALSE)
  
  p_data_interactions <- contrasts_interactions |>
    group_by(across(any_of(c(facet)))) |>
    mutate(
      x1 = 0.5 * ((match(pred2_1, levels(dat[[pred2]])) - 1) * length(unique(dat[[pred1]])) + match(pred1_1, levels(dat[[pred1]])) +
                    (match(pred2_1, levels(dat[[pred2]])) - 1) * length(unique(dat[[pred1]])) + match(pred1_2, levels(dat[[pred1]]))),
      x2 = 0.5 * ((match(pred2_2, levels(dat[[pred2]])) - 1) * length(unique(dat[[pred1]])) + match(pred1_1, levels(dat[[pred1]])) +
                    (match(pred2_2, levels(dat[[pred2]])) - 1) * length(unique(dat[[pred1]])) + match(pred1_2, levels(dat[[pred1]]))),
      p.signif = label_pval(p.value)
    ) |>
    arrange(x.diff := abs(x2 - x1)) |>
    mutate(
      step = 1:n() + choose(length(unique(dat[[pred1]])), 2),
      pos.x = (x2 + x1) * 0.5,
      pos.y = max + step * 0.1 * (max - min)
    ) |>
    ungroup()
  
  x_title <- str_c(
    ifelse(pred1 == "condition", "Genotype", stringr::str_to_title(pred1)),
    " by ",
    ifelse(pred2 == "location", "area", pred2)
  )

  if (!is.null(facet)) {
    x_title <- str_c(
      x_title,
      " across ",
      ifelse(facet == "condition", "genotype", facet)
    )
  }
  
  # -----------[ Plot ]----------- #
  
  plot <- (
    ggplot(dat, aes(x = interaction(.data[[pred1]], .data[[pred2]], sep = "_"), y = .data[[resp]], color = .data[[pred1]]))
    + geom_boxplot(outlier.alpha = 0, size = 1.1, fill = NA)
    + stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)), width = 0.75, size = 1.1, linetype = "dotted")
    + { 
      if (!is.null(cluster)) geom_jitter(
        data = \(x) x |> group_by(across(any_of(c(pred1, pred2)))) |> group_modify(\(d, g) slice_sample(d, n = min(nrow(d), max_points))) |> ungroup(), 
        size = 1.5, width = 0.1, alpha = 0.3
      )
      else geom_jitter(
        data = \(x) x |> group_by(across(any_of(c(pred1, pred2)))) |> group_modify(\(d, g) slice_sample(d, n = min(nrow(d), max_points))) |> ungroup(), 
        mapping = aes(fill = .data[[pred1]]), shape = 23, color = color_text, size = 3, width = 0.1, alpha = 0.9
      )
    }
    + {
      if (add_cluster_averages) stat_summary(
        aes(group = .data[[cluster]], fill = .data[[pred1]]), geom = "point", fun = mean, 
        size = 3, shape = 23, color = color_text, alpha = 0.9, position = position_dodge(0.2)
      )
    }
    + geom_errorbarh(
      data = p_data_contrasts, aes(xmin = paste(X1, .data[[pred2]], sep = "_"), xmax = paste(X2, .data[[pred2]], sep = "_"), y = pos.y), inherit.aes = FALSE,
      color = "black", height = 0.02 * amp, size = 0.5
    )
    + geom_text(
      data = p_data_contrasts, aes(x = pos.x, y = pos.y, label = p.signif), inherit.aes = FALSE,
      size = 5, color = "black", fontface = "bold", vjust = 0, hjust = 0.5, position = position_nudge(y = 0.02 * amp),
      family = "serif"
    )
    + geom_label(
      # Use group-specific min/amp for y-positioning
      aes(y = min - 0.05 * amp, fontface = "bold", label = N, color = .data[[pred1]]),
      data = extra_dat, fill = NA, size = 5, alpha = 0.7, # Reduced size slightly
      family = "serif"
    )
    ## Interactions
    + geom_errorbarh(
      data = p_data_interactions, aes(xmin = x1, xmax = x2, y = pos.y), inherit.aes = FALSE,
      color = "black", height = 0.02 * amp, size = 0.5
    )
    + geom_text(
      data = p_data_interactions, aes(x = pos.x, y = pos.y, label = p.signif), inherit.aes = FALSE,
      size = 5, color = "black", fontface = "bold", vjust = 0, hjust = 0.5, position = position_nudge(y = 0.02 * amp),
      family = "serif"
    )
    + theme(
      text = element_text(family = "serif"),
      legend.position = "none",
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, face = "plain"),
      axis.text.x = ggplot2::element_text(size = 15),
      axis.text.y = ggplot2::element_text(size = 15),
      plot.caption = ggplot2::element_text(size = 13)
    )
    + labs(y = resp_name, x = x_title)
    + {if(!is.null(stage)) labs(subtitle = str_glue("{stage}"))}
    + {if (!is.null(facet)) facet_wrap( ~ .data[[facet]], ncol = ncol)}
    #+ {if (add_cluster_averages) labs(caption = str_glue("Small round points are individual measurements\n Diamonds represent {cluster}-averages"))}
    + scale_x_discrete(labels = \(l) str_replace(l, "_", "\n"))
  )

    # -----------[ Formatted results ]----------- #
  
  if (print_eqs) {
    contrasts_eqs <- contrasts |> rowwise() |> mutate(
      contrast_name = pick(everything()) |> colnames() |> str_subset("^estimate|risk|odds|^ratio|^difference"),
      crit_val_name = pick(everything()) |> colnames() |> str_subset("^(z|t)"),
      Equation = glue::glue(
        "$<<str_extract({{crit_val_name}}, '^(z|t)')>>(<<df>>) = <<round(.data[[crit_val_name]], 3)>>; " %s+%
          "p = <<scales::pvalue(p.value)>>; " %s+%
          "<<str_to_sentence({{contrast_name}})>> = <<round(.data[[contrast_name]], 3)>>; " %s+%
          "CI_{95} = [<<round(asymp.LCL, 3)>>, <<round(asymp.UCL, 3)>>];$",
        .open = "<<", .close = ">>"
      )) |> select(Contrast, any_of(pred2), Equation)
    
    print(contrasts_eqs)
    
    contrasts_interactions_eqs <- contrasts_interactions |> rowwise() |> mutate(
      contrast_name = pick(everything()) |> colnames() |> str_subset("^estimate|risk|odds|^ratio|^difference"),
      crit_val_name = pick(everything()) |> colnames() |> str_subset("^(z|t)"),
      Equation = glue::glue(
        "$<<str_extract({{crit_val_name}}, '^(z|t)')>>(<<df>>) = <<round(.data[[crit_val_name]], 3)>>; " %s+%
          "p = <<scales::pvalue(p.value)>>; " %s+%
          "<<str_to_sentence({{contrast_name}})>> = <<round(.data[[contrast_name]], 3)>>; " %s+%
          "CI_{95} = [<<round(asymp.LCL, 3)>>, <<round(asymp.UCL, 3)>>];$",
        .open = "<<", .close = ">>"
      )) |> select(matches("_pairwise"), Equation)
    
    print(contrasts_interactions_eqs)
  }
  
  return(plot)
}

#------------------#
####🔺Timelines ####
#------------------#

make_fold_timeline_plot <- function(
    dat, facet_rows = "Pathway", trans = "identity", 
    color_by = NULL, colors = colors_effect, size_boost = 1
) {
  
  origin <- do.call(trans, list(1))
  
  dat <- (
    dat
    |> mutate(fold_trans = do.call(trans, list(fold)))
    |> mutate(fold_amp = ifelse(
      max(fold_trans, na.rm = TRUE) - min(fold_trans, na.rm = TRUE) != 0, 
      max(fold_trans, na.rm = TRUE) - min(fold_trans, na.rm = TRUE), 
      mean(fold_trans, na.rm = TRUE)) * 0.1,
      .by = all_of(c(facet_rows, "stage"))
    )
  )
  
  timeline <- (
    ggplot(dat)
    + { if(is.null(color_by)) aes(x = gene, color = fold >= 1) else aes(x = gene, color = .data[[color_by]]) }
    + geom_linerange(aes(ymax = fold_trans), ymin = origin, linewidth = 2 + (size_boost * 0.5))
    + geom_hline(yintercept = origin, linewidth = 0.3, linetype = "dotted")
    + geom_text(aes(
        label = str_c(round(fold, 2), stars_pval(p_value), sep = " "), 
        y = ifelse(fold_trans > origin, fold_trans + fold_amp, fold_trans - fold_amp),
        hjust = ifelse(fold > 1, 0, 1)
      ),
      vjust = 0.5, angle = 0, size = 2 + (size_boost * 0.25), check_overlap = TRUE
    )
    + scale_color_manual(" ", values = colors)
    + scale_y_continuous(breaks = c(0,1,2,3), expand = expansion(mult = 1.01 * (1 + (size_boost/100))))
    + scale_x_discrete(expand = expansion(add = 1 * size_boost), limits = \(x) rev(x))
    + labs(
      x = "",
      y = ifelse(trans != "identity", str_glue("Fold Change *({trans} scale)*"), "Fold Change")
    )
    + coord_flip()
    + facet_grid(
      vars(.data[[facet_rows]]), vars(stage), 
      scales = "free_y", space = "free_y", labeller = label_wrap_gen(width = 12, multi_line = TRUE)
    )
    + { if (!is.null(color_by)) guides(color = guide_legend(title = color_by)) }
    + theme(
      legend.position = ifelse(is.null(color_by), "none", "bottom")
      , axis.text.x = element_blank()
      , axis.title.x = element_markdown(size = 9)
      , axis.text.y = element_text(size = 7)
      , strip.text = element_text(size = 5 * size_boost)
      , plot.title = element_markdown(size = 9, face = "plain", vjust = 1, hjust = 0.5)
    )
  )
  
  return(timeline)
}



#--------------------------#
####🔺Model diagnostics ####
#--------------------------#

make_acf_plot <- function(mod) {
  forecast::ggAcf(residuals(mod, type = "response"), color = "#1b6ca8") + 
    geom_point() + 
    labs(
      title = "Autocorrelation of residuals",
      subtitle = "Data (lines) should be inside the dashed area"
    ) + 
    see::theme_lucid() +
    theme(
      plot.title = element_markdown(size = 15, margin = margin(0, 0, 0, 1)),
      plot.subtitle = element_markdown(size = 12, margin = margin(1, 0, 0, 0))
    )
}

ppc_plots <- function(mod, simulations, term = "condition", type = "fixed", is_count = NULL, max_cols_per_plot = 3) {
  
  Y <- insight::get_response(mod)
  n_unique <- n_distinct(insight::get_data(mod)[[term]])
  
  if(is.null(is_count)) is_count <- ifelse(insight::get_family(mod)$family |> str_detect("binom|poiss"), TRUE, FALSE)
  
  # ppc_fun <- ifelse(is_count, bayesplot::ppc_bars, bayesplot::ppc_dens_overlay)
  ppc_fun_grouped <- ifelse(is_count, bayesplot::ppc_bars_grouped, bayesplot::ppc_dens_overlay_grouped)
  ppc_fun_pred_grouped <- bayesplot::ppc_intervals_grouped
  
  if(type %in% c("fixed", "fe")) {
    .term <- insight::get_predictors(mod)[[term]]
    
    # ppc_global <- ppc_fun(y = Y, yrep = simulations) 
    if(is_count) ppc_root <- bayesplot::ppc_rootogram(Y, simulations, style = "suspended")
    
    ppc_grouped <- ppc_fun_grouped(Y, simulations, group = .term) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
    ppc_pred_grouped <- ppc_fun_pred_grouped(Y, simulations, group = .term, prob_outer = 0.95) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
  }
  else if(type %in% c("random", "re")) {
    .term <- insight::get_random(mod)[[term]]
    
    ppc_grouped <- ppc_fun_grouped(Y, simulations, group = .term) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
    ppc_pred_grouped <- ppc_fun_pred_grouped(Y, simulations, group = .term, prob_outer = 0.95) + 
      facet_wrap(~ group, ncol = min(max_cols_per_plot, n_unique), scales = "free")
  }
  
  return(
    if(type %in% c("fixed", "fe")) {
      if(is_count) { 
        (ppc_root / ppc_grouped / ppc_pred_grouped) + plot_layout(guides = 'collect', ncol = 1, nrow = 3) +
          # plot_annotation(title = "Simulation-based Posterior Predictive Checks", subtitle = str_glue("For [{term}]")) & 
          theme(legend.position = 'right', axis.title.x = element_blank())
      } else {
        (ppc_grouped / (ppc_pred_grouped + theme(axis.title.x = element_blank()))) + plot_layout(ncol = 1, nrow = 2) + 
          # plot_annotation(title = "Simulation-based Posterior Predictive Checks", subtitle = str_glue("For [{term}]")) & 
          theme(legend.position = 'right')
      }
    }
    else list(ppc_grouped, ppc_pred_grouped)
  )
}

ppc_stat_plots <- function(mod, simulations, term = "condition", type = "fixed", stats = c("min", "max", "mean", "sd"), n_cols = 2, max_cols_per_plot = 5) {
  
  n_unique <- n_distinct(insight::get_data(mod)[[term]])
  
  if(type %in% c("fixed", "fe")) .term <- insight::get_predictors(mod)[[term]]
  else if(type %in% c("random", "re")) .term <- insight::get_random(mod)[[term]]
  
  return(
    patchwork::wrap_plots(
      purrr::map(
        stats, 
        \(.x) bayesplot::ppc_stat_grouped(
          insight::get_response(mod), 
          simulations, group = .term, stat = .x,
          facet_args = list(ncol = min(max_cols_per_plot, n_unique))
        ) + scale_x_continuous(labels = \(l) signif(l, digits = 2))
      ), 
      ncol = n_cols, guides = 'auto'
    ) + 
      # plot_annotation(title = "Simulation-based Predictive Checks (on statistics)", subtitle = str_glue("For [{term}]")) & 
      theme(legend.position = 'right', axis.text.x = element_text(size = rel(1.5), angle = 30, hjust = 1))
  )
}
