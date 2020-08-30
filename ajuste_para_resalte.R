

# #Pruebas -----
# 
# 
# mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f") & class != "2seater")
lista = list(color="black",fill="#ff8c00")

# filtro=mpg2 %>% filter(manufacturer=="jeep")
# 
# nuevo_tema=function(...) {
#   if(...){
#     ggplot2::theme_update(tema_yuc)
#   }
# }
# 
# ggplot()+
#   geom_line(data = mpg2,aes(y=displ,x=cty,colour=class),size=1)+
#   geom_rect(data=filtro,aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
#             fill='red', alpha=0.1) +theme_classic()+
#   facet_wrap(~manufacturer)
# #se deja atras el filtrado
# +
#   yucatan_filtro(manufacturer=="jeep",calculate_per_facet = T,use_direct_label = F,
#                  unhighlighted_params =  lista,use_group_by = T)
# 
# 
# 
# 
# p2= ggplot(data = mpg2) +geom_line(data = filtro,mapping = aes(y=displ,x=cty))+tema_no_yuc
# 
# pp+p2

librerias=c("dplyr","rlang","ggplot2","gghighlight")

sapply(librerias,require, character.only = TRUE)


#función hermosa 
seleccion_filtro=function(...,datos,color,limitesx=c(-Inf,Inf),limitesy=c(-Inf,Inf),alfa=0.1){

  predicates <- enquos(...)
  check_bad_predicates(predicates)
  
  filtro=datos %>% filter(...)
  
  
  return(geom_rect(data=filtro,aes(xmin=limitesx[1],xmax=limitesx[2],ymin=limitesy[1], ymax=limitesy[2]),
            fill=color, alpha=alfa))
}

# testeo de la función de arriba ----
# ggplot()+
#   geom_line(data = mpg2,aes(y=displ,x=cty,colour=class),size=1)+
#   seleccion_filtro(manufacturer=="jeep",datos = mpg2,color = 'red')+
#   theme_classic()+
#   facet_wrap(vars(manufacturer))


yucatan_filtro=function(..., n = NULL, parametros_selecion = list(), 
                 use_group_by = NULL, use_direct_label = NULL, label_key = NULL, 
                 label_params = list(fill = "white"), keep_scales = FALSE, 
                 calculate_per_facet = FALSE){
  predicates <- enquos(...)
  label_key <- enquo(label_key)
  check_bad_predicates(predicates)
  check_bad_label_key(label_key)
  parametros_selecion <-deep1(parametros_selecion)
  #retorna la structura
  structure(list(predicates = predicates, n = n, 
                 unhighlighted_params = parametros_selecion, use_group_by = use_group_by, 
                 use_direct_label = use_direct_label, label_key = label_key, 
                 label_params = label_params, keep_scales = keep_scales, 
                 calculate_per_facet = calculate_per_facet,
                 strip.background = element_rect(fill = "#B3B3B3", color  = "#B3B3B3")),
            class = "yucatan_filtro")
}

deep1=function (aes_params) 
{
  if (!is.list(aes_params)) {
    abort("parametros_selecion deberia ser una list.")
  }
  if (!is.null(aes_params$color)) {
    aes_params$colour <- aes_params$colour %||% aes_params$color
    aes_params$color <- NULL
  }
  return(aes_params)
}

check_bad_predicates <- function(x) {
  have_name_idx <- have_name(x)
  
  if (any(have_name_idx)) {
    bad_x <- x[have_name_idx]
    bad_x_deparsed <- purrr::imap_chr(
      bad_x,
      ~ paste(.y, "=", quo_text(.x))
    )
    
    abort(
      sprintf(
        "Use ya sea `==`?: %s",
        paste(bad_x_deparsed, collapse = ",")
      )
    )
  }
}

check_bad_label_key <- function(x) {
  if (!quo_is_null(x) && !quo_is_symbol(x)) {
    abort("label_key deberia ser un simbolo")
  }
}

VERY_SECRET_COLUMN_NAME <- sym("highlight..........")


ggplot_add.yucatan_filtro <- function(object, plot, object_name) {
  if (length(plot$layers) == 0) {
    plot$data <- sieve_data(plot$data, plot$mapping, object$predicates,
                            max_highlight = object$max_highlight, use_group_by = object$use_group_by
    )
    return(plot)
  }
  
  n_layers <- length(plot$layers)
  if (is.null(object$n)) {
    idx_layers <- rep(TRUE, n_layers)
  } else {
    if (object$n > n_layers) {
      abort("n es mas grande que el actual numero de capas!")
    }
    idx_layers <- rep(FALSE, n_layers)
    idx_layers[utils::tail(seq_len(n_layers), object$n)] <- TRUE
  }
  
  # Layers are environments; if we modify an element in an environment, the
  # modified value is kept. So, we need to clone them before modifying.
  # Note that, since the plot data is overwritten later, we need to attach
  # the data to all layers and then assign back (#31).
  layers_cloned <- purrr::map(plot$layers, clone_layer)
  
  # Data and group IDs are used commonly both in the bleaching and sieving process
  purrr::walk(layers_cloned, merge_plot_to_layer,
              plot_data = plot$data, plot_mapping = plot$mapping
  )
  
  # Assign back layers that are not get bleached or sieved
  plot$layers[!idx_layers] <- layers_cloned[!idx_layers]
  layers_cloned <- layers_cloned[idx_layers]
  
  facet_vars <- if (object$calculate_per_facet) get_facet_vars(plot$facet) else NULL
  group_infos <- purrr::map(
    layers_cloned,
    ~ calculate_group_info(.$data, .$mapping, extra_vars = facet_vars)
  )
  
  # Clone layers again seperately (layers_cloned will be used later for keeping
  # the original scale)
  layers_bleached <- purrr::map(layers_cloned, clone_layer)
  layers_sieved <- purrr::map(layers_cloned, clone_layer)
  
  # Bleach the lower layer.
  purrr::walk2(
    layers_bleached,
    group_infos,
    bleach_layer,
    unhighlighted_params = object$unhighlighted_params,
    calculate_per_facet = object$calculate_per_facet
  )
  
  # Sieve the upper layer.
  success <- purrr::map2_lgl(
    layers_sieved,
    group_infos,
    sieve_layer,
    predicates = object$predicates,
    max_highlight = object$max_highlight,
    use_group_by = object$use_group_by
  )
  
  if (!any(success)) {
    abort("All calculations failed! Please provide a valid predicate.")
  }
  if (!all(success)) {
    warn(
      sprintf(
        "Could not calculate the predicate for %s; ignored",
        paste("layer", which(!success), collapse = ", ")
      )
    )
    # remove failed layers
    layers_sieved[!success] <- list(NULL)
  }
  
  # The plot data should also be sieved to deleting facets for unhighlighted levels
  plot$data <- layers_sieved[[1]]$data
  
  # skip failed layers
  plot$layers[idx_layers][success] <- layers_bleached[success]
  plot <- plot %+% layers_sieved
  
  # Add dummy layers (geom_blank()) to keep the original scales
  if (object$keep_scales) {
    plot <- plot %+% purrr::map(layers_cloned, ~ ggplot2::geom_blank(.$mapping, .$data))
  }
  
  # 1) use_direct_label is NULL
  #   - try to use direct labels but ignore failures
  #   - when the labels would be too many, do not add labels
  # 2) use_direct_label is TRUE
  #   - use direct labels, otherwise abort()
  #   - even when the labels would be too many, do add labels
  # 3) use_direct_label is FALSE
  #   - do not use direct labeys
  if (is_false(object$use_direct_label)) {
    return(plot)
  }
  
  must_add_labels <- is_true(object$use_direct_label)
  max_labels <- ifelse(must_add_labels, Inf, getOption("gghighlight_max_labels", 20))
  
  layer_labelled <- generate_labelled_layer(
    layers_sieved, group_infos,
    object$label_key, object$label_params,
    max_labels = max_labels
  )
  
  if (is.null(layer_labelled)) {
    if (must_add_labels) {
      abort("No layer can be used for labels")
    } else {
      return(plot)
    }
  }
  
  plot %+% layer_labelled %+% ggplot2::guides(colour = "none", fill = "none")
}

merge_plot_to_layer <- function(layer, plot_data, plot_mapping) {
  layer$data <- merge_data(layer, plot_data)
  # since gghighlight does grouped calculations, we want to specify the group key by ourselves.
  layer$data <- dplyr::ungroup(layer$data)
  layer$mapping <- merge_mapping(layer, plot_mapping)
  layer
}

merge_data <- function(layer, plot_data) {
  # c.f.) https://github.com/tidyverse/ggplot2/blob/54de616213d9811f422f45cf1a6c04d1de6ccaee/R/layer.r#L182
  layer$layer_data(plot_data)
}

merge_mapping <- function(layer, plot_mapping) {
  # Merge the layer's mapping with the plot's mapping
  mapping <- utils::modifyList(plot_mapping %||% aes(), layer$mapping %||% aes())
  # Filter out unused variables (e.g. fill aes for line geom)
  layer_aes <- base::union(layer$geom$aesthetics(), layer$stat$aesthetics())
  aes_names <- base::intersect(layer_aes, names(mapping))
  mapping <- mapping[aes_names]
  
  mapping
}

clone_layer <- function(layer) {
  new_layer <- env_clone(layer)
  class(new_layer) <- class(layer)
  new_layer
}

clone_position <- clone_layer

calculate_group_info <- function(data, mapping, extra_vars = NULL) {
  mapping <- purrr::compact(mapping)
  # the calculation may be possible only in the ggplot2 context (e.g. stat()).
  # So, wrap it with tryCatch() and remove the failed results, which can be
  # detected by checking if all elements are NA or not.
  mapping_wrapped <- purrr::map(mapping, ~ quo(tryCatch(!!., error = function(e) NA)))
  data_evaluated <- dplyr::transmute(data, !!!mapping_wrapped)
  data_evaluated <- dplyr::select_if(data_evaluated, ~ !all(is.na(.)))
  
  # Calculate group IDs as ggplot2 does.
  # (c.f. https://github.com/tidyverse/ggplot2/blob/8778b48b37d8b7e41c0f4f213031fb47810e70aa/R/grouping.r#L11-L28)
  if ("group" %in% names(data_evaluated)) {
    group_cols <- "group"
  } else {
    idx_discrete <- purrr::map_lgl(data_evaluated, ~ is.factor(.) || is.character(.) || is.logical(.))
    group_cols <- names(which(idx_discrete))
  }
  
  # for group key, use symbols only, and don't include extra_vars
  group_keys <- purrr::keep(mapping[group_cols], quo_is_symbol)
  
  # if extra variables (e.g. facet specs) are specified, use them.
  if (!is.null(extra_vars)) {
    if (!is_quosures(extra_vars) || !all(have_name(extra_vars))) {
      abort("extra_vars must be a named quosures object.")
    }
    extra_data <- dplyr::transmute(data, !!!extra_vars)
  } else {
    extra_data <- NULL
  }
  
  # no group
  if (length(group_cols) == 0 && is_empty(extra_data)) {
    return(NULL)
  }
  
  # calculate group IDs with extra_data
  group_df <- dplyr::group_by(
    dplyr::bind_cols(data_evaluated, extra_data),
    !!!syms(c(group_cols, names(extra_data)))
  )
  group_ids <- dplyr::group_indices(group_df)
  
  list(
    data = data_evaluated,
    id = group_ids,
    key = group_keys
  )
}

bleach_layer <- function(layer, group_info, unhighlighted_params, calculate_per_facet = FALSE) {
  # `colour` and `fill` are special in that they needs to be specified even when
  # it is not included in unhighlighted_params. But, if the default_aes is NA,
  # respect it (e.g. geom_bar()'s default colour is NA).
  # Note that this depends on the mapping, so this needs to be done before modifying the mapping.
  unhighlighted_params$colour <- unhighlighted_params$colour %||% get_default_aes_param("colour", layer$geom, layer$mapping)
  unhighlighted_params$fill <- unhighlighted_params$fill %||% get_default_aes_param("fill", layer$geom, layer$mapping)
  
  # c.f. https://github.com/tidyverse/ggplot2/blob/e9d4e5dd599b9f058cbe9230a6517f85f3587567/R/layer.r#L107-L108
  aes_params_bleached <- unhighlighted_params[names(unhighlighted_params) %in% layer$geom$aesthetics()]
  geom_params_bleached <- unhighlighted_params[names(unhighlighted_params) %in% layer$geom$parameters(TRUE)]
  
  layer$aes_params <- utils::modifyList(layer$aes_params, aes_params_bleached)
  layer$geom_params <- utils::modifyList(layer$geom_params, geom_params_bleached)
  
  # FIXME: Is this necessary while aes_params will override these mappings?
  # remove colour and fill from mapping
  layer$mapping[c("colour", "fill")] <- list(NULL)
  
  # FIXME: can this be removed?
  if (is.null(group_info$key)) {
    return(layer)
  }
  
  # In order to prevent the bleached layer from being facetted, we need to rename
  # columns of group keys to improbable names. But, what happens when the group
  # column disappears? Other calculations that uses the column fail. So, we need
  # to use the pre-evaluated values and rename everything to improbable name.
  bleached_data <- group_info$data
  secret_prefix <- expr_text(VERY_SECRET_COLUMN_NAME)
  mapping_names <- names(bleached_data)
  secret_names <- paste0(secret_prefix, seq_along(mapping_names))
  secret_quos <- quos(!!!syms(secret_names))
  bleached_data <- dplyr::rename(bleached_data, !!!stats::setNames(mapping_names, secret_names))
  layer$mapping <- utils::modifyList(layer$mapping, stats::setNames(secret_quos, mapping_names))
  
  secret_name_group <- paste0(secret_prefix, "group")
  bleached_data[secret_name_group] <- factor(group_info$id)
  layer$mapping$group <- quo(!!sym(secret_name_group))
  
  # FIXME:
  # Contradictorily to the comment above, we need the original data to let the
  # layer be facetted. Probably, we can make here more efficient...
  if (calculate_per_facet) {
    layer$data <- dplyr::bind_cols(bleached_data, layer$data)
  } else {
    layer$data <- bleached_data
  }
  
  layer
}

default_unhighlighted_params <- list(
  # scales::alpha("grey", 0.7)
  colour = "#BEBEBEB2",
  fill = "#BEBEBEB2"
)

get_default_aes_param <- function(aes_param_name, geom, mapping) {
  # no default is available
  if (!aes_param_name %in% names(default_unhighlighted_params)) {
    return(NULL)
  }
  
  # if it is specified in mapping, it needs to be overriden
  if (aes_param_name %in% names(mapping)) {
    return(default_unhighlighted_params[[aes_param_name]])
  }
  
  # remove NULL default_aes (#85)
  non_null_default_aes <- purrr::compact(geom$default_aes)
  
  # if the geom has default value and is NA, use NA
  if (aes_param_name %in% names(non_null_default_aes) &&
      is.na(non_null_default_aes[[aes_param_name]])) {
    return(NA)
  }
  
  # otherwise, use the default grey
  default_unhighlighted_params[[aes_param_name]]
}

sieve_layer <- function(layer, group_info, predicates,
                        max_highlight = 5L,
                        use_group_by = NULL) {
  # If there are no predicates, do nothing.
  if (length(predicates) == 0) return(TRUE)
  
  # If use_group_by is NULL, infer it from whether group_key is NULL or not.
  use_group_by <- use_group_by %||% !is.null(group_info$id)
  
  # 1) If use_group_by is FALSE, do not use group_by().
  # 2) If use_group_by is TRUE and group IDs don't exist, use group_by().
  # 3) If use_group_by is TRUE but group IDs exist, show a warning and do not use group_by().
  if (use_group_by) {
    if (is.null(group_info$id)) {
      msg <- paste0(
        "Tried to calculate with group_by(), but there seems no groups.\n",
        "Please provide group, colour or fill aes.\n",
        "Falling back to ungrouped filter operation..."
      )
      warn(msg)
      
      use_group_by <- FALSE
    }
  }
  
  names(predicates) <- paste0("p", seq_along(predicates))
  
  # If use_group_by is TRUE, try to calculate grouped
  if (use_group_by) {
    tryCatch({
      layer$data <- calculate_grouped(layer$data, predicates, max_highlight, group_info$id)
      # if this succeeds, return TRUE
      return(TRUE)
    },
    error = function(e) {
      msg <- paste0(
        "Tried to calculate with group_by(), but the calculation failed.\n",
        "Falling back to ungrouped filter operation..."
      )
      warn(msg)
    }
    )
  }
  
  # the grouped calculation failed or skipped, try ungrouped one.
  tryCatch({
    layer$data <- calculate_ungrouped(layer$data, predicates, max_highlight)
    return(TRUE)
  },
  error = function(e) {
    # do not warn here, but in ggplot_add.gg_highlighter()
    return(FALSE)
  }
  )
  
  FALSE
}

# TODO: integrate with sieve_layer
sieve_data <- function(data, mapping, predicates, group_info = NULL,
                       max_highlight = 5L, use_group_by = TRUE) {
  if (is.null(group_info)) {
    group_info <- calculate_group_info(data, mapping)
  }
  
  # If use_group_by is NULL, infer it from whether group_key is NULL or not.
  use_group_by <- use_group_by %||% !is.null(group_info$id)
  
  if (use_group_by) {
    tryCatch({
      return(calculate_grouped(data, predicates, max_highlight, group_info$id))
    },
    error = function(e) {
      msg <- paste0(
        "Tried to calculate with group_by(), but the calculation failed.\n",
        "Falling back to ungrouped filter operation..."
      )
      warn(msg)
    })
  }
  
  return(calculate_ungrouped(data, predicates, max_highlight))
}

calculate_grouped <- function(data, predicates, max_highlight, group_ids) {
  data_predicated <- data
  
  data_predicated <- dplyr::group_by(data_predicated, !!VERY_SECRET_COLUMN_NAME := !!group_ids)
  data_predicated <- dplyr::summarise(data_predicated, !!!predicates)
  
  group_with_multiple_result <- anyDuplicated(dplyr::pull(data_predicated, !!VERY_SECRET_COLUMN_NAME))
  if (!identical(group_with_multiple_result, 0L)) {
    abort("", "gghighlight_invalid_group_predicate_error")
  }
  
  cols <- choose_col_for_filter_and_arrange(data_predicated, VERY_SECRET_COLUMN_NAME)
  
  # Filter by the logical predicates.
  data_filtered <- dplyr::filter(data_predicated, !!!cols$filter)
  
  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols$arrange) > 0) {
    data_filtered <- dplyr::arrange(data_filtered, !!!cols$arrange)
    data_filtered <- utils::tail(data_filtered, max_highlight)
  }
  
  groups_filtered <- dplyr::pull(data_filtered, !!VERY_SECRET_COLUMN_NAME)
  
  data[group_ids %in% groups_filtered, , drop = FALSE]
}

calculate_ungrouped <- function(data, predicates, max_highlight) {
  data_predicated <- data
  
  data_predicated <- tibble::rowid_to_column(data_predicated, var = expr_text(VERY_SECRET_COLUMN_NAME))
  data_predicated <- dplyr::transmute(data_predicated, !!! predicates, !!VERY_SECRET_COLUMN_NAME)
  
  cols <- choose_col_for_filter_and_arrange(data_predicated, VERY_SECRET_COLUMN_NAME)
  
  # Filter by the logical predicates.
  data_filtered <- dplyr::filter(data_predicated, !!!cols$filter)
  
  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols$arrange) > 0) {
    data_filtered <- dplyr::filter_at(data_filtered, dplyr::vars(!!!cols$arrange), ~ !is.na(.))
    data_filtered <- dplyr::arrange(data_filtered, !!!cols$arrange)
    data_filtered <- utils::tail(data_filtered, max_highlight)
  }
  
  # sort to preserve the original order
  rowids_filtered <- sort(dplyr::pull(data_filtered, !!VERY_SECRET_COLUMN_NAME))
  
  data[rowids_filtered, , drop = FALSE]
}

choose_col_for_filter_and_arrange <- function(data, exclude_col) {
  # Do not use row IDs or group keys to arrange.
  data <- dplyr::select(data, -!!exclude_col)
  col_idx_lgl <- purrr::map_lgl(data, is.logical)
  col_idx_lst <- purrr::map_lgl(data, is.list)
  list(
    # Use logical columns for filter()
    filter = syms(names(data)[col_idx_lgl]),
    # Use other columns but lists for arrange() (arrange doesn't support list columns)
    arrange = syms(names(data)[!col_idx_lgl & !col_idx_lst])
  )
}

normalize_unhighlighted_params <- function(aes_params) {
  if (!is.list(aes_params)) {
    abort("unhighlighted_params must be a list.")
  }
  
  # color is an alias of colour
  if (!is.null(aes_params$color)) {
    aes_params$colour <- aes_params$colour %||% aes_params$color
    aes_params$color <- NULL
  }
  
  aes_params
}



# Utilities

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) {
    rhs
  } else {
    lhs
  }
}

get_facet_vars <- function(facet) {
  switch(class(facet)[1],
         "FacetGrid" = as_quosures(c(facet$params$rows, facet$params$cols)),
         "FacetWrap" = as_quosures(facet$params$facets),
         "FacetNull" = NULL,
         abort("Unknown facet class")
  )
}
  
  
  
  
generate_labelled_layer <- function(layers, group_infos, label_key, label_params, max_labels) {
  layer_for_label <- choose_layer_for_label(layers, group_infos, label_key)
  if (is.null(layer_for_label)) {
    return(NULL)
  }
  
  layer <- layer_for_label$layer
  label_key <- layer_for_label$label_key
  
  switch(class(layer$geom)[1],
         GeomLine = generate_label_for_line(layer, label_key, label_params, max_labels = max_labels),
         GeomPoint = generate_label_for_point(layer, label_key, label_params, max_labels = max_labels),
         # TODO: To distinguish NULL, return list() to hide guides here.
         #       But, can we use more explicit representation?
         GeomBar = list(),
         abort("Unsupported geom!")
  )
}

choose_layer_for_label <- function(layers, group_infos, label_key) {
  show_label_key <- FALSE
  if (quo_is_call(label_key)) {
    # if label_key is a call we can't check if the necessary variables exist in
    # the data. Just pray that the proper layer will be choosed... :pray:
    label_keys <- purrr::rerun(length(layers), label_key)
  } else if (quo_is_symbol(label_key)) {
    # If label_key is a symbol, some layer must have the key in their data.
    label_key_text <- quo_text(label_key)
    layers <- purrr::keep(layers, ~ label_key_text %in% names(.$data))
    label_keys <- purrr::rerun(length(layers), label_key)
  } else if (quo_is_null(label_key)) {
    # If label_key is not specified, some key might be usable for label.
    group_keys <- purrr::map(group_infos, "key")
    idx <- !purrr::map_lgl(group_keys, is.null)
    layers <- layers[idx]
    # group keys may be multiple, but use the first one for convenience.
    label_keys <- purrr::map(group_keys[idx], 1L)
    # display which key was chosen for label
    show_label_key <- TRUE
  } else {
    abort("Invalid label_key!")
  }
  
  # If no layer has labellable variable, give up labelling
  if (length(layers) == 0) {
    return(NULL)
  }
  
  # If there's line, point or bar geom, choose it. (bar geom doesn't generate labels, though).
  for (fun in list(is_identity_line, is_identity_point, is_bar)) {
    idx <- purrr::map_lgl(layers, fun)
    if (any(idx)) {
      label_key <- label_keys[idx][[1]]
      if (show_label_key) message("label_key: ", quo_text(label_key))
      return(list(layer = layers[idx][[1]], label_key = label_key))
    }
  }
  
  # Other geoms are currently unsupported.
  return(NULL)
}

is_identity_line <- function(x) {
  is_direct_class(x$stat, "StatIdentity") && is_direct_class(x$geom, "GeomLine")
}

is_identity_point <- function(x) {
  is_direct_class(x$stat, "StatIdentity") && is_direct_class(x$geom, "GeomPoint")
}

is_bar <- function(x) is_direct_class(x$geom, "GeomBar")

is_direct_class <- function(x, class) identical(class(x)[1], class)

generate_label_for_line <- function(layer, label_key, label_params, max_labels) {
  mapping <- layer$mapping
  mapping$label <- label_key
  
  x_key <- layer$mapping$x
  group_key <- layer$mapping$group %||% layer$mapping$colour
  
  # To restore the original group, extract group keys (I don't know this is really necessary, though...)
  group_key_orig <- dplyr::groups(layer$data)
  
  data <- dplyr::group_by(layer$data, !!group_key)
  if (dplyr::n_groups(data) > max_labels) {
    inform("Too many data series, skip labeling")
    return(list())
  }
  
  rightmost_points <- dplyr::filter(data, !!x_key == max(!!x_key))
  # max value can appear multiple times, so ensure only one row per group
  rightmost_points <- dplyr::slice(rightmost_points, 1)
  
  # restore the original group
  rightmost_points <- dplyr::group_by(rightmost_points, !!!group_key_orig)
  
  call_ggrepel_with_params(mapping, rightmost_points, label_params)
}

generate_label_for_point <- function(layer, label_key, label_params, max_labels) {
  if (nrow(layer$data) > max_labels) {
    inform("Too many data points, skip labeling")
    return(list())
  }
  
  mapping <- layer$mapping
  mapping$label <- label_key
  
  if (inherits(layer$position, "PositionJitter") && is.null(layer$position$seed)) {
    # FIXME when this is fixed on upstream: https://github.com/tidyverse/ggplot2/issues/2507
    position <- clone_position(layer$position)
    position$seed <- sample.int(.Machine$integer.max, 1L)
    layer$position <- position
  }
  
  label_params$position <- layer$position
  
  call_ggrepel_with_params(mapping, layer$data, label_params)
}


call_ggrepel_with_params <- function(mapping, data, params) {
  ggrepel_quo <- quo(ggrepel::geom_label_repel(mapping, data, !!!params))
  eval_tidy(ggrepel_quo)
}
  
  
  
  
  
  
  