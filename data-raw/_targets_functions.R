# =============================================================================
# Target functions for the orcedata {targets} pipeline
# Each function is named make_<target_name> and returns the object it creates.
# Side effects: usethis::use_data() or readr::write_rds() to persist outputs.
# =============================================================================

devtools::load_all()

# --- Helpers ------------------------------------------------------------------

#' Rename geobr's default "geom" geometry column to "geometry"
fix_geom <- function(sf_obj) {
  sf_obj |>
    sf::st_set_geometry("geom") |>
    dplyr::rename(geometry = geom)
}

# --- Phase 1: Geographic Base Data -------------------------------------------

make_ufs <- function(ano_ufs) {
  ufs <- geobr::read_state(year = ano_ufs) |>
    fix_geom() |>
    sf::st_centroid() |>
    orce::add_coordinates(lat = "uf_lat", lon = "uf_lon") |>
    rename_ibge()
  usethis::use_data(ufs, overwrite = TRUE)
  ufs
}

make_municipios_map <- function(ano_municipios) {
  municipios_map <- geobr::read_municipality(year = ano_municipios) |>
    fix_geom()
  readr::write_rds(municipios_map, "data-raw/municipios_map.rds")
  municipios_map
}

# --- Phase 2: CNEFE Processing -----------------------------------------------

make_pontos_setores <- function(municipios_map, ufs_filter, ano_cnefe, ano_setores,
                                limpar_cache = FALSE) {
  ufs <- if (is.null(ufs_filter)) {
    unique(substr(as.character(municipios_map$code_muni), 1, 2))
  } else {
    ufs_filter
  }

  all_setores <- list()
  all_centroids <- list()

  for (uf in ufs) {
    cli::cli_inform("Processing CNEFE for UF {uf}...")
    mun_codes <- municipios_map |>
      dplyr::filter(substr(as.character(code_muni), 1, 2) == uf) |>
      dplyr::pull(code_muni) |>
      unique()

    uf_setores <- list()
    for (mun_code in mun_codes) {
      cnefe <- tryCatch(
        cnefetools::read_cnefe(code_muni = as.integer(mun_code),
                               year = ano_cnefe, output = "arrow"),
        error = function(e) {
          cli::cli_warn("CNEFE not available for {mun_code}: {e$message}")
          NULL
        }
      )
      if (is.null(cnefe) || nrow(cnefe) == 0) next

      cnefe_df <- cnefe |>
        dplyr::collect() |>
        dplyr::mutate(
          setor = substr(as.character(COD_SETOR), 1, 15),
          dp = COD_ESPECIE == 1L
        )

      # Per sector: compute density point
      # Prefer residential addresses per sector, fall back to all addresses
      cnefe_by_setor <- split(cnefe_df, cnefe_df$setor)
      for (setor_data in cnefe_by_setor) {
        setor_res <- setor_data |> dplyr::filter(dp)
        setor_use <- if (nrow(setor_res) > 0) setor_res else setor_data
        setor_counted <- setor_use |>
          dplyr::count(setor, LATITUDE, LONGITUDE)
        setor_sf <- setor_counted |>
          sf::st_as_sf(
            coords = c("LONGITUDE", "LATITUDE"),
            crs = sf::st_crs("EPSG:4674")
          )
        ponto <- surveyzones::surveyzones_density_point(setor_sf, setor) |>
          orce::add_coordinates() |>
          sf::st_drop_geometry()
        ponto$n <- sum(setor_counted$n)
        uf_setores[[length(uf_setores) + 1]] <- ponto
      }
    }

    if (limpar_cache) {
      cache_dir <- tools::R_user_dir("cnefetools", which = "cache")
      if (dir.exists(cache_dir)) {
        uf_files <- list.files(cache_dir, pattern = paste0("^", uf), full.names = TRUE)
        if (length(uf_files) > 0) unlink(uf_files)
      }
    }

    all_setores <- c(all_setores, uf_setores)

    # Download census tract centroids for this UF (for fallback)
    cli::cli_inform("Downloading census tract centroids for UF {uf}...")
    uf_cent <- geobr::read_census_tract(year = ano_setores, code_tract = as.numeric(uf)) |>
      fix_geom() |>
      dplyr::mutate(setor = as.character(code_tract)) |>
      sf::st_centroid() |>
      dplyr::select(setor) |>
      orce::add_coordinates(lon = "setor_centroide_lon", lat = "setor_centroide_lat")
    all_centroids[[uf]] <- uf_cent
  }

  pontos_setores <- dplyr::bind_rows(all_setores) |>
    sf::st_as_sf(
      coords = c("lon", "lat"), remove = FALSE,
      crs = sf::st_crs("EPSG:4674")
    ) |>
    dplyr::rename(setor_cnefe_lon = lon, setor_cnefe_lat = lat)

  # Centroid fallback from downloaded census tracts
  setores_cent <- dplyr::bind_rows(all_centroids)

  pontos_setores <- pontos_setores |>
    dplyr::left_join(
      setores_cent |> sf::st_drop_geometry(),
      by = "setor"
    )

  # Add sectors that have no CNEFE data (centroid-only, n = 0)
  pontos_setores <- dplyr::bind_rows(
    pontos_setores,
    setores_cent |>
      dplyr::anti_join(
        pontos_setores |> sf::st_drop_geometry(),
        by = "setor"
      ) |>
      dplyr::mutate(n = 0L)
  )

  # Composite coordinates
  pontos_setores <- pontos_setores |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      setor_lon = dplyr::coalesce(setor_cnefe_lon, setor_centroide_lon),
      setor_lat = dplyr::coalesce(setor_cnefe_lat, setor_centroide_lat)
    ) |>
    dplyr::select(setor, setor_lon, setor_lat, dplyr::everything())

  usethis::use_data(pontos_setores, overwrite = TRUE)
  pontos_setores
}

make_pontos_municipios <- function(pontos_setores, municipios_map) {
  # Only use sectors with CNEFE data (n > 0) for density calculation
  pontos_setores_sf <- pontos_setores |>
    dplyr::filter(n > 0) |>
    dplyr::mutate(municipio_codigo = substr(setor, 1, 7)) |>
    sf::st_as_sf(
      coords = c("setor_lon", "setor_lat"),
      crs = sf::st_crs("EPSG:4674")
    )
  mun_codes <- unique(pontos_setores_sf$municipio_codigo)
  mun_points <- lapply(mun_codes, function(mc) {
    subset <- pontos_setores_sf |> dplyr::filter(municipio_codigo == mc)
    surveyzones::surveyzones_density_point(subset, municipio_codigo) |>
      orce::add_coordinates() |>
      sf::st_drop_geometry()
  })
  pontos_municipios <- dplyr::bind_rows(mun_points) |>
    sf::st_as_sf(
      crs = sf::st_crs("EPSG:4674"),
      coords = c("lon", "lat"), remove = FALSE
    ) |>
    dplyr::rename(municipio_cnefe_lon = lon, municipio_cnefe_lat = lat)

  # Fallback: geobr centroids for municipalities without CNEFE data
  municipios_geo_fallback <- municipios_map |>
    sf::st_centroid() |>
    dplyr::transmute(municipio_codigo = as.character(code_muni)) |>
    dplyr::anti_join(
      pontos_municipios |> sf::st_drop_geometry(),
      by = "municipio_codigo"
    ) |>
    orce::add_coordinates(
      lat = "municipio_cnefe_lat",
      lon = "municipio_cnefe_lon"
    )

  if (nrow(municipios_geo_fallback) > 0) {
    cli::cli_inform(
      "Adding {nrow(municipios_geo_fallback)} municipalities without CNEFE data (geobr centroids): {paste(municipios_geo_fallback$municipio_codigo, collapse = ', ')}"
    )
    pontos_municipios <- dplyr::bind_rows(pontos_municipios, municipios_geo_fallback)
  }

  usethis::use_data(pontos_municipios, overwrite = TRUE)
  pontos_municipios
}

# --- Phase 3: Municipality + Agency Data --------------------------------------

make_municipios_geo <- function(municipios_map) {
  municipios_map |>
    sf::st_centroid() |>
    rename_ibge()
}

make_pop <- function(ano_populacao) {
  sidrar::get_sidra(
    6579,
    variable = 9324,
    period = ano_populacao,
    geo = "City"
  ) |>
    dplyr::transmute(
      municipio_codigo = `Município (Código)`,
      municipio_populacao = Valor
    )
}

make_municipios <- function(pontos_municipios, municipios_geo, pop, ano_sedes) {
  pontos_municipios_sede_0 <- geobr::read_municipal_seat(year = ano_sedes) |>
    fix_geom()
  pontos_municipios_sede_1 <- pontos_municipios_sede_0 |>
    dplyr::transmute(municipio_codigo = as.character(code_muni))

  # Fallback 1: CNEFE points for municipalities without seat data
  codes_with_seat <- pontos_municipios_sede_1 |>
    sf::st_drop_geometry() |>
    dplyr::pull(municipio_codigo)

  pontos_municipios_sede <- pontos_municipios_sede_1 |>
    dplyr::bind_rows(
      pontos_municipios |>
        dplyr::select(municipio_codigo) |>
        dplyr::filter(!municipio_codigo %in% codes_with_seat)
    )

  # Fallback 2: geobr centroids for municipalities without seat or CNEFE
  codes_with_seat_or_cnefe <- pontos_municipios_sede |>
    sf::st_drop_geometry() |>
    dplyr::pull(municipio_codigo)

  pontos_municipios_sede <- pontos_municipios_sede |>
    dplyr::bind_rows(
      municipios_geo |>
        dplyr::select(municipio_codigo) |>
        dplyr::filter(!municipio_codigo %in% codes_with_seat_or_cnefe)
    ) |>
    orce::add_coordinates(lat = "municipio_sede_lat", lon = "municipio_sede_lon")

  municipios <- pontos_municipios_sede |>
    dplyr::left_join(
      municipios_geo |> sf::st_drop_geometry(),
      by = "municipio_codigo"
    ) |>
    dplyr::left_join(pop, by = "municipio_codigo")

  usethis::use_data(municipios, overwrite = TRUE)
  municipios
}

make_agencias_bdo <- function(municipios, bdo_agencias_file) {
  agencias_bdo_0 <- readr::read_csv2(
    bdo_agencias_file,
    col_types = "cccccc"
  ) |>
    rename_ibge() |>
    dplyr::mutate(
      agencia_lat = as.numeric(lat),
      agencia_lon = as.numeric(lon),
      lat = NULL, lon = NULL,
      municipio_codigo = substr(agencia_codigo, 1, 7)
    ) |>
    dplyr::left_join(
      municipios |>
        sf::st_drop_geometry() |>
        dplyr::select(municipio_codigo, municipio_sede_lat, municipio_sede_lon),
      by = "municipio_codigo"
    ) |>
    dplyr::select(-municipio_codigo) |>
    dplyr::transmute(
      agencia_codigo, agencia_nome,
      agencia_lon = dplyr::coalesce(agencia_lon, municipio_sede_lon),
      agencia_lat = dplyr::coalesce(agencia_lat, municipio_sede_lat)
    )

  # Fail if any agency still has NA coordinates after fallback
  na_coords <- agencias_bdo_0 |>
    dplyr::filter(is.na(agencia_lat) | is.na(agencia_lon))
  if (nrow(na_coords) > 0) {
    cli::cli_abort(c(
      "Agencies with NA coordinates after fallback:",
      paste(na_coords$agencia_codigo, collapse = ", ")
    ))
  }

  agencias_bdo <- agencias_bdo_0 |>
    dplyr::mutate(uf_codigo = substr(agencia_codigo, 1, 2)) |>
    sf::st_as_sf(
      coords = c("agencia_lon", "agencia_lat"),
      remove = FALSE,
      crs = sf::st_crs("EPSG:4674")
    )

  usethis::use_data(agencias_bdo, overwrite = TRUE)
  agencias_bdo
}

make_agencias_bdo_mun <- function(agencias_bdo, pontos_municipios, bdo_grid_file) {
  agencias_bdo_mun <- readr::read_csv(
    bdo_grid_file,
    col_types = "c"
  ) |>
    rename_ibge() |>
    dplyr::mutate(
      agencia_codigo = format(as.numeric(agencia_codigo), scientific = FALSE)
    ) |>
    dplyr::rename(uf_sigla = uf, agencia_nome_bdo = agencia_nome) |>
    dplyr::full_join(
      agencias_bdo |>
        sf::st_drop_geometry() |>
        dplyr::select(-agencia_lat, -agencia_lon),
      by = "agencia_codigo"
    )

  # Remove known invalid agency (CIPÓ)
  agencias_bdo_mun <- agencias_bdo_mun |>
    dplyr::filter(agencia_codigo != "290790500")

  stopifnot(
    nrow(agencias_bdo_mun |> dplyr::anti_join(pontos_municipios, by = "municipio_codigo")) == 0
  )
  stopifnot(
    nrow(pontos_municipios |> dplyr::anti_join(agencias_bdo_mun, by = "municipio_codigo")) == 0
  )

  usethis::use_data(agencias_bdo_mun, overwrite = TRUE)
  agencias_bdo_mun
}

make_municipios_codigos <- function(ano_censo_tracts, ano_rm) {
  municipios_micro <- censobr::read_tracts(
    year = ano_censo_tracts,
    dataset = "Preliminares"
  ) |>
    dplyr::collect() |>
    rename_ibge() |>
    dplyr::filter(!is.na(municipio_codigo)) |>
    dplyr::distinct(municipio_codigo, micro_codigo)

  # Download RM composition
  hlink <- sprintf(
    "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/municipios_por_regioes_metropolitanas/Situacao_2020a2029/Composicao_RM_%d.xlsx",
    ano_rm
  )
  fname <- file.path(here::here("data-raw"), basename(hlink))
  utils::download.file(hlink, fname, mode = "wb")

  rm_data <- readxl::read_excel(fname, sheet = 1) |>
    dplyr::filter(grepl(pattern = "^RM ", x = LABEL_CATMETROPOL)) |>
    dplyr::distinct(municipio_codigo = COD_MUN, rm_codigo = COD_CATMETROPOL) |>
    dplyr::group_by(municipio_codigo) |>
    tidyr::nest(data = rm_codigo)

  aglomeracoes <- readxl::read_excel(fname, sheet = 2) |>
    dplyr::distinct(municipio_codigo = COD_MUN, aglomeracao_codigo = COD_CATAU)

  municipios_codigos <- municipios_micro |>
    dplyr::as_tibble() |>
    dplyr::left_join(aglomeracoes, by = "municipio_codigo") |>
    dplyr::left_join(rm_data, by = "municipio_codigo")

  usethis::use_data(municipios_codigos, overwrite = TRUE)
  municipios_codigos
}

make_agencias_mun <- function(agencias_bdo, agencias_bdo_mun, municipios_codigos) {
  agencias <- agencias_bdo |>
    dplyr::mutate(agencia_municipio_codigo = substr(agencia_codigo, 1, 7)) |>
    dplyr::left_join(
      municipios_codigos,
      by = c("agencia_municipio_codigo" = "municipio_codigo")
    )

  agencias_mun <- agencias |>
    dplyr::distinct(agencia_municipio_codigo, micro_codigo, aglomeracao_codigo, data) |>
    dplyr::left_join(
      agencias_bdo_mun |>
        dplyr::distinct(
          agencia_municipio_codigo = substr(agencia_codigo, 1, 7),
          municipio_codigo
        ),
      by = "agencia_municipio_codigo"
    )

  usethis::use_data(agencias_mun, overwrite = TRUE)
  agencias_mun
}

# --- Phase 4: OSRM Distance Calculations -------------------------------------

make_osrm_start <- function(osm_pbf, ...) {
  # ... args are upstream targets that must complete before starting OSRM
  osrm_local_start(osm_pbf)
  Sys.time()  # sentinel value
}

make_osrm_stop <- function(...) {
  # ... args are distance targets that must complete before stopping
  osrm_local_stop()
  Sys.time()
}

make_distancias_osrm <- function(agencias_bdo, agencias_mun,
                                  municipios, municipios_codigos,
                                  osrm_start, ufs_filter) {
  ufs <- if (is.null(ufs_filter)) {
    unique(agencias_bdo$uf_codigo)
  } else {
    ufs_filter
  }

  ag_ag_list <- vector(mode = "list", length = length(ufs))
  ag_mun_list <- vector(mode = "list", length = length(ufs))
  names(ag_ag_list) <- names(ag_mun_list) <- ufs

  for (uf in ufs) {
    cli::cli_inform("Processing distances for UF {uf}...")
    ag_uf <- agencias_bdo |> dplyr::filter(uf_codigo == uf)
    mun_uf <- municipios |> dplyr::filter(substr(municipio_codigo, 1, 2) == uf)

    # Combine agencies + municipalities into a single sf for a richer graph
    combined <- dplyr::bind_rows(
      ag_uf |> dplyr::transmute(id = agencia_codigo, tipo = "agencia"),
      mun_uf |> dplyr::transmute(id = municipio_codigo, tipo = "municipio")
    )

    res <- get_distancias_osrm(src = combined, completar = TRUE)

    # Split: agency-to-agency pairs
    ag_ag_list[[uf]] <- res |>
      dplyr::filter(tipo_orig == "agencia", tipo_dest == "agencia") |>
      dplyr::select(
        agencia_codigo_orig = id_orig,
        agencia_codigo_dest = id_dest,
        distancia_km, duracao_horas,
        snap_km_orig, snap_km_dest
      )

    # Split: agency-to-municipality pairs
    ag_mun_list[[uf]] <- res |>
      dplyr::filter(tipo_orig == "agencia", tipo_dest == "municipio") |>
      dplyr::select(
        agencia_codigo = id_orig,
        municipio_codigo = id_dest,
        distancia_km, duracao_horas
      )
  }

  # --- Agency-to-agency distances ---
  distancias_agencias_osrm <- dplyr::bind_rows(ag_ag_list)

  stopifnot(
    nrow(
      distancias_agencias_osrm |>
        dplyr::count(agencia_codigo_orig, agencia_codigo_dest) |>
        dplyr::filter(n > 1)
    ) == 0
  )

  # --- Agency-to-municipality distances ---
  distancias_agencias_municipios_osrm <- dplyr::bind_rows(ag_mun_list)

  stopifnot(
    nrow(
      distancias_agencias_municipios_osrm |>
        dplyr::count(agencia_codigo, municipio_codigo) |>
        dplyr::filter(n > 1)
    ) == 0
  )

  # --- Compute diária flags ---
  d <- distancias_agencias_municipios_osrm |>
    dplyr::mutate(agencia_municipio_codigo = substr(agencia_codigo, 1, 7)) |>
    dplyr::left_join(
      agencias_mun |>
        dplyr::distinct(agencia_municipio_codigo, micro_codigo, aglomeracao_codigo, data),
      by = "agencia_municipio_codigo"
    ) |>
    dplyr::left_join(
      agencias_mun |>
        dplyr::distinct(
          municipio_codigo,
          agencia_municipio_codigo_jurisdicao = agencia_municipio_codigo
        ),
      by = "municipio_codigo"
    ) |>
    dplyr::left_join(
      municipios_codigos,
      by = "municipio_codigo",
      suffix = c("_distancia", "_municipio")
    )

  du <- d |>
    dplyr::rename(distancia = data_distancia, municipio = data_municipio) |>
    tidyr::unnest(c(distancia, municipio), names_sep = "_", keep_empty = TRUE)

  duf <- du |>
    dplyr::group_by(
      municipio_codigo,
      agencia_municipio_codigo_distancia = substr(agencia_codigo, 1, 7)
    ) |>
    dplyr::summarise(
      jurisdicao_agencia = any(
        agencia_municipio_codigo_jurisdicao == agencia_municipio_codigo_distancia
      ),
      jurisdicao_micro = any(micro_codigo_distancia == micro_codigo_municipio),
      jurisdicao_rm = any(municipio_rm_codigo == distancia_rm_codigo),
      jurisdicao_aglo = any(
        aglomeracao_codigo_municipio == aglomeracao_codigo_distancia
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("jurisdicao_"), ~ dplyr::coalesce(.x, FALSE)),
      sem_diaria = jurisdicao_agencia | jurisdicao_micro | jurisdicao_rm | jurisdicao_aglo
    )

  agencias_municipios_diaria <- distancias_agencias_municipios_osrm |>
    dplyr::mutate(agencia_municipio_codigo = substr(agencia_codigo, 1, 7)) |>
    dplyr::left_join(
      duf |>
        dplyr::mutate(
          agencia_municipio_codigo = agencia_municipio_codigo_distancia,
          diaria_municipio = !sem_diaria
        ),
      by = c("municipio_codigo", "agencia_municipio_codigo")
    ) |>
    dplyr::select(agencia_codigo, municipio_codigo, diaria_municipio)

  # Unroutable agencies: default to diaria_municipio = TRUE
  missing_ag <- setdiff(
    agencias_bdo$agencia_codigo,
    unique(agencias_municipios_diaria$agencia_codigo)
  )
  if (length(missing_ag) > 0) {
    all_mun <- unique(agencias_municipios_diaria$municipio_codigo)
    missing_rows <- tidyr::expand_grid(
      agencia_codigo = missing_ag,
      municipio_codigo = all_mun
    ) |>
      dplyr::mutate(diaria_municipio = TRUE)
    agencias_municipios_diaria <- dplyr::bind_rows(
      agencias_municipios_diaria,
      missing_rows
    )
    cli::cli_inform(
      "Added {length(missing_ag)} unroutable agencies to diaria table: {paste(missing_ag, collapse = ', ')}"
    )
  }

  # Combined distances + diária
  distancias_agencias_municipios_diaria <- distancias_agencias_municipios_osrm |>
    dplyr::left_join(
      agencias_municipios_diaria,
      by = c("agencia_codigo", "municipio_codigo")
    )

  usethis::use_data(distancias_agencias_osrm, overwrite = TRUE)
  usethis::use_data(distancias_agencias_municipios_osrm, overwrite = TRUE)
  usethis::use_data(agencias_municipios_diaria, overwrite = TRUE)
  usethis::use_data(distancias_agencias_municipios_diaria, overwrite = TRUE)

  # Return sentinel for dependency tracking
  Sys.time()
}

# --- Pipeline Metadata -------------------------------------------------------

make_pipeline_metadata <- function(osrm_stop = NULL, ...) {
  params <- list(...)
  pipeline_metadata <- tibble::tibble(
    parametro = names(params),
    valor = vapply(params, function(x) {
      if (is.null(x)) "NULL" else paste(x, collapse = ", ")
    }, character(1))
  )

  # Compute execution time from targets metadata if available
  duracao <- tryCatch({
    meta <- targets::tar_meta()
    t_start <- min(meta$time, na.rm = TRUE)
    t_end <- max(meta$time, na.rm = TRUE)
    n_erros <- sum(meta$error != "" & !is.na(meta$error))
    list(
      inicio = format(t_start, "%Y-%m-%d %H:%M:%S"),
      fim = format(t_end, "%Y-%m-%d %H:%M:%S"),
      duracao_min = round(as.numeric(difftime(t_end, t_start, units = "mins")), 1),
      erros = n_erros
    )
  }, error = function(e) NULL)

  run_info <- tibble::tibble(
    parametro = c("gerado_em", "orcedata_versao", "R_versao", "status"),
    valor = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      as.character(utils::packageVersion("orcedata")),
      paste(R.version$major, R.version$minor, sep = "."),
      "completo"
    )
  )

  if (!is.null(duracao)) {
    run_info <- dplyr::bind_rows(run_info, tibble::tibble(
      parametro = c("pipeline_inicio", "pipeline_fim", "duracao_minutos", "erros"),
      valor = c(duracao$inicio, duracao$fim, as.character(duracao$duracao_min),
                as.character(duracao$erros))
    ))
  }

  pipeline_metadata <- dplyr::bind_rows(pipeline_metadata, run_info)
  usethis::use_data(pipeline_metadata, overwrite = TRUE)
  pipeline_metadata
}

make_install <- function(...) {
  devtools::document()
  devtools::install(upgrade = "never", quick = TRUE)
  Sys.time()
}
