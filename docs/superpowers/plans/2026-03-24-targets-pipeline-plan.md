# Targets Pipeline Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Create a `{targets}` pipeline that orchestrates the orcedata data preparation workflow with configurable parameters, dependency tracking, and caching.

**Architecture:** Two new files: `_targets.R` (pipeline definition with configuration) and `data-raw/_targets_functions.R` (one function per target). Target functions call `usethis::use_data()` as a side effect and return the object for downstream consumption. OSRM lifecycle managed via dedicated start/stop targets with `cue = tar_cue(mode = "always")`.

**Tech Stack:** R, targets, geobr, cnefetools, sidrar, censobr, sf, dplyr, orce, osrm.backend

**Spec:** `docs/superpowers/specs/2026-03-24-targets-pipeline-design.md`

---

### Task 1: Add package dependencies to DESCRIPTION

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Add targets and data pipeline packages to Suggests**

Add these packages to the `Suggests:` section in `DESCRIPTION` (alphabetically):

```
Suggests:
    arrow,
    censobr,
    cnefetools,
    furrr,
    geobr,
    here,
    httr2,
    igraph,
    osrm.backend,
    readr,
    readxl,
    sidrar,
    targets,
    testthat (>= 3.0.0),
    tidyr,
    usethis
```

- [ ] **Step 2: Verify DESCRIPTION is valid**

Run: `Rscript -e 'devtools::check(args = "--no-tests --no-examples --no-vignettes --no-manual")'`

Only check for DESCRIPTION parse errors — a full check is not needed.

- [ ] **Step 3: Commit**

```bash
git add DESCRIPTION
git commit -m "chore: add targets pipeline dependencies to Suggests"
```

---

### Task 2: Create target functions — Phase 1 (Geographic Base Data)

**Files:**
- Create: `data-raw/_targets_functions.R`

These are the simplest targets — they download data from geobr APIs and save results. Each function takes year parameters, calls `usethis::use_data()` or `readr::write_rds()`, and returns the object.

- [ ] **Step 1: Create `data-raw/_targets_functions.R` with Phase 1 functions**

```r
# =============================================================================
# Target functions for the orcedata {targets} pipeline
# Each function is named make_<target_name> and returns the object it creates.
# Side effects: usethis::use_data() or readr::write_rds() to persist outputs.
# =============================================================================

devtools::load_all()

# --- Phase 1: Geographic Base Data -------------------------------------------

make_ufs <- function(ano_ufs) {
  ufs <- geobr::read_state(year = ano_ufs) |>
    sf::st_centroid() |>
    orce::add_coordinates(lat = "uf_lat", lon = "uf_lon") |>
    rename_ibge()
  usethis::use_data(ufs, overwrite = TRUE)
  ufs
}

make_setores_map <- function(ano_setores) {
  setores_map <- geobr::read_census_tract(year = ano_setores, code_tract = "all")
  readr::write_rds(setores_map, "data-raw/setores_map.rds")
  setores_map
}

make_municipios_map <- function(ano_municipios) {
  municipios_map <- geobr::read_municipality(year = ano_municipios)
  readr::write_rds(municipios_map, "data-raw/municipios_map.rds")
  municipios_map
}
```

- [ ] **Step 2: Verify functions parse correctly**

Run: `Rscript -e 'source("data-raw/_targets_functions.R"); cat("OK\n")'`

Expected: `OK` (no parse errors)

- [ ] **Step 3: Commit**

```bash
git add data-raw/_targets_functions.R
git commit -m "feat: add Phase 1 target functions (geographic base data)"
```

---

### Task 3: Create target functions — Phase 2 (CNEFE Processing)

**Files:**
- Modify: `data-raw/_targets_functions.R`

The `pontos_setores` function merges the CNEFE download + sector density calculation into one target. It loops through municipalities per UF, uses `cnefetools::read_cnefe()`, computes `orce::ponto_densidade()`, and cleans up CNEFE cache. The output includes an `n` column (CNEFE address count) for downstream weighting.

Reference: `data-raw/cnefe_calcula_pontos_setores.R` (lines 19-39 for density logic, lines 72-89 for centroid fallback) and `data-raw/cnefe_calcula_pontos_municipios.R` (lines 10-33 for municipality aggregation).

- [ ] **Step 1: Add `make_pontos_setores` function**

Append to `data-raw/_targets_functions.R`:

```r
# --- Phase 2: CNEFE Processing -----------------------------------------------

make_pontos_setores <- function(municipios_map, setores_map, ufs_filter, ano_cnefe) {
  ufs <- if (is.null(ufs_filter)) {
    unique(substr(as.character(municipios_map$code_muni), 1, 2))
  } else {
    ufs_filter
  }

  all_setores <- list()

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
      # Group by coordinate and count (ponto_densidade requires an n column)
      setores_in_mun <- unique(cnefe_df$setor)
      for (setor_now in setores_in_mun) {
        setor_data <- cnefe_df |> dplyr::filter(setor == setor_now)
        # Prefer residential addresses
        setor_res <- setor_data |> dplyr::filter(dp)
        setor_use <- if (nrow(setor_res) > 0) setor_res else setor_data
        setor_counted <- setor_use |>
          dplyr::count(setor, LATITUDE, LONGITUDE)
        setor_sf <- setor_counted |>
          sf::st_as_sf(
            coords = c("LONGITUDE", "LATITUDE"),
            crs = sf::st_crs("EPSG:4674")
          )
        ponto <- orce::ponto_densidade(setor_sf, setor)
        ponto$n <- sum(setor_counted$n)
        uf_setores[[length(uf_setores) + 1]] <- ponto
      }
    }

    # Clean up cnefetools cache for this UF
    cache_dir <- tools::R_user_dir("cnefetools", which = "cache")
    if (dir.exists(cache_dir)) {
      uf_files <- list.files(cache_dir, pattern = paste0("^", uf), full.names = TRUE)
      if (length(uf_files) > 0) unlink(uf_files)
    }

    all_setores <- c(all_setores, uf_setores)
  }

  pontos_setores <- dplyr::bind_rows(all_setores) |>
    sf::st_as_sf(
      coords = c("lon", "lat"), remove = FALSE,
      crs = sf::st_crs("EPSG:4674")
    ) |>
    dplyr::rename(setor_cnefe_lon = lon, setor_cnefe_lat = lat)

  # Centroid fallback from setores_map
  setores_cent <- setores_map |>
    dplyr::mutate(setor = as.character(code_tract)) |>
    sf::st_centroid() |>
    dplyr::select(setor) |>
    orce::add_coordinates(lon = "setor_centroide_lon", lat = "setor_centroide_lat")

  pontos_setores <- pontos_setores |>
    dplyr::left_join(
      setores_cent |> sf::st_drop_geometry(),
      by = "setor"
    )

  # Add sectors that have no CNEFE data (centroid-only, n = 0)
  pontos_setores <- dplyr::bind_rows(
    pontos_setores,
    setores_cent |>
      dplyr::rename(geometry = geom) |>
      dplyr::anti_join(
        pontos_setores |> sf::st_drop_geometry(),
        by = "setor"
      ) |>
      dplyr::mutate(n = 0L)
  )

  # Composite coordinates
  pontos_setores <- pontos_setores |>
    dplyr::mutate(
      setor_lon = dplyr::coalesce(setor_cnefe_lon, setor_centroide_lon),
      setor_lat = dplyr::coalesce(setor_cnefe_lat, setor_centroide_lat)
    ) |>
    dplyr::select(setor, setor_lon, setor_lat, dplyr::everything())

  usethis::use_data(pontos_setores, overwrite = TRUE)
  pontos_setores
}
```

- [ ] **Step 2: Add `make_pontos_municipios` function**

Append to `data-raw/_targets_functions.R`:

```r
make_pontos_municipios <- function(pontos_setores, municipios_map) {
  # Only use sectors with CNEFE data (n > 0) for density calculation
  pontos_municipios <- pontos_setores |>
    dplyr::filter(n > 0) |>
    dplyr::mutate(municipio_codigo = substr(setor, 1, 7)) |>
    dplyr::group_by(municipio_codigo) |>
    dplyr::group_modify(~ {
      orce::ponto_densidade(.x, municipio_codigo)
    }) |>
    dplyr::ungroup() |>
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
```

- [ ] **Step 3: Verify functions parse correctly**

Run: `Rscript -e 'source("data-raw/_targets_functions.R"); cat("OK\n")'`

- [ ] **Step 4: Commit**

```bash
git add data-raw/_targets_functions.R
git commit -m "feat: add Phase 2 target functions (CNEFE processing)"
```

---

### Task 4: Create target functions — Phase 3 (Municipality + Agency Data)

**Files:**
- Modify: `data-raw/_targets_functions.R`

Reference: `data-raw/geobrcache.R` (lines 24-74 for municipios), `data-raw/agencias.R` (full file).

- [ ] **Step 1: Add municipality target functions**

Append to `data-raw/_targets_functions.R`:

```r
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
  pontos_municipios_sede_0 <- geobr::read_municipal_seat(year = ano_sedes)
  pontos_municipios_sede_1 <- pontos_municipios_sede_0 |>
    dplyr::transmute(municipio_codigo = as.character(code_muni)) |>
    dplyr::rename(geometry = geom)

  # Fallback 1: CNEFE points for municipalities without seat data
  existing_codes <- pontos_municipios_sede_1 |>
    sf::st_drop_geometry() |>
    dplyr::pull(municipio_codigo)

  pontos_municipios_sede <- pontos_municipios_sede_1 |>
    dplyr::bind_rows(
      pontos_municipios |>
        dplyr::select(municipio_codigo) |>
        dplyr::filter(!municipio_codigo %in% existing_codes)
    )

  # Fallback 2: geobr centroids for municipalities without seat or CNEFE
  existing_codes <- pontos_municipios_sede |>
    sf::st_drop_geometry() |>
    dplyr::pull(municipio_codigo)

  pontos_municipios_sede <- pontos_municipios_sede |>
    dplyr::bind_rows(
      municipios_geo |>
        dplyr::select(municipio_codigo) |>
        dplyr::filter(!municipio_codigo %in% existing_codes)
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
```

- [ ] **Step 2: Add agency target functions**

Append to `data-raw/_targets_functions.R`:

```r
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
```

- [ ] **Step 3: Verify functions parse correctly**

Run: `Rscript -e 'source("data-raw/_targets_functions.R"); cat("OK\n")'`

- [ ] **Step 4: Commit**

```bash
git add data-raw/_targets_functions.R
git commit -m "feat: add Phase 3 target functions (municipality + agency data)"
```

---

### Task 5: Create target functions — Phase 4 (OSRM Distance Calculations)

**Files:**
- Modify: `data-raw/_targets_functions.R`

Reference: `data-raw/distancias_agencias_osrm.R` and `data-raw/distancias_agencias_municipios_osrm.R`.

- [ ] **Step 1: Add OSRM lifecycle and distance target functions**

Append to `data-raw/_targets_functions.R`:

```r
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

make_distancias_agencias_osrm <- function(agencias_bdo, osrm_start, ufs_filter) {
  on.exit(tryCatch(osrm_local_stop(), error = function(e) NULL))

  ufs <- if (is.null(ufs_filter)) {
    unique(agencias_bdo$uf_codigo)
  } else {
    ufs_filter
  }

  distancias_list <- vector(mode = "list", length = length(ufs))
  names(distancias_list) <- ufs

  for (uf in ufs) {
    cli::cli_inform("Processing inter-agency distances for UF {uf}...")
    ag_uf <- agencias_bdo |> dplyr::filter(uf_codigo == uf)
    res <- get_distancias_osrm(src = ag_uf, completar = TRUE)
    distancias_list[[uf]] <- res |>
      dplyr::select(
        agencia_codigo_orig,
        agencia_codigo_dest,
        distancia_km,
        duracao_horas,
        snap_km_orig,
        snap_km_dest
      )
  }

  distancias_agencias_osrm <- dplyr::bind_rows(distancias_list)

  stopifnot(
    nrow(
      distancias_agencias_osrm |>
        dplyr::count(agencia_codigo_orig, agencia_codigo_dest) |>
        dplyr::filter(n > 1)
    ) == 0
  )

  usethis::use_data(distancias_agencias_osrm, overwrite = TRUE)
  distancias_agencias_osrm
}

make_distancias_agencias_mun_osrm <- function(agencias_bdo, agencias_mun,
                                               municipios, municipios_codigos,
                                               pontos_municipios, osrm_start,
                                               ufs_filter) {
  on.exit(tryCatch(osrm_local_stop(), error = function(e) NULL))

  ufs <- if (is.null(ufs_filter)) {
    unique(agencias_bdo$uf_codigo)
  } else {
    ufs_filter
  }

  distancias_list <- vector(mode = "list", length = length(ufs))
  names(distancias_list) <- ufs

  for (j in ufs) {
    cli::cli_inform("Processing agency-municipality distances for UF {j}...")
    ag_uf <- agencias_bdo |> dplyr::filter(uf_codigo == j)
    mun_uf <- municipios |> dplyr::filter(substr(municipio_codigo, 1, 2) == j)
    res <- get_distancias_osrm(src = ag_uf, dst = mun_uf, completar = TRUE)
    distancias_list[[as.character(j)]] <- res |>
      dplyr::select(
        agencia_codigo = agencia_codigo_orig,
        municipio_codigo = municipio_codigo_dest,
        distancia_km,
        duracao_horas
      )
  }

  distancias_agencias_municipios_osrm <- dplyr::bind_rows(distancias_list)

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

  usethis::use_data(distancias_agencias_municipios_osrm, overwrite = TRUE)
  usethis::use_data(agencias_municipios_diaria, overwrite = TRUE)
  usethis::use_data(distancias_agencias_municipios_diaria, overwrite = TRUE)

  # Return the primary dataset for dependency tracking
  distancias_agencias_municipios_osrm
}
```

- [ ] **Step 2: Verify functions parse correctly**

Run: `Rscript -e 'source("data-raw/_targets_functions.R"); cat("OK\n")'`

- [ ] **Step 3: Commit**

```bash
git add data-raw/_targets_functions.R
git commit -m "feat: add Phase 4 target functions (OSRM distances)"
```

---

### Task 6: Create `_targets.R` pipeline definition

**Files:**
- Create: `_targets.R`

- [ ] **Step 1: Create `_targets.R` with configuration and target list**

```r
# =============================================================================
# orcedata {targets} pipeline
#
# Usage:
#   targets::tar_make()          # run full pipeline
#   targets::tar_visnetwork()    # visualize DAG
#   targets::tar_outdated()      # check what needs re-running
# =============================================================================

library(targets)

# --- Configuration -----------------------------------------------------------
ufs_filter <- NULL # NULL = all 27 UFs; e.g. c("29","28") for testing

# Data years — changing any of these invalidates dependent targets
ano_ufs           <- 2020  # geobr::read_state()
ano_setores       <- 2022  # geobr::read_census_tract()
ano_municipios    <- 2024  # geobr::read_municipality()
ano_sedes         <- 2010  # geobr::read_municipal_seat()
ano_cnefe         <- 2022  # cnefetools::read_cnefe()
ano_censo_tracts  <- 2022  # censobr::read_tracts() (microregion codes)
ano_populacao     <- "2024" # sidrar::get_sidra() period
ano_rm            <- 2024  # Composição RM (IBGE geoftp)

# BDO source files
bdo_agencias_csv  <- "data-raw/bdo_agencias/agencia20260210.csv"
bdo_grid_csv      <- "data-raw/bdo_agencias/grid-export_20260210.csv"

# OSRM data
osm_pbf           <- "brazil-260323.osm.pbf"

# --- Load packages and functions ---------------------------------------------
tar_option_set(
  packages = c("dplyr", "sf"),
  error = "stop"
)

source("data-raw/_targets_functions.R")

# --- Target list --------------------------------------------------------------
list(
  # Phase 1: Geographic Base Data
  tar_target(ufs, make_ufs(ano_ufs)),
  tar_target(setores_map, make_setores_map(ano_setores)),
  tar_target(municipios_map, make_municipios_map(ano_municipios)),

  # Phase 2: CNEFE Processing
  tar_target(
    pontos_setores,
    make_pontos_setores(municipios_map, setores_map, ufs_filter, ano_cnefe)
  ),
  tar_target(
    pontos_municipios,
    make_pontos_municipios(pontos_setores, municipios_map)
  ),

  # Phase 3: Municipality + Agency Data
  tar_target(municipios_geo, make_municipios_geo(municipios_map)),
  tar_target(pop, make_pop(ano_populacao)),
  tar_target(
    municipios,
    make_municipios(pontos_municipios, municipios_geo, pop, ano_sedes)
  ),
  tar_target(bdo_agencias_file, bdo_agencias_csv, format = "file"),
  tar_target(bdo_grid_file, bdo_grid_csv, format = "file"),
  tar_target(
    agencias_bdo,
    make_agencias_bdo(municipios, bdo_agencias_file)
  ),
  tar_target(
    agencias_bdo_mun,
    make_agencias_bdo_mun(agencias_bdo, pontos_municipios, bdo_grid_file)
  ),
  tar_target(
    municipios_codigos,
    make_municipios_codigos(ano_censo_tracts, ano_rm)
  ),
  tar_target(
    agencias_mun,
    make_agencias_mun(agencias_bdo, agencias_bdo_mun, municipios_codigos)
  ),

  # Phase 4: OSRM Distance Calculations
  tar_target(
    osrm_start,
    make_osrm_start(osm_pbf, agencias_bdo, agencias_mun, municipios, pontos_municipios),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    distancias_agencias_osrm,
    make_distancias_agencias_osrm(agencias_bdo, osrm_start, ufs_filter)
  ),
  tar_target(
    distancias_agencias_mun_osrm,
    make_distancias_agencias_mun_osrm(
      agencias_bdo, agencias_mun, municipios, municipios_codigos,
      pontos_municipios, osrm_start, ufs_filter
    )
  ),
  tar_target(
    osrm_stop,
    make_osrm_stop(distancias_agencias_osrm, distancias_agencias_mun_osrm),
    cue = tar_cue(mode = "always")
  )
)
```

- [ ] **Step 2: Verify pipeline definition is valid**

Run: `Rscript -e 'targets::tar_validate(); cat("Pipeline valid\n")'`

Expected: `Pipeline valid` (no errors)

- [ ] **Step 3: Verify DAG structure**

Run: `Rscript -e 'targets::tar_glimpse()'`

Verify the output shows the expected dependency graph with all targets connected correctly.

- [ ] **Step 4: Add `_targets/` to `.gitignore`**

The `_targets/` directory stores targets cache and should not be committed. Check if `.gitignore` already has it, and add if missing:

```
_targets/
```

- [ ] **Step 5: Commit**

```bash
git add _targets.R .gitignore
git commit -m "feat: add targets pipeline definition with configurable parameters"
```

---

### Task 7: Smoke test with a single small UF

**Files:**
- Modify: `_targets.R` (temporarily set `ufs_filter`)

This task verifies the pipeline runs end-to-end for Phases 1-3 (no OSRM needed). Use Sergipe (UF "28") — smallest state, fewest municipalities.

- [ ] **Step 1: Set `ufs_filter` for testing**

In `_targets.R`, change:
```r
ufs_filter <- NULL
```
to:
```r
ufs_filter <- c("28") # Sergipe — smoke test
```

- [ ] **Step 2: Run Phase 1-3 targets only**

Run: `Rscript -e 'targets::tar_make(names = c("ufs", "setores_map", "municipios_map", "pontos_setores", "pontos_municipios", "municipios_geo", "pop", "municipios", "agencias_bdo", "agencias_bdo_mun", "municipios_codigos", "agencias_mun"))'`

This should complete without OSRM. Monitor for:
- geobr downloads succeeding
- cnefetools downloading CNEFE for Sergipe municipalities
- `pontos_setores` producing sector density points
- `municipios` having valid geometry (no NA coordinates)
- `agencias_bdo_mun` stopifnot checks passing

- [ ] **Step 3: Inspect results**

Run:
```r
Rscript -e '
  targets::tar_load(pontos_setores)
  cat("pontos_setores:", nrow(pontos_setores), "rows\n")
  targets::tar_load(municipios)
  cat("municipios:", nrow(municipios), "rows\n")
  cat("NA geometries:", sum(sf::st_is_empty(municipios)), "\n")
  targets::tar_load(agencias_bdo)
  cat("agencias_bdo:", nrow(agencias_bdo), "rows\n")
'
```

Verify: no NA geometries, reasonable row counts.

- [ ] **Step 4: Reset `ufs_filter` back to `NULL`**

In `_targets.R`, change back to:
```r
ufs_filter <- NULL # NULL = all 27 UFs; e.g. c("29","28") for testing
```

- [ ] **Step 5: Commit (if any fixes were needed)**

```bash
git add _targets.R data-raw/_targets_functions.R
git commit -m "fix: adjustments from smoke test"
```

---

### Task 8: Smoke test OSRM distance targets

**Files:**
- Modify: `_targets.R` (temporarily)

This tests Phase 4 with a small UF. Requires OSRM and the OSM PBF file.

- [ ] **Step 1: Set `ufs_filter` for testing**

In `_targets.R`:
```r
ufs_filter <- c("28") # Sergipe — smoke test
```

- [ ] **Step 2: Run full pipeline including OSRM targets**

Run: `Rscript -e 'targets::tar_make()'`

Monitor for:
- OSRM server starting successfully
- `distancias_agencias_osrm` computing inter-agency distances for UF 28
- `distancias_agencias_mun_osrm` computing agency-municipality distances + diária
- OSRM server stopping
- All three distance `.rda` files created in `data/`

- [ ] **Step 3: Inspect distance results**

Run:
```r
Rscript -e '
  targets::tar_load(distancias_agencias_osrm)
  cat("distancias_agencias_osrm:", nrow(distancias_agencias_osrm), "rows\n")
  targets::tar_load(distancias_agencias_mun_osrm)
  cat("distancias_agencias_mun_osrm:", nrow(distancias_agencias_mun_osrm), "rows\n")
  load("data/agencias_municipios_diaria.rda")
  cat("agencias_municipios_diaria:", nrow(agencias_municipios_diaria), "rows\n")
'
```

- [ ] **Step 4: Reset `ufs_filter` and commit**

```r
ufs_filter <- NULL
```

```bash
git add _targets.R data-raw/_targets_functions.R
git commit -m "fix: adjustments from OSRM smoke test"
```
