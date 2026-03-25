#' IBGE Agency Master Data (BDO)
#'
#' Spatial dataset with IBGE agency locations and metadata, sourced from
#' BDO (Banco de Dados Operacional — IBGE's internal operational database).
#'
#' @format A `sf` data frame with 560 rows and 5 columns:
#' \describe{
#'   \item{agencia_codigo}{Agency code (character, 9 digits)}
#'   \item{agencia_nome}{Agency name}
#'   \item{agencia_lon}{Longitude of the agency}
#'   \item{agencia_lat}{Latitude of the agency}
#'   \item{geometry}{Point geometry (sf)}
#' }
#' @source BDO — IBGE internal database (CSV de Agências)
"agencias_bdo"

#' Agency–Municipality Mapping (BDO)
#'
#' Maps each IBGE agency to the municipalities under its jurisdiction,
#' combining BDO grid export data with agency metadata.
#'
#' @format A tibble with 5,610 rows and 10 columns:
#' \describe{
#'   \item{uf_sigla}{State abbreviation}
#'   \item{agencia_codigo}{Agency code}
#'   \item{agencia_nome_bdo}{Agency name from BDO}
#'   \item{municipio_codigo}{Municipality code (7 digits)}
#'   \item{municipio_nome}{Municipality name}
#'   \item{uf_codigo}{State code}
#'   \item{agencia_nome}{Standardized agency name}
#'   \item{uorg}{Organizational unit code}
#' }
#' @source BDO — IBGE internal database (Relação de Municípios grid export)
"agencias_bdo_mun"

#' Agency–Municipality Administrative Relationships
#'
#' Derived from `agencias_bdo`, filtered to municipalities included in the
#' collection network, with administrative grouping codes.
#'
#' @format A tibble with 5,572 rows and 5 columns:
#' \describe{
#'   \item{agencia_municipio_codigo}{Municipality code of the agency seat}
#'   \item{micro_codigo}{Microregion code}
#'   \item{aglomeracao_codigo}{Urban agglomeration code}
#'   \item{data}{Reference date}
#'   \item{municipio_codigo}{Municipality code}
#' }
"agencias_mun"

#' Agency–Municipality Daily Allowance (Diária) Requirements
#'
#' Lookup table indicating whether overnight stays (diárias) are required
#' when an agency collects data in a given municipality.
#'
#' @format A data frame with 222,187 rows and 3 columns:
#' \describe{
#'   \item{agencia_codigo}{Agency code}
#'   \item{municipio_codigo}{Municipality code}
#'   \item{diaria_municipio}{Logical: TRUE if a diária is required}
#' }
"agencias_municipios_diaria"

#' Collection Structure Changes (RCD 2025/31)
#'
#' Records of municipality reassignments between IBGE agencies following
#' administrative change RCD 2025/31.
#'
#' @format A tibble with 31 rows and 7 columns:
#' \describe{
#'   \item{municipio_nome}{Municipality name}
#'   \item{agencia_nome_original}{Original agency name}
#'   \item{agencia_nome_nova}{New agency name}
#'   \item{alteracao_diarias}{Change in daily allowance requirement}
#'   \item{municipio_codigo}{Municipality code}
#'   \item{agencia_codigo_original}{Original agency code}
#'   \item{agencia_codigo_nova}{New agency code}
#' }
"alteracoes_rcd202531"

#' Agency Assistance Areas (Bahia)
#'
#' Maps IBGE agencies in Bahia to their assigned assistance/support area names.
#'
#' @format A tibble with 50 rows and 2 columns:
#' \describe{
#'   \item{agencia_codigo}{Agency code}
#'   \item{assistencia_nome}{Assistance area name}
#' }
"assistencias_ba"

#' Agency–Municipality Distances with Daily Allowance Flags
#'
#' Combines OSRM-calculated distances with daily allowance (diária) rules
#' for all agency–municipality pairs in Brazil.
#'
#' @format A data frame with 222,187 rows and 5 columns:
#' \describe{
#'   \item{agencia_codigo}{Agency code}
#'   \item{municipio_codigo}{Municipality code}
#'   \item{distancia_km}{Road distance in kilometers}
#'   \item{duracao_horas}{Travel duration in hours}
#'   \item{diaria_municipio}{Logical: TRUE if a diária is required}
#' }
#' @source Distances calculated via OSRM using municipality representative points
"distancias_agencias_municipios_diaria"

#' Agency–Municipality Road Distances (OSRM)
#'
#' Road distances and travel durations between all IBGE agencies and
#' Brazilian municipalities, calculated via OSRM routing.
#'
#' @format A data frame with 222,187 rows and 5 columns:
#' \describe{
#'   \item{agencia_codigo}{Agency code}
#'   \item{municipio_codigo}{Municipality code}
#'   \item{distancia_km}{Road distance in kilometers}
#'   \item{duracao_horas}{Travel duration in hours}
#'   \item{metodo}{Calculation method: `"osrm"` (road-routed), `"grafo"`
#'     (augmented graph), or `"euclidiano"` (Euclidean fallback)}
#' }
#' @source Open Source Routing Machine (OSRM)
"distancias_agencias_municipios_osrm"

#' Inter-Agency Road Distances (OSRM)
#'
#' Road distances and travel durations between pairs of IBGE agencies,
#' calculated via OSRM routing.
#'
#' @format A data frame with 24,316 rows and 7 columns:
#' \describe{
#'   \item{agencia_codigo_orig}{Origin agency code}
#'   \item{agencia_codigo_dest}{Destination agency code}
#'   \item{distancia_km}{Road distance in kilometers}
#'   \item{duracao_horas}{Travel duration in hours}
#'   \item{snap_km_orig}{Snap distance for origin (km from input to nearest road)}
#'   \item{snap_km_dest}{Snap distance for destination (km from input to nearest road)}
#'   \item{metodo}{Calculation method: `"osrm"` (road-routed), `"grafo"`
#'     (augmented graph), or `"euclidiano"` (Euclidean fallback)}
#' }
#' @source Open Source Routing Machine (OSRM)
"distancias_agencias_osrm"

#' Brazilian Municipalities (Spatial)
#'
#' Spatial data with municipal seat locations, names, and population
#' estimates for all Brazilian municipalities.
#'
#' @format A `sf` data frame with columns:
#' \describe{
#'   \item{municipio_codigo}{Municipality code (7 digits)}
#'   \item{municipio_sede_lon}{Longitude of the municipal seat}
#'   \item{municipio_sede_lat}{Latitude of the municipal seat}
#'   \item{municipio_nome}{Municipality name}
#'   \item{uf_codigo}{State code}
#'   \item{uf_sigla}{State abbreviation}
#'   \item{uf_nome}{State name}
#'   \item{regiao_codigo}{Region code}
#'   \item{municipio_populacao}{Estimated population (SIDRA table 6579)}
#' }
#' @source IBGE geographic base (geobr) and SIDRA population estimates
"municipios"

#' Municipality Administrative Codes
#'
#' Reference table linking municipality codes to microregion and urban
#' agglomeration codes, with reference date.
#'
#' @format A tibble with 5,570 rows and 4 columns:
#' \describe{
#'   \item{municipio_codigo}{Municipality code (7 digits)}
#'   \item{micro_codigo}{Microregion code}
#'   \item{aglomeracao_codigo}{Urban agglomeration code}
#'   \item{data}{Reference date}
#' }
#' @source IBGE geographic base 2022
"municipios_codigos"

#' Municipality Representative Points (CNEFE)
#'
#' High-density representative points for each Brazilian municipality,
#' derived from CNEFE (National Address Register). Used as origin/destination
#' points for distance calculations.
#'
#' @format A `sf` data frame with 5,570 rows and 4 columns:
#' \describe{
#'   \item{municipio_codigo}{Municipality code}
#'   \item{municipio_cnefe_lon}{Longitude of the CNEFE representative point}
#'   \item{municipio_cnefe_lat}{Latitude of the CNEFE representative point}
#'   \item{geometry}{Point geometry (sf)}
#' }
#' @source CNEFE — Cadastro Nacional de Endereços para Fins Estatísticos (IBGE)
"pontos_municipios"

#' Census Sector Representative Points (CNEFE)
#'
#' High-density representative points for Brazilian census sectors (setores
#' censitários), derived from CNEFE. Used for routing and cost calculations
#' in survey allocation optimization.
#'
#' @format A `sf` data frame with 454,130 rows and 8 columns:
#' \describe{
#'   \item{setor}{Census sector code}
#'   \item{setor_lon}{Longitude of the sector representative point}
#'   \item{setor_lat}{Latitude of the sector representative point}
#'   \item{setor_cnefe_lon}{Longitude of the CNEFE-based point}
#'   \item{setor_cnefe_lat}{Latitude of the CNEFE-based point}
#'   \item{setor_centroide_lon}{Longitude of the geometric centroid}
#'   \item{setor_centroide_lat}{Latitude of the geometric centroid}
#'   \item{geometry}{Point geometry (sf)}
#' }
#' @source CNEFE — Cadastro Nacional de Endereços para Fins Estatísticos (IBGE)
"pontos_setores"

#' Pipeline Metadata
#'
#' Records the parameters and environment used to generate the orcedata
#' package datasets via the targets pipeline.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{parametro}{Parameter name}
#'   \item{valor}{Parameter value (as character)}
#' }
#' @source Generated by the orcedata targets pipeline
"pipeline_metadata"

#' Brazilian States (UFs)
#'
#' Spatial boundaries and metadata for Brazilian states (Unidades Federativas).
#'
#' @format A `sf` data frame with 27 rows and 8 columns:
#' \describe{
#'   \item{uf_codigo}{State code}
#'   \item{uf_sigla}{State abbreviation}
#'   \item{uf_nome}{State name}
#'   \item{regiao_codigo}{Region code}
#'   \item{regiao_nome}{Region name}
#'   \item{uf_lon}{Longitude of the state representative point}
#'   \item{uf_lat}{Latitude of the state representative point}
#'   \item{geom}{Polygon geometry (sf)}
#' }
#' @source IBGE geographic base 2022
"ufs"
