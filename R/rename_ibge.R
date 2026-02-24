#' Padroniza Nomes de Colunas para o Formato IBGE
#'
#' Esta função padroniza os nomes das colunas de um `data.frame` para o formato utilizado pelo IBGE,
#' utilizando um dicionário de nomes comuns e seus equivalentes padronizados.
#'
#' @param x Um `data.frame` com colunas a serem renomeadas.
#'
#' @return O `data.frame` com os nomes das colunas padronizados.
#'
#' @details
#' A função realiza as seguintes etapas:
#' 1. Padroniza os nomes das colunas para minúsculas e remove caracteres especiais
#'    usando `janitor::clean_names()`.
#' 2. Renomeia as colunas usando um dicionário pré-definido de nomes comuns e seus
#'    equivalentes padronizados.
#' 3. Substitui "longitude" por "lon" e "latitude" por "lat" nos nomes das colunas.
#' 4. Converte as colunas que contêm códigos (e.g., `cod_setor`, `cod_uf`) para o tipo `character`.
#'
#' @examples
#' df <- data.frame(
#'   COD_SETOR = c("1234567", "8901234"),
#'   NOME_muni = c("São Paulo", "Rio de Janeiro"),
#'   abbrev_state = c("SP", "RJ")
#' )
#'
#' df_padronizado <- rename_ibge(df)
#' print(df_padronizado)
#' #   setor        municipio_nome uf_sigla
#' # 1 1234567        São Paulo       SP
#' # 2 8901234 Rio de Janeiro       RJ
#'
#' @export
rename_ibge <- function(x) {
  lookup <- c(setor='cod_setor',
              setor='code_tract',
              setor="controle",
              uf_codigo='cod_uf',
              uf_codigo='code_state',
              uf_codigo='cd_uf',
              uf_nome='nome_uf',
              uf_sigla='abbrev_state',
              uf_sigla='sigla_uf',
              aglomeracao_codigo='cod_catau',
              aglomeracao_nome='nome_catau',
              municipio_codigo="cod_mun",
              municipio_codigo="cod_municipio",
              municipio_codigo="code_muni",
              municipio_codigo="cd_mun",
              micro_codigo="code_micro",
              micro_nome="name_micro",
              meso_codigo="code_meso",
              meso_nome="name_meso",
              municipio_nome="name_muni",
              uf_nome="name_state",
              distrito_codigo='code_district',
              distrito_codigo='cd_dist',
              distrito_nome='name_district',
              distrito_nome='nome_dist',
              subdistrito_codigo="code_subdistrict",
              subdistrito_nome='name_subdistrict',
              subdistrito_codigo="cd_subdist",
              subdistrito_nome='nome_subdi',
              municipio_nome="municipio",
              municipio_nome="nome_mun",
              agencia_codigo='cod_ag',
              agencia_codigo='cod_agenci',
              agencia_codigo='cod_agencia',
              agencia_nome='agencia',
              regiao_codigo='code_region',
              regiao_nome='name_region',
              ano='year',
              agencia_nome='nome_agenc',
              situacao_tipo='tipo_sit')
  newx <- x |>
    janitor::clean_names() |>
    dplyr::rename(dplyr::any_of(lookup)) |>
    dplyr::rename_with(.fn = function(x) {
      x <- gsub("longitude", "lon", x)
      gsub("latitude", "lat", x)
      }) |>
    dplyr::mutate(across(dplyr::any_of(tidyselect::matches("_codigo|^upa$|^setor$")), as.character))
  newx
}
