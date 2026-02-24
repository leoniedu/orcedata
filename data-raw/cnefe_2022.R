library(dplyr)
library(arrow)
out_dir <- here::here(file.path("data-raw","cnefe", "2022"))



get_cnefe_urls <- function() {
  ftp <- "https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/UF"
  h <- rvest::read_html(ftp)
  elements <- rvest::html_elements(h, "a")
  files <- rvest::html_attr(elements, "href")
  filenameext <- files[ grepl(x = files, '.zip$') ]
  tibble::tibble(url=file.path(ftp,filenameext), uf_codigo=substr(filenameext,1,2), uf_sigla=substr(filenameext,4,5))
}


dir.create(out_dir, recursive = TRUE)
cnefe_urls <- get_cnefe_urls()%>%
  mutate(dest_file=file.path(out_dir,basename(url)))#%>%head(3)


res <- curl::multi_download(cnefe_urls$url,
                            destfiles = cnefe_urls$dest_file,
                            progress = TRUE, resume = TRUE,
                            timeout = 30*60, multiplex = TRUE, accept_encoding = c("gzip,deflate"), ssl_verifypeer=FALSE)

## might need to repeat!






library(readr)
#tempdir <- file.path(tempdir(), "cnefe", '2022')
#unlink(tempdir, recursive = TRUE)
#dest_files <- cnefe_urls$dest_file
dest_files <- dir(here::here("data-raw/cnefe/2022"), full.names = TRUE, pattern = "zip$")
for (k in dest_files) {
  print(k)
  cnefe_in <- readr::read_delim(file=k,
                                delim = ";",
                                locale = locale(decimal_mark = "."),
                                col_select=c(setor_cnefe=COD_SETOR, latitude=LATITUDE, longitude=LONGITUDE, nv_geo_coord=NV_GEO_COORD, especie_codigo=COD_ESPECIE))%>%
    count(setor=substr(setor_cnefe,1,15),latitude,longitude,nv_geo_coord, especie_codigo)
  cnefe_in%>%group_by(uf_codigo=substr(setor,1,2))%>%arrow::write_dataset(file.path(out_dir,"arrow"))
}


## "EPSG:4674" = Sirgas 2000
pontos_setores <- sf::st_as_sf(pontos_setores, crs=sf::st_crs("EPSG:4674"))%>%
  add_coordinates(latitude = "setor_cnefe_lat", longitude = "setor_cnefe_lon")

pontos_municipios <- sf::st_as_sf(pontos_municipios, crs=sf::st_crs("EPSG:4674"))%>%
  add_coordinates(latitude = "municipio_cnefe_lat", longitude = "municipio_cnefe_lon")

## verifica se há municipios faltando
## preliminares do censo
## ideia é pegar um setor denso para usar como lat lon do municipio
censo2022 <- censobr::read_tracts(year=2022, dataset="Preliminares")%>%
  collect()%>%
  group_by(code_muni, name_muni)%>%
  filter((V0001>500)|(V0001==max(V0001)))%>%
  arrange(area_km2)%>%
  slice(1)%>%
  transmute(code_muni, name_muni,ano=2022, code_tract)

munbr_falta <- censo2022%>%
  filter(!is.na(code_muni))%>%
  anti_join(pontos_municipios%>%transmute(code_muni=municipio_codigo), by="code_muni")
stopifnot(nrow(munbr_falta)==0)



# usethis::use_data(pontos_setores, overwrite = TRUE)
# usethis::use_data(pontos_municipios, overwrite = TRUE)


