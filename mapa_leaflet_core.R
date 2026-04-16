  # mapa_leaflet_core.R - datos y agregar_capas_mapa()

# ANTES de library(): si .Rprofile cargó tidyverse/purrr/maps, `map` es una función
# y leaflet::invokeMethod(map,...) hace map$x → error "closure is not subsettable".
suppressWarnings({
  for (pkg in c("tidyverse", "purrr", "maps")) {
    dn <- paste0("package:", pkg)
    while (dn %in% search()) {
      try(detach(dn, unload = TRUE, character.only = TRUE), silent = TRUE)
    }
  }
})

library(sf)
library(dplyr)
library(leaflet)
library(readr)
library(tidyr)
library(viridisLite)
library(stringr)
library(htmltools)
library(jsonlite)

# 1) Tabla índice PBA
df_pba <- read_csv("PBA indice.csv", show_col_types = FALSE)

# 2) Normalizar claves y construir PDA
df_pba <- df_pba %>%
  mutate(
    partido_pdo = as.character(partido_pdo),
    partida     = as.character(partida),
    PDA = paste0(
      sprintf("%03d", as.integer(partido_pdo)),
      sprintf("%06d", as.integer(partida))
    )
  )

# 3) Leer y unir todas las parcelas
partidos_unicos <- df_pba %>%
  distinct(partido_pdo) %>%
  pull(partido_pdo)

leer_parcela_partido <- function(pdo) {
  carpeta  <- sprintf("%03d_parcela", as.integer(pdo))
  ruta_shp <- file.path(carpeta, "110101.shp")
  
  if (!file.exists(ruta_shp)) {
    warning("No se encontró shapefile para partido ", pdo, " en ", ruta_shp)
    return(NULL)
  }
  
  shp <- st_read(ruta_shp, quiet = TRUE)
  
  shp %>%
    mutate(
      partido_pdo = as.character(as.integer(pdo)),
      PDA         = as.character(PDA)
    )
}

parc_todos <- purrr::map(partidos_unicos, leer_parcela_partido) %>%
  purrr::compact() %>%
  do.call(rbind, .)

# 4) Filtrar solo las parcelas que están en el índice PBA
parc_mias <- parc_todos %>%
  inner_join(
    df_pba %>% select(PBA_ID, PDA, partido_pdo, nombre_partido, C, S, MZ, PA, Descripción),
    by = "PDA"
  ) %>%
  mutate(partido_pdo = partido_pdo.y) %>%
  select(-partido_pdo.x, -partido_pdo.y)

# 5) Leer departamentos y calcular conteo de parcelas por partido
deptos <- st_read("000_departamentos/070121.shp", quiet = TRUE)

normalizar_nombre <- function(x) {
  x %>%
    toupper() %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

mapeo_nombres <- tibble(
  nombre_indice = c("Esteban Etcheberría", "Gral. Pueyrredón", "Gral Rodriguez", "José C.Paz"),
  nombre_shape = c("Esteban Echeverría", "General Pueyrredón", "General Rodríguez", "José C. Paz")
)

conteo_partidos <- parc_mias %>%
  st_drop_geometry() %>%
  mutate(
    nombre_normalizado = normalizar_nombre(nombre_partido),
    partido_pdo_indice = as.character(partido_pdo)
  ) %>%
  count(partido_pdo_indice, nombre_partido, nombre_normalizado, name = "n_parcelas") %>%
  rename(partido_pdo = partido_pdo_indice)

conteo_con_mapeo <- conteo_partidos %>%
  left_join(mapeo_nombres, by = c("nombre_partido" = "nombre_indice")) %>%
  mutate(
    nombre_shape_mapeo = if_else(!is.na(nombre_shape), nombre_shape, NA_character_)
  )

deptos_plot <- deptos %>%
  mutate(
    nombre_partido_shape = NAM,
    nombre_normalizado = normalizar_nombre(NAM),
    partido_pdo_shape = as.character(as.integer(CCA))
  ) %>%
  left_join(
    conteo_partidos %>% select(nombre_normalizado, nombre_partido, n_parcelas),
    by = "nombre_normalizado"
  ) %>%
  left_join(
    conteo_con_mapeo %>%
      filter(!is.na(nombre_shape_mapeo)) %>%
      select(nombre_shape_mapeo, nombre_partido_mapeo = nombre_partido, n_parcelas_mapeo = n_parcelas),
    by = c("nombre_partido_shape" = "nombre_shape_mapeo")
  ) %>%
  left_join(
    conteo_partidos %>% 
      select(partido_pdo, nombre_partido_codigo = nombre_partido, n_parcelas_codigo = n_parcelas),
    by = c("partido_pdo_shape" = "partido_pdo")
  ) %>%
  mutate(
    nombre_partido = case_when(
      !is.na(nombre_partido) ~ nombre_partido,
      !is.na(nombre_partido_mapeo) ~ nombre_partido_mapeo,
      !is.na(nombre_partido_codigo) ~ nombre_partido_codigo,
      TRUE ~ nombre_partido_shape
    ),
    n_parcelas = case_when(
      !is.na(n_parcelas) ~ n_parcelas,
      !is.na(n_parcelas_mapeo) ~ n_parcelas_mapeo,
      !is.na(n_parcelas_codigo) ~ n_parcelas_codigo,
      TRUE ~ 0L
    )
  ) %>%
  select(-nombre_normalizado, -partido_pdo_shape, 
         -nombre_partido_mapeo, -nombre_partido_codigo,
         -n_parcelas_mapeo, -n_parcelas_codigo) %>%
  replace_na(list(n_parcelas = 0))

# Orden de prioridad categoría de uso (mapa uso): si hay varias etiquetas, gana la de menor número
orden_categoria_uso <- c(
  "No AZB",
  "a definir",
  "en juicio",
  "Comodato GCBA",
  "Cedidos a terceros",
  "Comerciales",
  "Parroquias y anexos",
  "Escuela",
  "Instituciones diocesanas propias",
  "Cáritas",
  "Otros"
)
colores_uso_fijos <- c(
  "No AZB"                         = "#000000",
  "a definir"                      = "#FFCC00",
  "en juicio"                      = "#7B1FA2",
  "Comodato GCBA"                  = "#1565C0",
  "Cedidos a terceros"             = "#00897B",
  "Comerciales"                    = "#C2185B",
  "Parroquias y anexos"            = "#EF6C00",
  "Escuela"                        = "#2E7D32",
  "Instituciones diocesanas propias" = "#4E342E",
  "Cáritas"                        = "#C62828",
  "Otros"                          = "#9E9E9E"
)

priorizar_categoria_uso <- function(txt) {
  if (is.na(txt)) return("Otros")
  cu <- str_trim(as.character(txt))
  if (cu == "") return("Otros")
  partes <- str_split(cu, ",")[[1]] %>% str_trim()
  fl <- tolower(paste(c(cu, partes), collapse = " "))

  if (str_detect(fl, "no\\s*azb|noazb")) return("No AZB")
  if (str_detect(fl, "a\\s*definir")) return("a definir")
  if (str_detect(fl, "en juicio")) return("en juicio")
  if (str_detect(fl, "comodato")) return("Comodato GCBA")
  if (str_detect(fl, "cedidos") && str_detect(fl, "terceros")) return("Cedidos a terceros")
  if (str_detect(fl, "comercial")) return("Comerciales")
  if (str_detect(fl, "parroquia|parroquias y anexos")) return("Parroquias y anexos")
  if (str_detect(fl, "escuela")) return("Escuela")
  if (str_detect(fl, "instituciones diocesanas|diocesanas propias")) return("Instituciones diocesanas propias")
  if (str_detect(fl, regex("cáritas|caritas", ignore_case = TRUE))) return("Cáritas")
  "Otros"
}

# 6) Leer propiedades de CABA
# Encabezados en fila 3; columnas: Propiedad_ID, Descripción, Dirección oficial, Ubicación, Localidad, Lat, Long, etc.
propiedades_caba_raw <- read_csv("Tabla única v4.csv", skip = 2, show_col_types = FALSE)
# Normalizar nombres de columnas (quitar BOM, espacios) para que coincidan con el código
names(propiedades_caba_raw) <- str_trim(str_remove(names(propiedades_caba_raw), "^\\ufeff"))
propiedades_caba_raw <- propiedades_caba_raw %>%
  filter(Localidad == "CABA")

# Misma referencia en ambos mapas: total de filas CABA en la tabla única (antes del filtro por coordenadas)
n_propiedades_caba_ref <- nrow(propiedades_caba_raw)

propiedades_caba_raw <- propiedades_caba_raw %>%
  mutate(
    fuente_priorizada = purrr::map_chr(`Fuentes disponibles`, function(fuentes_str) {
      # Orden de prioridad para el color si hay varias fuentes: 1 Templos, 2 Entes dependientes, 3 Alquileres,
      # 4 Índice (GCBA / de propiedad), 5 Planilla Destinos (texto "Destinos" o "Destino" en Fuentes disponibles), 6 Relevamiento
      prioridades <- c("Templos", "Entes dependientes", "Alquileres", "Índice GCBA", "Índice de propiedad", "Destinos", "Relevamiento")
      pat_planilla_destinos <- regex("destinos?|planilla\\s*destinos?", ignore_case = TRUE)
      pat_indice <- regex("índice|indice|ndice\\s*gcba|índice\\s*de\\s*propiedad", ignore_case = TRUE)
      
      if (is.na(fuentes_str) || fuentes_str == "") {
        return("Sin fuente")
      }
      
      fuentes <- str_split(fuentes_str, "\\s*[,;]\\s*") %>%
        unlist() %>%
        str_trim()
      fuentes <- fuentes[nzchar(fuentes)]
      
      # Buscar cada prioridad en las fuentes (case-insensitive)
      for (prioridad in prioridades) {
        hit <- if (prioridad == "Destinos") {
          any(str_detect(fuentes, pat_planilla_destinos))
        } else {
          any(str_detect(fuentes, regex(prioridad, ignore_case = TRUE)))
        }
        if (hit) {
          # Unificar "Índice de propiedad" e "Índice GCBA" para un solo color
          if (prioridad == "Índice de propiedad") return("Índice GCBA")
          if (prioridad == "Destinos") return("Planilla Destinos")
          return(prioridad)
        }
      }
      
      # Si no coincide con ninguna prioridad, intentar reconocer la primera fuente
      if (length(fuentes) > 0) {
        primera_fuente <- str_trim(fuentes[1])
        primera_fuente_lower <- tolower(primera_fuente)
        
        if (str_detect(primera_fuente_lower, regex("templo", ignore_case = TRUE))) {
          return("Templos")
        } else if (str_detect(primera_fuente_lower, regex("entes dependientes|entes", ignore_case = TRUE))) {
          return("Entes dependientes")
        } else if (str_detect(primera_fuente_lower, regex("alquiler", ignore_case = TRUE))) {
          return("Alquileres")
        } else if (str_detect(primera_fuente_lower, pat_indice)) {
          return("Índice GCBA")
        } else if (str_detect(primera_fuente_lower, pat_planilla_destinos)) {
          return("Planilla Destinos")
        } else if (str_detect(primera_fuente_lower, regex("relevamiento|relev", ignore_case = TRUE))) {
          return("Relevamiento")
        } else {
          return("Sin fuente")
        }
      }
      return("Sin fuente")
    })
  )

propiedades_caba_flags <- propiedades_caba_raw %>%
  mutate(
    Lat_str = as.character(Lat),
    Long_str = as.character(Long),
    Lat_clean = if_else(
      str_detect(Lat_str, "\\."),
      paste0(
        str_extract(Lat_str, "^-?\\d+"),
        ".",
        str_replace_all(str_extract(Lat_str, "(?<=\\.).+"), "\\.", "")
      ),
      Lat_str
    ),
    Long_clean = if_else(
      str_detect(Long_str, "\\."),
      paste0(
        str_extract(Long_str, "^-?\\d+"),
        ".",
        str_replace_all(str_extract(Long_str, "(?<=\\.).+"), "\\.", "")
      ),
      Long_str
    ),
    Lat = as.numeric(Lat_clean),
    Long = as.numeric(Long_clean)
  ) %>%
  mutate(
    tiene_lat = !is.na(Lat),
    tiene_long = !is.na(Long),
    lat_valida = Lat > -35 & Lat < -34,
    long_valida = Long > -59 & Long < -58
  )

propiedades_caba <- propiedades_caba_flags %>%
  filter(tiene_lat & tiene_long & lat_valida & long_valida) %>%
  select(-Lat_str, -Long_str, -Lat_clean, -Long_clean, 
         -tiene_lat, -tiene_long, -lat_valida, -long_valida) %>%
  # Limpiar y normalizar fuente_priorizada (mismo orden de prioridad: Templos, Entes dependientes, Alquileres, Índice, Planilla Destinos, Relevamiento)
  mutate(
    fuente_priorizada = str_trim(as.character(fuente_priorizada)),
    fuente_priorizada = case_when(
      is.na(fuente_priorizada) | fuente_priorizada == "" ~ "Sin fuente",
      str_detect(tolower(fuente_priorizada), regex("templo", ignore_case = TRUE)) ~ "Templos",
      str_detect(tolower(fuente_priorizada), regex("entes dependientes|entes", ignore_case = TRUE)) ~ "Entes dependientes",
      str_detect(tolower(fuente_priorizada), regex("alquiler", ignore_case = TRUE)) ~ "Alquileres",
      str_detect(fuente_priorizada, regex("índice|indice|ndice\\s*gcba", ignore_case = TRUE)) ~ "Índice GCBA",
      str_detect(fuente_priorizada, regex("destinos?|planilla\\s*destinos?", ignore_case = TRUE)) ~ "Planilla Destinos",
      str_detect(tolower(fuente_priorizada), regex("relevamiento|relev", ignore_case = TRUE)) ~ "Relevamiento",
      TRUE ~ "Sin fuente"
    ),
    # Asignar color aquí para que Leaflet siempre lo tenga (evita puntos negros)
    color_fill = case_when(
      fuente_priorizada == "Templos" ~ "#FF6600",
      fuente_priorizada == "Entes dependientes" ~ "#9944CC",
      fuente_priorizada == "Alquileres" ~ "#0066CC",
      fuente_priorizada == "Índice GCBA" ~ "#FF0000",
      fuente_priorizada == "Planilla Destinos" ~ "#00AA00",
      fuente_priorizada == "Relevamiento" ~ "#66FF66",
      TRUE ~ "#888888"  # Sin fuente: gris
    ),
    categoria_uso_priorizada = purrr::map_chr(`Categoría de uso`, priorizar_categoria_uso),
    color_fill_uso = unname(colores_uso_fijos[as.character(categoria_uso_priorizada)])
  ) %>%
  mutate(
    color_fill_uso = if_else(
      is.na(color_fill_uso),
      unname(colores_uso_fijos["Otros"]),
      color_fill_uso
    )
  )

propiedades_caba_sin_coord <- propiedades_caba_flags %>%
  filter(!(tiene_lat & tiene_long & lat_valida & long_valida)) %>%
  select(-Lat_str, -Long_str, -Lat_clean, -Long_clean,
         -tiene_lat, -tiene_long, -lat_valida, -long_valida) %>%
  mutate(categoria_uso_priorizada = purrr::map_chr(`Categoría de uso`, priorizar_categoria_uso))

n_propiedades_caba_geo <- nrow(propiedades_caba)
n_propiedades_caba_sin_coord <- nrow(propiedades_caba_sin_coord)

max_items_popup_sin_coord <- 20
if (n_propiedades_caba_sin_coord > 0) {
  items_popup_sin_coord <- propiedades_caba_sin_coord %>%
    mutate(
      texto_item = paste0(
        "<li><b>", Propiedad_ID, "</b>: ",
        if_else(
          !is.na(Descripción) & str_trim(Descripción) != "",
          Descripción,
          "(sin descripción)"
        ),
        if_else(
          !is.na(`Dirección oficial`) & str_trim(`Dirección oficial`) != "",
          paste0(" - ", `Dirección oficial`),
          ""
        ),
        "</li>"
      )
    ) %>%
    pull(texto_item)

  items_popup_sin_coord_preview <- head(items_popup_sin_coord, max_items_popup_sin_coord)
  n_items_restantes_sin_coord <- n_propiedades_caba_sin_coord - length(items_popup_sin_coord_preview)
  texto_restantes_sin_coord <- if (n_items_restantes_sin_coord > 0) {
    paste0("<br><i>... y ", n_items_restantes_sin_coord, " más</i>")
  } else {
    ""
  }

  popup_caba_sin_coord <- paste0(
    "<b>CABA sin coordenadas válidas</b><br>",
    "Total CABA (tabla): ", n_propiedades_caba_ref, "<br>",
    "Georreferenciadas: ", n_propiedades_caba_geo, "<br>",
    "Sin coordenadas: ", n_propiedades_caba_sin_coord, "<br><br>",
    "<b>Listado (ID, descripción, dirección):</b>",
    "<ul style='max-height:220px;overflow:auto;padding-left:18px;margin:6px 0 0 0;'>",
    paste(items_popup_sin_coord_preview, collapse = ""),
    "</ul>",
    texto_restantes_sin_coord
  )
} else {
  popup_caba_sin_coord <- ""
}
# 7) Mapa: partidos coloreados (viridis) + parcelas + propiedades CABA
# transformar a WGS84 (lat/lon) para Leaflet
deptos_plot_ll <- st_transform(deptos_plot, 4326)
parc_mias_ll   <- st_transform(parc_mias,   4326)

# centroides de parcelas para clustering al hacer zoom out
parc_centroides <- st_centroid(parc_mias_ll)

# partidos con al menos una parcela
deptos_con_parcelas <- deptos_plot_ll %>%
  filter(n_parcelas > 0)

# usar nombre_partido del índice (no normalizado) para la paleta
partidos_sin_caba <- sort(unique(parc_mias_ll$nombre_partido))

# agregar CABA a la lista de partidos para la paleta, pero primero
partidos_con_parcelas <- c("CABA", partidos_sin_caba)

# paleta tipo viridis (buen contraste), un color por partido
# CABA tendrá un color destacado (naranja pastel), el resto usa viridis
# Crear vector de colores viridis para los partidos (sin CABA)
colores_viridis <- viridis(length(partidos_sin_caba), option = "D")

# Crear vector completo con nombres explícitos para asegurar mapeo correcto
# CABA en naranja pastel, resto en viridis
colores_partidos_nombrados <- setNames(
  c("#FFB347", colores_viridis),
  partidos_con_parcelas
)

# Crear función de paleta personalizada para asegurar mapeo correcto
# Esto evita problemas con colorFactor cuando hay nombres similares
pal_partidos_leaflet <- function(x) {
  resultado <- character(length(x))
  for (i in seq_along(x)) {
    if (x[i] == "CABA") {
      resultado[i] <- "#FFB347"
    } else {
      idx <- which(partidos_sin_caba == x[i])
      if (length(idx) > 0) {
        resultado[i] <- colores_viridis[idx]
      } else {
        resultado[i] <- "#808080"  # gris por defecto
      }
    }
  }
  return(resultado)
}

# También crear el objeto colorFactor para la leyenda
pal_partidos_leaflet_factor <- colorFactor(
  palette = colores_partidos_nombrados,
  domain  = partidos_con_parcelas
)

# Paleta para fuentes disponibles de propiedades CABA
# Orden de prioridad: 1 Templos, 2 Entes dependientes, 3 Alquileres, 4 Índice GCBA, 5 Planilla Destinos, 6 Relevamiento, Sin fuente
colores_fuentes <- c(
  "Templos" = "#FF6600",           # naranja intenso
  "Entes dependientes" = "#9944CC", # violeta
  "Alquileres" = "#0066CC",        # azul intenso
  "Índice GCBA" = "#FF0000",       # rojo intenso
  "Planilla Destinos" = "#00AA00", # verde intenso (referencia: Destinos en tabla)
  "Relevamiento" = "#66FF66",      # verde claro
  "Sin fuente" = "#888888"         # gris medio (evitar negro puro)
)

# Inicializar variables para que estén disponibles fuera del bloque if
fuentes_disponibles <- character(0)
labels_leyenda_fuente <- character(0)
colores_fuentes_completos <- colores_fuentes
categorias_para_leyenda_uso <- character(0)
labels_leyenda_uso <- character(0)
colores_uso_completos <- character(0)

if (n_propiedades_caba_ref > 0) {
  # Orden de prioridad (igual que en el case_when): nombres exactos que pueden aparecer en los datos
  orden_fuentes <- c("Templos", "Entes dependientes", "Alquileres", "Índice GCBA", "Planilla Destinos", "Relevamiento", "Sin fuente")
  
  # Dominio fijo: siempre usar todos los niveles para que colorFactor nunca devuelva NA
  # Así "Templos" siempre tiene el mismo color aunque haya variantes en los datos
  colores_fuentes_completos <- colores_fuentes
  
  # Función que asigna color por nombre de categoría
  pal_fuentes <- function(x) {
    x <- as.character(x)
    x_norm <- str_trim(x)
    x_norm[!x_norm %in% names(colores_fuentes)] <- "Sin fuente"
    resultado <- colores_fuentes[x_norm]
    resultado[is.na(resultado)] <- colores_fuentes["Sin fuente"]
    return(resultado)
  }
  
  # color_fill ya viene calculado en propiedades_caba (chunk anterior); no sobrescribir
  
  # Leyendas según puntos georreferenciados (puede haber filas en tabla sin coordenadas válidas)
  if (nrow(propiedades_caba) > 0) {
    fuentes_disponibles_raw <- unique(propiedades_caba$fuente_priorizada)
    fuentes_para_leyenda <- orden_fuentes[orden_fuentes %in% fuentes_disponibles_raw]
    if (length(fuentes_para_leyenda) == 0) fuentes_para_leyenda <- "Sin fuente"
    fuentes_disponibles <- fuentes_para_leyenda
    labels_leyenda_fuente <- purrr::map_chr(fuentes_disponibles, function(f) {
      n <- sum(propiedades_caba$fuente_priorizada == f, na.rm = TRUE)
      paste0(f, " (", n, ")")
    })
    
    cats_en_datos <- unique(propiedades_caba$categoria_uso_priorizada)
    categorias_para_leyenda_uso <- orden_categoria_uso[orden_categoria_uso %in% cats_en_datos]
    colores_uso_completos <- colores_uso_fijos[categorias_para_leyenda_uso]
    conteos_uso <- purrr::map_int(categorias_para_leyenda_uso, function(cat) {
      sum(propiedades_caba$categoria_uso_priorizada == cat, na.rm = TRUE)
    })
    ancho_conteo_uso <- max(nchar(as.character(conteos_uso)))
    labels_leyenda_uso <- purrr::map2_chr(categorias_para_leyenda_uso, conteos_uso, function(cat, n) {
      paste0(sprintf(paste0("%0", ancho_conteo_uso, "d"), n), " - ", cat)
    })
  } else {
    fuentes_disponibles <- character(0)
    labels_leyenda_fuente <- character(0)
    categorias_para_leyenda_uso <- character(0)
    labels_leyenda_uso <- character(0)
    colores_uso_completos <- character(0)
  }
  
  # leer shapefile de CABA desde la carpeta caba
  # intentar primero con export1.shp, si no existe probar con export2.shp
  ruta_caba_shp <- file.path("caba", "export1.shp")
  if (!file.exists(ruta_caba_shp)) {
    ruta_caba_shp <- file.path("caba", "export2.shp")
  }
  
  if (file.exists(ruta_caba_shp)) {
    caba_raw <- st_read(ruta_caba_shp, quiet = TRUE)
    
    # verificar el CRS y transformar si es necesario
    if (is.na(st_crs(caba_raw))) {
      # si no tiene CRS, asumir que es el mismo que los departamentos
      st_crs(caba_raw) <- st_crs(deptos)
    }
    
    caba_raw <- st_transform(caba_raw, 4326)
    
    # filtrar solo polígonos (POLYGON o MULTIPOLYGON) y eliminar geometrías vacías
    tipos_geom <- as.character(st_geometry_type(caba_raw))
    es_polygon <- tipos_geom %in% c("POLYGON", "MULTIPOLYGON")
    no_vacio <- !st_is_empty(caba_raw)
    
    caba_filtrado <- caba_raw[es_polygon & no_vacio, ]
    
    if (nrow(caba_filtrado) > 0) {
      # si hay múltiples features, unificar en un solo polígono
      if (nrow(caba_filtrado) > 1) {
        caba_geom <- st_geometry(caba_filtrado)
        caba_unificado <- st_union(caba_geom)
      } else {
        caba_unificado <- st_geometry(caba_filtrado)
      }
      
      # crear el objeto sf con el polígono unificado
      caba_polygon <- st_sf(
        nombre_partido = "CABA", 
        n_parcelas = n_propiedades_caba_ref, 
        geometry = caba_unificado
      )
    } else {
      # si no hay polígonos válidos, usar bounding box
      caba_bbox <- st_bbox(c(
        xmin = -58.5317, ymin = -34.7050,
        xmax = -58.3352, ymax = -34.5269
      ), crs = 4326)
      
      caba_polygon <- st_as_sfc(caba_bbox) %>%
        st_sf(nombre_partido = "CABA", n_parcelas = n_propiedades_caba_ref, .)
    }
  } else {
    # fallback: crear polígono simple de CABA (bounding box aproximado)
    caba_bbox <- st_bbox(c(
      xmin = -58.5317, ymin = -34.7050,
      xmax = -58.3352, ymax = -34.5269
    ), crs = 4326)
    
    caba_polygon <- st_as_sfc(caba_bbox) %>%
      st_sf(nombre_partido = "CABA", n_parcelas = n_propiedades_caba_ref, .)
  }
} else {
  # crear paleta vacía y polígono vacío si no hay propiedades
  fuentes_disponibles <- character(0)
  labels_leyenda_fuente <- character(0)
  colores_fuentes_completos <- colores_fuentes
  categorias_para_leyenda_uso <- character(0)
  labels_leyenda_uso <- character(0)
  colores_uso_completos <- character(0)
  pal_fuentes_factor <- colorFactor(palette = colores_fuentes, domain = character(0))
  # Crear función wrapper que siempre devuelva un color válido
  pal_fuentes <- function(x) {
    return(rep(colores_fuentes["Sin fuente"], length(x)))
  }
  caba_polygon <- st_sf(nombre_partido = "CABA", n_parcelas = 0, geometry = st_sfc(crs = 4326))
}

# bounding box para centrar el mapa en PBA
bb <- st_bbox(deptos_plot_ll)

# Panes Leaflet: puntos CABA debajo; contadores por partido; agregado PBA; resumen CABA (naranja/gris) arriba
PANE_GEOREF_PROP_CABA <- "georefPropCaba"
PANE_GEOREF_CLUSTER_PARTIDO <- "georefClusterPartido"
PANE_GEOREF_CLUSTER_AGREGADO <- "georefClusterAgregado"
PANE_GEOREF_CABA_RESUMEN <- "georefCabaResumen"

# Umbrales zoom (mismo valor en JS vía onRender)
zoom_puntos_caba <- 11L
zoom_partido_min <- 10L

opts_pane_prop_caba <- leaflet::markerOptions(pane = PANE_GEOREF_PROP_CABA)
opts_pane_cluster_partido <- leaflet::markerOptions(pane = PANE_GEOREF_CLUSTER_PARTIDO)
opts_pane_cluster_agregado <- leaflet::markerOptions(pane = PANE_GEOREF_CLUSTER_AGREGADO)
opts_pane_caba_resumen <- leaflet::markerOptions(pane = PANE_GEOREF_CABA_RESUMEN)

aplicar_vista <- function(mapa_leaflet) {
  if (length(bb) == 4 && 
      all(!is.na(bb)) && 
      bb["xmin"] < bb["xmax"] && 
      bb["ymin"] < bb["ymax"] &&
      bb["xmin"] > -180 && bb["xmax"] < 180 &&
      bb["ymin"] > -90 && bb["ymax"] < 90) {
    mapa_leaflet %>% fitBounds(
      lng1 = as.numeric(bb["xmin"]), 
      lat1 = as.numeric(bb["ymin"]),
      lng2 = as.numeric(bb["xmax"]), 
      lat2 = as.numeric(bb["ymax"])
    )
  } else {
    mapa_leaflet %>% setView(lng = -60, lat = -36, zoom = 7)
  }
}

crear_base <- function() {
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addMapPane(PANE_GEOREF_PROP_CABA, zIndex = 540L) %>%
    addMapPane(PANE_GEOREF_CLUSTER_PARTIDO, zIndex = 620L) %>%
    addMapPane(PANE_GEOREF_CLUSTER_AGREGADO, zIndex = 630L) %>%
    addMapPane(PANE_GEOREF_CABA_RESUMEN, zIndex = 700L) %>%
    aplicar_vista()
}

m <- crear_base()
m_uso <- crear_base()

agregar_capas_mapa <- function(mapa_inicial, modo) {
  stopifnot(modo %in% c("fuente", "uso"))
  stopifnot(inherits(mapa_inicial, "leaflet"))
  # Mapa "uso": una capa por categoría (nombre legible, sin prefijo) para el control de capas (sin crosstalk).
  grupos_capas_prop_caba <- if (modo == "fuente") {
    "Propiedades CABA"
  } else if (nrow(propiedades_caba) > 0) {
    orden_categoria_uso[orden_categoria_uso %in% unique(propiedades_caba$categoria_uso_priorizada)]
  } else {
    character(0)
  }

  categorias_uso_json <- if (modo == "uso" && length(grupos_capas_prop_caba) > 0) {
    jsonlite::toJSON(grupos_capas_prop_caba, auto_unbox = FALSE)
  } else {
    "[]"
  }

  sin_coord_conteos_json <- if (modo == "uso" && length(grupos_capas_prop_caba) > 0) {
    n_resto <- if (n_propiedades_caba_sin_coord > 0) {
      as.integer(sum(!propiedades_caba_sin_coord$categoria_uso_priorizada %in% grupos_capas_prop_caba, na.rm = TRUE))
    } else {
      0L
    }
    conteos_por_cat <- setNames(
      purrr::map_int(grupos_capas_prop_caba, function(cat) {
        if (n_propiedades_caba_sin_coord == 0) {
          0L
        } else {
          as.integer(sum(propiedades_caba_sin_coord$categoria_uso_priorizada == cat, na.rm = TRUE))
        }
      }),
      grupos_capas_prop_caba
    )
    lst <- c(as.list(conteos_por_cat), list(sinCoordResto = n_resto))
    jsonlite::toJSON(lst, auto_unbox = TRUE)
  } else {
    "{}"
  }

  esperar_contador_sin_coord_js <- if (modo == "uso" && n_propiedades_caba_sin_coord > 0) {
    "true"
  } else {
    "false"
  }

  mapa_inicial %>%
  {
    if (modo == "fuente") {
      mapa_fuente <- .
      mapa_fuente %>%
        addPolygons(
          data = deptos_plot_ll,
          fillColor   = ~ifelse(
            n_parcelas > 0,
            pal_partidos_leaflet(nombre_partido),
            "#DDDDDD"
          ),
          color       = "#666666",
          weight      = 1,
          fillOpacity = 0.6,
          label = ~paste0(
            if_else(!is.na(NAM), NAM, ""),
            "<br>Parcelas índice: ", n_parcelas
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color  = "#000000",
            bringToFront = TRUE
          ),
          options = pathOptions(interactive = FALSE),
          group = "Partidos"
        ) %>%
        {
          mapa_temp <- .
          if (n_propiedades_caba_ref > 0 && nrow(caba_polygon) > 0) {
            mapa_temp <- mapa_temp %>%
              addPolygons(
                data = caba_polygon,
                fillColor   = "#FFB347",
                color       = "#666666",
                weight      = 2,
                fillOpacity = 0.6,
                label = paste0(
                  "CABA<br>",
                  "Total CABA (tabla): ", n_propiedades_caba_ref, "<br>",
                  "Georreferenciadas: ", n_propiedades_caba_geo, "<br>",
                  "Sin coordenadas: ", n_propiedades_caba_sin_coord
                ),
                highlightOptions = highlightOptions(
                  weight = 3,
                  color  = "#000000",
                  bringToFront = TRUE
                ),
                options = pathOptions(interactive = FALSE),
                group = "Partidos"
              )
          }
          mapa_temp
        } %>%
        addPolygons(
          data        = parc_mias_ll,
          color       = "#000000",
          fillColor   = "#FF0000",
          weight      = 2.5,
          fillOpacity = 0.9,
          label = ~paste0(
            "<b>",
            if_else(
              !is.na(Descripción) & str_trim(Descripción) != "",
              str_trim(Descripción),
              "Parcela"
            ),
            "</b><br>",
            "PBA_ID: ", PBA_ID,
            "<br>Partido: ", nombre_partido,
            "<br>Partida: ", PDA,
            "<br>C/S/MZ/PA: ", C, "-", S, "-", MZ, "-", PA
          ),
          popup = ~paste0(
            "<b>",
            if_else(
              !is.na(Descripción) & str_trim(Descripción) != "",
              str_trim(Descripción),
              "Parcela"
            ),
            "</b><br>",
            "<b>Partido:</b> ", nombre_partido, " (", partido_pdo, ")<br>",
            "<b>Manzana:</b> ", MZ, "<br>",
            "<b>Parcela:</b> ", PA, "<br>",
            "<b>Circunscripción:</b> ", C, "<br>",
            "<b>Sección:</b> ", S, "<br>",
            "<b>Partida:</b> ", PDA, "<br>",
            "<b>PBA_ID:</b> ", PBA_ID
          ),
          highlightOptions = highlightOptions(
            weight = 4,
            color  = "#FFFF00",
            fillOpacity = 1,
            bringToFront = TRUE
          ),
          group = "Parcelas (polígonos)"
        )
    } else {
      mapa_uso_base <- .
      if (n_propiedades_caba_ref > 0 && nrow(caba_polygon) > 0) {
        mapa_uso_base <- mapa_uso_base %>%
          addPolygons(
            data = caba_polygon,
            fillColor   = "#FFB347",
            color       = "#666666",
            weight      = 2,
            fillOpacity = 0.45,
            label = paste0(
              "CABA<br>",
              "Total CABA (tabla): ", n_propiedades_caba_ref, "<br>",
              "Georreferenciadas: ", n_propiedades_caba_geo, "<br>",
              "Sin coordenadas: ", n_propiedades_caba_sin_coord
            ),
            highlightOptions = highlightOptions(
              weight = 3,
              color  = "#000000",
              bringToFront = TRUE
            ),
            options = pathOptions(interactive = FALSE),
            group = "CABA (contorno)"
          )
      }
      if (n_propiedades_caba_sin_coord > 0 && nrow(caba_polygon) > 0) {
        centroide_caba_sin_coord_uso <- caba_polygon %>%
          st_centroid() %>%
          st_coordinates() %>%
          as.data.frame() %>%
          rename(lng = X, lat = Y)
        if (nrow(centroide_caba_sin_coord_uso) > 0) {
          lng_gris_u <- centroide_caba_sin_coord_uso$lng[1] + 0.028
          lat_gris_u <- centroide_caba_sin_coord_uso$lat[1] - 0.024
          mapa_uso_base <- leaflet::addCircleMarkers(
              map = mapa_uso_base,
              lng = lng_gris_u,
              lat = lat_gris_u,
              radius = 14,
              stroke = TRUE,
              color = "#FFFFFF",
              weight = 2,
              fillOpacity = 0.95,
              fillColor = "#7A7A7A",
              label = as.character(n_propiedades_caba_sin_coord),
              labelOptions = labelOptions(
                noHide = TRUE,
                direction = "center",
                textOnly = TRUE,
                style = list(
                  "color" = "white",
                  "font-weight" = "bold",
                  "font-size" = "13px",
                  "text-align" = "center"
                )
              ),
              popup = popup_caba_sin_coord,
              options = opts_pane_caba_resumen,
              group = "CABA sin coordenadas",
              layerId = "caba_sin_coord_contador"
            )
        }
      }
      mapa_uso_base
    }
  } %>%
  
  # Círculos de conteo (mapa PBA / fuente). Modo uso: solo CABA; contador total naranja (Clusters agregados) para zoom out.
  {
    mapa_temp <- .
    if (modo != "fuente") {
      if (n_propiedades_caba_ref > 0 && nrow(caba_polygon) > 0) {
        centroide_caba_agg_uso <- caba_polygon %>%
          st_centroid() %>%
          st_coordinates() %>%
          as.data.frame() %>%
          rename(lng = X, lat = Y)
        if (nrow(centroide_caba_agg_uso) > 0) {
          mapa_temp <- leaflet::addCircleMarkers(
            map = mapa_temp,
            lng = centroide_caba_agg_uso$lng[1],
            lat = centroide_caba_agg_uso$lat[1],
            radius = 28,
            stroke = TRUE,
            color = "#FFFFFF",
            weight = 3,
            fillOpacity = 0.95,
            fillColor = "#FFB347",
            label = as.character(n_propiedades_caba_ref),
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = TRUE,
              style = list(
                "color" = "white",
                "font-weight" = "bold",
                "font-size" = "18px",
                "text-align" = "center"
              )
            ),
            popup = paste0(
              "<b>CABA</b><br>",
              "Total CABA (tabla): ", n_propiedades_caba_ref, "<br>",
              "Georreferenciadas: ", n_propiedades_caba_geo, "<br>",
              "Sin coordenadas: ", n_propiedades_caba_sin_coord
            ),
            options = opts_pane_caba_resumen,
            group = "Clusters agregados"
          )
        }
      }
      mapa_temp
    } else {
    centroides_partidos <- deptos_plot_ll %>%
      filter(n_parcelas > 0) %>%
      st_centroid() %>%
      st_coordinates() %>%
      as.data.frame() %>%
      mutate(
        nombre_partido = deptos_plot_ll %>% filter(n_parcelas > 0) %>% pull(nombre_partido),
        n_parcelas = deptos_plot_ll %>% filter(n_parcelas > 0) %>% pull(n_parcelas)
      ) %>%
      rename(lng = X, lat = Y)
    
    # --- Vista agregada: PBA + CABA (zoom < zoom_partido_min; JS ZOOM_PARTIDO_MIN)
    if (nrow(centroides_partidos) > 0) {
      total_parcelas_pba <- sum(centroides_partidos$n_parcelas)
      # Centro geográfico de la provincia de Buenos Aires (centroide del polígono)
      centro_pba_sf <- deptos_plot_ll %>% st_union() %>% st_centroid()
      centro_pba_xy <- st_coordinates(centro_pba_sf)
      centro_pba_lng <- as.numeric(centro_pba_xy[1, 1])
      centro_pba_lat <- as.numeric(centro_pba_xy[1, 2])
      mapa_temp <- leaflet::addCircleMarkers(
        map = mapa_temp,
        lng = centro_pba_lng,
        lat = centro_pba_lat,
        radius = 28,
        stroke = TRUE, color = "#FFFFFF", weight = 3,
        fillOpacity = 0.95, fillColor = "#5B7C99",
        label = as.character(total_parcelas_pba),
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
          style = list("color" = "white", "font-weight" = "bold", "font-size" = "18px",
            "text-align" = "center")),
        popup = paste0("<b>PBA</b><br>Parcelas en índice: ", total_parcelas_pba),
        options = opts_pane_cluster_agregado,
        group = "Clusters agregados"
      )
    }
    if (n_propiedades_caba_ref > 0 && nrow(caba_polygon) > 0) {
      centroide_caba_agg <- caba_polygon %>% st_centroid() %>% st_coordinates() %>% as.data.frame() %>% rename(lng = X, lat = Y)
      if (nrow(centroide_caba_agg) > 0) {
        mapa_temp <- leaflet::addCircleMarkers(
          map = mapa_temp,
          lng = centroide_caba_agg$lng[1], lat = centroide_caba_agg$lat[1],
          radius = 28, stroke = TRUE, color = "#FFFFFF", weight = 3,
          fillOpacity = 0.95, fillColor = "#FFB347",
          label = as.character(n_propiedades_caba_ref),
          labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
            style = list("color" = "white", "font-weight" = "bold", "font-size" = "18px",
              "text-align" = "center")),
          popup = paste0(
            "<b>CABA</b><br>",
            "Total CABA (tabla): ", n_propiedades_caba_ref, "<br>",
            "Georreferenciadas: ", n_propiedades_caba_geo, "<br>",
            "Sin coordenadas: ", n_propiedades_caba_sin_coord
          ),
          options = opts_pane_caba_resumen,
          group = "Clusters agregados"
        )
      }
    }
    if (n_propiedades_caba_sin_coord > 0 && nrow(caba_polygon) > 0) {
      centroide_caba_sin_coord <- caba_polygon %>%
        st_centroid() %>%
        st_coordinates() %>%
        as.data.frame() %>%
        rename(lng = X, lat = Y)

      if (nrow(centroide_caba_sin_coord) > 0) {
        # Desplazar respecto al círculo naranja (total CABA) para no superponer dos etiquetas permanentes
        lng_gris <- centroide_caba_sin_coord$lng[1] + 0.028
        lat_gris <- centroide_caba_sin_coord$lat[1] - 0.024
        mapa_temp <- leaflet::addCircleMarkers(
          map = mapa_temp,
          lng = lng_gris,
          lat = lat_gris,
          radius = 14,
          stroke = TRUE,
          color = "#FFFFFF",
          weight = 2,
          fillOpacity = 0.95,
          fillColor = "#7A7A7A",
          label = as.character(n_propiedades_caba_sin_coord),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "white",
              "font-weight" = "bold",
              "font-size" = "13px",
              "text-align" = "center"
            )
          ),
          popup = popup_caba_sin_coord,
          options = opts_pane_caba_resumen,
          group = "CABA sin coordenadas",
          layerId = "caba_sin_coord_contador"
        )
      }
    }
    
    # --- Vista detalle: un círculo por partido
    for (i in 1:nrow(centroides_partidos)) {
      partido <- centroides_partidos$nombre_partido[i]
      centro_lng <- centroides_partidos$lng[i]
      centro_lat <- centroides_partidos$lat[i]
      total_parcelas <- centroides_partidos$n_parcelas[i]
      color_partido <- pal_partidos_leaflet(partido)
      
      mapa_temp <- leaflet::addCircleMarkers(
        map = mapa_temp,
        lng = centro_lng,
        lat = centro_lat,
        radius = 25,
        stroke = TRUE,
        color = "#FFFFFF",
        weight = 3,
        fillOpacity = 0.95,
        fillColor = color_partido,
        label = as.character(total_parcelas),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "font-size" = "16px",
            "text-align" = "center"
          )
        ),
        popup = paste0(
          "<b>", partido, "</b><br>",
          "Total parcelas: ", total_parcelas
        ),
        options = opts_pane_cluster_partido,
        group = "Clusters por partido"
      )
    }
    
    if (n_propiedades_caba_ref > 0 && nrow(caba_polygon) > 0) {
      centroide_caba <- caba_polygon %>%
        st_centroid() %>%
        st_coordinates() %>%
        as.data.frame() %>%
        rename(lng = X, lat = Y)
      
      if (nrow(centroide_caba) > 0) {
        total_propiedades_caba <- n_propiedades_caba_ref
        color_caba <- "#FFB347"
        
        mapa_temp <- leaflet::addCircleMarkers(
          map = mapa_temp,
          lng = centroide_caba$lng[1],
          lat = centroide_caba$lat[1],
          radius = 25,
          stroke = TRUE,
          color = "#FFFFFF",
          weight = 3,
          fillOpacity = 0.95,
          fillColor = color_caba,
          label = as.character(total_propiedades_caba),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "white",
              "font-weight" = "bold",
              "font-size" = "16px",
              "text-align" = "center"
            )
          ),
          popup = paste0(
            "<b>CABA</b><br>",
            "Total CABA (tabla): ", total_propiedades_caba, "<br>",
            "Georreferenciadas: ", n_propiedades_caba_geo, "<br>",
            "Sin coordenadas: ", n_propiedades_caba_sin_coord
          ),
          options = opts_pane_caba_resumen,
          group = "Clusters por partido"
        )
      }
    }
    
    mapa_temp
    }
  } %>%
  
  # Propiedades CABA: por fuente disponible o por categoría de uso (según modo)
  # Se ocultan automáticamente al hacer zoom out cuando se muestran los contadores
  {
    mapa_temp <- .
    if (nrow(propiedades_caba) > 0) {
      if (modo == "fuente") {
        mapa_temp <- leaflet::addCircleMarkers(
          map = mapa_temp,
          data = propiedades_caba,
          lng = ~Long,
          lat = ~Lat,
          radius = 5,
          stroke = TRUE,
          color = "#000000",
          weight = 1.5,
          fillOpacity = 0.95,
          fillColor = ~color_fill,
          options = opts_pane_prop_caba,
          label = ~paste0(
            "<b>", if_else(
              is.na(Descripción) | Descripción == "" | str_detect(Descripción, "(?i)averiguar"),
              Propiedad_ID,
              Descripción
            ), "</b><br>",
            if_else(!is.na(`Dirección oficial`), paste0("Dirección oficial: ", `Dirección oficial`, "<br>"), ""),
            if_else(!is.na(`Direcciones secundarias`) & `Direcciones secundarias` != "", 
                    paste0("Direcciones secundarias: ", `Direcciones secundarias`, "<br>"), ""),
            if_else(!is.na(`Categoría de uso`) & `Categoría de uso` != "", 
                    paste0("Categoría de uso: ", `Categoría de uso`, "<br>"), ""),
            if_else(!is.na(`Fuentes disponibles`), paste0("Fuentes: ", `Fuentes disponibles`, "<br>"), ""),
            "ID: ", Propiedad_ID
          ),
          popup = ~paste0(
            "<b>", if_else(
              is.na(Descripción) | Descripción == "" | str_detect(Descripción, "(?i)averiguar"),
              Propiedad_ID,
              Descripción
            ), "</b><br>",
            "<hr>",
            if_else(!is.na(`Dirección oficial`), paste0("<b>Dirección oficial:</b> ", `Dirección oficial`, "<br>"), ""),
            if_else(!is.na(`Direcciones secundarias`) & `Direcciones secundarias` != "", 
                    paste0("<b>Direcciones secundarias:</b> ", `Direcciones secundarias`, "<br>"), ""),
            if_else(!is.na(`Categoría de uso`) & `Categoría de uso` != "", 
                    paste0("<b>Categoría de uso:</b> ", `Categoría de uso`, "<br>"), ""),
            if_else(!is.na(`Fuentes disponibles`), paste0("<b>Fuentes disponibles:</b> ", `Fuentes disponibles`, "<br>"), ""),
            "<b>ID:</b> ", Propiedad_ID
          ),
          group = "Propiedades CABA"
        )
      } else {
        cats_uso <- orden_categoria_uso[
          orden_categoria_uso %in% unique(propiedades_caba$categoria_uso_priorizada)
        ]
        for (cat in cats_uso) {
          sub_df <- propiedades_caba %>% filter(categoria_uso_priorizada == cat)
          if (nrow(sub_df) == 0) next
          nm_capa <- as.character(cat)
          mapa_temp <- leaflet::addCircleMarkers(
            map = mapa_temp,
            data = sub_df,
            lng = ~Long,
            lat = ~Lat,
            radius = 5,
            stroke = TRUE,
            color = "#000000",
            weight = 1.5,
            fillOpacity = 0.95,
            fillColor = ~color_fill_uso,
            options = opts_pane_prop_caba,
            label = ~paste0(
              "<b>", if_else(
                is.na(Descripción) | Descripción == "" | str_detect(Descripción, "(?i)averiguar"),
                Propiedad_ID,
                Descripción
              ), "</b><br>",
              if_else(!is.na(`Dirección oficial`), paste0("Dirección oficial: ", `Dirección oficial`, "<br>"), ""),
              if_else(!is.na(`Direcciones secundarias`) & `Direcciones secundarias` != "", 
                      paste0("Direcciones secundarias: ", `Direcciones secundarias`, "<br>"), ""),
              if_else(!is.na(`Categoría de uso`) & `Categoría de uso` != "", 
                      paste0("Categoría de uso: ", `Categoría de uso`, "<br>"), ""),
              if_else(!is.na(`Fuentes disponibles`), paste0("Fuentes: ", `Fuentes disponibles`, "<br>"), ""),
              "ID: ", Propiedad_ID
            ),
            popup = ~paste0(
              "<b>", if_else(
                is.na(Descripción) | Descripción == "" | str_detect(Descripción, "(?i)averiguar"),
                Propiedad_ID,
                Descripción
              ), "</b><br>",
              "<hr>",
              if_else(!is.na(`Dirección oficial`), paste0("<b>Dirección oficial:</b> ", `Dirección oficial`, "<br>"), ""),
              if_else(!is.na(`Direcciones secundarias`) & `Direcciones secundarias` != "", 
                      paste0("<b>Direcciones secundarias:</b> ", `Direcciones secundarias`, "<br>"), ""),
              if_else(!is.na(`Categoría de uso`) & `Categoría de uso` != "", 
                      paste0("<b>Categoría de uso:</b> ", `Categoría de uso`, "<br>"), ""),
              if_else(!is.na(`Fuentes disponibles`), paste0("<b>Fuentes disponibles:</b> ", `Fuentes disponibles`, "<br>"), ""),
              "<b>ID:</b> ", Propiedad_ID
            ),
            group = nm_capa
          )
        }
      }
    }
    mapa_temp
  } %>%
  
  {
    mapa_temp <- .
    if (modo == "fuente") {
      mapa_temp %>%
        addLayersControl(
          overlayGroups = c(
            "Partidos",
            "Parcelas (polígonos)",
            "Clusters por partido",
            "CABA sin coordenadas",
            grupos_capas_prop_caba
          ),
          options = layersControlOptions(collapsed = TRUE)
        )
    } else {
      og_uso <- c(
        if (n_propiedades_caba_ref > 0 && nrow(caba_polygon) > 0) "CABA (contorno)",
        if (n_propiedades_caba_ref > 0 && nrow(caba_polygon) > 0) "Clusters agregados",
        if (n_propiedades_caba_sin_coord > 0 && nrow(caba_polygon) > 0) "CABA sin coordenadas",
        grupos_capas_prop_caba
      )
      if (length(og_uso) == 0) {
        mapa_temp
      } else if (length(grupos_capas_prop_caba) > 0) {
        mapa_temp %>%
          addControl(
            html = htmltools::div(
              class = "leaflet-cat-uso-header",
              style = "background:#fff;padding:8px 10px;border-radius:4px;box-shadow:0 1px 4px rgba(0,0,0,0.35);font-size:12px;min-width:200px;",
              htmltools::tags$div(style = "font-weight:bold;font-size:13px;margin-bottom:6px;border-bottom:1px solid #ddd;padding-bottom:4px;", "Categorías de uso"),
              htmltools::tags$div(
                style = "margin-bottom:6px;",
                htmltools::tags$button(
                  type = "button",
                  `data-georef-btn` = "uso-todas",
                  onclick = htmltools::HTML("if(window._georefUsoBtn)window._georefUsoBtn(\"uso-todas\");return false;"),
                  style = "font-size:11px;margin-right:4px;cursor:pointer;",
                  "Ver todas"
                ),
                htmltools::tags$button(
                  type = "button",
                  `data-georef-btn` = "uso-ninguna",
                  onclick = htmltools::HTML("if(window._georefUsoBtn)window._georefUsoBtn(\"uso-ninguna\");return false;"),
                  style = "font-size:11px;margin-right:4px;cursor:pointer;",
                  "Ocultar todas"
                ),
                htmltools::tags$button(
                  type = "button",
                  `data-georef-btn` = "uso-toggle-lista",
                  onclick = htmltools::HTML("if(window._georefUsoBtn)window._georefUsoBtn(\"uso-toggle-lista\");return false;"),
                  style = "font-size:11px;cursor:pointer;",
                  "Ocultar lista"
                )
              )
            ),
            position = "topleft",
            className = "leaflet-uso-acciones"
          ) %>%
          addLayersControl(
            overlayGroups = og_uso,
            position = "topleft",
            options = layersControlOptions(collapsed = FALSE)
          )
      } else {
        mapa_temp %>%
          addLayersControl(
            overlayGroups = og_uso,
            position = "topleft",
            options = layersControlOptions(collapsed = FALSE)
          )
      }
    }
  } %>%
  
  # Leyenda de partidos (solo mapa por fuente; en mapa por uso se omite)
  {
    mapa_temp <- .
    if (modo == "fuente") {
      valores_leyenda <- c("CABA", sort(partidos_sin_caba))
      colores_leyenda <- colores_partidos_nombrados[valores_leyenda]
      mapa_temp <- mapa_temp %>%
        addLegend(
          "bottomright",
          colors = colores_leyenda,
          labels = valores_leyenda,
          title  = "Partidos",
          opacity = 0.7
        )
    }
    mapa_temp
  } %>%
  
  # Leyenda: fuentes o categorías de uso (CABA)
  {
    mapa_temp <- .
    if (nrow(propiedades_caba) > 0) {
      if (modo == "fuente") {
        colores_leyenda <- colores_fuentes_completos[fuentes_disponibles]
        mapa_temp <- mapa_temp %>%
          addLegend(
            "bottomleft",
            colors = colores_leyenda,
            labels = labels_leyenda_fuente,
            title  = "Fuentes disponibles (CABA)",
            opacity = 0.7
          )
      } else {
        colores_leyenda <- unname(colores_uso_fijos[categorias_para_leyenda_uso])
        mapa_temp <- mapa_temp %>%
          addLegend(
            "bottomright",
            colors = colores_leyenda,
            labels = labels_leyenda_uso,
            title  = "Categorías de uso (CABA)",
            opacity = 0.7
          )
      }
    }
    mapa_temp
  } %>%
  {
    mapa_temp <- .
    if (modo == "uso" && nrow(caba_polygon) > 0) {
      bb_cb <- st_bbox(caba_polygon)
      mapa_temp %>%
        fitBounds(
          lng1 = as.numeric(bb_cb["xmin"]),
          lat1 = as.numeric(bb_cb["ymin"]),
          lng2 = as.numeric(bb_cb["xmax"]),
          lat2 = as.numeric(bb_cb["ymax"])
        )
    } else {
      mapa_temp
    }
  } %>%
  # JavaScript: URL + zoom. Fuente y uso: zoom out oculta puntos CABA y muestra contadores; uso alinea totales con mapa fuente.
  htmlwidgets::onRender(
    paste0(
      "
    function(el, x) {
      var widgetInstance = (this && typeof this.getMap === 'function') ? this : null;
      function getMapSafe() {
        if (!widgetInstance || typeof widgetInstance.getMap !== 'function') return null;
        var m = widgetInstance.getMap();
        if (m && typeof m.eachLayer === 'function') return m;
        return null;
      }
      var modoUso = ", if (modo == "uso") "true" else "false", ";
      var categoriasUso = ", categorias_uso_json, ";
      if (!Array.isArray(categoriasUso)) {
        if (categoriasUso != null && typeof categoriasUso === 'object') {
          categoriasUso = Object.keys(categoriasUso).map(function(k) { return categoriasUso[k]; }).filter(function(x) { return x != null && x !== ''; });
        } else {
          categoriasUso = [];
        }
      }
      var conteosSinCoord = ", sin_coord_conteos_json, ";
      var esperarContadorSinCoord = ", esperar_contador_sin_coord_js, ";
      var ZOOM_PUNTOS = ", zoom_puntos_caba, ";
      var ZOOM_PARTIDO_MIN = ", zoom_partido_min, ";

      function categoriasUsoIncludesGroup(g) {
        if (!g || !categoriasUso || categoriasUso.length === 0) return false;
        var j;
        for (j = 0; j < categoriasUso.length; j++) {
          if (categoriasUso[j] === g) return true;
        }
        return false;
      }

      var propiedadesMarkers = [];
      var clustersPartido = [];
      var clustersAgregados = [];
      var cabaSinCoordMarkers = [];

      function recollectClusterMarkers(m) {
        propiedadesMarkers.length = 0;
        clustersPartido.length = 0;
        clustersAgregados.length = 0;
        function walk(layer) {
          if (layer instanceof L.CircleMarker && layer.options) {
            var g = layer.options.group;
            if (g === 'Propiedades CABA' || (modoUso && g && categoriasUsoIncludesGroup(g))) propiedadesMarkers.push(layer);
            if (g === 'Clusters por partido') clustersPartido.push(layer);
            if (g === 'Clusters agregados') clustersAgregados.push(layer);
          }
          if (layer.eachLayer) layer.eachLayer(walk);
        }
        m.eachLayer(walk);
      }

      function recollectCabaSinCoord(m) {
        cabaSinCoordMarkers.length = 0;
        var idSinCoord = 'caba_sin_coord_contador';
        if (m.layerManager && typeof m.layerManager.getLayer === 'function') {
          var lm = m.layerManager.getLayer('marker', idSinCoord);
          if (lm) cabaSinCoordMarkers.push(lm);
        }
        function walk(layer) {
          if (layer instanceof L.CircleMarker && layer.options) {
            var g = layer.options.group;
            if (g === 'CABA sin coordenadas' && cabaSinCoordMarkers.indexOf(layer) === -1) cabaSinCoordMarkers.push(layer);
          }
          if (layer.eachLayer) layer.eachLayer(walk);
        }
        m.eachLayer(walk);
      }

      function normalizeCatStr(s) {
        if (s == null || s === '') return '';
        try { return String(s).normalize('NFC').replace(/\\s+/g, ' ').trim(); } catch (e) { return String(s).trim(); }
      }

      function textoDesdeLabel(lab) {
        if (!lab) return '';
        var sp = lab.querySelector('span');
        if (sp) return normalizeCatStr(sp.textContent || sp.innerText || '');
        return normalizeCatStr(lab.textContent || '');
      }

      function matchCategoriaKey(raw) {
        var t = normalizeCatStr(raw);
        if (!t) return null;
        var i;
        for (i = 0; i < categoriasUso.length; i++) {
          if (normalizeCatStr(categoriasUso[i]) === t) return categoriasUso[i];
        }
        for (i = 0; i < categoriasUso.length; i++) {
          var c = normalizeCatStr(categoriasUso[i]);
          if (t.indexOf(c) !== -1 || c.indexOf(t) !== -1) return categoriasUso[i];
        }
        return null;
      }

      function domRootMapa() {
        var mm = el._georefLeafletMap;
        if (mm && mm.getContainer) return mm.getContainer();
        var gm = getMapSafe();
        if (gm && gm.getContainer) return gm.getContainer();
        return el;
      }

      function queryOverlayCheckboxes() {
        var acc = [];
        var root = domRootMapa();
        var overlays = root.querySelectorAll('.leaflet-control-layers-overlays');
        if (!overlays.length) {
          overlays = el.querySelectorAll('.leaflet-control-layers-overlays');
        }
        if (!overlays.length) {
          overlays = document.querySelectorAll('.leaflet-control-layers-overlays');
        }
        function addFrom(root) {
          var sel = root.querySelectorAll('input.leaflet-control-layers-selector[type=\"checkbox\"]');
          var ii;
          if (sel.length) {
            for (ii = 0; ii < sel.length; ii++) acc.push(sel[ii]);
          } else {
            var sel2 = root.querySelectorAll('input[type=\"checkbox\"]');
            for (ii = 0; ii < sel2.length; ii++) acc.push(sel2[ii]);
          }
        }
        if (overlays.length) {
          for (var k = 0; k < overlays.length; k++) addFrom(overlays[k]);
        } else {
          var fallback = document.querySelectorAll('.leaflet-control-layers-overlays input[type=\"checkbox\"]');
          for (var f = 0; f < fallback.length; f++) acc.push(fallback[f]);
        }
        return acc;
      }

      function getCategoriaUsoCheckboxes() {
        var out = [];
        queryOverlayCheckboxes().forEach(function(input) {
          var lab = input.closest('label');
          var key = matchCategoriaKey(textoDesdeLabel(lab));
          if (!key) return;
          var dup = false;
          var j;
          for (j = 0; j < out.length; j++) {
            if (out[j].input === input) { dup = true; break; }
          }
          if (!dup) out.push({ input: input, texto: key });
        });
        return out;
      }

      function conteoPorClave(key) {
        if (!conteosSinCoord || typeof conteosSinCoord !== 'object') return 0;
        if (typeof conteosSinCoord[key] === 'number') return conteosSinCoord[key];
        var nk = normalizeCatStr(key);
        var k;
        for (k in conteosSinCoord) {
          if (k === 'sinCoordResto') continue;
          if (normalizeCatStr(k) === nk) return conteosSinCoord[k];
        }
        return 0;
      }

      function actualizarContadorSinCoord() {
        if (!modoUso || !categoriasUso || categoriasUso.length === 0 || cabaSinCoordMarkers.length === 0) return;
        if (!conteosSinCoord || typeof conteosSinCoord !== 'object') return;
        var cbs = getCategoriaUsoCheckboxes();
        if (cbs.length === 0) return;
        var nChecked = 0;
        var totalSel = 0;
        var allChecked = true;
        cbs.forEach(function(item) {
          if (item.input.checked) {
            nChecked++;
            totalSel += conteoPorClave(item.texto);
          } else {
            allChecked = false;
          }
        });
        cabaSinCoordMarkers.forEach(function(m) {
          if (nChecked === 0) {
            m.setOpacity(0);
            if (m.closeTooltip) m.closeTooltip();
            var t = m.getTooltip && m.getTooltip();
            if (t) {
              if (t.setContent) t.setContent('');
              var tipEl = t.getElement && t.getElement();
              if (tipEl) tipEl.style.display = 'none';
            }
            return;
          }
          var n = totalSel;
          if (allChecked && conteosSinCoord.sinCoordResto != null) {
            n += (conteosSinCoord.sinCoordResto || 0);
          }
          m.setOpacity(1);
          var t2 = m.getTooltip && m.getTooltip();
          if (t2) {
            if (t2.setContent) t2.setContent(String(n));
            var tipEl2 = t2.getElement && t2.getElement();
            if (tipEl2) tipEl2.style.display = '';
          }
          if (m.openTooltip) m.openTooltip();
        });
      }

      function marcarUsoTodos(visible) {
        getCategoriaUsoCheckboxes().forEach(function(item) {
          if (item.input.checked !== visible) item.input.click();
        });
        setTimeout(actualizarContadorSinCoord, 0);
      }

      function toggleListaUsoCapas() {
        var root = domRootMapa();
        var tr = root.querySelector('.leaflet-top.leaflet-left');
        if (!tr) return;
        var todasCapas = tr.querySelectorAll('.leaflet-control-layers');
        var capasCtrl = null;
        var ci, cj, labs;
        for (ci = todasCapas.length - 1; ci >= 0; ci--) {
          labs = todasCapas[ci].querySelectorAll('.leaflet-control-layers-overlays label');
          for (cj = 0; cj < labs.length; cj++) {
            if (matchCategoriaKey(textoDesdeLabel(labs[cj]))) {
              capasCtrl = todasCapas[ci];
              break;
            }
          }
          if (capasCtrl) break;
        }
        if (!capasCtrl && todasCapas.length) capasCtrl = todasCapas[todasCapas.length - 1];
        if (!capasCtrl) return;
        el._georefListaUsoVisible = !el._georefListaUsoVisible;
        var listaVisible = el._georefListaUsoVisible;
        capasCtrl.style.display = listaVisible ? '' : 'none';
        var btnToggle = root.querySelector('button[data-georef-btn=\"uso-toggle-lista\"]');
        if (btnToggle) btnToggle.textContent = listaVisible ? 'Ocultar lista' : 'Mostrar lista';
      }

      function handleUsoBotonAction(action) {
        if (!action || action.indexOf('uso-') !== 0) return;
        if (action === 'uso-todas') marcarUsoTodos(true);
        else if (action === 'uso-ninguna') marcarUsoTodos(false);
        else if (action === 'uso-toggle-lista') toggleListaUsoCapas();
      }

      if (modoUso) {
        window._georefUsoBtn = function(a) { handleUsoBotonAction(a); };
      }


      function bringClusterMarkersToFront(map) {
        if (!map) return;
        var i;
        for (i = 0; i < clustersPartido.length; i++) {
          var p = clustersPartido[i];
          if (p && map.hasLayer(p) && p.bringToFront) p.bringToFront();
        }
        for (i = 0; i < clustersAgregados.length; i++) {
          var a = clustersAgregados[i];
          if (a && map.hasLayer(a) && a.bringToFront) a.bringToFront();
        }
        recollectCabaSinCoord(map);
        for (i = 0; i < cabaSinCoordMarkers.length; i++) {
          var g = cabaSinCoordMarkers[i];
          if (g && map.hasLayer(g) && g.bringToFront) g.bringToFront();
        }
      }

      function updateByZoom() {
        var map = getMapSafe();
        if (!map) return;
        var zoom = map.getZoom();

        if (modoUso) {
          if (zoom >= ZOOM_PUNTOS) {
            clustersPartido.forEach(function(l) { if (map.hasLayer(l)) map.removeLayer(l); });
            clustersAgregados.forEach(function(l) { if (map.hasLayer(l)) map.removeLayer(l); });
            propiedadesMarkers.forEach(function(mm) { if (!map.hasLayer(mm)) map.addLayer(mm); });
          } else {
            propiedadesMarkers.forEach(function(mm) { if (map.hasLayer(mm)) map.removeLayer(mm); });
            clustersPartido.forEach(function(l) { if (map.hasLayer(l)) map.removeLayer(l); });
            clustersAgregados.forEach(function(l) { if (!map.hasLayer(l)) map.addLayer(l); });
          }
          recollectCabaSinCoord(map);
          bringClusterMarkersToFront(map);
          if (esperarContadorSinCoord) actualizarContadorSinCoord();
          return;
        }

        if (zoom >= ZOOM_PUNTOS) {
          clustersPartido.forEach(function(l) { if (map.hasLayer(l)) map.removeLayer(l); });
          clustersAgregados.forEach(function(l) { if (map.hasLayer(l)) map.removeLayer(l); });
          propiedadesMarkers.forEach(function(mm) { if (!map.hasLayer(mm)) map.addLayer(mm); });
          recollectCabaSinCoord(map);
          bringClusterMarkersToFront(map);
          return;
        }

        propiedadesMarkers.forEach(function(mm) { if (map.hasLayer(mm)) map.removeLayer(mm); });

        if (zoom < ZOOM_PARTIDO_MIN) {
          clustersPartido.forEach(function(l) { if (map.hasLayer(l)) map.removeLayer(l); });
          clustersAgregados.forEach(function(l) { if (!map.hasLayer(l)) map.addLayer(l); });
        } else {
          clustersAgregados.forEach(function(l) { if (map.hasLayer(l)) map.removeLayer(l); });
          clustersPartido.forEach(function(l) { if (!map.hasLayer(l)) map.addLayer(l); });
        }
        recollectCabaSinCoord(map);
        bringClusterMarkersToFront(map);
      }

      function installSinCoordDelegation(map) {
        if (!esperarContadorSinCoord || el._georefSinCoordDelegado) return true;
        recollectCabaSinCoord(map);
        var cbs = getCategoriaUsoCheckboxes();
        if (cabaSinCoordMarkers.length === 0 || cbs.length === 0) return false;
        el._georefSinCoordDelegado = true;
        el.addEventListener('change', function(ev) {
          var t = ev.target;
          var hostCh = (el.querySelector && el.querySelector('.leaflet')) || el;
          if (!modoUso || !t || t.type !== 'checkbox' || !hostCh.contains(t)) return;
          if (!t.closest || !t.closest('.leaflet-control-layers-overlays')) return;
          var lab = t.closest('label');
          if (!lab || !matchCategoriaKey(textoDesdeLabel(lab))) return;
          var mm = getMapSafe();
          if (mm) recollectCabaSinCoord(mm);
          actualizarContadorSinCoord();
        }, true);
        if (!el._georefSinCoordMapEv) {
          el._georefSinCoordMapEv = true;
          map.on('overlayadd', function() {
            var mm = getMapSafe();
            if (mm) recollectCabaSinCoord(mm);
            actualizarContadorSinCoord();
          });
          map.on('overlayremove', function() {
            var mm = getMapSafe();
            if (mm) recollectCabaSinCoord(mm);
            actualizarContadorSinCoord();
          });
        }
        return true;
      }

      function tryInstallSinCoord(map, attempt) {
        if (!esperarContadorSinCoord) return;
        if (installSinCoordDelegation(map)) {
          actualizarContadorSinCoord();
        } else if (attempt < 60) {
          setTimeout(function() {
            var m = getMapSafe();
            if (m) tryInstallSinCoord(m, attempt + 1);
          }, 120);
        }
      }

      function usoBotonDesdeEvento(ev) {
        var t = ev.target;
        var btn = t && t.closest ? t.closest('button[data-georef-btn]') : null;
        if (!btn) return;
        if (!btn.closest || !btn.closest('.leaflet-uso-acciones')) return;
        var action = btn.getAttribute('data-georef-btn');
        if (!action || action.indexOf('uso-') !== 0) return;
        ev.preventDefault();
        ev.stopPropagation();
        handleUsoBotonAction(action);
      }

      function wireUsoBotonesEnMapa(map) {
        if (!modoUso || !map || !map.getContainer) return;
        el._georefLeafletMap = map;
        var mc = map.getContainer();
        if (!mc) return;
        var panel = mc.querySelector('.leaflet-uso-acciones');
        var node = panel || mc;
        if (node._georefUsoClickOk) return;
        node._georefUsoClickOk = true;
        node.addEventListener('click', usoBotonDesdeEvento, false);
      }

      if (modoUso && !el._georefUsoCallbacksReady) {
        el._georefUsoCallbacksReady = true;
        el._georefListaUsoVisible = true;
        el._georefUsoClick = function(action) {
          handleUsoBotonAction(action);
        };
      }

      function runInit(attempt) {
        var map = getMapSafe();
        if (!map) {
          if (attempt < 60) setTimeout(function() { runInit(attempt + 1); }, 100);
          return;
        }
        if (!el._georefClusterInit) {
          recollectClusterMarkers(map);
          el._georefClusterInit = true;
        }
        if (!el._georefUrlParams) {
          el._georefUrlParams = true;
          (function() {
            var params = new URLSearchParams(window.location.search);
            var lat = params.get('lat'); var lng = params.get('lng'); var zoom = params.get('zoom');
            if (lat != null && lng != null) {
              var latNum = parseFloat(lat), lngNum = parseFloat(lng);
              if (!isNaN(latNum) && !isNaN(lngNum) && latNum >= -90 && latNum <= 90 && lngNum >= -180 && lngNum <= 180) {
                var z = (zoom != null && !isNaN(parseInt(zoom))) ? parseInt(zoom) : 16;
                setTimeout(function() {
                  var m = getMapSafe();
                  if (m) m.setView([latNum, lngNum], z);
                }, 300);
              }
            }
          })();
        }
        if (!el._georefZoomWired) {
          el._georefZoomWired = true;
          map.on('zoomend', updateByZoom);
          setTimeout(updateByZoom, 100);
        }
        if (!el._georefResizeHook) {
          el._georefResizeHook = true;
          map.on('resize', function() {
            if (!modoUso || !esperarContadorSinCoord) return;
            var m = getMapSafe();
            if (!m) return;
            if (!el._georefSinCoordDelegado) tryInstallSinCoord(m, 0);
            else {
              recollectCabaSinCoord(m);
              actualizarContadorSinCoord();
            }
          });
        }
        if (modoUso) {
          wireUsoBotonesEnMapa(map);
          if (esperarContadorSinCoord) tryInstallSinCoord(map, attempt);
        }
      }

      runInit(0);
      setTimeout(function() { runInit(0); }, 0);
      setTimeout(function() { runInit(0); }, 300);
      setTimeout(function() { runInit(0); }, 800);
      window.addEventListener('load', function() { runInit(0); });
    }
  "
    )
  )
}
