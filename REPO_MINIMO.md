# Contenido mínimo para reproducir los mapas (R / Git)

Archivos y carpetas que usa [`mapa_leaflet_core.R`](mapa_leaflet_core.R). Útil para armar un repo liviano o un clon sin datos de trabajo extra.

## Código

- `mapa_leaflet_core.R`
- `mapa_fuentes.Rmd`, `mapa_uso.Rmd`

## Tablas

- `PBA indice.csv`
- `Tabla única v4.csv` (propiedades CABA; encabezados según el script)

## Shapefiles

- `000_departamentos/070121.shp` (+ `.dbf`, `.shx`, `.prj`, etc.)
- `caba/export1.shp` o `caba/export2.shp` (+ sidecars)
- Una carpeta por **partido** presente en el índice, con el patrón `NNN_parcela/110101.shp` (`NNN` = código de partido en 3 dígitos).

Partidos únicos en `PBA indice.csv` actual (códigos `partido_pdo`):

| Código | Carpeta |
|--------|---------|
| 4 | `004_parcela` |
| 30 | `030_parcela` |
| 45 | `045_parcela` |
| 46 | `046_parcela` |
| 72 | `072_parcela` |
| 74 | `074_parcela` |
| 97 | `097_parcela` |
| 124 | `124_parcela` |
| 131 | `131_parcela` |
| 132 | `132_parcela` |

Si el índice crece, ejecutá en R:

```r
unique(read.csv("PBA indice.csv")$partido_pdo)
```

y añadí las carpetas `sprintf("%03d_parcela", partido)` correspondientes.

## No incluir en el repo (ejemplos)

- `.Rproj.user/`
- Caches de notebooks (`**/.Rproj.user/**`, etc.)
- CSV viejos (`Tabla única v2.csv`, …) si no se usan

## Publicación web (solo HTML)

Para **GitHub Pages** no hace falta subir shapefiles ni CSV: el sitio en [`docs/`](docs/) contiene los HTML ya renderizados. Para actualizar mapas, renderizá los `.Rmd` en local y volvé a copiar `mapa_fuentes.html` y `mapa_uso.html` a `docs/`. Ver [`docs/README.md`](docs/README.md).
