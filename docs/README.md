# Sitio estático (GitHub Pages)

Esta carpeta es la **raíz del sitio** publicado. Incluye:

- `index.html` — portada con pestañas para alternar entre los dos mapas (iframes con rutas relativas).
- `mapa_fuentes.html`, `mapa_uso.html` — salidas de R Markdown (Leaflet embebido).
- `.nojekyll` — evita que Jekyll procese el sitio (recomendado para HTML generado).

## Cómo activar GitHub Pages

1. Creá un repositorio en GitHub y subí el proyecto (o solo lo que necesites; para ver solo el mapa alcanza con commitear la carpeta `docs/` y los archivos mínimos si usás [submódulo o solo el repo de mapas](https://docs.github.com/pages/getting-started-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site)).
2. En el repo: **Settings → Pages**.
3. **Build and deployment → Source:** Branch `main` (o la rama que uses), folder **`/docs`**.
4. Tras el primer deploy, el sitio estará en:

   `https://<usuario>.github.io/<repositorio>/`

   La página de entrada es `index.html` (GitHub Pages la sirve automáticamente).

## Actualizar los mapas

Los HTML de esta carpeta se generan en la carpeta del proyecto con R:

```r
rmarkdown::render("mapa_fuentes.Rmd")
rmarkdown::render("mapa_uso.Rmd")
```

Luego copiá `mapa_fuentes.html` y `mapa_uso.html` desde la raíz del proyecto a `docs/` y hacé commit.

**Estrategia de despliegue:** HTML **precompilado** en git (simple y predecible). Un workflow con **GitHub Actions** que renderice los `.Rmd` en la nube es opcional si más adelante querés CI sin R local; implica `renv`, datos en el repo o artefactos, y más tiempo de build.

## Rutas

Todos los enlaces en `index.html` son **relativos** (`mapa_fuentes.html`, `mapa_uso.html`), válidos para un *project site* bajo `/<repo>/`.
