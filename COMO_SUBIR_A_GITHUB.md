# Cómo subir este proyecto a GitHub (paso a paso)

## A. Crear el repositorio en GitHub (en el navegador)

1. Entrá a [https://github.com/new](https://github.com/new).
2. **Repository name:** por ejemplo `georef-pba` (sin espacios).
3. Dejá **Public** (GitHub Pages gratis en repos públicos).
4. **No marques** “Add a README” ni .gitignore ni licencia si vas a usar el primer `push` desde tu PC (evita conflictos).
5. Clic en **Create repository**.

GitHub te mostrará una página con comandos; podés seguir la **sección B** de acá.

## B. Conectar tu carpeta local y subir el código

Abrí **PowerShell** o **Git Bash** en la carpeta del proyecto:

```text
cd "c:\Users\USUARIO\Documents\Arzobispado\georef PBA"
```

Si todavía no inicializaste Git en esta carpeta:

```bash
git init
git add docs/ .gitignore REPO_MINIMO.md mapa_fuentes.Rmd mapa_uso.Rmd mapa_leaflet_core.R COMO_SUBIR_A_GITHUB.md
git commit -m "Sitio GitHub Pages y fuentes de los mapas"
git branch -M main
```

Sustituí `TU_USUARIO` y `TU_REPO` por los tuyos (los ves en la URL del repo en GitHub):

```bash
git remote add origin https://github.com/TU_USUARIO/TU_REPO.git
git push -u origin main
```

Te pedirá usuario y contraseña: en GitHub la “contraseña” para HTTPS suele ser un **Personal Access Token** (no la clave de la cuenta). Si te falla, en GitHub: **Settings → Developer settings → Personal access tokens** y generá uno con permiso `repo`.

## C. Activar GitHub Pages

1. En el repo: **Settings** (pestaña del repositorio).
2. Menú izquierdo: **Pages**.
3. **Build and deployment → Branch:** elegí `main` y carpeta **`/docs`**.
4. Guardá. A los pocos minutos el sitio estará en:

   `https://TU_USUARIO.github.io/TU_REPO/`

## Si querés versionar también los datos (CSV, shapefiles)

Son archivos grandes: sumalos con `git add` cuando quieras. Si GitHub rechaza el tamaño, considerá [Git LFS](https://git-lfs.com/) o un repo aparte solo con `docs/` para la web.
