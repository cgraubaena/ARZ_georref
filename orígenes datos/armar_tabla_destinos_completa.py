# -*- coding: utf-8 -*-
"""
Construye una tabla con el CSV Destinos como base y agrega las filas del Excel
original que no tenían N° de carpeta en Destinos (mismo esquema de columnas).
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

import pandas as pd

BASE = Path(__file__).resolve().parent
DESTINOS_CSV = BASE / "Tabla única de propiedades - Destinos (P. Eduardo).csv"
FALTANTES_CSV = BASE / "carpetas_en_excel_ausentes_en_destinos.csv"
OUT_CSV = BASE / "Tabla Destinos más faltantes Excel.csv"
OUT_XLSX = BASE / "Tabla Destinos más faltantes Excel.xlsx"


def _read_csv_utf8(path: Path) -> pd.DataFrame:
    for enc in ("utf-8-sig", "utf-8", "cp1252"):
        try:
            return pd.read_csv(path, encoding=enc)
        except UnicodeDecodeError:
            continue
    return pd.read_csv(path, encoding="latin-1")


def infer_localidad(direccion: str) -> str:
    if not direccion or not isinstance(direccion, str):
        return ""
    s = direccion.strip()
    u = s.upper().replace(" ", "")
    if "C.A.B.A" in s.upper() or "CABA" in u:
        return "C.A.B.A."
    if re.search(r"PCI\w*\.\s*DE\s+C[ÓO]RDOBA", s, re.I):
        return "Pcia. Córdoba"
    if re.search(r"PCI\w*\.\s*DE\s+R[IÍ]O\s+NEGRO", s, re.I) or "BARILOCHE" in u:
        return "Pcia. Río Negro"
    if "PCI" in s.upper() and "BUENOS" in s.upper():
        return "Pcia. Buenos Aires"
    if "PARTIDO" in s.upper():
        return "Pcia. Buenos Aires"
    if re.search(r"\bADROGU[EÉ]\b", s, re.I):
        return "Adrogué"
    if re.search(r"\bSAN\s+ISIDRO\b", s, re.I):
        return "San Isidro"
    return ""


def normalizar_direccion_basica(s: str) -> str:
    if not s:
        return ""
    t = re.sub(r"\s+", " ", str(s).strip())
    return t.upper()


def parse_anio(fecha: str) -> str:
    if not fecha or not isinstance(fecha, str):
        return ""
    m = re.search(r"(\d{4})", fecha)
    if m:
        return m.group(1)
    m = re.search(r"(\d{2})/(\d{2})/(\d{4})", fecha)
    if m:
        return m.group(3)
    return ""


def carpeta_val(x) -> str:
    if pd.isna(x) or x == "":
        return ""
    try:
        n = int(float(x))
        return str(n)
    except (TypeError, ValueError):
        return str(x).strip()


def _str_clean(x) -> str:
    if x is None or (isinstance(x, float) and pd.isna(x)):
        return ""
    return str(x).strip()


def build_added_rows(df_f: pd.DataFrame, start_idx: int) -> pd.DataFrame:
    rows = []
    for i, r in enumerate(df_f.itertuples(index=False), start=start_idx):
        direccion = _str_clean(getattr(r, "direccion", ""))
        destino = _str_clean(getattr(r, "destino", ""))
        fecha = _str_clean(getattr(r, "fecha", ""))
        obs = _str_clean(getattr(r, "obs", ""))
        hoja = _str_clean(getattr(r, "hoja", ""))
        n_partida = _str_clean(getattr(r, "n_partida", ""))
        carpeta = carpeta_val(getattr(r, "carpeta", ""))

        com = f"Incorporada desde Excel original (hoja «{hoja}»)."
        if obs:
            com = f"{com} {obs}"

        rows.append(
            {
                "Eduardo ID": f"Eduardo_{i}",
                "Propiedad ID": "",
                "N° de Carpeta": carpeta,
                "Nro de parcela": "",
                "Dirección": direccion,
                "Localidad": infer_localidad(direccion),
                "Dirección normalizada": normalizar_direccion_basica(direccion),
                "Destino": destino,
                "Categoría tipo": "Inmueble",
                "Categoría uso": "",
                "Descripción del Inmueble": destino,
                "Observaciones": obs,
                "Dirección OFICIAL": "",
                "Fecha de adquisición": fecha,
                "Año adquisición": parse_anio(str(fecha)),
                "Observaciones.1": "",
                "N° de Partida": n_partida,
                "Comentarios": com,
            }
        )
    return pd.DataFrame(rows)


def main() -> int:
    if not DESTINOS_CSV.is_file():
        print("No se encuentra:", DESTINOS_CSV, file=sys.stderr)
        return 1
    if not FALTANTES_CSV.is_file():
        print("No se encuentra:", FALTANTES_CSV, file=sys.stderr)
        return 1

    base = _read_csv_utf8(DESTINOS_CSV)
    falt = _read_csv_utf8(FALTANTES_CSV)

    nums = []
    for x in base["Eduardo ID"].astype(str):
        m = re.search(r"_(\d+)$", x)
        if m:
            nums.append(int(m.group(1)))
    next_n = max(nums) + 1 if nums else 1

    added = build_added_rows(falt, next_n)

    # Misma orden de columnas que la base
    cols = list(base.columns)
    for c in cols:
        if c not in added.columns:
            added[c] = ""
    added = added[cols]

    out = pd.concat([base, added], ignore_index=True)

    out.to_csv(OUT_CSV, index=False, encoding="utf-8-sig")
    out.to_excel(OUT_XLSX, index=False, engine="openpyxl")

    print(f"Base Destinos: {len(base)} filas")
    print(f"Agregadas (Excel faltantes): {len(added)} filas")
    print(f"Total: {len(out)} filas")
    print(f"CSV: {OUT_CSV}")
    print(f"Excel: {OUT_XLSX}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
