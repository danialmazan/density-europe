#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
RAW_DIR="$ROOT_DIR/data/raw"
ARCHIVE="$RAW_DIR/Eurostat_Census-GRID_2021_V3.zip"
URL="https://gisco-services.ec.europa.eu/census/2021/Eurostat_Census-GRID_2021_V3.zip"
EXPECTED_SHA="18a467e3d96b71dd89f0440cc184751bded45721cdff8ba778ece79d7a4f5843"
TOTALS_URL="https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/demo_urespop?time=2021&geoLevel=country"
TOTALS_SHA="ff0d2d356c6a2a2e1ea26fdd729d6f1116e11e2de0730e5760b2bcfadf5a6109"
BOUNDARIES_URL="https://gisco-services.ec.europa.eu/distribution/v2/countries/geojson/CNTR_RG_20M_2024_3035.geojson"
BOUNDARIES_SHA="246f95bf40be3c33bc2c43092ecfa82b44ad5597a1fe7410f980419aec690834"

mkdir -p "$RAW_DIR"

if [[ ! -s "$ARCHIVE" ]]; then
  curl -L --fail --retry 3 --output "$ARCHIVE" "$URL"
fi

ACTUAL_SHA="$(shasum -a 256 "$ARCHIVE" | awk '{print $1}')"
if [[ "$ACTUAL_SHA" != "$EXPECTED_SHA" ]]; then
  echo "Checksum mismatch for $ARCHIVE" >&2
  echo "Expected: $EXPECTED_SHA" >&2
  echo "Actual:   $ACTUAL_SHA" >&2
  exit 1
fi

if [[ ! -s "$RAW_DIR/ESTAT_Census_2021_country.csv.zip" ]]; then
  unzip -jo "$ARCHIVE" '*/ESTAT_Census_2021_country.csv.zip' -d "$RAW_DIR"
fi
if [[ ! -s "$RAW_DIR/ESTAT_Census_2021_V3.tiff" ]]; then
  unzip -jo "$ARCHIVE" '*/ESTAT_Census_2021_V3.tiff' -d "$RAW_DIR"
fi
if [[ ! -s "$RAW_DIR/ESTAT_Census_2021_LAND_SURFACE_V3.tiff" ]]; then
  unzip -jo "$ARCHIVE" '*/ESTAT_Census_2021_LAND_SURFACE_V3.tiff' -d "$RAW_DIR"
fi
if [[ ! -s "$RAW_DIR/read.me" ]]; then
  unzip -jo "$ARCHIVE" '*/read.me' -d "$RAW_DIR"
fi

if [[ ! -s "$RAW_DIR/demo_urespop_2021.json" ]]; then
  curl -L --fail --retry 3 --output "$RAW_DIR/demo_urespop_2021.json" "$TOTALS_URL"
fi
if [[ "$(shasum -a 256 "$RAW_DIR/demo_urespop_2021.json" | awk '{print $1}')" != "$TOTALS_SHA" ]]; then
  echo "Checksum mismatch for Eurostat national totals." >&2
  exit 1
fi

if [[ ! -s "$RAW_DIR/CNTR_RG_20M_2024_3035.geojson" ]]; then
  curl -L --fail --retry 3 --output "$RAW_DIR/CNTR_RG_20M_2024_3035.geojson" "$BOUNDARIES_URL"
fi
if [[ "$(shasum -a 256 "$RAW_DIR/CNTR_RG_20M_2024_3035.geojson" | awk '{print $1}')" != "$BOUNDARIES_SHA" ]]; then
  echo "Checksum mismatch for GISCO country boundaries." >&2
  exit 1
fi

echo "Source archive verified and required files extracted."
