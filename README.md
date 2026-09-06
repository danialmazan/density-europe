# How much of a country do its people occupy?

A short bilingual data story about the difference between national-average population density and the density distribution experienced by residents, using Spain as the main case study.

## Reproduce

Requirements: R plus `curl`, `unzip`, `shasum`, and `cwebp`. R dependencies are pinned in `renv.lock`.

```bash
Rscript -e 'renv::restore()'
make all
make serve
```

Then open <http://127.0.0.1:8765/> or <http://127.0.0.1:8765/?lang=es>.

The 540 MB source archive and extracted interim data are excluded from version control. Compact derived tables, QA results, web data, and rendered map assets are retained so published claims can be audited.

## Source

Eurostat/GISCO, Census Grid 2021, version 3.0 (released 30 May 2026), ETRS89-LAEA / EPSG:3035. The archive is pinned in `config.yml` by URL, byte size, and SHA-256 checksum.

The analysis covers the 30 EU and EFTA countries represented in the release: the EU-27, Norway, Switzerland, and Liechtenstein. It does not claim coverage of every European country.

Confidential values (`-8888`) and other unavailable values (`-9999`) are retained as missing, never converted to zero. Grid-based population metrics exclude geometryless unallocated-population records and disclose their share separately.
