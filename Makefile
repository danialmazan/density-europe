.PHONY: data prepare analyse assets validate serve all

data:
	bash scripts/01_download_data.sh

prepare:
	Rscript scripts/02_prepare_grid.R

analyse:
	Rscript scripts/03_calculate_metrics.R

assets:
	Rscript scripts/04_generate_assets.R

validate:
	Rscript scripts/05_validate.R

serve:
	python3 -m http.server 8765 --bind 127.0.0.1 --directory site

all: data prepare analyse assets validate

