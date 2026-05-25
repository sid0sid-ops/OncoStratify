.PHONY: help install run test docker-build docker-run docker-stop clean docs

help:
	@echo "OncoStratify — Available Commands"
	@echo "=================================="
	@echo ""
	@echo "Development:"
	@echo "  make install      Install R packages"
	@echo "  make run          Run the Shiny app"
	@echo "  make test         Run unit tests"
	@echo "  make style        Fix code style"
	@echo ""
	@echo "Docker:"
	@echo "  make docker-build Build Docker image"
	@echo "  make docker-run   Run Docker container"
	@echo "  make docker-stop  Stop Docker container"
	@echo ""
	@echo "Utilities:"
	@echo "  make docs         Generate documentation"
	@echo "  make clean        Remove logs and cache"
	@echo ""

install:
	@echo "Installing R packages..."
	Rscript -e "install.packages(c('shiny', 'bslib', 'survival', 'survminer', 'dplyr', 'ggplot2', 'DT', 'shinycssloaders'), repos = 'https://cran.r-project.org')"
	@echo "✓ Packages installed"

run:
	@echo "Starting OncoStratify..."
	Rscript -e "shiny::runApp()"

test:
	@echo "Running tests..."
	Rscript -e "testthat::test_dir('tests/')"

style:
	@echo "Fixing code style..."
	Rscript -e "styler::style_dir('R/')"
	@echo "✓ Code styled"

docker-build:
	@echo "Building Docker image..."
	docker build -t oncostratify:2.0 .
	@echo "✓ Image built (oncostratify:2.0)"

docker-run:
	@echo "Starting Docker container..."
	docker-compose up -d
	@echo "✓ Container running at http://localhost:3838"
	@echo "   Stop with: make docker-stop"

docker-stop:
	@echo "Stopping Docker container..."
	docker-compose down
	@echo "✓ Container stopped"

docs:
	@echo "Documentation available at:"
	@echo "  - README.md (quick start)"
	@echo "  - PROJECT_SPEC.md (features)"
	@echo "  - DEVELOPMENT.md (coding)"
	@echo "  - DEPLOYMENT.md (production)"
	@echo "  - DATA_SOURCES.md (TCGA data)"

clean:
	@echo "Cleaning up..."
	rm -f logs/*.log
	rm -rf inst/cache/*
	@echo "✓ Cleaned"

version:
	@echo "OncoStratify v2.0"
	@grep "^Version:" DESCRIPTION | cut -d' ' -f2
