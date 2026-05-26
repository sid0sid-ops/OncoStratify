# =============================================================================
# Dockerfile — OncoStratify Docker Container
# Multi-Platform Support: Windows, Linux, macOS (Intel & Apple Silicon arm64)
#
# Build:
#   docker build -t oncostratify:2.0 .
#
# Run:
#   docker run -p 3838:3838 oncostratify:2.0
# =============================================================================

FROM rocker/shiny:4.3.2

# Clear SHINY_SERVER_VERSION to bypass Shiny package version comparison crash
ENV SHINY_SERVER_VERSION=""

# Set system variables to prevent interactive prompts during apt-get
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies required by R packages (survival, survminer, DT, ggplot2, etc.)
RUN apt-get update && apt-get install -y --no-install-recommends \
  curl \
  git \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libpng-dev \
  libtiff-dev \
  libjpeg-dev \
  pandoc \
  && rm -rf /var/lib/apt/lists/*

# Set working directory inside container
WORKDIR /srv/shiny-server

# Copy application source files
COPY . /srv/shiny-server/OncoStratify/

# Install R package dependencies (using binary repository where possible for ultra-fast builds)
RUN R -e " \
  # Use Posit Public Package Manager for binary Debian package installation to speed up build
  options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')); \
  \
  install.packages(c( \
    'shiny', \
    'bslib', \
    'dplyr', \
    'ggplot2', \
    'survival', \
    'survminer', \
    'DT', \
    'shinycssloaders', \
    'shinyjs', \
    'bsicons', \
    'randomForestSRC' \
  ), quietly = TRUE); \
  \
  message('✓ All packages successfully installed') \
"

# Prevent Shiny version comparison error site-wide by unsetting SHINY_SERVER_VERSION in Rprofile.site
RUN mkdir -p /usr/local/lib/R/etc /usr/lib/R/etc /etc/R && \
    echo "Sys.setenv(SHINY_SERVER_VERSION = '')" >> /usr/local/lib/R/etc/Rprofile.site && \
    echo "Sys.setenv(SHINY_SERVER_VERSION = '')" >> /usr/lib/R/etc/Rprofile.site && \
    echo "Sys.setenv(SHINY_SERVER_VERSION = '')" >> /etc/R/Rprofile.site

# Set directory permissions for runtime logs and file exports
RUN mkdir -p /srv/shiny-server/OncoStratify/inst/cache && \
    mkdir -p /srv/shiny-server/OncoStratify/logs && \
    chmod -R 777 /srv/shiny-server/OncoStratify

# Expose the default Shiny port
EXPOSE 3838

# Container Healthcheck (Checks every 15s to ensure the application is responsive)
HEALTHCHECK --interval=15s --timeout=5s --start-period=15s --retries=3 \
  CMD curl -f http://localhost:3838/ || exit 1

# Run the Shiny application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/OncoStratify', host = '0.0.0.0', port = 3838)"]
