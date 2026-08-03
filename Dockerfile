FROM rocker/shiny:4.3.1

# Install system dependencies for R packages (libxml2, libssl, etc.)
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages from CRAN
RUN R -e "install.packages(c('shiny', 'bslib', 'bsicons', 'dplyr', 'tidyr', 'ggplot2', 'readxl', 'flextable', 'officer', 'DT', 'devtools', 'shinyjs'), repos='https://cloud.r-project.org/')"

# Copy application files
COPY . /srv/shiny-server/analytix.gui

# Expose Shiny port
EXPOSE 3838

# Run shiny server
CMD ["/usr/bin/shiny-server"]
