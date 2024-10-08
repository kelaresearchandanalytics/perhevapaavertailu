#!/bin/bash

## Update configuration files for R and install some libraries.
##
## The URL of the default repository for R packages written to Rprofile.site,
## refers to the CRAN environment variable or to the first argument of this script.
## In order of preference, first argument of the script, the CRAN variable.
## ex. https://cloud.r-project.org, https://cran.r-project.org

set -e

CRAN=${1:-${CRAN:-"https://nexus.kela.fi/repository/r-public/"}}

ARCH=$(uname -m)

##  mechanism to force source installs if we're using RSPM
UBUNTU_VERSION=$(lsb_release -sc)
CRAN_SOURCE=${CRAN/"__linux__/$UBUNTU_VERSION/"/""}

## source install if using RSPM and arm64 image
if [ "$ARCH" = "aarch64" ]; then
    CRAN=$CRAN_SOURCE
fi

## Add a default CRAN mirror
echo "options(repos = c(CRAN = '${CRAN}'), download.file.method = 'libcurl')" >>"${R_HOME}/etc/Rprofile.site"

## Set HTTPUserAgent for RSPM (https://github.com/rocker-org/rocker/issues/400)
cat <<EOF >>"${R_HOME}/etc/Rprofile.site"
# https://docs.rstudio.com/rspm/admin/serving-binaries/#binaries-r-configuration-linux
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))
EOF

## Install OpenBLAS and hot-switching to it
## https://github.com/rocker-org/rocker-versioned2/issues/390
if ! dpkg -l | grep -q libopenblas-dev; then
    apt-get update
    apt-get install -y --no-install-recommends libopenblas-dev
    update-alternatives --set "libblas.so.3-${ARCH}-linux-gnu" "/usr/lib/${ARCH}-linux-gnu/openblas-pthread/libblas.so.3"
fi

## Install littler
if [ ! -x "$(command -v r)" ]; then
    BUILDDEPS="libpcre2-dev \
        liblzma-dev \
        libbz2-dev \
        zlib1g-dev \
        libicu-dev"

    if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
        apt-get update
    fi
    # shellcheck disable=SC2086
    apt-get install -y --no-install-recommends ${BUILDDEPS}
    Rscript -e "install.packages(c('littler', 'docopt'), repos='${CRAN_SOURCE}')"

    # Clean up
    # shellcheck disable=SC2086
    apt-get remove --purge -y ${BUILDDEPS}
    apt-get autoremove -y
    apt-get autoclean -y
fi

## Symlink littler and littler's installation scripts
ln -sf "${R_HOME}/site-library/littler/bin/r" /usr/local/bin/r
ln -sf "${R_HOME}/site-library/littler/examples/installGithub.r" /usr/local/bin/installGithub.r

## Use rocker scripts version install2.r if it exists
if [ -f "/rocker_scripts/bin/install2.r" ]; then
    ln -sf /rocker_scripts/bin/install2.r /usr/local/bin/install2.r
else
    ln -sf "${R_HOME}/site-library/littler/examples/install2.r" /usr/local/bin/install2.r
fi

# Clean up
rm -rf /var/lib/apt/lists/*

# Check the R info
echo -e "Check the littler info...\n"

r --version

echo -e "Check the R info...\n"

R -q -e "sessionInfo()"

echo -e "Setup R, done!"
