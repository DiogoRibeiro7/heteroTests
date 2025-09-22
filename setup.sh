#!/usr/bin/env bash
# setup.sh - Bootstrap an R project using renv
# Usage: ./setup.sh
# Requires: bash, Rscript
# This script installs renv if missing, then restores the project's locked package library.


set -euo pipefail
APT_ARGS="-y -qq"

has_net() {
  curl -I -s https://cran.r-project.org >/dev/null 2>&1
}

# run apt-get and capture output; return status
apt_get() {
  if ! has_net; then
    echo "Network unavailable; skipping apt-get $1" >&2
    return 1
  fi
  if [[ -z "${SUDO_BIN}" ]]; then
    command apt-get "$@" ${APT_ARGS} >/tmp/apt.log 2>&1
  else
    ${SUDO_BIN} apt-get "$@" ${APT_ARGS} >/tmp/apt.log 2>&1
  fi
  local status=$?
  if [[ $status -ne 0 ]]; then
    cat /tmp/apt.log >&2
  fi
  return $status
}

# Ensure we run from the project root regardless of the calling location
cd "$(dirname "$0")"

declare -r SCRIPT_NAME="${0##*/}"
RSCRIPT_BIN="$(command -v Rscript || true)"
SUDO_BIN=""

init_renv_bare() {
  "${RSCRIPT_BIN}" -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos='https://cran.r-project.org'); renv::init(bare = TRUE, restart = FALSE)" || \
    error "Failed to initialize renv"

  # verify renv/activate.R exists
  if [[ ! -f renv/activate.R ]]; then
    echo "renv initialization did not create activate.R; retrying..." >&2
    "${RSCRIPT_BIN}" -e "renv::init(bare = TRUE, restart = FALSE)" || true
  fi
}

ensure_activate() {
  if [[ ! -f renv/activate.R ]]; then
    echo "Writing activate.R" >&2
    "${RSCRIPT_BIN}" -e "renv::activate()" || true
  fi
}

ensure_renv_dir() {
  if [[ ! -d renv ]]; then
    echo "Initializing renv library..."
    init_renv_bare
    ensure_activate
  fi
}

error() {
  echo "${SCRIPT_NAME}: ERROR: $1" >&2
  exit 1
}

ensure_r() {
  if command -v Rscript >/dev/null 2>&1; then
    return
  fi

  echo "Rscript not found."

  if ! command -v apt-get >/dev/null 2>&1; then
    error "apt-get unavailable; install R manually and rerun."
  fi

  if ! has_net; then
    error "No internet connection; cannot install R automatically."
  fi

  SUDO_BIN=""
  if [[ $EUID -ne 0 ]]; then
    if command -v sudo >/dev/null 2>&1; then
      SUDO_BIN="sudo"
    else
      error "Need root privileges or sudo to install R via apt-get."
    fi
  fi

  echo "Installing R via apt-get..."
  if apt_get update && \
     apt_get install r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev; then
    RSCRIPT_BIN="$(command -v Rscript || true)"
  else
    cat /tmp/apt.log >&2
    error "apt-get installation failed"
  fi

  if [[ -z "${RSCRIPT_BIN}" ]]; then
    error "Rscript still missing after installation attempt"
  fi
}

ensure_r

# Install common R packages from the system repositories when available.
install_r_packages() {
  local pkg
  for pkg in "$@"; do
    "${RSCRIPT_BIN}" - <<EOF || true
if (!requireNamespace("${pkg}", quietly = TRUE)) {
  install.packages("${pkg}", repos = "https://cran.r-project.org", quiet = TRUE)
}
EOF

    if ! "${RSCRIPT_BIN}" -e "quit(status = ifelse(requireNamespace('${pkg}', quietly = TRUE), 0, 1))"; then
      if command -v apt-get >/dev/null 2>&1; then
        echo "Attempting apt-get install for ${pkg}" >&2
        local apt_pkg="r-cran-$(echo "${pkg}" | tr 'A-Z' 'a-z')"
        if apt_get install "${apt_pkg}"; then
          echo "Installed ${pkg} via apt-get" >&2
        else
          echo "apt-get install for ${pkg} failed" >&2
          return 1
        fi
      fi
    fi
  done
}

if has_net; then
  if ! install_r_packages renv testthat ggplot2 covr lintr styler; then
    error "failed to install some R packages"
  fi
else
  echo "${SCRIPT_NAME}: no network; cannot install R packages" >&2
fi

ensure_renv_dir

# Ensure run from project root: check for DESCRIPTION or renv.lock
if [[ ! -f "DESCRIPTION" && ! -f "renv.lock" ]]; then
  error "No DESCRIPTION or renv.lock found. Run this from the project root."
fi

# Bootstrap renv and restore library
echo "Bootstrapping renv and restoring dependencies..."


ensure_renv_dir

set +e
"${RSCRIPT_BIN}" scripts/bootstrap.R
BOOTSTRAP_STATUS=$?
set -e

# Fallback if bootstrap failed or did not create the renv directory

if [[ ! -d renv || ${BOOTSTRAP_STATUS} -ne 0 ]]; then
  echo "Bootstrap failed or renv missing; running fallback initialization..."
  init_renv_bare
  ensure_activate
fi

retry_restore() {
  local attempt=1
  local max=3
  while [[ $attempt -le $max ]]; do
    if "${RSCRIPT_BIN}" -e "renv::restore(prompt = FALSE)"; then
      return 0
    fi
    echo "renv restore attempt $attempt failed" >&2
    attempt=$((attempt + 1))
  done
  return 1
}

if ! retry_restore; then
  echo "renv restore failed after multiple attempts" >&2
fi

# Verify required packages are installed; install via apt-get when missing
verify_packages() {
  local pkg
  local missing=""
  for pkg in renv testthat ggplot2 covr lintr styler; do
    if ! "${RSCRIPT_BIN}" -e "quit(status = ifelse(requireNamespace('${pkg}', quietly = TRUE), 0, 1))"; then
      echo "Package ${pkg} missing after restore" >&2
      if command -v apt-get >/dev/null 2>&1; then
        local apt_pkg="r-cran-$(echo "${pkg}" | tr 'A-Z' 'a-z')"
        if ! apt_get install "${apt_pkg}"; then
          echo "apt-get install for ${pkg} failed" >&2
        fi
      fi
      if ! "${RSCRIPT_BIN}" -e "quit(status = ifelse(requireNamespace('${pkg}', quietly = TRUE), 0, 1))"; then
        missing+=" ${pkg}"
      fi
    fi
  done
  if [[ -n "${missing}" ]]; then
    error "Missing required packages:${missing}"
  fi
}

# Ensure the renv library exists after all setup attempts
# Ensure the renv library exists after all setup attempts
if [[ ! -d renv/library ]]; then
  echo "renv library not found; creating minimal environment..."
  init_renv_bare
  ensure_activate
fi

# Always attempt a final restore to ensure all packages are installed
if ! "${RSCRIPT_BIN}" -e "renv::restore(prompt = FALSE)"; then
  echo "renv restore failed" >&2
fi
verify_packages

# Final sanity check: create bare library if restore still failed
if [[ ! -d renv/library ]]; then
  echo "Final attempt to create renv library..."
  init_renv_bare
  ensure_activate
  "${RSCRIPT_BIN}" -e "renv::hydrate()" || true
  [[ -d renv/library ]] || error "Unable to create renv library"
fi

# Ensure activate.R exists for future sessions
if [[ ! -f renv/activate.R ]]; then
  echo "activate.R missing after setup; generating..." >&2
  init_renv_bare
  ensure_activate
fi

echo "R project setup complete."

exit 0
