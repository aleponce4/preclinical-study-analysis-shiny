#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: bash automation/scripts/install_system_deps.sh [--latex tinytex|texlive|none]

Install system packages required for automated report rendering on Debian/Ubuntu.

Options:
  --latex tinytex   Install pandoc and build libraries only. TinyTeX is installed later from R.
  --latex texlive   Install pandoc plus a full system TeX Live toolchain.
  --latex none      Install pandoc only.
  -h, --help        Show this help text.

Default:
  --latex tinytex
EOF
}

require_root() {
  if [[ "${EUID}" -ne 0 ]]; then
    echo "This script must run with sudo/root because it installs apt packages." >&2
    exit 1
  fi
}

detect_distro() {
  if [[ ! -r /etc/os-release ]]; then
    echo "Unsupported Linux host: /etc/os-release not found." >&2
    exit 1
  fi

  # shellcheck disable=SC1091
  source /etc/os-release

  case "${ID:-}" in
    ubuntu|debian)
      ;;
    *)
      echo "Unsupported distribution: ${ID:-unknown}. This installer currently supports Debian/Ubuntu only." >&2
      exit 1
      ;;
  esac
}

install_packages() {
  local latex_mode="$1"
  local -a packages=(
    pandoc
    curl
    git
    make
    gcc
    g++
    libcurl4-openssl-dev
    libfontconfig1-dev
    libfribidi-dev
    libharfbuzz-dev
    libssl-dev
    libxml2-dev
  )

  case "${latex_mode}" in
    tinytex)
      ;;
    texlive)
      packages+=(
        texlive-latex-extra
        texlive-fonts-recommended
        texlive-fonts-extra
        texlive-xetex
      )
      ;;
    none)
      ;;
    *)
      echo "Invalid --latex value: ${latex_mode}" >&2
      usage
      exit 1
      ;;
  esac

  export DEBIAN_FRONTEND=noninteractive
  apt-get update
  apt-get install -y "${packages[@]}"
}

main() {
  local latex_mode="tinytex"

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --latex)
        shift
        [[ $# -gt 0 ]] || { echo "Missing value for --latex" >&2; exit 1; }
        latex_mode="$1"
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      *)
        echo "Unknown argument: $1" >&2
        usage
        exit 1
        ;;
    esac
    shift
  done

  require_root
  detect_distro
  install_packages "${latex_mode}"

  cat <<'EOF'
System package installation complete.

Next steps:
  Rscript --vanilla -e "install.packages('renv', repos='https://cloud.r-project.org')"
  Rscript --vanilla -e "renv::restore(prompt = FALSE)"
  Rscript --vanilla automation/bootstrap_reports.R
EOF
}

main "$@"
