#!/usr/bin/env bash

set -eu
trap cleanup SIGINT SIGTERM ERR EXIT

setup_colors() {
    if [[ -t 2 ]] && [[ -z "${NO_COLOR-}" ]] && [[ "${TERM-}" != "dumb" ]]; then
        NOFORMAT='\033[0m' RED='\033[0;31m' GREEN='\033[0;32m' ORANGE='\033[0;33m' BLUE='\033[0;34m' PURPLE='\033[0;35m' CYAN='\033[0;36m' YELLOW='\033[1;33m'
    else
        NOFORMAT='' RED='' GREEN='' ORANGE='' BLUE='' PURPLE='' CYAN='' YELLOW=''
    fi
}

usage() {
    cat <<EOF
Usage: $(basename "${BASH_SOURCE[0]}") [-h] [-v]

Install some tools for development setup

Available options:

-h, --help      Print this help and exit
-v, --verbose   Print script debug info
EOF
    exit
}

cleanup() {
    trap - SIGINT SIGTERM ERR EXIT
}


msg() {
    echo >&2 -e "${1-}"
}

die() {
    local msg=$1
    local code=${2-1} # default exit status 1
    msg "${RED}$msg${NOFORMAT}"
    exit "$code"
}

parse_params() {
    while :; do
        case "${1-}" in
            -h | --help) usage ;;
            -v | --verbose) set -x ;;
            --no-color) NO_COLOR=1 ;;
            -?*) die "Unknown option: $1" ;;
            *) break ;;
        esac
        shift
    done

    return 0
}

parse_params "$@"
setup_colors

msg "${CYAN}Read parameters:${NOFORMAT}"
platform=$(uname -s)

cd /tmp
if [ "$platform" = "Linux" ]; then
    curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/linux/init.sh | sh
elif [ "$platform" = "Darwin" ]; then
    curl -fsSL https://raw.githubusercontent.com/asatake/dotfiles/main/macos/init.sh | sh
else
    die "Invalid env: $env"
fi

final_stats=$?
if [[ $final_stats -ne 0 ]]; then
    die "Error Occured!" $final_stats
else
    msg "${GREEN}"
    msg "==============="
    msg "=  INSTALLED  ="
    msg "===============${NOFORMAT}"
fi
