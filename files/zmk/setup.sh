#!/bin/sh
set -e

WORKSPACE="$HOME/src/zmk-charybdis"
ZEPHYR_SDK_VERSION="0.17.0"
ZEPHYR_SDK_INSTALL_DIR="$HOME/.local/zephyr-sdk-${ZEPHYR_SDK_VERSION}"

# Install Zephyr SDK if missing
if [ ! -d "$ZEPHYR_SDK_INSTALL_DIR" ]; then
    echo "Installing Zephyr SDK ${ZEPHYR_SDK_VERSION}..."
    SDK_URL="https://github.com/zephyrproject-rtos/sdk-ng/releases/download"
    SDK_FILE="zephyr-sdk-${ZEPHYR_SDK_VERSION}_linux-x86_64_minimal.tar.xz"
    wget -q "${SDK_URL}/v${ZEPHYR_SDK_VERSION}/${SDK_FILE}" -O "/tmp/${SDK_FILE}"
    tar xf "/tmp/${SDK_FILE}" -C "$HOME/.local/"
    "$ZEPHYR_SDK_INSTALL_DIR/setup.sh" -t arm-zephyr-eabi
    rm "/tmp/${SDK_FILE}"
    echo "Zephyr SDK installed to $ZEPHYR_SDK_INSTALL_DIR"
fi

# Enter Guix shell from workspace dir
cd "$WORKSPACE"
exec guix shell -m guix-manifest.scm -- sh -c '
    export ZEPHYR_SDK_INSTALL_DIR="'"$ZEPHYR_SDK_INSTALL_DIR"'"
    export ZEPHYR_BASE="$PWD/zephyr"
    export CMAKE_PREFIX_PATH="$PWD/zephyr/share/zephyr-package/cmake"
    unset C_INCLUDE_PATH CPLUS_INCLUDE_PATH LIBRARY_PATH
    if [ ! -d zmk ]; then
        echo "Initializing west workspace..."
        west init -l config
        west update
    fi
    echo "Ready. Build with:"
    echo "  west build -s zmk/app -b nice_nano -- -DSHIELD=charybdis_left_bt -DZMK_CONFIG=\$(pwd)/config"
    exec "$SHELL"
'
