#!/bin/bash
set -euo pipefail

SRC_DIR=$(realpath ${1})
BUILD_DIR=$(realpath ${2})
INSTALL_DIR=$(realpath ${3})

if [ ! -d ${BUILD_DIR} ]; then (
  mkdir -p ${BUILD_DIR}
  cd ${BUILD_DIR}
  
  echo Building Qt...

  ${SRC_DIR}/configure \
    -prefix ${INSTALL_DIR} -warnings-are-errors -release \
    -opensource -recheck-all -nomake examples -nomake tests \
    -confirm-license -platform linux-clang -no-feature-concurrent \
    -no-feature-xml -no-feature-testlib -skip qt3d -skip qtactiveqt \
    -skip qtandroidextras -skip qtcanvas3d -skip qtcharts \
    -skip qtconnectivity -skip qtdatavis3d -skip qtdoc \
    -skip qtgamepad -skip qtgraphicaleffects -skip qtimageformats \
    -skip qtlocation -skip qtmacextras -skip qtmultimedia \
    -skip qtnetworkauth -skip qtpurchasing -skip qtquickcontrols \
    -skip qtquickcontrols2 -skip qtremoteobjects -skip qtscript \
    -skip qtscxml -skip qtsensors -skip qtserialbus \
    -skip qtserialport -skip qtsvg -skip qtspeech -skip qttools \
    -skip qttranslations -skip qtvirtualkeyboard -skip qtwayland \
    -skip qtwinextras -skip qtx11extras -skip qtxmlpatterns \
    -skip qtwebview -no-webrtc -no-webengine-pepper-plugins \
    -no-webengine-printing-and-pdf -no-webengine-spellchecker \
    -no-webengine-kerberos -no-qml-debug -no-sql-odbc \
    -no-compile-examples -no-pdf-widgets -skip qtquick3d \
    -skip qtquickcontrols -no-quick-designer -no-quick-canvas \
    -no-quick-animatedimage -no-quick-shadereffect -no-ffmpeg \
    -no-opus -no-webp -no-webengine-icu -no-extensions -no-alsa \
    -no-pulseaudio -xcb -xcb-xlib -bundled-xcb-xinput \
    -feature-vulkan -I ${VULKAN_SDK}/include -L ${VULKAN_SDK}/lib

  make -j $(nproc)

  echo Done building Qt.
) fi

echo Installing Qt...
cd ${BUILD_DIR}
make install -j $(nproc)

echo Done installing Qt.
