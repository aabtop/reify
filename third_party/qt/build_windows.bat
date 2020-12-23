REM @echo off

SET SRC_DIR=%~f1
SET BUILD_DIR=%~f2
SET INSTALL_DIR=%~f3

echo Building and installing Qt...
echo Install dir: %INSTALL_DIR%
echo Build dir: %BUILD_DIR%
echo Src dir: %SRC_DIR%

REM Set up \Microsoft Visual Studio 2015, where <arch> is \c amd64, \c x86, etc.
REM CALL "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64
IF NOT DEFINED BAZEL_VC SET BAZEL_VC="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC"
CALL %BAZEL_VC%"\Auxiliary\Build\vcvarsall.bat" amd64
IF %ERRORLEVEL% NEQ 0 EXIT 1

REM Edit this location to point to the source code of Qt
SET _ROOT=%SRC_DIR%

SET PATH=%_ROOT%\qtbase\bin;%_ROOT%\gnuwin32\bin;%PATH%

REM Uncomment the below line when using a git checkout of the source repository
SET PATH=%_ROOT%\qtrepotools\bin;%PATH%

REM Uncomment the below line when building with OpenSSL enabled. If so, make sure the directory points
REM to the correct location (binaries for OpenSSL).
REM SET PATH=C:\OpenSSL-Win32\bin;%PATH%

REM When compiling with ICU, uncomment the lines below and change <icupath> appropriately:
REM SET INCLUDE=<icupath>\include;%INCLUDE%
REM SET LIB=<icupath>\lib;%LIB%
REM SET PATH=<icupath>\lib;%PATH%

REM Contrary to earlier recommendations, do NOT set QMAKESPEC.

SET _ROOT=

IF NOT EXIST "%BUILD_DIR%" (
  mkdir %BUILD_DIR%
  cd %BUILD_DIR%
  
  echo Building Qt...

  REM Configure the build.
  CALL %SRC_DIR%\configure ^
    -prefix %INSTALL_DIR% -warnings-are-errors -release ^
    -opensource -recheck-all -nomake examples -nomake tests ^
    -confirm-license -no-feature-concurrent -no-feature-xml ^
    -no-feature-testlib -skip qt3d -skip qtactiveqt ^
    -skip qtandroidextras -skip qtcanvas3d -skip qtcharts ^
    -skip qtconnectivity -skip qtdatavis3d -skip qtdoc ^
    -skip qtgamepad -skip qtgraphicaleffects -skip qtimageformats ^
    -skip qtlocation -skip qtmacextras -skip qtmultimedia ^
    -skip qtnetworkauth -skip qtpurchasing -skip qtquickcontrols ^
    -skip qtquickcontrols2 -skip qtremoteobjects -skip qtscript ^
    -skip qtscxml -skip qtsensors -skip qtserialbus ^
    -skip qtserialport -skip qtsvg -skip qtspeech -skip qttools ^
    -skip qttranslations -skip qtvirtualkeyboard -skip qtwayland ^
    -skip qtwinextras -skip qtx11extras -skip qtxmlpatterns ^
    -skip qtwebview -no-webrtc -no-webengine-pepper-plugins ^
    -no-webengine-printing-and-pdf -no-webengine-spellchecker ^
    -no-webengine-kerberos -no-qml-debug -no-sql-odbc ^
    -no-compile-examples -no-pdf-widgets -skip qtquick3d ^
    -skip qtquickcontrols -no-quick-designer -no-quick-canvas ^
    -no-quick-animatedimage -no-quick-shadereffect -no-ffmpeg ^
    -no-opus -no-webp -no-webengine-icu -no-extensions -no-alsa ^
    -no-pulseaudio
  IF %ERRORLEVEL% NEQ 0 EXIT 1

  REM Actually do the build.
  CALL jom
  IF %ERRORLEVEL% NEQ 0 EXIT 1

  echo Done building Qt.
)

REM Install Qt.
echo Installing Qt...
cd %BUILD_DIR%
CALL jom install
REM IF %ERRORLEVEL% NEQ 0 EXIT 1

echo Done installing Qt.
