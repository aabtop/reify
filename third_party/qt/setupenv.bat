@echo off

REM Set up \Microsoft Visual Studio 2015, where <arch> is \c amd64, \c x86, etc.
CALL "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64

REM Edit this location to point to the source code of Qt
SET _ROOT=C:\Users\Andrew\src\qt5

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

REM Keeps the command line open when this script is run.
cmd /k
