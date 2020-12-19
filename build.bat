@echo off

SET THIS_SCRIPT_LOCATION=%~dp0
SET HOST_SRC_DIR=%THIS_SCRIPT_LOCATION%
SET HOST_OUT_DIR=%~f1
SET DOCKERFILE_DIRECTORY=%THIS_SCRIPT_LOCATION%dockerdev\windows

if not exist %HOST_OUT_DIR% md %HOST_OUT_DIR%

shift

SET RUN_COMMAND="C:/build/src/dockerdev/windows/build_in_docker.bat"

:GETOPTS
 if /I "%1" == "-r" set RUN_COMMAND=%2 & shift
 shift
if not "%1" == "" goto GETOPTS

docker build -t reify-build-env %DOCKERFILE_DIRECTORY%
if %errorlevel% neq 0 exit /b %errorlevel%

rem Note that without setting --isolation=process, you will encounter errors
rem when attempting to build from a bind mount:
rem https://github.com/docker/for-win/issues/829
@echo off
docker run^
    --rm --name reify-build-env-instance^
    -it^
    --memory 32gb^
    --storage-opt size=80G^
    --mount type=bind,source=%HOST_SRC_DIR%,target=C:\build\src^
    --mount type=bind,source=%HOST_OUT_DIR%,target=C:\build\out^
    reify-build-env^
    %RUN_COMMAND%
if %errorlevel% neq 0 exit /b %errorlevel%
