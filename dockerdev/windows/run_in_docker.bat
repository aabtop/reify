@echo off

SET THIS_SCRIPT_LOCATION=%~dp0
SET HOST_SRC_DIR=%THIS_SCRIPT_LOCATION%..\..
SET HOST_OUT_DIR=%~f1
SET DOCKERFILE_DIRECTORY=%THIS_SCRIPT_LOCATION%

if not exist %HOST_OUT_DIR% md %HOST_OUT_DIR%

SET RUN_COMMAND=%2

docker build -t reify-build-env %DOCKERFILE_DIRECTORY%
if %errorlevel% neq 0 exit /b %errorlevel%

rem Note that without setting --isolation=process, you will encounter errors
rem when attempting to build from a bind mount:
rem https://github.com/docker/for-win/issues/829
@echo off
powershell -command ^"docker run^
    --env CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64=$env:CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64^
    --env CODE_SIGNING_CERTIFICATE_WIN_PASSWORD=$env:CODE_SIGNING_CERTIFICATE_WIN_PASSWORD^
    --rm --name reify-build-env-instance^
    --memory 32gb^
    --storage-opt size=80G^
    --mount type=bind,source=%HOST_SRC_DIR%,target=C:\build\src^
    --mount type=bind,source=%HOST_OUT_DIR%,target=C:\build\out^
    reify-build-env^
    %RUN_COMMAND%^"
if %errorlevel% neq 0 exit /b %errorlevel%
