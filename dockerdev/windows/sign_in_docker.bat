@echo off

if not defined CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64 (
  echo "Environment variable CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64 is not set."
  EXIT 1
)

SET TIMESTAMP_URL="http://timestamp.sectigo.com"
SET CERTIFICATE_FILE="code_signing_cert.pfx"
SET CERTIFICATE_FILE_BASE64="%CERTIFICATE_FILE%.base64"

REM We use Powershell to echo out the certificate because it's a long value and
REM bat files don't like it when it's larger than 2048.
powershell -command "echo $env:CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64" > %CERTIFICATE_FILE_BASE64%
IF %ERRORLEVEL% NEQ 0 EXIT 1

CALL certutil -decode %CERTIFICATE_FILE_BASE64% %CERTIFICATE_FILE%
IF %ERRORLEVEL% NEQ 0 EXIT 1

CALL signtool sign /f %CERTIFICATE_FILE% /p %CODE_SIGNING_CERTIFICATE_WIN_PASSWORD% /tr %TIMESTAMP_URL% /td sha256 C:\build\out\hypo.exe
IF %ERRORLEVEL% NEQ 0 EXIT 1

CALL signtool sign /f %CERTIFICATE_FILE% /p %CODE_SIGNING_CERTIFICATE_WIN_PASSWORD% /tr %TIMESTAMP_URL% /td sha256 C:\build\out\ide\*.exe C:\build\out\ide\*.dll C:\build\out\ide\qt\plugins\platforms\*.dll
IF %ERRORLEVEL% NEQ 0 EXIT 1

CALL del %CERTIFICATE_FILE% %CERTIFICATE_FILE_BASE64%

CALL echo Done!
