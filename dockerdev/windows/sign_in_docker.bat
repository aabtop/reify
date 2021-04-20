@echo off

if not defined CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64 (
  echo "Environment variable CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64 is not set."
  EXIT 1
)

REM Bazel leaves everything as read-only, so we must make the files writable
REM in order to sign them.
CALL attrib -r C:\build\out\*.* /s

SET TIMESTAMP_URL="http://timestamp.sectigo.com"
SET CERTIFICATE_FILE="code_signing_cert.pfx"
SET CERTIFICATE_FILE_BASE64="%CERTIFICATE_FILE%.base64"

SET EXITCODE=0

REM We use Powershell to echo out the certificate because it's a long value and
REM bat files don't like it when it's larger than 2048.
powershell -command "echo $env:CODE_SIGNING_CERTIFICATE_WIN_PFX_SHA256_BASE64" > %CERTIFICATE_FILE_BASE64%
IF %ERRORLEVEL% NEQ 0 goto :error

CALL certutil -decode %CERTIFICATE_FILE_BASE64% %CERTIFICATE_FILE%
IF %ERRORLEVEL% NEQ 0 goto :error

CALL signtool sign /f %CERTIFICATE_FILE% /p %CODE_SIGNING_CERTIFICATE_WIN_PASSWORD% /tr %TIMESTAMP_URL% /td sha256 C:\build\out\hypo.exe
IF %ERRORLEVEL% NEQ 0 goto :error

CALL signtool sign /f %CERTIFICATE_FILE% /p %CODE_SIGNING_CERTIFICATE_WIN_PASSWORD% /tr %TIMESTAMP_URL% /td sha256 C:\build\out\*.exe C:\build\out\*.dll C:\build\out\qt\plugins\platforms\*.dll
IF %ERRORLEVEL% NEQ 0 goto :error

CALL echo Done signing, deleting temporary files...
goto :cleanup

:error
SET EXITCODE=1
CALL echo An error occurred above, cleaning up temporary files now...

:cleanup
CALL del %CERTIFICATE_FILE%
CALL del %CERTIFICATE_FILE_BASE64%

CALL echo Done!

EXIT %EXITCODE%