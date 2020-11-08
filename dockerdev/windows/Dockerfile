FROM mcr.microsoft.com/dotnet/framework/sdk:4.8-windowsservercore-ltsc2019

LABEL description="Environment for building the project in Windows."

SHELL ["powershell", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]

# Install Chocolatey
RUN iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# This is a big one, be careful when it gets rebuilt, it can take 15 minutes.
RUN choco install -y visualstudio2019-workload-vctools

RUN choco install -y python3
RUN choco install -y cmake --installargs 'ADD_CMAKE_TO_PATH=System'


# Install MSYS2 and ensure that we have MSYS2's bin directory in our path,
# this is required for proper Bazel setup.  Some of this setup code is taken
# from https://github.com/mback2k/docker-windows-buildbot-msys2/blob/6b02cd848796fd5971e8d4364a0e23af88766f74/Dockerfile.
RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile 7zsetup.exe http://www.7-zip.org/a/7z1604-x64.exe
RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile msys2-x86_64-latest.tar.xz http://repo.msys2.org/distrib/msys2-x86_64-latest.tar.xz 
RUN Start-Process .\7zsetup -ArgumentList '/S /D=c:/7zip' -Wait
RUN C:\7zip\7z e msys2-x86_64-latest.tar.xz -Wait
RUN C:\7zip\7z x msys2-x86_64-latest.tar -o"C:\\"
RUN C:\msys64\usr\bin\bash.exe -l -c 'exit 0'; \
  C:\msys64\usr\bin\bash.exe -l -c 'echo "Now installing MSYS2..."'; \
  C:\msys64\usr\bin\bash.exe -l -c 'pacman -Syuu --needed --noconfirm --noprogressbar'; \
  C:\msys64\usr\bin\bash.exe -l -c 'pacman -Syu  --needed --noconfirm --noprogressbar'; \
  C:\msys64\usr\bin\bash.exe -l -c 'pacman -Su   --needed --noconfirm --noprogressbar'; \
  C:\msys64\usr\bin\bash.exe -l -c 'pacman -Scc --noconfirm'; \
  C:\msys64\usr\bin\bash.exe -l -c 'echo "Successfully installed MSYS2"'; \
  C:\msys64\usr\bin\bash.exe -l -c 'exit 0'; \
  Get-Process | Where-Object {$_.Path -Like 'C:\msys64\*'} | Stop-Process -Force -PassThru | Wait-Process; \
  Get-Process @('bash', 'dirmngr', 'gpg-agent', 'pacman') -ErrorAction SilentlyContinue | Stop-Process -Force -PassThru | Wait-Process; \
  Remove-Item @('C:\$Recycle.Bin\*') -Force -Recurse -ErrorAction Continue; \
  Write-Host 'Cleared Recycle Bin ...';
ADD ./update_path_with_msys.ps1 c:/update_path_with_msys.ps1
RUN powershell -Command c:\update_path_with_msys.ps1

RUN pacman --noconfirm -S zip unzip patch diffutils git gcc python python3

# rules_haskell doesn't currently support beyond this version of Bazel.
RUN choco install -y bazel --version 3.3.1

# Enable long file path support, required by Bazel.
RUN Set-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem' -Name 'LongPathsEnabled' -Value 1

# V8

# Specifically install Windows SDK version 10.0.19041.0, for the Windows that's what V8 needs.
RUN (New-Object Net.WebClient).DownloadFile('https://download.microsoft.com/download/1/c/3/1c3d5161-d9e9-4e4b-9b43-b70fe8be268c/windowssdk/winsdksetup.exe', 'winsdksetup.exe')
RUN $p = Start-Process -Wait -PassThru -FilePath 'winsdksetup.exe' -ArgumentList '/features OptionId.WindowsDesktopDebuggers OptionId.DesktopCPPx86 OptionId.DesktopCPPx64 /Quiet /NoRestart /Ceip on'; if (($ret = $p.ExitCode) -and ($ret -ne 3010)) { throw ('winsdksetup.exe install failed with exit code 0x{0:x}' -f $ret) }
