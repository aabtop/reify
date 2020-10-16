FROM mcr.microsoft.com/dotnet/framework/sdk:4.8-windowsservercore-ltsc2019

LABEL description="Environment for building the project in Windows."

SHELL ["powershell", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]

# Install Chocolatey
RUN iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# This is a big one, be careful when it gets rebuilt, it can take 15 minutes.
RUN choco install -y visualstudio2019-workload-vctools

RUN choco install -y python3
RUN choco install -y cmake --installargs 'ADD_CMAKE_TO_PATH=System'

# The following two commands make it so that the results of running
# vcvars64.bat are baked into the image.
ADD msvc_docker_env_setup_scripts C:/msvc_env_setup_scripts
RUN powershell C:/msvc_env_setup_scripts/setvcbuildtoolsenv.ps1

# Install MSYS2 and ensure that we have MSYS2's bin directory in our path,
# this is required for proper Bazel setup.
RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile 7zsetup.exe http://www.7-zip.org/a/7z1604-x64.exe
RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile msys2-x86_64-latest.tar.xz http://repo.msys2.org/distrib/msys2-x86_64-latest.tar.xz 

RUN Start-Process .\7zsetup -ArgumentList '/S /D=c:/7zip' -Wait
RUN C:\7zip\7z e msys2-x86_64-latest.tar.xz -Wait
RUN C:\7zip\7z x msys2-x86_64-latest.tar -o"C:\\" 
RUN del *
ENV PATH="C:\msys64\usr\bin;${PATH}"
RUN pacman -S zip unzip patch diffutils git gcc python python3

# rules_haskell doesn't currently support beyond this version of Bazel.
RUN choco install -y bazel --version 3.3.1

# V8
ENV DEPOT_TOOLS_UPDATE=0
