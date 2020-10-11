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

# Due to a bug with Bazel getting confused between WSL's bash, we explicitly
# specify a bash shell for it to use.  This is mainly only used by the Bazel
# npm modules.
ENV BAZEL_SH="C:\Program Files\Git\git-bash.exe"
