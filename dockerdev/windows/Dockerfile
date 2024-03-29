FROM mcr.microsoft.com/dotnet/framework/sdk:4.8-windowsservercore-ltsc2019

LABEL description="Environment for building the project in Windows."

SHELL ["powershell", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]


# Specifically install Windows SDK version 10.0.19041.0, for the Windows that's what V8 needs.
# See archive of releases here: https://developer.microsoft.com/en-us/windows/downloads/sdk-archive/
RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile winsdksetup.exe https://go.microsoft.com/fwlink/?linkid=2120843
RUN $p = Start-Process -Wait -PassThru -FilePath 'winsdksetup.exe' -ArgumentList '/features OptionId.WindowsDesktopDebuggers OptionId.DesktopCPPx86 OptionId.DesktopCPPx64 /Quiet /NoRestart /Ceip on'; if (($ret = $p.ExitCode) -and ($ret -ne 3010)) { throw ('winsdksetup.exe install failed with exit code 0x{0:x}' -f $ret) }

# Download the Build Tools bootstrapper, specific versions can be found here:
#   https://docs.microsoft.com/en-us/visualstudio/releases/2019/history#release-dates-and-build-numbers
# We install it to C:\2019\BuildTools instead of the default directory because
# the base image already has a few things installed in the default directory,
# and that really messes with the installer :(.  We just steer clear of that.
# We also need to ensure that "2019" appears in the path, or else Bazel doesn't
# look for 2019 paths.
RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile vs_buildtools.exe https://download.visualstudio.microsoft.com/download/pr/f1e43525-cd53-4012-9644-d7846e2c4963/9eb84f3bf5695fd108713fb15b827fe3755fc7c9ea3fa78eb83ed40015fd866b/vs_BuildTools.exe
# Yup, we're calling the exact same command twice here. Not sure why but the first call
# doesn't actually install anything, but the second one does. I suspect it has something
# to do with the base image already having some components of VS installed and maybe
# that's messing with the vs_buildtools.exe installer, really hard to debug though.
RUN $p = Start-Process -Wait -PassThru -FilePath '.\vs_buildtools.exe' -ArgumentList ' \
  --quiet --wait --norestart --nocache \
  --installPath "C:\2019\BuildTools \
  --add Microsoft.Component.MSBuild \
  --add Microsoft.VisualStudio.Workload.VCTools \
  --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 \
  --add Microsoft.VisualStudio.Component.VC.ATLMFC \
  '; if (($ret = $p.ExitCode) -and ($ret -ne 3010)) { throw ('vs_buildtools.exe install failed with exit code 0x{0:x}' -f $ret) }
RUN $p = Start-Process -Wait -PassThru -FilePath '.\vs_buildtools.exe' -ArgumentList ' \
  --quiet --wait --norestart --nocache \
  --installPath "C:\2019\BuildTools \
  --add Microsoft.Component.MSBuild \
  --add Microsoft.VisualStudio.Workload.VCTools \
  --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 \
  --add Microsoft.VisualStudio.Component.VC.ATLMFC \
  '; if (($ret = $p.ExitCode) -and ($ret -ne 3010)) { throw ('vs_buildtools.exe install failed with exit code 0x{0:x}' -f $ret) }
ENV BAZEL_VC=C:/2019/BuildTools/VC

# Install Chocolatey
RUN iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

RUN choco install -y python3 python2

# Python 2 is needed by Qt in order to build Chromium.
RUN copy C:\Python27\python.exe C:\Python27\python2.exe

# Install MSYS2 and ensure that we have MSYS2's bin directory in our path,
# this is required for proper Bazel setup.  Some of this setup code is taken
# from https://github.com/mback2k/docker-windows-buildbot-msys2/blob/6b02cd848796fd5971e8d4364a0e23af88766f74/Dockerfile.
RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile 7zsetup.exe http://www.7-zip.org/a/7z1604-x64.exe

RUN Invoke-WebRequest -UserAgent 'DockerCI' -outfile msys2-x86_64.tar.xz http://repo.msys2.org/distrib/x86_64/msys2-base-x86_64-20210228.tar.xz
RUN Start-Process .\7zsetup -ArgumentList '/S /D=c:/7zip' -Wait
RUN C:\7zip\7z e msys2-x86_64.tar.xz -Wait
RUN C:\7zip\7z x msys2-x86_64.tar -o"C:\\"
RUN C:\msys64\usr\bin\bash.exe -l -c 'exit 0'; \
  C:\msys64\usr\bin\bash.exe -l -c 'echo "Now installing MSYS2..."'; \
  Get-Process | Where-Object {$_.Path -Like 'C:\msys64\*'} | Stop-Process -Force -PassThru | Wait-Process; \
  Get-Process @('bash', 'dirmngr', 'gpg-agent', 'pacman') -ErrorAction SilentlyContinue | Stop-Process -Force -PassThru | Wait-Process; \
  Write-Host 'Done installing MSYS2 ...';
ADD ./update_path_with_msys.ps1 c:/update_path_with_msys.ps1
RUN powershell -Command c:\update_path_with_msys.ps1

RUN pacman --noconfirm -S zip unzip patch diffutils git gcc python python3

# rules_haskell doesn't currently support beyond this version of Bazel.
RUN choco install -y bazel --version 3.3.1

RUN choco install -y jom --version 1.1.2

# Enable long file path support.  This actually doesn't do anything for the
# container because a restart is required, however it's good for documentation
# at least if you're setting up your host environment.  We work around it in
# the container by setting the Bazel --output_user_root=C:/_bzl command line
# parameter.
RUN Set-ItemProperty -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem' -Name 'LongPathsEnabled' -Value 1

