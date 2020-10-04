&"$PSScriptRoot/getvcbuildtoolsenv.cmd" | Foreach-Object {
    $cmdVar,$cmdVal=$_.split('=')
    if((Get-Item -Path env:$cmdVar -Exclude SilentlyContinue).Value -ne $cmdVal)
    {
        Write-Verbose "setting machine variable $cmdVar to $cmdVal" -Verbose
        [System.Environment]::SetEnvironmentVariable($cmdVar,$cmdVal,[System.EnvironmentVariableTarget]::Machine)
    }
}
