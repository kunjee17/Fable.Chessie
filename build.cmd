@echo off
cls

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

REM pushd Content\
REM call build.cmd
REM call build.cmd Clean
REM popd

packages\build\FAKE\tools\FAKE.exe build.fsx %*
