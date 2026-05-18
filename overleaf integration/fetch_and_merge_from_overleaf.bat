@echo off
setlocal

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "REPO_DIR=%%~fI"
set "SYNC_REF=refs/overleaf-sync/last-synced"
set "PATCH_FILE=%TEMP%\overleaf_changes_%RANDOM%%RANDOM%.patch"

echo.
echo Fetching changes from Overleaf and importing them into the current local branch...
echo Repository: %REPO_DIR%
echo.

cd /d "%REPO_DIR%"
if errorlevel 1 (
  echo Could not open the repository folder.
  pause
  exit /b 1
)

git remote get-url overleaf >nul 2>&1
if errorlevel 1 (
  echo.
  echo This repository does not have an Overleaf remote named "overleaf".
  echo Add it once on this computer, then run this script again:
  echo git remote add overleaf https://git.overleaf.com/6893b89b62ad521339331585
  pause
  exit /b 1
)

for /f "usebackq delims=" %%B in (`git branch --show-current`) do set "CURRENT_BRANCH=%%B"
if "%CURRENT_BRANCH%"=="" (
  echo.
  echo Could not detect the current Git branch.
  pause
  exit /b 1
)

for /f "usebackq delims=" %%S in (`git status --porcelain`) do set "DIRTY=1"
if defined DIRTY (
  echo.
  echo Your working tree has uncommitted changes.
  echo Commit them in GitHub Desktop first, or discard them if you do not want them affected by the import.
  echo.
  git status --short
  echo.
  pause
  exit /b 1
)

echo Current branch: %CURRENT_BRANCH%
echo.
echo Fetching the current Overleaf state...
git fetch overleaf
if errorlevel 1 (
  echo.
  echo Fetch failed. Read the Git message above.
  echo.
  echo If you see a 403 error, Windows may be using old Overleaf credentials.
  echo Delete saved credentials for git.overleaf.com in Windows Credential Manager, then run this script again.
  pause
  exit /b 1
)

git rev-parse --verify overleaf/master >nul 2>&1
if errorlevel 1 (
  echo.
  echo Could not find overleaf/master after fetching.
  echo Check that the Overleaf remote URL points to the right project.
  pause
  exit /b 1
)

git rev-parse --verify %SYNC_REF% >nul 2>&1
if errorlevel 1 (
  echo.
  echo No previous Overleaf sync marker was found.
  echo Creating one at the current overleaf/master and not importing anything yet.
  echo Run push_to_overleaf.bat next if your local files are the version that should be on Overleaf.
  git update-ref %SYNC_REF% overleaf/master
  if errorlevel 1 (
    echo Could not create the local sync marker.
    pause
    exit /b 1
  )
  pause
  exit /b 0
)

for /f "usebackq delims=" %%O in (`git rev-parse overleaf/master`) do set "OVERLEAF_HEAD=%%O"
for /f "usebackq delims=" %%L in (`git rev-parse %SYNC_REF%`) do set "LAST_SYNCED=%%L"

if "%OVERLEAF_HEAD%"=="%LAST_SYNCED%" (
  echo.
  echo Done. There are no new Overleaf changes to import.
  pause
  exit /b 0
)

git merge-base --is-ancestor %SYNC_REF% overleaf/master
if errorlevel 1 (
  echo.
  echo Overleaf history does not build on the last sync marker.
  echo This can happen after a manual reset or forced change on Overleaf.
  echo Ask for help before importing so local files are not overwritten incorrectly.
  pause
  exit /b 1
)

echo Building a patch of Overleaf changes since the last successful sync...
git diff --binary %SYNC_REF% overleaf/master -- > "%PATCH_FILE%"
if errorlevel 1 (
  echo.
  echo Could not build the Overleaf change patch.
  if exist "%PATCH_FILE%" del "%PATCH_FILE%"
  pause
  exit /b 1
)

for %%P in ("%PATCH_FILE%") do set "PATCH_SIZE=%%~zP"
if "%PATCH_SIZE%"=="0" (
  echo.
  echo Done. Overleaf has no file changes to import.
  if exist "%PATCH_FILE%" del "%PATCH_FILE%"
  git update-ref %SYNC_REF% overleaf/master
  pause
  exit /b 0
)

echo Applying Overleaf changes to the current branch...
git apply --index --whitespace=nowarn "%PATCH_FILE%"
if errorlevel 1 (
  echo.
  echo Could not apply the Overleaf changes cleanly.
  echo Your working tree may contain partial changes.
  echo Review Git status before trying again, or ask for help resolving the import.
  if exist "%PATCH_FILE%" del "%PATCH_FILE%"
  pause
  exit /b 1
)

if exist "%PATCH_FILE%" del "%PATCH_FILE%"

git commit -m "Import Overleaf changes"
if errorlevel 1 (
  echo.
  echo The patch applied, but Git could not create the import commit.
  echo Review Git status before trying again.
  pause
  exit /b 1
)

git update-ref %SYNC_REF% overleaf/master
if errorlevel 1 (
  echo.
  echo Import succeeded, but the local sync marker could not be updated.
  echo Run this command manually before the next sync:
  echo git update-ref %SYNC_REF% overleaf/master
  pause
  exit /b 1
)

echo.
echo Done. Overleaf changes were imported and committed locally.
echo You can now push to GitHub from GitHub Desktop if needed.
pause
