@echo off
setlocal

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "REPO_DIR=%%~fI"
set "SYNC_REF=refs/overleaf-sync/last-synced"

echo.
echo Pushing the current committed files to Overleaf without sending GitHub history...
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
  echo Commit them in GitHub Desktop first, or discard them if you do not want them synced.
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
  echo Treating the current overleaf/master as the starting sync point.
  git update-ref %SYNC_REF% overleaf/master
  if errorlevel 1 (
    echo Could not create the local sync marker.
    pause
    exit /b 1
  )
)

for /f "usebackq delims=" %%O in (`git rev-parse overleaf/master`) do set "OVERLEAF_HEAD=%%O"
for /f "usebackq delims=" %%L in (`git rev-parse %SYNC_REF%`) do set "LAST_SYNCED=%%L"

if not "%OVERLEAF_HEAD%"=="%LAST_SYNCED%" (
  echo.
  echo Overleaf has changes that have not been imported locally yet.
  echo Run fetch_and_merge_from_overleaf.bat first, review/commit the result, then run this push script again.
  echo.
  echo Last synced:      %LAST_SYNCED%
  echo Current Overleaf: %OVERLEAF_HEAD%
  pause
  exit /b 1
)

echo Creating a small Overleaf commit from the current committed file tree...
for /f "usebackq delims=" %%T in (`git write-tree`) do set "HEAD_TREE=%%T"
if "%HEAD_TREE%"=="" (
  echo.
  echo Could not read the current file tree.
  pause
  exit /b 1
)

for /f "usebackq delims=" %%C in (`git commit-tree %HEAD_TREE% -p overleaf/master -m "Sync local files to Overleaf"`) do set "NEW_COMMIT=%%C"
if "%NEW_COMMIT%"=="" (
  echo.
  echo Could not create the Overleaf sync commit.
  pause
  exit /b 1
)

echo Pushing the small sync commit to Overleaf...
git push overleaf %NEW_COMMIT%:refs/heads/master
if errorlevel 1 (
  echo.
  echo Push failed. Read the Git message above.
  echo.
  echo If you see "HTTP 413", the current committed file tree is still too large for Overleaf to accept.
  echo Remove or stop tracking unnecessary large files, commit that cleanup, then run this script again.
  echo.
  echo If you see a 403 error, Windows may be using old Overleaf credentials.
  echo Delete saved credentials for git.overleaf.com in Windows Credential Manager, then run this script again.
  pause
  exit /b 1
)

git update-ref %SYNC_REF% %NEW_COMMIT%
if errorlevel 1 (
  echo.
  echo Push succeeded, but the local sync marker could not be updated.
  echo Run this command manually before the next sync:
  echo git update-ref %SYNC_REF% %NEW_COMMIT%
  pause
  exit /b 1
)

echo.
echo Done. The current committed files were pushed to Overleaf.
echo GitHub history was not pushed to Overleaf.
pause
