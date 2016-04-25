Echo Off
rem Batch file to copy a package from a Local (LOC) or Subversion (SVN) trunk directory to %Build%

set mydir=C:\Users\haighr\Files
set logfile=%mydir%\Archive\Logs\svn2git.log

set buildDir=C:\Users\haighr\Files\Projects\R\Build
rem Check that %buildDir% exists
if not exist %buildDir%\NUL goto NoBuild
set buildProj=%buildDir%\%1
set githubDir=C:\Users\haighr\Files\Projects\GitHub

if "%1"=="" (
	ECHO ERROR - you must specify a package name
	ECHO example: %0 PBSmodelling SVN
	goto end 
	) else (
	set package=%1
	)
rem echo %package%

if "%2"=="" (
	SET repo=SVN
	) else (
	SET repo=%2
	)

if "%repo%"=="LOC" (
	call set sourceDir=C:\Users\haighr\Files\Projects\R\Source\Local
	if not exist %sourceDir%\NUL goto NoSource
	call set sourceProj=%%sourceDir%%\%package%
	)
if "%repo%"=="SVN" (
	call set sourceDir=C:\Users\haighr\Files\Projects\R\Source\pbs-software
	if not exist %sourceDir%\NUL goto NoSource
	set string=
	set string=%package%
	call :lenStr !string! nchar
	call set sourceProj=%%sourceDir%%\%%package:~0,3%%-%%package:~3,!nchar!%%\trunk\%package%
	rem Might as well update the Github repository with new files
	call set githubProj=%githubDir%\%%package:~0,3%%-%%package:~3,!nchar!%%\%package%
	)

rem echo %sourceDir% --- %sourceProj%
rem echo %buildDir%  --- %buildProj%
rem echo %githubDir% --- %githubProj%
rem goto:exit

rem Check that the source directory exists
rem if not exist .\%1\NUL goto NoSource
if not exist %sourceProj%\NUL goto NoSource
rem cd /d %sourceDir%

rem Clear the destination directory if it exists
if exist %buildProj%.Rcheck\NUL (
  echo Removing destination %buildProj%.Rcheck
  rmdir /S %buildProj%.Rcheck
  rm -fv %1*.zip
)
rem Goto:Exit
if exist %buildProj%\NUL (
  echo Removing destination %buildProj%
  rmdir /S %buildProj%
)

:OKdest
rem Hidden .svn directories will not get copied
xxcopy /E /I %sourceProj% %buildProj%

if "%repo%"=="LOC" goto copyBuild
if "%package%"=="PBSsatellite" goto copyBuild

if not exist %githubDir%\NUL goto copyBuild
echo ===============================================================================>> %logfile%
echo Copy %1 files from SVN to GIT repo, where>> %logfile%
echo SVN: %sourceProj%>> %logfile%
echo GIT: %githubProj%>> %logfile%
echo DATE: %date%  %time% >> %logfile%
echo.>> %logfile%
xxcopy /C /D /H /R /S /Y /PJ0 /Q1 %sourceProj% %githubProj% >> %logfile%

:copyBuild
Rem Change to the destination for checking and building
cd /d %buildDir%

Goto:Exit

:lenStr %string% nchar ::returns the length of a string minus 3 characters for 'PBS'
rem                    -- string  [in] - variable name containing the string being measured for length
rem                    -- nchar  [out] - variable to be used to return the length of the string
rem Based on 'revStr' from 'devcom': http://www.computerhope.com/forum/index.php?topic=85897.0
	SETLOCAL ENABLEDELAYEDEXPANSION
	rem set line=sdas sfsa fwfwa21321
	set nchar=0
	:LOOP
		call set tmpa=%%string:~!nchar!,1%%%
rem echo !tmpa!
		if not "!tmpa!" equ "" (
			rem set rline=!tmpa!%rline%
			set /a nchar+=1
rem echo !nchar!
			goto LOOP
		)
		set /a nchar-=3  :: PBS = 3 characters
rem echo %nchar%
	rem )
   ENDLOCAL & set /a "%~2"="%nchar%" :: seen outside as !nchar!
exit /b
	rem ( ENDLOCAL & REM RETURN VALUES
	rem 	IF "%~2" NEQ "" SET /a %~2=%nchar%
	rem )
	rem EXIT /b

:NoBuild
Echo You must designate a Build directory with the variable Build
Goto Exit

:NoSource
Echo source project %sourceProj% not found
Goto Exit

:Exit
set sourceDir=
set buildDir=
set githubDir=
set sourceProj=
set buildProj=
set githubProj=
set package=
set string=
set nchar=
set tmpa=
set repo=
set TEMP=



