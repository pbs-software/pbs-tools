@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

if "%1"=="" (
	ECHO ERROR - you must specify a package name
	ECHO example: %0 PBSmodelling true
	goto end 
	) else (
	set package=%1
	)

if "%2"=="" (
	SET cran=true
	) else (
	SET cran=false
	)

if "%package%"=="PBStools" (
  set arch=64
  ) else (
  set arch=64
)

SET PBS_NO_PAUSE=1
call RPathCheck.bat %arch%

if not defined PBSERROR (
  if "%cran%"=="true" (
    R CMD check --as-cran %package%
  ) else (
    R CMD check %package%
  )
)

:end
set package=
set arch=

