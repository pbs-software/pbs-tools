@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1
)

call ADMpaths.bat

if not exist "%ADM_HOME%" (
	ECHO Can't find ADMB's Home - check ADM_HOME "%ADM_HOME%"
	set ADM_ERROR=1
	)
if not exist "%ADM_PATH%\admb.bat" (
	ECHO Can't find admb.bat - check ADM_PATH "%ADM_PATH%"
	set ADM_ERROR=1
	)
if not exist "%GCC_PATH%\g++.exe" (
	ECHO Can't find g++.exe - check GCC_PATH "%GCC_PATH%"
	set ADM_ERROR=1
	)
if not exist "%DOS_PATH%\choice.exe" (
	ECHO Can't find old DOS path - check DOS_PATH "%DOS_PATH%"
	set ADM_ERROR=1
	)
if not exist "%TXT_EDIT%" (
	ECHO Can't find editor - check TXT_EDIT "%TXT_EDIT%"
	set ADM_ERROR=1
	)

rem set oldPath=%Path%
set Path=.;%ADM_PATH%;%GCC_PATH%;%BAT_PATH%;%DOS_PATH%
rem cat %Path%
set TMPDIR=%TMP%

if not defined ADM_ERROR (
	echo All program paths look good
)
if defined ADM_ERROR (
	pause
)
