@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

call RPaths.bat

if not exist "%TOOLS_PATH%\tar.exe" (
  ECHO Cannot find Tools - check TOOLS_PATH "%TOOLS_PATH%"
  set PBSERROR=1 )

if not exist "%R64_PATH%\Rcmd.exe" (
  ECHO Cannot find R - check R_PATH "%R_Root%"
  set PBSERROR=1 )

if not exist "%GCC_PATH%\gcc.exe" (
  ECHO Cannot find gcc - check GCC_PATH "%GCC_PATH%"
  set PBSERROR=1 )

rem if not exist "%MINGW_PATH%\gcc.exe" (
rem   ECHO Cannot find MinGW - check MINGW_PATH "%MINGW_PATH%"
rem   set PBSERROR=1 )

if not exist "%TEX_PATH%\latex.exe" (
  ECHO Cannot find latex - check TEX_PATH "%TEX_PATH%"
  set PBSERROR=1 )

if not exist "%PERL_PATH%\perl.exe" (
   ECHO Cannot find perl -  check PERL_PATH "%PERL_PATH%"
   set PBSERROR=1 )

if not exist "%PYTHON_PATH%\python.exe" (
   ECHO Cannot find python -  check PYTHON_PATH "%PYTHON_PATH%"
   set PBSERROR=1 )

rem if not exist "%HTMLHELP_PATH%\hhc.exe" (
rem   ECHO Cannot find HTML Help Workshop - check HTMLHELP_PATH "%HTMLHELP_PATH%"
rem   set PBSERROR=1 )

if not exist "%SVN_PATH%\svn.exe" (
   ECHO Cannot find svn - check SVN_PATH "%SVN_PATH%"
   set PBSERROR=1 )

if not exist "%QPDF_PATH%\qpdf.exe" (
   ECHO Cannot find qpdf - check QPDF_PATH "%QPDF_PATH%"
   set PBSERROR=1 )

if not exist "%GS_PATH%\gswin64c.exe" (
   ECHO Cannot find gswin32c - check GS_PATH "%GS_PATH%"
   set PBSERROR=1 )

if not exist "%XPDF_PATH%\pdfinfo.exe" (
   ECHO Cannot find pdfinfo - check XPDF_PATH "%XPDF_PATH%"
   set PBSERROR=1 )

if not exist "%ORA_PATH%\sqlplus.exe" (
   ECHO Cannot find sqlplus - check ORA_PATH "%ORA_PATH%"
   set PBSERROR=1 )

if not exist "%CURL_PATH%\curl.exe" (
   ECHO Cannot find curl - check CURL_PATH "%CURL_PATH%"
   set PBSERROR=1 )

if "%1"=="" (
	SET arch=64
	) else (
	SET arch=%1
	)
if "%arch%"=="32" (
	SET R_PATH=%R32_PATH%
	) else (
	if "%arch%"=="64" (
		SET R_PATH=%R64_PATH%
		) else (
		ECHO Cannot set architecture %arch% -- specify 32 or 64 in second argument
		set PBSERROR=1
		)
	)
rem echo %PBSERROR%
rem echo %arch%

if not defined PBSERROR (
  set Path=.;%TOOLS_PATH%;%R_PATH%;%GCC_PATH%;%TEX_PATH%;%SVN_PATH%;%QPDF_PATH%;%GS_PATH%;%PERL_PATH%;%PYTHON_PATH%;%XPDF_PATH%;%ORA_PATH%;
  rem ECHO %Path%
  set TMPDIR=%TMP%
  rem set HOMEDRIVE=C:
  rem set HOMEPATH=\Documents and Settings\HaighR
  echo All program paths look good
  if not defined PBS_NO_PAUSE (
    pause
  )
)
if defined PBSERROR (
  pause )

