@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the seven PATH variables listed below *****

set R_Base=C:\Apps\R
set R_Root=%R_Base%\R300
set TOOLS_PATH=%R_Base%\Rtools\bin
rem set PERL_PATH=%R_Base%\Rtools\perl\bin
set GCC_PATH=%R_Base%\Rtools\gcc-4.6.3\bin
rem set MINGW_PATH=%R_Base%\Rtools\MinGW\bin
set TEX_PATH=C:\Apps\MiKTeX\miktex\bin
rem set HTMLHELP_PATH=C:\Apps\HHW
set QPDF_PATH=%R_Base%\Rtools\qpdf-4.1.0\bin
set GS_PATH=C:\Apps\Ghostgum\gscript\gs9.07\bin

set R_PATH=%R_Root%\bin\i386
rem set perlR=%R_Root%\share\perl\R
set SVN_PATH=C:\Apps\CollabNetSVC


rem ***** Edit environment variables below *****
set xcopy=%windir%\system32\xcopy.exe
set wzzip=C:\Apps\WinZip\wzzip.exe
set R_PAPERSIZE=letter
set R_OSTYPE=windows
set _R_BUILD_COMPACT_VIGNETTES_=yes
set R_QPDF=%QPDF_PATH%/qpdf.exe
set R_GSCMD=%GS_PATH%/gswin32c.exe

