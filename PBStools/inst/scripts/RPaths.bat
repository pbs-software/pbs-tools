@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the PATH variables listed below *****

set R_Base=C:\Apps\R
set R_Root=%R_Base%\R323
set TOOLS_PATH=%R_Base%\Rtools\bin
set GCC_PATH=%R_Base%\Rtools\gcc-4.6.3\bin
rem set MINGW_PATH=%R_Base%\Rtools\MinGW\bin
set TEX_PATH=C:\Apps\MiKTeX\miktex\bin
set PERL_PATH=C:\Apps\Perl\bin
set PYTHON_PATH=C:\Apps\Python\P332
rem set HTMLHELP_PATH=C:\Apps\HHW
set QPDF_PATH=%R_Base%\Rtools\qpdf-5.1.2\bin
set GS_PATH=C:\Apps\Ghostscript\gs9.15\bin
set CURL_PATH=%R_Base%\Rtools\curl-7.40.0-win64\bin

set R32_PATH=%R_Root%\bin\i386
set R64_PATH=%R_Root%\bin\x64
rem set perlR=%R_Root%\share\perl\R
set SVN_PATH=C:\Apps\CollabNetSVN
set XPDF_PATH=C:\Apps\R\xpdfbin-win-3.03\bin32
set ORA_PATH=C:\Apps\Oracle11\Home\bin

rem ***** Edit environment variables below *****
rem set xcopy=%windir%\system32\xxcopy.exe
rem set wzzip=C:\Apps\WinZip\wzzip.exe
set R_PAPERSIZE=letter
set R_OSTYPE=windows
set _R_BUILD_COMPACT_VIGNETTES_=yes
set R_QPDF=%QPDF_PATH%\qpdf.exe
set R_GSCMD=%GS_PATH%\gswin64c.exe
set HOME=C:\Users\haighr\Files
set R_TEMP=%HOME%\Temp\R
