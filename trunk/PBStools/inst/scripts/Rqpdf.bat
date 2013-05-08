@ECHO OFF
rem Compress a PDF file with qpdf.exe
rem http://qpdf.sourceforge.net/files/qpdf-manual.html
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

if "%1"=="" (
  echo ERROR - you must specify a PDF file
  echo example: %0 infile outfile
  goto:end 
) else (
  set infile=%1)

if "%2"=="" (
	set outfile=%infile%
	) else (
	set outfile=%2)

SET PBS_NO_PAUSE=1
call RPathCheck.bat

rem ------------------------------------
rem Taken from R's tools::compactPDF 
rem  Either alone won't reduce PDF size
rem  as much as both run consecutively.
rem ------------------------------------
gswin32c -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dPDFSETTINGS=/ebook -dCompatibilityLevel=1.5 -dAutoRotatePages=/None -sOutputFile=gs%outfile%.pdf %infile%.pdf
qpdf --stream-data=compress --object-streams=generate gs%outfile%.pdf %outfile%.pdf

:end
set infile=
set outfile=
