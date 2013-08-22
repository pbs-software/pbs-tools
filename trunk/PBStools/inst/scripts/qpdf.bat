@ECHO OFF
rem Compress a PDF file with qpdf.exe
rem http://qpdf.sourceforge.net/files/qpdf-manual.html

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

set qpdf=C:\Apps\R\Rtools\qpdf-4.1.0\bin\qpdf.exe
%qpdf%  %infile%.pdf  --stream-data=compress --object-streams=generate  %outfile%.pdf

:end
set qpdf=
set infile=
set outfile=
