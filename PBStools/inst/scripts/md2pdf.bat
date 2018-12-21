@ECHO OFF
rem Convert mardown file to pdf using html intermediary
rem RH modified to convert GitHub README files to pdf

setlocal
set PBSERROR=

set mdfile=%~n1
set mdext=%~x1

where /q pandoc.exe || ECHO Cound not find pandoc.exe -- install (https://pandoc.org) and put in your PATH && set PBSERROR=1

where /q wkhtmltopdf.exe || ECHO Cound not find wkhtmltopdf.exe -- install (https://wkhtmltopdf.org/) and put in your PATH && set PBSERROR=1

where /q %1 || ECHO Cound not %1 -- supply a markdown file (.md or .Rmd)  && set PBSERROR=1

if not defined PBSERROR (
  pandoc.exe -t html5  --metadata pagetitle="Read Me" --css=readme.css --pdf-engine wkhtmltopdf -o %mdfile%.html %mdfile%%mdext%
  wkhtmltopdf.exe --user-style-sheet readme.css --page-size Letter --encoding utf-8 --margin-top 1in --margin-bottom 1in --margin-left 1in --margin-right 1in %mdfile%.html %mdfile%.pdf
) else (
  ECHO Conversion aborted
)
:exit
set PBSERROR=
set mdfile=
set mdext=
endlocal
:eof
