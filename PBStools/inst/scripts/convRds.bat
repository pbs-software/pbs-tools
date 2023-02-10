@ECHO OFF
if not defined PBS_SETLOCAL (
	SETLOCAL
	SET PBS_SETLOCAL=1 )

if "%1"=="" (
	echo ERROR - you must specify an R project with 'man' subdirectory
	echo example: %0 PBSawatea
	goto end )

if "%2"=="" (
	set page=1
	) else (
	set page=%2)

set dviP=.Rd2dvi$
set ext=aux;dvi;idx;ilg;ind;log;out;pdf;tex;toc
SET PBS_NO_PAUSE=1
call RPathCheck.bat

if not defined PBSERROR (
	for %%a in (%ext%) do (
		if exist "%1.%%a" (
			rm -f "%1.%%a" ) )
	if exist Rd2.pdf (rm -rf Rd2.pdf)
	if exist %1.pdf (rm -rf %1.pdf)
	%R_Path%\R CMD Rd2pdf --no-clean --output=%1.pdf %1
	rem cp Rd2.pdf %1.pdf
)

:end

