@rem : This version creates and runs a new bash file Rd2dvi4pbs.sh from Rd2dvi.sh.
@rem : Note, only effective starting with R-2.8.0.

@rem Individual Rd files: C:\Apps\R\R2121\bin\i386\R CMD Rd2dvi --pdf map.Rd
@rem package documentation: C:\Apps\R\R2121\bin\i386\R CMD Rd2dvi --pdf testRd

@ECHO OFF
SETLOCAL ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

if not defined PBS_SETLOCAL (
	SETLOCAL
	SET PBS_SETLOCAL=1 )

if "%1"=="" (
	echo ERROR - you must specify an Rd file
	echo example: %0 glimmer
	goto end )

if "%2"=="" (
	set page=1
	) else (
	set page=%2)

set dviP=.Rd2dvi$
set ext=aux;dvi;idx;ind;log;out;pdf;tex;toc
@ rem set ext=aux;dvi;idx;ilg;ind;log;out;tex;toc
SET PBS_NO_PAUSE=1
call RdevPathCheck.bat

set Path=!pPath!;%SystemRoot%\system32;
set R_Path=!rPath!

if not defined PBSERROR (
	for %%a in (%ext%) do (
		if exist "%1.%%a" (
			rm -f "%1.%%a" ) )
	if exist %dviP% (rm -rf %dviP%)
	@rem %R_Path%\R CMD Rd2dvi --pdf  %1.Rd rem deprecated
	%R_Path%\R CMD Rd2pdf --batch --no-preview %1.Rd
	start "C:\Program Files (x86)\Foxit Software\Foxit PhantomPDF\FoxitPhantomPDF.exe" %1.pdf /A page=1 zoom=150%
	@rem start C:\Apps\MiKTeX\miktex\bin\x64\miktex-texworks.exe %1.pdf
	@rem C:\Apps\Ghostgum\gsview\gsview64.exe %1.pdf

	@rem	%wzzip% %1-Manual.zip %dviP%\%1.*
	@rem	cp -f %dviP%\%1.pdf %1.pdf
	@rem	if exist Rd2.pdf rm -f Rd2.pdf
)

:end

