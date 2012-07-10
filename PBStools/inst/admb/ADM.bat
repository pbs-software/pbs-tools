@ECHO OFF
:: By Jon Schnute - Version 1A (Created January 23, 1998)
::                - Version 1B (Revised July 20, 2000)
::                - version 2A (Revised to GCC July 23, 2001)
::                - version 2B (Revised from NT to W2K, February 2, 2002)
::                - version 3A (Revised July 18, 2007, for ACMB`s new structure)
:: Batch file to control AD Model Builder programming
::   with Mingw in Windows 95/NT
:: Required utilities in DOS_PATH: 
::   BlankL, Ecoh, SetEnv, Choice.Exe (from DOS 6.x)
::   Redir.exe (from DJGPP and Mumit Khan) not needed in W2K
:: The following utilities are also used:
::   text editor (%TXT_EDIT%)
::   ascii file lister (%AList%)
::   directory viewer (%DirView%)
::   ADM file cleaner (ADMclean.bat)
:: In Windows 95, configure with lots of environment space
::   e.g., open a DOS Window with: Command.exe /E:4096
::   or, from a desktop icon, set:
::     Properties, Memory, Initial environment 4096
:: Customize the environment variable settings below:


if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1
)
if "%1"=="" (
	echo ERROR - you must specify a package name
	echo example: %0 POP
	goto end
)
call ADMcheck.bat
if defined ADM_ERROR goto Quit

:: Initial settings for some variables
  Set CPPFile=%1
  Set CPPFold=%CPPFile%
  Set Run_out=T

:SetScr
  Cls
  Echo ษอออออออออออออออออออออออออออออออออออออออออออออออออออป
  Echo บ ADM Options: (mingw G++ 3.4.5)          Version 3Aบ
  Echo ฬอออออออออออออออออออออออออัอออออออออออออออออออออออออน
  Echo บ T - edit .Tpl file      ณ C - view .Cor file      บ
  Echo บ D - edit .Dat file      ณ S - view .Std file      บ
  Echo บ P - edit .Pin file      ณ A - view .pAr file      บ
  Echo บ                         ณ L - view .Log file      บ
  Echo บ                         ณ E - view .rEp file      บ
  Echo บ                         ณ U - view .rUn file      บ
  Echo ฬอออออออออออออออออออออออออุอออออออออออออออออออออออออน
  Echo บ M - Make                ณ R - Run .exe            บ
  Echo ฬอออออออออออออออออออออออออุอออออออออออออออออออออออออน
  Echo บ G - toGgle output       ณ F - File prefix         บ
  Echo ฬอออออออออออออออออออออออออุอออออออออออออออออออออออออน
  Echo บ H - DOS sHell           ณ X - eXit to DF          บ
  Echo บ V - enVironment         ณ ? - help                บ
  Echo บ Y - tidY directory      ณ Q - Quit                บ
  Echo ศอออออออออออออออออออออออออฯอออออออออออออออออออออออออผ
  BlankL
  Ecoh Current file prefix:  %CPPFile%
  If !%Run_out%==!T Ecoh Current output:  *.run
  If !%Run_out%==!F Ecoh Current output:  StdOut
  BlankL

:Choose
  choice /c:tpdcsalmrfhxyqugv?e    /n Which?_
  ::        0000000001111111111
  ::        1234567890123456789

  If ErrorLevel == 19 Goto V_rep
  If ErrorLevel == 18 Goto Help
  If ErrorLevel == 17 Goto CPPsets
  If ErrorLevel == 16 Goto T_out
  If ErrorLevel == 15 Goto V_run
  If ErrorLevel == 14 Goto Quit
  If ErrorLevel == 13 Goto Tidy
  If ErrorLevel == 12 Goto Xdf
  If ErrorLevel == 11 Goto DOS
  If ErrorLevel == 10 Goto FileName
  If ErrorLevel ==  9 Goto Run
  If ErrorLevel ==  8 Goto Make
  If ErrorLevel ==  7 Goto V_log
  If ErrorLevel ==  6 Goto V_par
  If ErrorLevel ==  5 Goto V_std
  If ErrorLevel ==  4 Goto V_cor
  If ErrorLevel ==  3 Goto E_dat
  If ErrorLevel ==  2 Goto E_pin
  If ErrorLevel ==  1 Goto E_tpl

:Pause
  Pause Press return to continue . . .
  GoTo SetScr

:PauseView
  Pause Press return to continue . . .
  GoTo V_log

:T_out
  If !%Run_out%==!T Goto T_out2
  Set Run_out=T
  Goto SetScr

:T_out2
  Set Run_out=F
  Goto SetScr

:V_cor
  %AList% %CPPFile%.cor
  Goto SetScr

:V_std
  %AList% %CPPFile%.std
  Goto SetScr

:V_par
  %AList% %CPPFile%.par
  Goto SetScr

:V_rep
  %AList% %CPPFile%.rep
  Goto SetScr

:V_run
  %AList% %CPPFile%.run
  Goto SetScr

:V_log
  %AList% %CPPFile%.log
  Goto SetScr

:DOS
  Set OldPrompt=%Prompt%
  Set Prompt=$P [CPP]$G
  Start Command /E:4096
  Set Prompt=%OldPrompt%
  Set OldPrompt=
  GoTo SetScr

:E_dat
  start %TXT_EDIT% %CPPFile%.dat
  GoTo SetScr

:E_pin
  start %TXT_EDIT% %CPPFile%.pin
  GoTo SetScr

:E_tpl
  start %TXT_EDIT% %CPPFile%.cpp %CPPFile%.log %CPPFile%.tpl
  GoTo SetScr

:FileName
  Set CPPFold=%CPPFile%
  Dir *.tpl
  SetEnv CPPFile "File prefix? (omit .tpl)"
  Call SetEnvr
  If !%CPPFile% == ! Set CPPFile=%CPPFold%
  GoTo SetScr

:CPPSets
  Ecoh Root directories:
  Echo   ADM_HOME : %ADM_HOME%
  Echo   ADM_PATH : %ADM_PATH%
  Echo   GCC_PATH : %GCC_PATH%
  Echo   DOS_PATH : %DOS_PATH%
  Echo   TXT_EDIT : %TXT_EDIT%
  Pause
  GoTo SetScr

:Help
  Start %ADMhelp%
  Goto SetScr

:Make
  :: Fournier: tpl2cpp %1 > tmp_admb && mycc %1 && mylink %1
  Echo ********** TPL to CPP > %CPPFile%.log
  Echo On
  tpl2cpp %CPPFile% 2>> %CPPFile%.log
  @Echo off
  BlankL >> %CPPFile%.log

  Echo ********** Compile >> %CPPFile%.log
  Echo On
  g++ -w -g -DUSE_LAPLACE -D__GNUDOS__ -O3 -c -fpermissive -Wno-deprecated  -DOPT_LIB -I. -I"%ADM_HOME%\include" %CPPFile%.cpp 2>> %CPPFile%.log
  @Echo off
  BlankL >> %CPPFile%.log

  Echo ********** Link >> %CPPFile%.log
  Echo On
  g++ -o%CPPFile%.exe %CPPFile%.o  -L"%ADM_HOME%\lib" -ldf1b2stub -ladmod -ladt -lado  -ldf1b2stub -lado 2>> %CPPFile%.log
  @Echo off
  GoTo PauseView

:Run
  If !%Run_out%==!F Goto Run2
  Echo on
  :: %ReDir% -o %CPPFile%.run -eo %CPPFile%.Exe
  %CPPFile%.exe > %CPPFile%.run
  Pause
  Echo off
  GoTo V_run

:Run2
  %CPPFile%.exe
  GoTo Pause

:Tidy
  Call ADMclean
  Goto Pause

:Xdf
  DF
  GoTo SetScr

:Quit
  ENDLOCAL

