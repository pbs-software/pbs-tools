@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1 )


:: EDIT THE FOLLOWING PATHS BELOW

set ADM_HOME=C:\Apps\ADMB
set GCC_PATH=E:\Apps\Dev-Cpp\bin
set BAT_PATH=E:\Archive\Bat
set DOS_PATH=E:\Archive\DOS
set TXT_EDIT=C:\Apps\UltraEdit\uedit32.exe

:: Paths dependent on those above
:: ADM directories and files
   set ADM_PATH=%ADM_HOME%\bin
   set ADMinc=%ADM_HOME%\include
   set ADMlib=%ADM_HOME%\lib
   set ADMdoc=%ADM_HOME%\doc
   set ADMhelp=%ADMdoc%\admodb.pdf

:: Utility directory and files
   :: Text viewer
   set AList=%DOS_PATH%\list.com
   :: File manager
   set DirView=%DOS_PATH%\DF.com
   :: DJGPP redirector, revised by Mumit Khan
   set ReDir=%DOS_PATH%\redir.exe

