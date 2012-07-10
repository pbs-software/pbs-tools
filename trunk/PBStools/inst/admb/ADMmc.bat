@Echo Off
  Ecoh Run prog.exe with 'P1' simulations, saved every 'P2' steps
  if !%1==! Goto Exit
  if !%2==! Goto Exit
  if !%3==! Goto Exit

:: Echo On
  %1.exe -mcmc %2 -mcsave %3 > %1mc.log
  %1.exe -mceval > %1mc.dat >> %1mc.log
  goto End

:Exit
  echo ERROR - Specify: (1) a package name, (2) # simulations, and (3) sampling frequency
  echo example: %0 POP 100000 1000

:End