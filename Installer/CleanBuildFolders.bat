cd BuildFull
erase /F /Q /S count.dat KaM_Remake_Server_Status.html *.log thumbs.db *.ini *.mi PerfLog.txt
erase /F /Q /S /A:H count.dat KaM_Remake_Server_Status.html *.log thumbs.db *.ini *.mi PerfLog.txt
rmdir /S /Q Logs
rmdir /S /Q Saves
rmdir /S /Q SavesMP

cd ..\BuildUpdate
erase /F /Q /S count.dat KaM_Remake_Server_Status.html *.log thumbs.db *.ini *.mi PerfLog.txt
erase /F /Q /S /A:H count.dat KaM_Remake_Server_Status.html *.log thumbs.db *.ini *.mi PerfLog.txt
rmdir /S /Q Logs
rmdir /S /Q Saves
rmdir /S /Q SavesMP