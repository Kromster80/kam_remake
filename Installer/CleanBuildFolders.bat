cd BuildFull
erase /F /Q /S count.dat KaM_Remake_Server_Status.html KaM_Remake_Server_Status.xml *.log thumbs.db *.ini *.mi PerfLog.txt
erase /F /Q /S /A:H count.dat KaM_Remake_Server_Status.html KaM_Remake_Server_Status.xml *.log thumbs.db *.ini *.mi PerfLog.txt
rmdir /S /Q Logs
rmdir /S /Q Saves
rmdir /S /Q SavesMP
rmdir /S /Q "Crash Reports"
rmdir /S /Q Export

cd ..\BuildUpdate
erase /F /Q /S count.dat KaM_Remake_Server_Status.html KaM_Remake_Server_Status.xml *.log thumbs.db *.ini *.mi PerfLog.txt
erase /F /Q /S /A:H count.dat KaM_Remake_Server_Status.html KaM_Remake_Server_Status.xml *.log thumbs.db *.ini *.mi PerfLog.txt
rmdir /S /Q Logs
rmdir /S /Q Saves
rmdir /S /Q SavesMP
rmdir /S /Q "Crash Reports"
rmdir /S /Q Export