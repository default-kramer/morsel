FOR /d /r . %%d IN ("compiled") DO @IF EXIST "%%d" rd /s /q "%%d"