for /R src %%f in (*.erl) do erlc -o ebin %%f
pause
