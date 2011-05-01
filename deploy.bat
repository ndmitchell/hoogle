@echo off
echo Running Hoogle deploy script
if "%1"=="--quick" goto remote

REM Check no patches need pushing
if exist deploy.patch del deploy.patch
darcs send -o deploy.path --quiet
if exist deploy.patch del deploy.patch && echo You must push first && goto error

REM Build Hoogle
if not exist .hpc mkdir .hpc
if not exist .hpc\opt mkdir .hpc\opt
ghc --make -isrc -i. src/Paths.hs src/Main.hs -w -odir .hpc/opt -hidir .hpc/opt -o .hpc/opt/hoogle -threaded -O -package transformers

REM Run the tests
.hpc\opt\hoogle data --redownload
if not %errorlevel%==0 goto error
.hpc\opt\hoogle test --example
if not %errorlevel%==0 goto error

:remote
echo Starting to deploy
ssh ndm@haskell.org -m deploy.sh


goto end

:error
echo Error!

:end
