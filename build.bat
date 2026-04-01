cd src
cd bank-application-frontend
call yarn build package
call updateWebUI.bat
cd ..
cd ..
call mvnw clean package
