rmdir /S /Q ..\webui\WebContent\static
mkdir ..\webui\WebContent\static
mkdir ..\webui\WebContent\static\css
mkdir ..\webui\WebContent\static\js
mkdir ..\webui\WebContent\static\media

copy build ..\webui\WebContent
copy build\static\css ..\webui\WebContent\static\css
copy build\static\js ..\webui\WebContent\static\js
copy build\static\media ..\webui\WebContent\static\media
