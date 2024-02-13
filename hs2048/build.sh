stack build
sass static/main.scss static/main.css
cp static/* "$(stack path --local-install-root)/bin/app.jsexe/."
