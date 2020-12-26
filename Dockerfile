FROM silex/emacs:latest  

WORKDIR /usr/local/src
COPY publish.el ./
COPY org/ ./org
COPY static ./static

RUN apt update && apt install -y graphviz sqlite3 npm
RUN npm install http-server -g
RUN PROJECT_DIR=$(pwd) emacs --batch -Q -L $(pwd) --eval "(progn (require 'publish) (wiki/publish))"
RUN mkdir -p ./public_html/wiki; mv ./public_html/static ./public_html/wiki

CMD http-server ./public_html
