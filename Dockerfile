FROM silex/emacs:27.1-dev

WORKDIR /usr/local/src
COPY publish.el ./
COPY org/ ./org
COPY static ./static

RUN apt update && apt install -y graphviz sqlite nodejs npm
RUN npm install -g http-server
RUN PROJECT_DIR=$(pwd) emacs --batch -Q -L $(pwd) --eval "(progn (require 'publish) (wiki/publish))"
RUN mkdir -p ./public_html/wiki; mv ./public_html/static ./public_html/wiki

CMD http-server $(pwd)/public_html -p 80 --silent
