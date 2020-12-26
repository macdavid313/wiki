# FROM silex/emacs:27.1-ci-cask 
FROM python:3.8.7-alpine

WORKDIR /usr/local/src
COPY publish.el ./
COPY graph_node_url.py ./
COPY requirements.txt ./
COPY org/ ./org
COPY static ./static

# RUN apt update && apt install -y graphviz sqlite python3.8 python3-pip
RUN apk update && apk add --update graphviz sqlite emacs
RUN python3.8 -m pip install --no-cache-dir -U pip setuptools beautifulsoup4
# RUN python3.8 -m pip install --no-cache-dir -r requirements.txt

RUN PROJECT_DIR=$(pwd) emacs --batch -Q -L $(pwd) --eval "(progn (require 'publish) (wiki/publish))"
RUN python3.8 -m graph_node_url
RUN mkdir -p ./public_html/wiki; mv ./public_html/static ./public_html/wiki

CMD python3.8 -m http.server 8080 --bind 0.0.0.0 -d ./public_html
