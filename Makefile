GIT = git
DOCKER = docker
CONTAINER = publish-wiki
PWD = ` pwd `
PORT = 8080

all: 
	echo "Dummpy Makfile"

quick_update:
	$(GIT) add -A
	$(GIT) commit -m "update content"
	$(GIT) push -u origin master

build_container:
	$(DOCKER) build -t $(CONTAINER) $(PWD)

start_server: build_container
	$(DOCKER) run -p $(PORT):80 --rm -d $(CONTAINER)

