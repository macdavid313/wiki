GIT = git
DOCKER = docker
CONTAINER = publish-wiki
PWD = ` pwd `

all: 
	echo "Dummpy Makfile"

quick_update:
	$(GIT) add -A
	$(GIT) commit -m "update content"
	$(GIT) push -u origin master

build_container:
	$(DOCKER) build -t $(CONTAINER) $(PWD)
