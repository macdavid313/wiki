GIT = git

all: 
	echo "Dummpy Makfile"

quick_update:
	$(GIT) add -A
	$(GIT) commit -m "update content"
	$(GIT) push -u origin master
