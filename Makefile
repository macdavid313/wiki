GIT = git

all: push
	echo "Content updated successfully"

stage_all:
	$(GIT) add -A

commit: stage_all
	$(GIT) commit -m "update content"

push: commit
	$(GIT) push -u origin master
