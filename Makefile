
install: install-git-secret install-stack


install-git-secret:
	bash install.sh

install-stack:
	stack install
