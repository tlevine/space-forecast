.PHONY: install

space.csv:
	spacestatus-export > space.csv

install:
	sudo pip3 install spacestatus-export
