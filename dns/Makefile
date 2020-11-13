creds.json:
	cp ../.secrets/d1c8ea80 creds.json
	if ! jq keys <creds.json; then echo "====> you should run git-crypt unlock <===="; exit 1; fi

.PHONY: preview push clean

preview: creds.json
	dnscontrol preview
	rm creds.json

push: creds.json
	dnscontrol push
	rm creds.json

clean:
	rm -f creds.json
