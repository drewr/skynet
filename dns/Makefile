creds.json:
	cp ../.secrets/d1c8ea80 creds.json

.PHONY: preview push

preview: creds.json
	dnscontrol preview
	rm creds.json

push: creds.json
	dnscontrol push
	rm creds.json
