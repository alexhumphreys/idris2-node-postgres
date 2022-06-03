IPKG_FILE = ./idris2-node-postgres.ipkg
DOCKER_IMAGE = snazzybucket/idris2-node-postgres
PACK_DB = nightly-220530

repl:
	rlwrap pack --with-ipkg $(IPKG_FILE) --cg node repl ./src/Main.idr

install-node-deps:
	npm install

.PHONY : build
build:
	pack --cg node build $(IPKG_FILE)

docker-build:
	docker build --build-arg db=$(PACK_DB) -t $(DOCKER_IMAGE) .

docker-run:
	docker run --rm -it $(DOCKER_IMAGE) /bin/bash

clean:
	rm -rf ./build
	rm -rf ./node_modules

run-db:
	docker run -p 5432:5432 \
		--name some-postgres \
		-v $(CURDIR)/fixtures:/docker-entrypoint-initdb.d/ \
		-e POSTGRES_PASSWORD=mysecretpassword \
		-e POSTGRES_DB=foo \
		-d postgres

kill-db:
	docker rm -f some-postgres

run:
	PGUSER=postgres \
	PGHOST=127.0.0.1 \
	PGPASSWORD=mysecretpassword \
	PGDATABASE=foo \
	PGPORT=5432 \
	node ./build/exec/idris2-node-postgres

run-node:
	PGUSER=postgres \
	PGHOST=127.0.0.1 \
	PGPASSWORD=mysecretpassword \
	PGDATABASE=foo \
	PGPORT=5432 \
	node

run-pgcli:
	echo pw: mysecretpassword
	pgcli -u postgres -h 127.0.0.1
