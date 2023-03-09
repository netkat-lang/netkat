# Copyright 2020-present Cornell University
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

NAME=nerode

.PHONY: all build clean test

all: build

build:
	dune build @install

doc:
	dune build @doc
	rm -rf ./docs
	mv _build/default/_doc/_html ./docs

run:
	dune exec $(NAME)

install:
	dune install

test:
	dune runtest

clean:
	dune clean
	rm -rf ./docs
