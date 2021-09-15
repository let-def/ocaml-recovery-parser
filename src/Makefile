# Copyright (c) 2019 Frédéric Bour
#
# SPDX-License-Identifier: MIT

all:
	dune build driver.bc

test:
	dune exec ./driver.bc driver.ml

test-incorrect:
	dune exec ./driver.bc driver.ml.incorrect
