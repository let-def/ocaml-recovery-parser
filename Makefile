# Copyright (c) 2019 Frédéric Bour
#
# SPDX-License-Identifier: MIT

all:
	dune build lib/recovery_parser.cma

clean:
	dune clean
