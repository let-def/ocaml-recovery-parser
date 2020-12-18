all:
	dune build lib/recovery_parser.cma

test:
	dune exec src/driver.exe

clean:
	dune clean
