.PHONY: run profile time docs clean

EXECUTABLE=Raytracer-exe

run:
	stack build && zsh -c 'time stack exec -- $(EXECUTABLE) +RTS -N -s'

profile:
	stack build && stack exec -- $(EXECUTABLE) +RTS -N -ls -s

memory:
	stack build --profile && stack exec --profile -- $(EXECUTABLE) +RTS -N -h
	hp2ps -e8in -c $(EXECUTABLE).hp
	open $(EXECUTABLE).ps

docs:
	stack haddock --keep-going

clean:
	rm *.aux *.hp *.ps *.eventlog *.bmp