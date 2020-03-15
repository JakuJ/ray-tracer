.PHONY: run profile time clean

EXECUTABLE=Raytracer-exe

run:
	stack build && zsh -c 'time stack exec -- $(EXECUTABLE) +RTS -N'
	open obraz.bmp

profile:
	stack build && stack exec -- $(EXECUTABLE) +RTS -N -ls

memory:
	stack build --profile && stack exec --profile -- $(EXECUTABLE) +RTS -N -hc
	hp2ps -e8in -c $(EXECUTABLE).hp
	open $(EXECUTABLE).ps

clean:
	rm *.aux *.hp *.ps *.eventlog *.bmp