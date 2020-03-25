EXECUTABLE=Raytracer

run:
	stack build
	zsh -c 'time stack exec -- $(EXECUTABLE) +RTS -s'

threadscope:
	stack build
	stack exec -- $(EXECUTABLE) +RTS -ls -s
	threadscope $(EXECUTABLE).eventlog

memory:
	stack build --profile
	stack exec --profile -- $(EXECUTABLE) +RTS -hd
	hp2pretty $(EXECUTABLE).hp
	open $(EXECUTABLE).svg

profiteur:
	stack build --profile
	stack exec --profile -- $(EXECUTABLE) +RTS -p
	profiteur $(EXECUTABLE).prof
	open $(EXECUTABLE).prof.html

docs:
	stack haddock --keep-going

clean:
	rm *.aux *.hp *.ps *.eventlog *.bmp *.prof *.svg