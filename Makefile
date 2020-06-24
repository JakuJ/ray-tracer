EXECUTABLE=Raytracer
TESTFILE=scenes/diffuse.txt

.PHONY: scenes clean

# Render all examples in the scenes folder
scenes:
	stack build
	for scene in scenes/*.txt; do stack exec -- $(EXECUTABLE) "$$scene"; done

# Use zsh 'time' utility to see how much CPU we used
bench:
	stack build
	zsh -c 'time stack exec -- $(EXECUTABLE) $(TESTFILE) +RTS -s'

# Render a scene and show parallel performance info
threadscope:
	stack build
	stack exec -- $(EXECUTABLE) $(TESTFILE) +RTS -ls -s
	threadscope $(EXECUTABLE).eventlog

# Render a scene and show memory usage info
memory:
	stack build --profile
	stack exec --profile -- $(EXECUTABLE) $(TESTFILE) +RTS -hd
	hp2pretty $(EXECUTABLE).hp
	open $(EXECUTABLE).svg

# Render a scene and show profiling info
profiteur:
	stack build --profile
	stack exec --profile -- $(EXECUTABLE) $(TESTFILE) +RTS -p
	profiteur $(EXECUTABLE).prof
	open $(EXECUTABLE).prof.html

# Build Haddock documentation
docs:
	stack haddock --keep-going

# Clear the workspace of junk files
clean:
	rm -f *.aux *.hp *.ps *.eventlog *.bmp *.prof *.prof.html *.svg scenes/*.bmp