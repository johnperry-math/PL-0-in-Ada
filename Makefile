all: compiler_main doc
	echo Done!

compiler_main: compiler.ads compiler.adb compiler_main.adb
	gnatmake -g compiler_main.adb

doc: compiler_main
	gnatdoc -d -p -Pcompiler

test: compiler_main
	./compiler_main < test0.pas
	./compiler_main < test1.pas

clean:
	rm -rf *.ali *.o b~* compiler_main gnatinspect.db

cleandoc:
	rm -rf gnatdoc
