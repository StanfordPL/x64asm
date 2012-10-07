
stokeasm.so: stokeasm.cc setup.py
	python setup.py build
	find build -name "*.so" | xargs /bin/cp -t .

clean:
	- rm *.so
	- rm -r build
