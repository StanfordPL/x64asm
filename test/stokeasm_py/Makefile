
all:
	python setup.py build
	find build -name "*.so" | xargs /bin/cp -t .

clean:
	- rm -f *.so
	- rm -rf build
