GCC=g++
CFLAGS=-Wall -g -pedantic --std=c++11

FILES=Makefile Barrier.h main.cpp
#data.txt
OBJECTS=main.o

build: clean main
	$(GCC) $(CFLAGS) -o main main.o -lpthread
	./main data.txt

test:
	./main data.txt

main:
	$(GCC) $(CFLAGS) -c main.cpp -o main.o

clean:
	rm -rf main *.o rm $(ARCHIVE_FOLDER) $(ARCHIVE_FOLDER).tar.gz

tar:
	make clean
	mkdir $(ARCHIVE_FOLDER)
	cp -r $(FILES) $(ARCHIVE_FOLDER)
	tar cvzf $(ARCHIVE_FOLDER).tar.gz $(ARCHIVE_FOLDER)
	rm -rf $(ARCHIVE_FOLDER)
