STUDENT_ID=j901y365
LAB_NAME=assignment-1
GCC=g++
CFLAGS=-Wall -g -pedantic --std=c++11

FILES=Makefile main.cpp data.txt
OBJECTS=main.o

ARCHIVE_FOLDER=$(STUDENT_ID)-$(LAB_NAME)

build: main
	$(GCC) $(CFLAGS) -o main main.o

test:
	./main

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
