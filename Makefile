# [ SWE2001 System Program sping 2020 ]
# [ HW1 - 24-bit small Floating Point ]

CC = gcc
TAR = tar
TARGET = hw1
OBJECTS = sfp.o hw1.o
STUDENT_ID = 2019000000

$(TARGET) : hw1.o sfp.o
	$(CC) -o $(TARGET) $(OBJECTS)

hw1.o : hw1.c
	$(CC) -c -o hw1.o hw1.c

sfp.o : sfp.c
	$(CC) -c -o sfp.o sfp.c

tar : 
	$(TAR) -cvzf $(STUDENT_ID).tar.gz sfp.c sfp.h

clean :
	rm *.o $(TARGET)
