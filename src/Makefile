CXXFLAGS =	-O2 -g -Wall -fmessage-length=0 `xml2-config --cflags`

OBJS =		src.o flowJoWorkspace.o

LIBS = `xml2-config --libs`

TARGET =	src

$(TARGET):	$(OBJS)
	$(CXX) -o $(TARGET) $^ $(LIBS) 


all:	$(TARGET) clean

clean:
	rm -f *.o
