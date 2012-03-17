CXXFLAGS =	-O2 -g -Wall -fmessage-length=0 

OBJS =		src.o 

LIBS = 

TARGET =	src

LDFLAGS = -lxml2

$(TARGET):	$(OBJS)
	$(CXX) -o $(TARGET) $^ $(LIBS) $(LDFLAGS)


all:	$(TARGET) clean

clean:
	rm -f *.o
