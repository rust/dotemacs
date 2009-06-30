TARGET=udclient
SOURCE=udclient.c 
INCLUDE=udclient.h

prefix=/usr/local
CFLAGS+=-O2 -DUSE_POLL

INSTALL=install -s
MKDIR=install -d

all: build

build:$(TARGET)

$(TARGET): $(SOURCE) $(INCLUDE)
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCE)

install:
	$(MKDIR) ${prefix}/bin
	$(INSTALL) -m 555 $(TARGET) ${prefix}/bin/${TARGET}

clean:
	$(RM) $(TARGET)
