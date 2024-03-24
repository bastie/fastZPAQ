CXX=g++
CXXFLAGS=-Ofast -mcpu=apple-m1
PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
MANDIR=$(PREFIX)/share/man

all: zpaq zpaq.1

libzpaq.o: ./fastZPAQ/libzpaq.cpp ./fastZPAQ/libzpaq.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ -c ./fastZPAQ/libzpaq.cpp

zpaq.o: ./fastZPAQ/zpaq.cpp ./fastZPAQ/libzpaq.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ -c ./fastZPAQ/zpaq.cpp -pthread

zpaq: zpaq.o libzpaq.o
	$(CXX) $(LDFLAGS) -o $@ zpaq.o libzpaq.o -pthread

zpaq.1: zpaq.pod
	pod2man $< >$@

install: zpaq zpaq.1
	install -m 0755 -d $(DESTDIR)$(BINDIR)
	install -m 0755 zpaq $(DESTDIR)$(BINDIR)
	install -m 0755 -d $(DESTDIR)$(MANDIR)/man1
	install -m 0644 zpaq.1 $(DESTDIR)$(MANDIR)/man1

clean:
	rm -f zpaq.o libzpaq.o zpaq zpaq.1 archive.zpaq zpaq.new

check: zpaq
	./zpaq add archive.zpaq zpaq -m9
	mv ./zpaq ./zpaq.old
	./zpaq.old extract archive.zpaq
	cmp zpaq.old zpaq
	mv -vf ./zpaq.old ./zpaq
	rm archive.zpaq
