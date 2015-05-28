# This program

PROG = main

# Setup

LIBS = \
	graphics.cma

CAMLC = ocamlc
CAMLFLAGS = -g

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

# Source and Object files
SOURCES = \
  hand.ml main.ml \

OBJECTS = $(SOURCES:.ml=.cmo)

# Final Program

$(PROG): $(OBJECTS)
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) -o $(PROG)

# Other

all: $(PROG)

clean:
	rm -rf *.cmo *.cmi $(PROG)

.DEFAULT_GOAL := $(PROG)
