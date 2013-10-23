SRC = CSV2MIDI.java
CSV := ExampleMIDI.csv
TAR = $(CSV:%.csv=%.midi)

all: $(SRC:%.java=%.class)

%.class: %.java
	javac $^

gen: $(CSV) $(SRC:%.java=%.class)
	java $(SRC:%.java=%) $(CSV) $(TAR)

jar: $(SRC:%.java=%.mf) $(SRC:%.java=%.class)
	jar cvfm $(SRC:%.java=%.jar) $< *.class
    
clean:
	rm *.class

veryclean:
	rm *.class *.midi *.jar
	
.PHONY:
	all clean veryclean gen jar
	
