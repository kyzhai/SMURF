/**
 * listInstruments.java
 * June 11, 2003
 * @author: Stephen Steffes
 * Purpose: prints out a list of available instruments in the java synthesizer and their instrument number
 */

import java.io.*;
import javax.sound.midi.*;

public class listInstruments{
  public static Synthesizer synth;

  public static void listAvailableInstruments(){
    Instrument[] instrument = synth.getAvailableInstruments();
    for (int i=0; i<instrument.length; i++){
      System.out.println(i + "   " + instrument[i].getName());
    }
  }

  public static void main(String[] args)	throws InvalidMidiDataException {
    try {
      synth = MidiSystem.getSynthesizer();
      synth.open();
    } catch (MidiUnavailableException e) {
      e.printStackTrace();
    }
    listAvailableInstruments();
    System.exit(0);
  }
}
