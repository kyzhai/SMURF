/**
 * CSV2MIDI.java
 * June 11, 2003
 * @author: Stephen Steffes
 * Purpose:  Converts a .csv file to a MIDI file according to ExampleMIDI.csv
 */


import java.io.*;
import javax.sound.midi.*;
import java.lang.*;


public class CSV2MIDI{

	public static void main(String[] args)	throws InvalidMidiDataException {

		//***** Get Inputs *****
		if (args.length != 2)
			printUsageAndExit();
	
		File outputFile = new File(args[1]);
		Sequence sequence = null;

		//Open and save the CSV file
		CSV csvFile=new CSV(args[0]);
		csvFile.fillVector();


		//figure out how many channels there are
		//nChannels=number of integers in the first line containing any numbers, skipping the first number encountered
		int nChannels=0,temp=0;
		for(int i=0;i<csvFile.data.size();i++){
			try{																																//check if this is an integer
				Integer.parseInt(csvFile.data.elementAt(i).toString());
				temp++;																														//counts number of instruments
			}catch(NumberFormatException e){																		//not a number
				if(temp>1){																												//if other than first number
					if(csvFile.data.elementAt(i).toString().compareTo("\n")==0){		//if a new line
						nChannels=temp-1; // skip the first number which is the resolution
						break;																												//found nChannels, so stop for loop.  this is the number of instruments counted
					}
				}
			}
		}


/*
		for(int i=0;i<csvFile.data.size();i++)
			System.out.println(csvFile.data.elementAt(i));
*/


		//***** Read in timing resolution and instruments *****
		int currentCSVPos=0, timingRes=1, instrument[]=new int[nChannels];

		//read in timing resolution
		for(;currentCSVPos<csvFile.data.size();currentCSVPos++){
            try{	        //check if this is an integer
                timingRes=Integer.parseInt(csvFile.data.elementAt(currentCSVPos).toString()); //this is the first number, therefore, it's the timing resolution
                System.out.println("\nTiming Resolution set to "+timingRes+" PPQ\n");  
                currentCSVPos++;
                break;
            }catch(NumberFormatException e){
			}
        }

		//read in instrument numbers
		temp=0;
		for(;currentCSVPos<csvFile.data.size();currentCSVPos++){
			try{																																//check if this is an integer
				instrument[temp]=Integer.parseInt(csvFile.data.elementAt(currentCSVPos).toString()); //this is a number, it has to be an intrument
				System.out.println("Instrument set to "+instrument[temp]+" on channel "+temp);
				temp++;
				if(temp>=nChannels){																							//collect numbers until you've reached the number of channels
					currentCSVPos++;
					break;
				}
			}catch(NumberFormatException e){
			}
        }


		//***** Initialize Sequencer *****
		try{
			sequence = new Sequence(Sequence.PPQ, timingRes);   //initialize sequencer with timingRes
		}catch (InvalidMidiDataException e){
			e.printStackTrace();
			System.exit(1);
		}


		//***** Create tracks and notes *****
		/* Track objects cannot be created by invoking their constructor
		   directly. Instead, the Sequence object does the job. So we
		   obtain the Track there. This links the Track to the Sequence
		   automatically.
		*/
		Track track[] = new Track[nChannels];
        for(int i=0;i<nChannels;i++){
            track[i]=sequence.createTrack();                    //create tracks

            ShortMessage sm = new ShortMessage( );
            sm.setMessage(ShortMessage.PROGRAM_CHANGE, i, instrument[i], 0);  //put in instrument[i] in this track
            track[i].add(new MidiEvent(sm, 0));
        }

		int channel=0,note=0,tick=0,velocity=90,column=0;

        int previous_note[] = new int[nChannels];
        int previous_vol[] = new int[nChannels];
        for(int i=0; i<nChannels; ++i){
            previous_note[i] = -1;
            previous_vol[i] = -1;
        }
		//go through each of the following lines and add notes
        for(;currentCSVPos<csvFile.data.size();){							//loop through rest of CSV file
            try{																																			  //check that the current CSV position is an integer
                tick=Integer.parseInt(csvFile.data.elementAt(currentCSVPos).toString());  //first number is tick
                currentCSVPos+=2;
                note=Integer.parseInt(csvFile.data.elementAt(currentCSVPos).toString());  //next number is note
                currentCSVPos+=2;
                velocity=Integer.parseInt(csvFile.data.elementAt(currentCSVPos).toString());  //next number is velocity
                currentCSVPos++;
                channel=column/3;
                column+=2;

                //System.out.println("*** trace" + channel + " size: " + track[channel].size());

                if(previous_note[channel] == -1)
                    track[channel].add(createNoteOnEvent(note,tick,channel,velocity));	// turn on the current note			
                else if(note != previous_note[channel] || velocity != previous_vol[channel]){
                    track[channel].add(createNoteOffEvent(previous_note[channel],tick,channel)); // turn off the previous note
                    track[channel].add(createNoteOnEvent(note,tick,channel,velocity));	// turn on the current note			
                }

                previous_note[channel] = note;
                previous_vol[channel] = velocity;
                //				track[channel].add(createNoteOffEvent(note,tick+5,channel));
            }catch(NumberFormatException e){																						//current CSV position not an integer
                if(csvFile.data.elementAt(currentCSVPos).toString().compareTo("\n")==0){  //if it's a new line
                    column=0;																																//go back to 1st column
                }else if(csvFile.data.elementAt(currentCSVPos).toString().compareTo(",")==0){ //if it's just a comma
                    column++;
                }
                currentCSVPos++;
            }
        }


		// Print track information
		System.out.println();
        if ( track != null ) {
            for ( int i = 0; i < track.length; i++ ) {
                System.out.println( "Track " + i + ":" );

                for ( int j = 0; j < track[i].size(); j++ ) {
                    MidiEvent event = track[i].get( j );
                    System.out.println(" tick "+event.getTick()+", "+MessageInfo.toString(event.getMessage()));
                } // for
            } // for
        } // if



		/* Now we just save the Sequence to the file we specified.
		   The '0' (second parameter) means saving as SMF type 0.
		   (type 1 is for multiple tracks).
		*/
		try{
			MidiSystem.write(sequence, 1, outputFile);
		}catch (IOException e){
			e.printStackTrace();
			System.exit(1);
		}
	}// end main






    //turns note on
    private static MidiEvent createNoteOnEvent(int nKey, long lTick,int channel,int velocity){
        return createNoteEvent(ShortMessage.NOTE_ON,nKey,velocity,lTick,channel);
    }

	//turns note off
    private static MidiEvent createNoteOffEvent(int nKey, long lTick,int channel){
        return createNoteEvent(ShortMessage.NOTE_OFF,nKey,0,lTick,channel);  //set note to 0 velocity
    }

	//turns note on or off
    private static MidiEvent createNoteEvent(int nCommand,int nKey,int nVelocity,long lTick,int channel){
        ShortMessage message = new ShortMessage();
        try{
            message.setMessage(nCommand,channel,nKey,nVelocity);
        }catch (InvalidMidiDataException e){
            e.printStackTrace();
            System.exit(1);
        }
        MidiEvent event = new MidiEvent(message,lTick);
        return event;
    }

	private static void printUsageAndExit(){
		out("usage:");
		out("java CSV2MIDI <infile.csv> <outfile.midi>");
		System.exit(1);
	}

    private static void out(String strMessage){
        System.out.println(strMessage);
    }
}

