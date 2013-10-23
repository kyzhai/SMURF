/**
 * CSV.java
 * June 11, 2003
 * @author: Stephen Steffes
 * Purpose: fill and extract data from a CSV file
 */

import java.util.*;
import java.io.*;

class CSV{
    public FileOutputStream outstr;
    PrintWriter writer;
    FileInputStream instr;
    BufferedReader reader;
    Vector data;							//holds data from the CSV file, commas, new lines and all
    String fileName;					//name of CSV file to write or read

    //open the csv file
    public CSV(String fileName){
        this.fileName=fileName;
        data=new Vector();
    }
    public CSV(){
    }

    //put string into the vector
    public void addLineToVector(String str){
        long i=0;
        StringTokenizer st=new StringTokenizer(str,"\n\r,",true);			//seperate str into an array where elements are seperated by commas or new lines, keep the commas and new lines
        while(st.hasMoreTokens()){
            String tempStr = st.nextToken();
            //System.out.println("adding Element "+(i++)+"------>"+tempStr);
            data.addElement(tempStr); //put all the strings into the data vector
        }
    }

    //removes all instances of the string str from the data
    public void removeString(String str){
        for(int i=0;i<data.size();i++){
            if(data.elementAt(i).toString().compareTo(str)==0){
                data.remove(i);
                i--;
            }
        }
    }


    //fill the data Vector from the CSV file
    public Vector fillVector(){
        //Open and save the file
        try{
            FileInputStream instr=new FileInputStream(fileName);
            BufferedReader reader=new BufferedReader(new InputStreamReader(instr));
            String csvText="",line="";
            while((line=reader.readLine())!=null){
                csvText+=line+"\n";
                //				System.out.println("read line: "+line);
            }

            reader.close();
            instr.close();

            //Parse the csv data and put into a vector
            //			data=new Vector();
            data.removeAllElements();
            addLineToVector(csvText);
        } catch(FileNotFoundException e){
            //if no data file exists
            System.out.println("FileNotFoundException (fillVector): "+e);
            data.removeAllElements();
        } catch(IOException e){
            System.out.println("IOException (fillVector): "+e);
        }

        return data;
    }

    //update the csv file with the data
    public void fillCSV(){
        System.out.println("Saving File: "+fileName);
        try{
            FileOutputStream outstr=new FileOutputStream(fileName);
            PrintWriter writer=new PrintWriter(outstr);

            for(int i=0;i<data.size();i++)													//go through the data vector
                writer.print(data.elementAt(i).toString());						//write each element to the CSV file

            writer.close();
            outstr.close();
        } catch(FileNotFoundException e){
            System.out.println("FileNotFoundException (fillCSV): "+e);
        } catch(IOException e){
            System.out.println("IOException (fillCSV): "+e);
        }
    }
}

