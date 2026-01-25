Implement the translation to Assem-instructions for your favorite instruction 
set (let   stand for Sparc, Mips, Alpha, Pentium, etc.) using Maximal Munch. If 
you would like to generate code for a RISC machine, but you have no RISC 
computer on which to test it, you may wish to use SPIM (a MIPS 
simulator implemented by James Larus), described on the Web page for this book. 
First write the class  .Codegen implementing the  Maximal Munch  trans-lation 
algorithm from IR trees to the Assem data structure. Use the Canon module 
(described in Chapter 8) to simplify the trees before applying your Codegen 
module to them. Use the format function to trans-late the resulting Assem trees 
to   assembly language. Since you won t have done register assignment, just 
pass new Temp.DefaultMap() to format as the translation function from 
temporaries to strings. 

package Temp; 

public class DefaultMap implements TempMap { 

	public String tempMap(Temp.Temp t) 
			{ return t.toString(); } 
}


This will produce  assembly  language that does not use register names at all: the 
instructions will use names such as t3, t283, and so on. But some of these 
temps are the  built-in  ones created by the Frame module to stand for 
particular machine registers (see page 143), such as Frame.FP. The assembly 
language will be easier to read if these registers appear with their natural 
names (e.g., fp instead of t1). The Frame module must provide a mapping from 
the special temps to their names, and nonspecial temps to null: 

package Frame; 

public class Frame implements Temp.TempMap { 
	: 
        :
        : abstract public String tempMap(Temp temp); } 

Then, for the purposes of displaying your assembly 
language prior to regis-ter allocation, make a new TempMap function that first 
tries frame.tempMap, and if that returns null, resorts to Temp.toString().
