/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
-- Copyright (c) 2013,2014  Indian Institute of Technology Madras (IIT Madras)
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and 
--    the following disclaimer in the documentation and/or other materials provided with the distribution.
-- 3. Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
--    promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
-- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
-- IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
-- OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
-- OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- 
--------------------------------------------------------------------------------------------------------------------------------------------------------                                                                                         
*/


package TestCaseCommands ;


function Bit#(512) get_command ( Bit#(64) address_in) ;

	 Bit#(8)   opcode 		    ;
	 Bit#(2)   fuse  			    ;
	 Bit#(16)   command_id 	    ;
	 Bit#(6)    reserved0       ;
	 Bit#(32)   nsid	 		    ;
	 Bit#(32)	 reserved1				;
	 Bit#(32)	 reserved2			;
	 Bit#(64)   mptr 			    ;
	 Bit#(64)   prp1 			    ;
	 Bit#(64)   prp2 			    ;
	 Bit#(32)   cdw10 		    ;
	 Bit#(32)   cdw11 		    ;
	 Bit#(32)   cdw12 		    ;
	 Bit#(32)   cdw13 		    ;
	 Bit#(32)   cdw14 		    ;
	 Bit#(32)   cdw15 		   ;

	case(address_in) matches
		0 : begin						  // Base Address of SQ 0 						
				 opcode 	= 8'h01		; // Create I/O Submission Queue
				 fuse  		= 0		   	; // No Fuse 
				 command_id = 0    		;
				 reserved0 	= 0			;
				 nsid	 	= 0    		;
				 reserved1	= 0			;
				 reserved2	= 0			;
				 mptr 		= 0	   		;
				 prp1 		= 64'd200	   		;// Base address of the Q to be created is 200 
				 prp2 		= 0	   		;
				 cdw10 		= 32'h0005_0002    ; // qID is "2" .. sizeof Q is 5 ..
				 cdw11 		= 32'h0001_0000    	;// qID of CQ to be used is 1 
				 cdw12 		= 0    		;
				 cdw13 		= 0    		;
				 cdw14 		= 0    		;
				 cdw15 		= 0    		;
			end	
		64 : begin		
				 opcode = 8'h05		   ; // Create I/O completion Q command
				 fuse  	= 0		   ;
				 command_id = 0    ;
				 reserved0 = 0		;
				 nsid	 	= 0    ;
				 reserved1	= 0			;
				 reserved2	= 0		;
				 mptr 		= 0	   ;
				 prp1 		= 64'd400	   ; // Base Address of Q to be created
				 prp2 		= 0	   ;
				 cdw10 		= 32'h0005_0001    ;// qID is "1" .. sizeof Q is 5 ..
				 cdw11 		= 5    ; // Interrupt Vector Number for the CQ is 5 	
				 cdw12 		= 0    ;
				 cdw13 		= 0    ;
				 cdw14 		= 0    ;
				 cdw15 		= 0    ;
			end
		128 : begin
				 opcode = 8'h08		   ; // ABORT Command 
				 fuse  	= 0		   ;
				 command_id = 0    ;
				 reserved0 = 0		;
				 nsid	 	= 0    ;
				 reserved1	= 0			;
				 reserved2	= 0		;
				 mptr 		= 0	   ;
				 prp1 		= 0	   ; 
				 prp2 		= 0	   ;
				 cdw10 		= 32'h0014_0002    ; // Abort the Command with CQID = 20 and SQID = 2 ... 
				 cdw11 		= 0    ; 	
				 cdw12 		= 0    ;
				 cdw13 		= 0    ;
				 cdw14 		= 0    ;
				 cdw15 		= 0    ;
			end
		200 : begin							// Base Address of I/O SQ 2 
				 opcode = 8'h01		   ; 	// Write Comand 
				 fuse  	= 0		   ;
				 command_id = 'd20    ;		// Command ID is 20 
				 reserved0 = 0		;
				 nsid	 	= 0    ;
				 reserved1	= 0			;
				 reserved2	= 0		;
				 mptr 		= 0	   ;
				 prp1 		= 64'd1008	   ; // Data to be written is in location 1008 in Main Memory 
				 prp2 		= 0	   ;
				 cdw10 		= 32'd0    ;   // The Logical Block Address is given combinedly by cdw10 and cdw11 .. its 100 here 
				 cdw11 		= 0    ; 	
				 cdw12 		= 0    ;
				 cdw13 		= 0    ;
				 cdw14 		= 0    ;
				 cdw15 		= 0    ;
			end
		 264 : begin						// Command in SQ 2 
				 opcode = 'h02		   ;	// READ Command
				 fuse  	= 0		   ;
				 command_id = 'd21    ;		// Command ID is 21
				 reserved0 = 0		;
				 nsid	 	= 0    ;
				 reserved1	    = 0			;
				 reserved2	= 0		;
				 mptr 		= 0	   ;
				 prp1 		= 'd2008	   ; // Data read from NAND is to be transfered to memory Location 2008 
				 prp2 		= 0	   ;
				 cdw10 		= 32'd0    ;	// The Logical Block Address is given Combinedly by cdw10 and cdw11 .. its 100 here 
				 cdw11 		= 0    ;
				 cdw12 		= 0    ;
				 cdw13 		= 0    ;
				 cdw14 		= 0    ;
				 cdw15 		= 0    ;
			end
		default : begin
				 opcode = 0		   ;
				 fuse  	= 0		   ;
				 command_id = 0    ;
				 reserved0 = 0		;
				 nsid	 	= 0    ;
				 reserved1	    = 0			;
				 reserved2	= 0		;
				 mptr 		= 0	   ;
				 prp1 		= 0	   ;
				 prp2 		= 0	   ;
				 cdw10 		= 0    ;
				 cdw11 		= 0    ;
				 cdw12 		= 0    ;
				 cdw13 		= 0    ;
				 cdw14 		= 0    ;
				 cdw15 		= 0    ;
			end
	endcase
	
	return( {  cdw15,cdw14 ,cdw13,cdw12 ,cdw11,cdw10,prp2,prp1,mptr,reserved2 , reserved1,nsid ,command_id,reserved0,fuse ,opcode  } ) ;

endfunction

endpackage
