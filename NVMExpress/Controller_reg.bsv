/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
-- Copyright (c) 2013, Indian Institute of Technology Madras (IIT Madras)
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



/*
Module Name : Controller Registers
Author Name : M.S.Abhishek
email id :    shanmukh.abhishek@gmail.com
last updated : 23/7/2012
*/

package Controller_reg ;

import Vector :: * ;


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
typedef struct {
//// DWORD - 1 //////////////////////////////////////////////////
		Bit#(8) reserved1 ;
		Bit#(4) mpsmax ;
		Bit#(4) mpsmin ;
		Bit#(7) reserved2 ;
		Bit#(4) css ;
		Bit#(1) reserved3 ;
		Bit#(4) dstrd ;
//// DWORD - 0 //////////////////////////////////////////////////
		Bit#(8) to ;
		Bit#(5) reserved4 ;
		Bit#(2) ams ;
		Bit#(1) cqr ;
		Bit#(16) mqes ;		
		
		} Controller_capabilities deriving(Bits,Eq) ;

typedef struct {
//// DWORD - 2 //////////////////////////////////////////////////
		Bit#(16) mjr ;
		Bit#(16) mnr ;
		
		} Version deriving(Bits,Eq) ;

/*typedef struct {
		// register IVMS 
		} Interrupt_mask_set deriving(Bits,Eq) ;

typedef struct {
		// register IVMC
		} Interrupt_mask_clear deriving(Bits,Eq) ;*/

typedef struct {
		Bit#(8) reserved1 ;
		Bit#(4) iocqes ;
		Bit#(4) iosqes ;
		Bit#(2) shn ;
		Bit#(3) ams ;
		Bit#(4) mps ;
		Bit#(3) css ;
		Bit#(3) reserved2 ;
		Bit#(1) en ;
		} Controller_configuration deriving(Bits,Eq) ;

/*typedef struct {
		
		} Reserved1 deriving(Bits,Eq) ;*/

typedef struct {
		Bit#(28) reserved ;
		Bit#(2) shst ;
		Bit#(1) cfs ;
		Bit#(1) rdy ;
		} Controller_status deriving(Bits,Eq) ;

/*typedef struct {
		
		} Reserved2 deriving(Bits,Eq) ;*/

typedef struct {
		Bit#(4) reserved1 ;
		Bit#(12) acqs ;
		Bit#(4) reserved2 ;
		Bit#(12) asqs ;
		} AQA deriving(Bits,Eq) ;

typedef struct {

		Bit#(52) asqb ;
		Bit#(12) reserved ;
		} ASQ deriving(Bits,Eq) ;

typedef struct {

		Bit#(52) acqb ;
		Bit#(12) reserved ;		
		} ACQ deriving(Bits,Eq) ;

/*typedef struct {
		
		} Reserved3 deriving(Bits,Eq) ;

typedef struct {
		
		} Reserved4 deriving(Bits,Eq) ;*/

typedef struct {
		Bit#(16) reserved ;
		Bit#(16) sqt ;
		} SQTDBL deriving(Bits,Eq) ;

typedef struct {
		Bit#(16) reserved ;
		Bit#(16) cqh ;		
		} CQHDBL deriving(Bits,Eq) ;


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
		Bit#(8) hpw ;
		Bit#(8) mpw ;
		Bit#(8) lpw ;
		Bit#(3) ab ;
		} Arbitration deriving(Bits,Eq) ;

typedef struct {
		Bit#(16) ncqr ;
		Bit#(16) nsqr ;
		}  NumberOfQs_Requested deriving(Bits,Eq) ;

typedef struct {
		Bit#(16) ncqa ;
		Bit#(16) nsqa ;
		} NumberOfQs_Allocated deriving(Bits,Eq) ;

typedef struct {
		Bit#(8) int_time ;
		Bit#(8) thr ;
		} InterruptCoalescing deriving(Bits,Eq) ;

typedef struct {
		bit cd ;
		Bit#(16) iv ;
		} InterruptVectorConfiguration deriving(Bits,Eq) ;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct {
		Bit#(16) sqID ;
		Bit#(16) sqHeadPointer ;
		} CQ_DWord2 deriving(Bits,Eq) ;

typedef struct {
		bit status_field_DNR ;
		bit status_field_M   ;
		Bit#(2) status_field_res ;	
		Bit#(3) status_field_SCT ;
		Bit#(8) status_field_SC  ;
		bit phase_tag		 ;
		Bit#(16) commandID		 ;
		} CQ_DWord3 deriving(Bits,Eq) ;
	
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

endpackage
