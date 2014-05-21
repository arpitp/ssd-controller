/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
-- Copyright (c) 2013,2014 Indian Institute of Technology Madras (IIT Madras)
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

package NandFlashController_Model ;

import BRAM :: * ;
import InterfaceNandFlashController :: * ;

interface NFC_Interface ;
	interface NandFlashInterface nfcB_interface  ;
//	interface ONFI_Interface onfi_interface ;
endinterface

module mkNandFlashController_Model ( NFC_Interface ) ;

// Wires and Regs related to the Nand Flash Interface 
	Wire#(Bit#(12))	wr_address_from_nvm <- mkDWire(0) ;	// address
	Wire#(Bit#(32))	wr_data_from_nvm 	<- mkDWire(0) ;	// data out
	Reg#(Bit#(32))	rg_data_to_nvm 		<- mkReg(0) ;	// data in
	Wire#(bit)		wr_nand_ce_l		<- mkDWire(1) ;	// active low
	Wire#(bit)		wr_nand_we_l		<- mkDWire(1) ;	// active low
	Wire#(bit)		wr_nand_oe_l 		<- mkDWire(1) ;	// active low
	Wire#(bit)		wr_nand_reset_l 	<- mkDWire(1) ;	// active low
	Reg#(bit)		rg_interrupt		<- mkReg(0) ;	// active high
	Reg#(bit)		rg_ready_busy_l		<- mkReg(1) ;	// active low

// Regs and Wires for Onfi Interface

//	Reg#(bit) rg_onfi_ce <- mkReg(0) // 

// Register File Definition
	Reg#(Bit#(8)) id_register0 <- mkReg(0) ;	// ID Reg 0
	Reg#(Bit#(8)) id_register1 <- mkReg(0) ;	// ID Reg 1
	Reg#(Bit#(8)) id_register2 <- mkReg(0) ;	// ID Reg 2
	Reg#(Bit#(8)) id_register3 <- mkReg(0) ;	// ID Reg 3
	Reg#(Bit#(25)) block_address <- mkReg(0) ; 	// Block Address Register
	Reg#(Bit#(8)) part_type_ECC <- mkReg(0); 	// Part Type and ECC Register 
	Reg#(bit) buffer_number <- mkReg(0) ; 		// Buffer Number
	Reg#(Bit#(8)) status_register <- mkReg(0) ; // Status Register 
	Reg#(Bit#(8)) command_register <- mkReg(0) ;// Command Register 

// Data Buffers 
	BRAM_Configure cfg_buffer0 = defaultValue ;
	BRAM2Port#( Bit#(9) , Bit#(32) ) buffer0 <- mkBRAM2Server (cfg_buffer0) ; // Address lines = 9 bits ,Data Width = 32bits.. 0.5K x 4Bytes = 2KB

	BRAM_Configure cfg_buffer1 = defaultValue ;
	BRAM2Port#( Bit#(9) , Bit#(32) ) buffer1 <- mkBRAM2Server (cfg_buffer1) ; // Address lines = 9 bits ,Data Width = 32bits.. 0.5K x 4Bytes = 2KB

	BRAM_Configure cfg_nand = defaultValue ;
	BRAM2Port#( Bit#(10) , Bit#(32) ) nand_chip <- mkBRAM2Server (cfg_nand) ; // Address lines = 10 bits ,Data Width = 32bits.. 1K x 4Bytes = 4KB


	Reg#(Bool) rg_command_execution <- mkReg(False ) ;
	Reg#(Bool) rg_disable_interrupt <- mkReg(False) ;

	Reg#(bit) rg_nand_buffer_control <- mkReg(1) ;

(* descending_urgency = "rl_disable, rl_write, rl_read" *)
	

	rule rl_disable (wr_nand_ce_l == 1 ) ;
		$display(" NAND FLASH CONTROLLER IN DISABLED STATE " ) ;
		rg_ready_busy_l <= 0 ; // NFC is READY
		rg_command_execution <= False ;
	endrule

	rule rl_write (  wr_nand_ce_l == 0 && wr_nand_we_l == 0 && rg_ready_busy_l == 0) ;
		$display ( " NAND FLASH CONTROLLER IN WRITE STATE " ) ;
			// Writing into Buffers
			if ( wr_address_from_nvm <= 'h1FF ) 
					// Buffer Number 0 is in Control of the HOST ( i.e. NVMe ) 
					buffer0.portA.request.put ( BRAMRequest{
															write : True, 
															address: wr_address_from_nvm[8:0]  , 
															datain : wr_data_from_nvm, 
															responseOnWrite : False} ) ;

			if ( wr_address_from_nvm > 'h1FF && wr_address_from_nvm <= 'h3FF ) 					
					// Buffer Number 1 is in Control of the HOST ( i.e. NVMe ) 
					buffer1.portA.request.put ( BRAMRequest{
															write : True, 
															address: wr_address_from_nvm[8:0], 
															datain : wr_data_from_nvm, 
															responseOnWrite : False} ) ;
		

			// if address is > 'h1FF and < 'hFF0 , then the address is invalid .. its reserved 

			// if ( wr_address_from_nvm == 'hFF0 )  // NO WRITE OPTION TO READ ID REGISTERS
			// if ( wr_address_from_nvm == 'hFF1 )  // NO WRITE OPTION TO READ ID REGISTERS
			// if ( wr_address_from_nvm == 'hFF2 )  // NO WRITE OPTION TO READ ID REGISTERS
			// if ( wr_address_from_nvm == 'hFF3 )  // NO WRITE OPTION TO READ ID REGISTERS			
			if ( wr_address_from_nvm == 'hFF4 ) begin
				block_address[7:0] <=  wr_data_from_nvm[7:0] ;
				$display(" ****NAND **** Block Address Prt 1 written " ) ;
			end			
		
			if ( wr_address_from_nvm == 'hFF5 ) begin
				block_address[15:8] <=  wr_data_from_nvm[7:0] ;
				$display(" ****NAND **** Block Address Prt 2 written " ) ;				
			end
			if ( wr_address_from_nvm == 'hFF6 ) begin
				block_address[24:16] <=  wr_data_from_nvm[8:0] ;
				$display(" ****NAND **** Block Address Prt 3 written " ) ;
			end
			// if ( wr_address_from_nvm == 'hFF7 )  // NO WRITE OPTION TO PART TYPE AND ECC REGISTERS	
			if ( wr_address_from_nvm == 'hFF8 ) begin
				buffer_number <=  wr_data_from_nvm[0] ;			
				rg_nand_buffer_control <= ~wr_data_from_nvm[0] ;
				$display( "****NAND ****  Buffer Control Changed " ) ;
			end
			// if ( wr_address_from_nvm == 'hFF9 )  // NO WRITE OPTION TO STATUS REGISTERS	 
			if ( wr_address_from_nvm == 'hFFA ) begin
				$display( " ****NAND ****  Obtained the Command to be executed " ) ;
				command_register <=  wr_data_from_nvm[7:0] ;	
			//	rg_state <= COMMAND_EXECUTION ;		
				rg_command_execution <= True ;
				rg_ready_busy_l <= 1 ; // NFC is BUSY
			end	
	//	end
	endrule

	rule rl_read ( /*rg_state == READ */ wr_nand_ce_l == 0 && wr_nand_oe_l == 0 && rg_ready_busy_l == 0) ;
		$display ( " NAND FLASH CONTROLLER IN READ STATE " ) ;

			// Reading from Buffers
			if ( wr_address_from_nvm <= 'h1FF ) 
					// Buffer Number 0 is in Control of the HOST ( i.e. NVMe ) 
					buffer0.portA.request.put ( BRAMRequest{
															write : False, 
															address: wr_address_from_nvm[8:0]  , 
															datain : ?, 
															responseOnWrite : False} ) ;

			if ( wr_address_from_nvm > 'h1FF && wr_address_from_nvm <= 'h3FF )  	
				// Buffer Number 1 is in Control of the HOST ( i.e. NVMe ) 
					buffer1.portA.request.put ( BRAMRequest{
															write : False, 
															address: wr_address_from_nvm[8:0]  , 
															datain : ?, 
															responseOnWrite : False} ) ;
			

			// if address is > 'h1FF and < 'hFF0 , then the address is invalid .. its reserved 

			if ( wr_address_from_nvm == 'hFF0 ) 
				rg_data_to_nvm <= {'d0 , id_register0 } ;			// READ ID Register0
			if ( wr_address_from_nvm == 'hFF1 ) 
				rg_data_to_nvm <= {'d0 , id_register1 } ;			// READ ID Register1
			if ( wr_address_from_nvm == 'hFF2 ) 
				rg_data_to_nvm <= {'d0 , id_register2 } ;			// READ ID Register2
			if ( wr_address_from_nvm == 'hFF3 ) 	
				rg_data_to_nvm <= {'d0 , id_register3 } ;			// READ ID Register3		
			if ( wr_address_from_nvm == 'hFF4 ) 
				rg_data_to_nvm <= {'d0 , block_address[7:0] } ;		// Read Block Address
			if ( wr_address_from_nvm == 'hFF5 ) 
				rg_data_to_nvm <= {'d0 , block_address[15:8]  } ;	// Read Block Address
			if ( wr_address_from_nvm == 'hFF6 ) 
				rg_data_to_nvm <= {'d0 , block_address[24:16]  } ;	// Read Block Address
			if ( wr_address_from_nvm == 'hFF7 ) 
				rg_data_to_nvm <= {'d0 , part_type_ECC } ;			// Read Part type and ECC 
			if ( wr_address_from_nvm == 'hFF8 ) begin
				rg_data_to_nvm <= {'d0 , buffer_number } ;			// Read Buffer Number	
				$display("****NAND ****  Requested for Buffer Number Information " ) ;
			end
			if ( wr_address_from_nvm == 'hFF9 ) 	 
				rg_data_to_nvm <= {'d0 , status_register } ;		// Read Status Register
			if ( wr_address_from_nvm == 'hFFA ) 
				rg_data_to_nvm <= {'d0 , command_register } ;		// Read Command Register

		//end
	endrule

Reg#(Bit#(10)) rg_buffer_read_address <- mkReg(0) ;
Reg#(Bit#(10)) rg_buffer_write_address <- mkReg(0) ;
Reg#(Bit#(2)) page_count <- mkReg(0) ;

rule rl_command_execution(rg_command_execution == True && wr_nand_ce_l == 0 )  ;
$display( " ****NAND **** in COMMAND EXECUTION STAGE " ) ;
		rg_ready_busy_l <= 1 ; // NFC is BUSY 
		/* Code Here */
		if ( command_register == 'h00 ) begin // READ Page
			$display ( " NAND EXECUTING READ PAGE COMMAND " ) ;
			rg_buffer_read_address <= rg_buffer_read_address + 1 ;

			if( rg_buffer_read_address == 'h3FF ) begin
			//	rg_state <= ENABLED ;
				rg_command_execution <= False ;
				rg_disable_interrupt <= True ;
				rg_interrupt <= 1 ;
				$display( "****NAND ****  INTERRUPTED THE NVMe " ) ;
			end

			nand_chip.portA.request.put ( BRAMRequest{
													write : False, 
													address: rg_buffer_read_address  , 
													datain : ?, 
													responseOnWrite : False} ) ;
			
		end

		else if ( command_register == 'h08 ) begin // WRITE Page 
			$display( " NAND EXECUTING WRITE TO NAND CHIP COMMAND " ) ;
			if( page_count == 2 ) begin
				page_count <= 0 ;
				rg_disable_interrupt <= True ;
				rg_command_execution <= False ;
				rg_interrupt <= 1 ;
				rg_nand_buffer_control <= 0 ;
			end
			else begin
				if ( rg_buffer_read_address == 'h1FF ) begin
					page_count <= page_count + 1 ;
					rg_buffer_read_address <= 0 ;
					rg_nand_buffer_control <= 1 ;
	/*				if ( rg_nand_buffer_control == 0 ) begin
						rg_nand_buffer_control <= 1 ;
						buffer_number <= 0 ;
						$display ( " BUFFER IN CONTROL OF NVM = 0 ..... " ) ;
					end
					else begin 
						rg_nand_buffer_control <= 0 ;
						buffer_number <= 1 ;
						$display ( " BUFFER IN CONTROL OF NVM = 1 ..... " ) ;
					end*/
				end
		
				else  begin
					rg_buffer_read_address <= rg_buffer_read_address + 1 ;
				end

				if ( rg_nand_buffer_control == 0 ) 					// Buffer Number 0 is in Control of the NAND 
					buffer0.portB.request.put ( BRAMRequest{
															write : False, 
															address: rg_buffer_read_address[8:0]  , 
															datain : ?, 
															responseOnWrite : False} ) ;


				else if ( rg_nand_buffer_control == 1 ) 					// Buffer Number 1 is in Control of the NAND
					buffer1.portB.request.put ( BRAMRequest{
															write : False, 
															address: rg_buffer_read_address[8:0] , 
															datain : ?, 
															responseOnWrite : False} ) ;
			end
		end
endrule

rule rl_disable_interrupt ( rg_disable_interrupt == True) ;
	rg_ready_busy_l <= 0 ; // NFC is READY
	rg_disable_interrupt <= False ;
	rg_interrupt <= 0 ;
endrule

rule rl_reading_from_nand_chip ;

	let data <- nand_chip.portA.response.get() ;
	$display( " DATA RETRIEVED FROM NAND CHIP = %d " , data ) ;
	if ( rg_buffer_write_address == 'h1FF ) begin
		rg_buffer_write_address <= 0 ;
		rg_nand_buffer_control <= ~rg_nand_buffer_control ;
		buffer_number <= rg_nand_buffer_control ;
	end

	else begin
		rg_buffer_write_address <= rg_buffer_write_address + 1 ;
	end

	if ( rg_nand_buffer_control == 0 ) 					// Buffer Number 0 is in Control of the NAND 
		buffer0.portA.request.put ( BRAMRequest{
												write : True, 
												address: rg_buffer_write_address[8:0]  , 
												datain : data, 
												responseOnWrite : False} ) ;


	else if ( rg_nand_buffer_control == 1 ) 					// Buffer Number 1 is in Control of the NAND
		buffer1.portA.request.put ( BRAMRequest{
												write : True, 
												address: rg_buffer_write_address[8:0]  , 
												datain : data, 
												responseOnWrite : False} ) ;

endrule

// The following rule fires only when the data from the Buffers are ready 
// Once the read request is given, it takes 2 cycles for the BRAM to get the data out ..
// PortA is used for NVMe 
// portB is used for and flash

//portA
rule rl_reading_data_from_buf0_portA ;


	let data0 <- buffer0.portA.response.get() ;
	$display ( " NAND FLASH CONTROLLER IS SENDING DATA STORED IN BUFFER 0 TO NVMe .. data sent = %d ", data0 ) ;
	rg_data_to_nvm <= data0 ;
endrule

rule rl_reading_data_from_buf1_portA ;
	let data1 <- buffer1.portA.response.get() ;
	$display ( " NAND FLASH CONTROLLER IS SENDING DATA STORED IN BUFFER 1 TO NVMe .. data sent = %d ", data1 ) ;
	rg_data_to_nvm <= data1 ;
endrule


//portB
rule rl_reading_data_from_buf0_portB ;
	let data0 <- buffer0.portB.response.get() ;
			$display ( " DATA TRANSFERD TO NAND FLASH CHIP = %d .... FROM BUFFER 0" , data0 ) ;
			rg_buffer_write_address <= rg_buffer_write_address + 1 ;
			nand_chip.portA.request.put ( BRAMRequest{
													write : True, 
													address: rg_buffer_write_address  , 
													datain : data0 , 
													responseOnWrite : False} ) ;
endrule

rule rl_reading_data_from_buf1_portB ;
	let data1 <- buffer1.portB.response.get() ;
			$display ( " DATA TRANSFERD TO NAND FLASH CHIP = %d...... FROM BUFFER 1 " , data1 ) ;
			rg_buffer_write_address <= rg_buffer_write_address + 1 ;
			nand_chip.portA.request.put ( BRAMRequest{
													write : True, 
													address: rg_buffer_write_address  , 
													datain : data1 , 
													responseOnWrite : False} ) ;
endrule

interface nfcB_interface = fn_nfcB_interface ( wr_address_from_nvm , wr_data_from_nvm ,rg_data_to_nvm ,wr_nand_ce_l ,wr_nand_we_l ,wr_nand_oe_l ,
wr_nand_reset_l,rg_interrupt ,rg_ready_busy_l ) ;

//interface onfi_interface = fn_onfi_interface (rg_onfi_ce_l , rg_onfi_cle) ;

endmodule

endpackage 
