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


package PCIe_model ;

import TestCaseCommands :: * ;
import Interface_Pcie_model :: * ;
import BRAM :: * ;

interface PCIe_model_interface ;
	interface Pcie_Tx pcie_transmit ;
	interface Pcie_Rx pcie_receive ;
endinterface 

module mkPCIe_model ( PCIe_model_interface ) ;

// Registers For Pcie Transmit interface 

Reg#(Bit#(32)) rg_completionDataOut <- mkReg(0) ;
Reg#(bit) rg_completionDataValid <- mkReg(0) ;
Reg#(bit) rg_completionLastDWord <- mkReg(0) ;
Reg#(Bit#(2)) rg_tag <- mkReg(0) ;

// Regs and Wires for Pcie Receive interface 

Wire#(Bit#(32)) wr_data_from_device <- mkDWire(0) ;
Wire#(Bit#(64)) wr_address_from_device <- mkDWire(0) ;
Wire#(Bit#(2)) wr_requested_tag <- mkDWire(0) ; 
Wire#(bit) wr_send_completion_tlp <- mkDWire(0) ;
Wire#(bit) wr_send_write_tlp <- mkDWire(0) ;
Wire#(bit) wr_send_read_tlp <- mkDWire(0) ;
Wire#(bit) wr_send_valid_data <- mkDWire(0) ;
Wire#(Bit#(10)) wr_payload_length <- mkDWire(0) ;
Wire#(bit) wr_64b_address <- mkDWire(1) ;
Wire#(bit) wr_data_valid <- mkDWire(0) ;
Wire#(bit) wr_nvm_wait <- mkDWire(0) ;
Reg#(bit) wr_wait <- mkReg(0) ;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Reg#(Bool) rg_pcie_busy <- mkReg(False) ;
Reg#(Bool) rg_send_command <- mkReg(False) ;
Reg#(Bool) rg_send_data <- mkReg(False) ;
Reg#(Bool) rg_receive_data_from_device <- mkReg(False) ;
Reg#(Bit#(32)) rg_command_DWord[16] ;
for( Integer i = 0 ; i < 16 ; i = i + 1 ) begin
	rg_command_DWord[i] <- mkReg(0) ;
end
Reg#(Bit#(4)) rg_dword_count <- mkReg(0) ;
Reg#(Bool) free_pcie <- mkReg(False) ;

Reg#(Bool) rg_read_request_granted <- mkReg(False) ;
Reg#(Bool) rg_write_request_granted <- mkReg(False) ;
// Brams
Reg#(Bit#(10)) rg_bram_count <- mkReg(0) ;

	BRAM_Configure cfg_data_bram = defaultValue ;
		cfg_data_bram .loadFormat = tagged Hex "data_for_reading.txt" ;

	BRAM2Port#( Bit#(10) , Bit#(32) ) data_ram <- mkBRAM2Server (cfg_data_bram) ;

	BRAM_Configure cfg_write_data_bram = defaultValue ;
		cfg_write_data_bram .loadFormat = tagged Hex "written_data.txt" ;

	BRAM2Port#( Bit#(10) , Bit#(32) ) write_data_ram <- mkBRAM2Server (cfg_write_data_bram) ;

Reg#(Bit#(64)) rg_address_in <- mkReg(0) ;

Reg#(bit) read <- mkReg(0) ;
Reg#(bit) write <- mkReg(0) ;

rule rl_write_priority ;
	if( wr_send_read_tlp == 1 && wr_send_write_tlp == 1 ) begin
		read <= 0 ;
		write <= 1 ;
		rg_read_request_granted <= False ;
		rg_write_request_granted <= True ;
		$display ( " Write Request granted " )  ;
	end

	else if ( wr_send_read_tlp == 1 && wr_send_write_tlp == 0)  begin
		read <= 1 ;
		write <= 0 ;		
		rg_read_request_granted <= True ;
		rg_write_request_granted <= False ;
		$display ( " Read Request granted " )  ;
	end

	else if ( wr_send_read_tlp == 0 && wr_send_write_tlp == 1)  begin
		read <= 0 ;
		write <= 1 ;		
		rg_read_request_granted <= False ;
		rg_write_request_granted <= True ;
		$display ( " Write Request granted " )  ;
	end
	
	else begin 
		read <= 0 ;
		write <= 0 ;
		rg_read_request_granted <= False ;
		rg_write_request_granted <= False  ;
		$display ( " NO .... Request granted " )  ;
	end		
endrule

rule rl_command_to_device (	wr_send_read_tlp == 1 && wr_requested_tag == 'd1 && !rg_pcie_busy  ) ;
	$display( " ******* PCIe model ****** received request for command " ) ;
	rg_address_in <=  wr_address_from_device ;
	rg_pcie_busy <= True ;
	let command = get_command( wr_address_from_device ) ;
	rg_command_DWord[0] <= command[31:0] ;
	rg_command_DWord[1] <= command[63:32] ;
	rg_command_DWord[2] <= command[95:64] ;
	rg_command_DWord[3] <= command[127:96] ;
	rg_command_DWord[4] <= command[159:128] ;
	rg_command_DWord[5] <= command[191:160] ;
	rg_command_DWord[6] <= command[223:192] ;
	rg_command_DWord[7] <= command[255:224] ;
	rg_command_DWord[8] <= command[287:256] ;
	rg_command_DWord[9] <= command[319:288] ;
	rg_command_DWord[10] <= command[351:320] ;
	rg_command_DWord[11] <= command[383:352] ;
	rg_command_DWord[12] <= command[415:384] ;
	rg_command_DWord[13] <= command[447:416] ;
	rg_command_DWord[14] <= command[479:448] ;
	rg_command_DWord[15] <= command[511:480] ;
	rg_send_command <= True ; 
	rg_tag <= 'd1 ;
//	$display("******PCIe ******** OPcode sent from PCie == %d " , command[31:0] ) ;
endrule

rule rl_send_command (rg_send_command) ;
	$display( " **** PCIE **********address from nvm = %d ", rg_address_in ) ;
	rg_completionDataOut <= rg_command_DWord[rg_dword_count] ;
	rg_completionDataValid <= 1 ;
	if(rg_dword_count == 'd15 ) begin
		rg_dword_count <= 0 ;
		rg_send_command <= False ;
		free_pcie <= True ;
		rg_completionLastDWord <= 1 ;
		$display(" **** PCIE **** Sending Last DWord " ) ;
	end
	else
		rg_dword_count <= rg_dword_count + 1 ;

endrule 

rule rl_pcie_status ;
	if( rg_pcie_busy == True ) 
		$display (" ****************PCIe is BUSY****************** " ) ;
	else
		$display (" ****************PCIe is NOT NOT NOT BUSY****************** " ) ;		
endrule

rule rl_free_pcie ( free_pcie ) ;
	rg_tag <= 0 ;
	read <= 0 ;
	write <= 0 ;
	rg_completionDataValid <= 0 ;
	rg_completionLastDWord <= 0 ;
	wr_send_valid_data <= 0 ;
	free_pcie <= False ;
	rg_pcie_busy <= False ;
endrule

Reg#(Bool ) rg_wait_state1 <- mkReg(False) ;
Reg#(Bool ) rg_wait_state2 <- mkReg(False) ;
rule rl_data_to_pcie  (wr_send_read_tlp == 1 && wr_requested_tag == 'd2 && !rg_pcie_busy  ) ;
	$display( " PCIE accepted to SEND DATA TO NVMe <<<<<<<<<<<<<<<<-------------------------- " ) ;
	rg_pcie_busy <= True ;
	//rg_send_data <= True ;
//	rg_wait_state2 <= False ;
// test purpose 
	// rg_address_in <=  wr_address_from_device  ;
	rg_wait_state1 <= True ;
endrule

rule wait_state ( rg_wait_state1 ) ;
	rg_wait_state1 <= False ;
	rg_send_data <= True ;
	rg_tag <= 'd2 ;
//	rg_wait_state2 <= True ;
endrule
/*
rule rl_wait_state2 (rg_wait_state2 ) ;
	rg_send_data <= True ;
	rg_wait_state2 <= False ;
// test purpose 
	 rg_address_in <=  wr_address_from_device  ;	
endrule
*/
rule rl_send_data ( rg_send_data ) ;
	
	rg_address_in <=  wr_address_from_device  ;
	data_ram.portA.request.put( BRAMRequest{
											write : False, 
											address: rg_bram_count , 
											datain : ?, 
											responseOnWrite : False} ) ;
	if( wr_nvm_wait == 0 ) begin

		$display ( "Sending requested DATA from Memory location %d " ,  rg_address_in ) ;
		if (rg_bram_count == 'd1023 ) begin
			rg_bram_count <= 0 ;
			rg_send_data <= False ;
			free_pcie <= True ;
			rg_completionLastDWord <= 1 ;
			rg_tag <= 'd2 ;
		end
		else
			rg_bram_count <= rg_bram_count + 1 ;
	end

	else begin
		$display ( "************************ PCIe is Waiting ************************** " );
	end

endrule

rule rl_read_bram ;
	rg_completionDataValid <= 1 ;
	let data <- data_ram.portA.response.get () ;
	$display(" data sent = %d " , data ) ;
	rg_completionDataOut <= data ;
endrule

rule rl_data_from_device ( wr_send_write_tlp == 1 && !rg_pcie_busy ) ;
	$display("************ Receiving DATA from NVM express *********** " ) ;
	rg_pcie_busy <= True ;
	rg_receive_data_from_device <= True ;
endrule

rule rl_receive_data_from_device ( rg_receive_data_from_device ) ;
	wr_send_valid_data <= 1 ;
	if( wr_data_valid == 1 && wr_nvm_wait == 0) begin
		$display ( " ************* data is valid ********** " ) ;
		write_data_ram.portA.request.put( BRAMRequest{
												write : True, 
												address: rg_bram_count , 
												datain : wr_data_from_device, 
												responseOnWrite : False} ) ;

		if (rg_bram_count == (wr_payload_length -1) ) begin
			rg_bram_count <= 0 ;
			rg_receive_data_from_device <= False ;
			free_pcie <= True ;
			rg_completionLastDWord <= 1 ;
		end
		else begin
			rg_bram_count <= rg_bram_count + 1 ;
		end
	end
	else if (  wr_data_valid == 0 ) 
		$display( " DATA SENT FROM NVMe IS NOT YET VALIDATED " ) ;
	else if ( wr_nvm_wait == 1 )
		$display ( " NVMe is NOT YET READY TO SEND THE DATA TO PCIe " ) ;
		
endrule

interface pcie_transmit = fn_pcie_transmit ( rg_completionDataOut , rg_completionDataValid , rg_completionLastDWord ,rg_tag , rg_pcie_busy ,rg_read_request_granted , rg_write_request_granted) ; 

interface pcie_receive = fn_pcie_receive ( wr_data_from_device , wr_address_from_device , wr_requested_tag , wr_send_completion_tlp , wr_send_write_tlp , wr_send_read_tlp , wr_send_valid_data , wr_payload_length , wr_64b_address , wr_data_valid , wr_nvm_wait, wr_wait ) ;

endmodule 

endpackage 
