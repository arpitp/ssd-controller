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

package NandFlashTarget ;

import BRAM :: * ;
import InterfaceNandFlashTarget :: * ;

interface Target_Interface ;
	interface TargetInterface target_interface ;
endinterface

typedef enum {
				St1, St2, St3, St4, St5, St6, St7, St8, St9, St10
			 } State deriving( Bits, Eq ) ;

module mkNandFlashTarget ( Target_Interface ) ;

// Regs and Wires for Target Inetrface
	Reg#(Bit#(16))	rg_data_to_nfc	  <- mkReg(0) ;	  // data out
	Wire#(Bit#(16))	wr_data_from_nfc  <- mkDWire(0) ; // data in
	Wire#(bit)		wr_onfi_ce_l	  <- mkDWire(1) ; // active low
	Wire#(bit)		wr_onfi_we_l	  <- mkDWire(1) ; // active low
	Wire#(bit)		wr_onfi_re_l	  <- mkDWire(1) ; // active low
	Wire#(bit)		wr_onfi_wp_l	  <- mkDWire(1) ; // active low
	Wire#(bit)		wr_onfi_cle		  <- mkDWire(0) ; // active high
	Wire#(bit)		wr_onfi_ale		  <- mkDWire(0) ; // active high
	Reg#(bit)		rg_t_ready_busy_l <- mkReg(1) ;   // active low
	Reg#(bit)		rg_t_interrupt 	  <- mkReg(0) ;   // active high

// Register files
	Reg#(Bit#(32))	rg_target_address <- mkReg(0) ;
	Reg#(Bit#(12))	rg_chip_address	  <- mkReg(0) ;
	Reg#(Bit#(32))	rg_nand_flash_id  <- mkReg(0) ; // contains info about the nand flash device - so connect it to the data module of nand flash chip

// Necessary Registers
	Reg#(Bit#(16))	rg_temp_data_from_nfc <- mkReg(0) ;	
	Reg#(Bool) 		rg_block_erase_ready  <- mkReg(False) ;
	Reg#(Bool) 		rg_read_ready 		  <- mkReg(False) ;

// STATE Register 
	Reg#(State) rg_chip0_state <- mkReg(St1) ;
	Reg#(State) rg_chip1_state <- mkReg(St1) ;
	Reg#(State) rg_chip2_state <- mkReg(St1) ;
	Reg#(State) rg_chip3_state <- mkReg(St1) ;
	Reg#(State) rg_chip4_state <- mkReg(St1) ;

	Reg#(State) rg_address_input_state  <- mkReg(St1) ;
	Reg#(State) rg_data_output_state 	<- mkReg(St1) ;
	Reg#(State) rg_data_input_state 	<- mkReg(St1) ;
	Reg#(State) rg_read_id_state		<- mkReg(St1) ;
	Reg#(State)	rg_reset_flash_state	<- mkReg(St1) ;
	Reg#(State) rg_erase_state			<- mkReg(St1) ;

// Nand Flash Chip as a collection of BRAMs \\ 0 to 19
	BRAM_Configure cfg_nand0 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip0 <- mkBRAM2Server (cfg_nand0) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand1 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip1 <- mkBRAM2Server (cfg_nand1) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand2 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip2 <- mkBRAM2Server (cfg_nand2) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand3 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip3 <- mkBRAM2Server (cfg_nand3) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand4 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip4 <- mkBRAM2Server (cfg_nand4) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand5 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip5 <- mkBRAM2Server (cfg_nand5) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand6 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip6 <- mkBRAM2Server (cfg_nand6) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand7 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip7 <- mkBRAM2Server (cfg_nand7) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand8 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip8 <- mkBRAM2Server (cfg_nand8) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand9 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip9 <- mkBRAM2Server (cfg_nand9) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand10 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip10 <- mkBRAM2Server (cfg_nand10) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand11 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip11 <- mkBRAM2Server (cfg_nand11) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand12 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip12 <- mkBRAM2Server (cfg_nand12) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand13 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip13 <- mkBRAM2Server (cfg_nand13) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand14 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip14 <- mkBRAM2Server (cfg_nand14) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand15 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip15 <- mkBRAM2Server (cfg_nand15) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand16 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip16 <- mkBRAM2Server (cfg_nand16) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand17 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip17 <- mkBRAM2Server (cfg_nand17) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand18 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip18 <- mkBRAM2Server (cfg_nand18) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB

	BRAM_Configure cfg_nand19 = defaultValue ;
	BRAM2Port#( Bit#(11) , Bit#(32) ) nand_chip19 <- mkBRAM2Server (cfg_nand19) ; // Address lines = 11 bits, Data Width = 32 bits.. 1K x 4Bytes = 4KB


//&&&&&&&&&&&&&&&&&&&& 20 BRAMs - each BRAM representing each page - divided into 5 blocks, each having 4 pages &&&&&&&&&&&&&&&&&&&&&&&&&//
//&&&&&&&&&&&&&&&&&&&&&&&&& we won't be using all the BRAMs coz 4 BRAMs are enough to verify the states &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&//

// Rules
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		DATA OUTPUT
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
rule rl_nand_data_output ( wr_onfi_ce_l == 0 && wr_onfi_ale == 0 && wr_onfi_cle == 0 && wr_onfi_we_l == 1 && wr_onfi_re_l == 0 ) ;
	$display ( " **** TARGET **** data output cycle " ) ;
	case( rg_data_output_state ) matches

		St1 :	begin
					if( rg_target_address == 'd0 )
						nand_chip0.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;
					else if( rg_target_address == 'd2048 )
						nand_chip1.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;
					else if( rg_target_address == 'd4096 )
						nand_chip2.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;	
					else if( rg_target_address == 'd6144 )
						nand_chip3.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;		

					rg_data_output_state <= St2 ;
				end

		St2 :	begin
					if( rg_target_address == 'd0 )
						nand_chip0.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;
					else if( rg_target_address == 'd2048 )
						nand_chip1.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;
					else if( rg_target_address == 'd4096 )
						nand_chip2.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;	
					else if( rg_target_address == 'd6144 )
						nand_chip3.portA.request.put ( BRAMRequest{
																write : False, 
																address: rg_chip_address[10:0], 
																datain : ?, 
																responseOnWrite : False} ) ;	

					rg_chip_address <= rg_chip_address + 1 ;
			
					if( rg_chip_address <= 'h7FF )
						rg_data_output_state <= St1 ;
					else if( rg_chip_address > 'h7FF ) begin
						// rg_chip_address <= 0 ; // gives error in bluespec - so, we update in the next cycle		
				
						rg_data_output_state <= St3 ;
					end
				end	

		St3 :	begin
				rg_chip_address <= 0 ;

				$display ( " **** TARGET **** page data sent to nfc " ) ;
				rg_data_output_state <= St3 ;
				end
	endcase

endrule

rule rl_chip0_read ; // fires 1 cycle after nand_chip0.portA.request.put(...) is called in "DATA OUTPUT" rule
	
	case( rg_chip0_state ) matches

	St1 :	begin
				let data <- nand_chip0.portA.response.get( ) ;
				rg_data_to_nfc <= data[15:0] ;
				$display ( " **** TARGET **** 1st 16 bit data sent from chip0" ) ;

				rg_chip0_state <= St2 ;
			end

	St2 :	begin
				let data <- nand_chip0.portA.response.get( ) ;
				rg_data_to_nfc <= data[31:16] ;
				$display ( " **** TARGET **** 2nd 16 bit data sent from chip0" ) ;

				rg_chip0_state <= St1 ;
			end
	
	endcase

endrule

rule rl_chip1_read ; // fires 1 cycle after nand_chip1.portA.request.put(...) is called in "DATA OUTPUT" rule
	
	case( rg_chip1_state ) matches

	St1 :	begin
				let data <- nand_chip1.portA.response.get( ) ;
				rg_data_to_nfc <= data[15:0] ;
				$display ( " **** TARGET **** 1st 16 bit data sent from chip1 " ) ;

				rg_chip1_state <= St2 ;
			end

	St2 :	begin
				let data <- nand_chip1.portA.response.get( ) ;
				rg_data_to_nfc <= data[31:16] ;
				$display ( " **** TARGET **** 2nd 16 bit data sent from chip1 " ) ;

				rg_chip1_state <= St1 ;
			end
	
	endcase

endrule

rule rl_chip2_read ; // fires 1 cycle after nand_chip2.portA.request.put(...) is called in "DATA OUTPUT" rule
	
	case( rg_chip2_state ) matches

	St1 :	begin
				let data <- nand_chip2.portA.response.get( ) ;
				rg_data_to_nfc <= data[15:0] ;
				$display ( " **** TARGET **** 1st 16 bit data sent from chip2 " ) ;

				rg_chip2_state <= St2 ;
			end

	St2 :	begin
				let data <- nand_chip2.portA.response.get( ) ;
				rg_data_to_nfc <= data[31:16] ;
				$display ( " **** TARGET **** 2nd 16 bit data sent from chip2 " ) ;

				rg_chip2_state <= St1 ;
			end
	
	endcase

endrule

rule rl_chip3_read ; // fires 1 cycle after nand_chip3.portA.request.put(...) is called in "DATA OUTPUT" rule
	
	case( rg_chip3_state ) matches

	St1 :	begin
				let data <- nand_chip3.portA.response.get( ) ;
				rg_data_to_nfc <= data[15:0] ;
				$display ( " **** TARGET **** 1st 16 bit data sent from chip3 " ) ;

				rg_chip3_state <= St2 ;
			end

	St2 :	begin
				let data <- nand_chip3.portA.response.get( ) ;
				rg_data_to_nfc <= data[31:16] ;
				$display ( " **** TARGET **** 2nd 16 bit data sent from chip3 " ) ;

				rg_chip3_state <= St1 ;
			end
	
	endcase

endrule

/*
rule rl_chip4_read ; // fires 1 cycle after nand_chip4.portA.request.put(...) is called in "DATA OUTPUT" rule
	
	case( rg_chip4_state ) matches

	St1 :	begin
				let data <- nand_chip4.portA.response.get( ) ;
				rg_data_to_nfc <= data[15:0] ;
				$display ( " **** TARGET **** 1st 16 bit data sent from chip4 " ) ;

				rg_chip4_state <= St2 ;
			end

	St2 :	begin
				let data <- nand_chip4.portA.response.get( ) ;
				rg_data_to_nfc <= data[31:16] ;
				$display ( " **** TARGET **** 2nd 16 bit data sent fom chip4 " ) ;

				rg_chip4_state <= St1 ;
			end
	
	endcase

endrule
*/
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		DATA INPUT
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
rule rl_nand_data_input ( wr_onfi_ce_l == 0 && wr_onfi_ale == 0 && wr_onfi_cle == 0 && wr_onfi_we_l == 0 && wr_onfi_re_l == 1 && wr_onfi_wp_l == 1 ) ;
	$display ( " **** TARGET **** data input cycle " ) ;
	case( rg_data_input_state ) matches

		St1 :	// wait state
				rg_data_input_state <= St2 ;

		St2 :	// wait state
				rg_data_input_state <= St3 ;

		St3 :	begin
					rg_temp_data_from_nfc <= wr_data_from_nfc ;
					$display ( " **** TARGET **** 1st 16 bit data received from nfc & copied " ) ;
				
					rg_data_input_state <= St4 ;
				end
	
		St4 :	begin
				if( rg_chip_address <= 'h7FF ) begin
					if( rg_target_address == 'd0 )
						nand_chip0.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { wr_data_from_nfc , rg_temp_data_from_nfc }, 
																responseOnWrite : False} ) ;
					else if( rg_target_address == 'd2048 )
						nand_chip1.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { wr_data_from_nfc , rg_temp_data_from_nfc }, 
																responseOnWrite : False} ) ;
					else if( rg_target_address == 'd4096 )
						nand_chip2.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { wr_data_from_nfc , rg_temp_data_from_nfc }, 
																responseOnWrite : False} ) ;
					else if( rg_target_address == 'd6144 )
						nand_chip3.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { wr_data_from_nfc , rg_temp_data_from_nfc }, 
																responseOnWrite : False} ) ;

					$display ( " **** TARGET **** 32 bit data written onto NAND flash device " ) ;

					rg_chip_address <= rg_chip_address + 1 ;				

					rg_data_input_state <= St3 ;
				end
				else if( rg_chip_address > 'h7FF ) begin
					rg_chip_address <= 0 ; 
					$display ( " **** TARGET ************************************************ program page done " ) ;

					rg_data_input_state <= St5 ;
				end
				end

		St5 :	rg_t_ready_busy_l <= 1 ; // indicating end of write operation

	endcase

endrule
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		COMMAND INPUT
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
rule rl_nand_command_input ( wr_onfi_ce_l == 0 && wr_onfi_ale == 0 && wr_onfi_cle == 1 && wr_onfi_we_l == 0 && wr_onfi_re_l == 1 ) ;

	if( wr_data_from_nfc[7:0] == 'h00 ) begin
		$display( " **** TARGET **** READ request from Nand Flash Controller " ) ;
		rg_read_ready <= True ;
	end
	else if( wr_data_from_nfc[7:0] == 'h30 ) begin
		$display( " **** TARGET **** 2nd cmd of READ request from Nand Flash Controller " ) ;
		// when reading is started, 'rg_t_ready_busy_l' must be '0' 
		// when reading is finished, 'rg_t_ready_busy_l' must be '1' 
	end
	
	else if( wr_data_from_nfc[7:0] == 'h80 )
		$display( " **** TARGET **** WRITE request from Nand Flash Controller " ) ;
	else if( wr_data_from_nfc[7:0] == 'h10 ) begin
		$display( " **** TARGET **** 2nd cmd of WRITE request from Nand Flash Controller " ) ;
		// when writing is started, 'rg_t_ready_busy_l' must be '0'
		// when writing is finished, 'rg_t_ready_busy_l' must be '1' 	
	end
	
	else if( wr_data_from_nfc[7:0] == 'h60 ) begin
		$display( " **** TARGET **** BLOCK ERASE request from Nand Flash Controller " ) ;
		rg_block_erase_ready <= True ;
	end
	else if( wr_data_from_nfc[7:0] == 'hD0 ) begin
		$display( " **** TARGET **** 2nd cmd of BLOCK ERASE request from Nand Flash Controller " ) ;
		// when erasing is started, 'rg_t_ready_busy_l' must be '0'
		// when erasing is finished, 'rg_t_ready_busy_l' must be '1'
	end
	
	else if( wr_data_from_nfc[7:0] == 'h70 ) begin
		$display( " **** TARGET **** READ STATUS request from Nand Flash Controller " ) ;

		if( rg_t_ready_busy_l == 0 )
			rg_data_to_nfc <= 'd0 ; // 00000000_0000_000_0
		else if( rg_t_ready_busy_l == 1 )
			rg_data_to_nfc <= 'd1 ; // 00000000_0000_000_1
	end
	
	else if( wr_data_from_nfc[7:0] == 'h90 ) begin
		$display( " **** TARGET **** READ ID request from Nand Flash Controller " ) ;

		case( rg_read_id_state ) matches
		
			St1 :	begin
						rg_data_to_nfc <= rg_nand_flash_id[15:0] ;
						
						rg_read_id_state <= St2 ;
					end

			St2 :	begin
						rg_data_to_nfc <= rg_nand_flash_id[31:16] ;
					end
		endcase

	end
	
	else if( wr_data_from_nfc[7:0] == 'hFF ) begin
		$display( " **** TARGET **** RESET request from Nand Flash Controller " ) ;

		case( rg_reset_flash_state ) matches

			St1 :	begin
					rg_t_ready_busy_l <= 0 ; // start of reset
					
					rg_reset_flash_state <= St2 ;
					end

			St2 :	begin
					if( rg_chip_address <= 'h7FF ) begin
						nand_chip0.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { 'd0 }, 
																responseOnWrite : False} ) ;

						rg_chip_address <= rg_chip_address + 1 ;

						rg_reset_flash_state <= St2 ;
					end

					else if( rg_chip_address > 'h7FF ) begin // else begin
						rg_chip_address <= 0 ; 

						rg_reset_flash_state <= St3 ;
					end
					end

			St3 :	begin
					if( rg_chip_address <= 'h7FF ) begin
						nand_chip1.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { 'd0 }, 
																responseOnWrite : False} ) ;

						rg_chip_address <= rg_chip_address + 1 ;

						rg_reset_flash_state <= St3 ;
					end

					else if( rg_chip_address > 'h7FF ) begin // else begin
						rg_chip_address <= 0 ; 

						rg_reset_flash_state <= St4 ;
					end
					end

			St4 :	begin
					if( rg_chip_address <= 'h7FF ) begin
						nand_chip2.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { 'd0 }, 
																responseOnWrite : False} ) ;

						rg_chip_address <= rg_chip_address + 1 ;

						rg_reset_flash_state <= St4 ;
					end

					else if( rg_chip_address > 'h7FF ) begin // else begin
						rg_chip_address <= 0 ; 

						rg_reset_flash_state <= St5 ;
					end
					end

			St5 :	begin
					if( rg_chip_address <= 'h7FF ) begin
						nand_chip3.portA.request.put ( BRAMRequest{
																write : True, 
																address: rg_chip_address[10:0], 
																datain : { 'd0 }, 
																responseOnWrite : False} ) ;

						rg_chip_address <= rg_chip_address + 1 ;

						rg_reset_flash_state <= St5 ;
					end

					else if( rg_chip_address > 'h7FF ) begin // else begin
						rg_chip_address <= 0 ; 

						rg_reset_flash_state <= St6 ;
					end
					end

			St6 :	begin
					rg_t_ready_busy_l <= 1 ; // end of reset

					rg_reset_flash_state <= St7 ;
					end

			St7 :	rg_reset_flash_state <= St7 ;

		endcase
		
	end

endrule
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		ADDRESS INPUT
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
rule rl_nand_address_input ( wr_onfi_ce_l == 0 && wr_onfi_ale == 1 && wr_onfi_cle == 0 && wr_onfi_we_l == 0 && wr_onfi_re_l == 1 ) ;
	$display ( " **** TARGET ***** address input cycle " ) ;
	case( rg_address_input_state ) matches

		St1 :	begin
					$display ( " **** TARGET **** address_1 received " ) ;	
		
					rg_target_address[15:0] <= wr_data_from_nfc ;

					rg_address_input_state <= St2 ;
				end

		St2 :	begin
					$display ( " **** TARGET **** address_2 received " ) ;
					rg_target_address[31:16] <= wr_data_from_nfc ;
		
					rg_t_ready_busy_l <= 0 ; // indicating end of address-read and start of either data-read, data-write or block-erase

					rg_address_input_state <= St3 ;
				end
		
		St3 :	begin
					if( rg_block_erase_ready == True ) begin // address of the block (to be erased) is loaded, block-erase process - to be started
				
					case( rg_erase_state ) matches

					St1 :	begin
							if( rg_chip_address <= 'h7FF ) begin
								nand_chip0.portA.request.put ( BRAMRequest{
																		write : True, 
																		address: rg_chip_address[10:0], 
																		datain : { 'd0 }, 
																		responseOnWrite : False} ) ;

								rg_chip_address <= rg_chip_address + 1 ;

								rg_erase_state <= St1 ;
							end

							else if( rg_chip_address > 'h7FF ) begin // else begin
								rg_chip_address <= 0 ; 

								rg_erase_state <= St2 ;
							end
							end

					St2 :	begin
							if( rg_chip_address <= 'h7FF ) begin
								nand_chip1.portA.request.put ( BRAMRequest{
																		write : True, 
																		address: rg_chip_address[10:0], 
																		datain : { 'd0 }, 
																		responseOnWrite : False} ) ;

								rg_chip_address <= rg_chip_address + 1 ;

								rg_erase_state <= St2 ;
							end

							else if( rg_chip_address > 'h7FF ) begin // else begin
								rg_chip_address <= 0 ; 

								rg_erase_state <= St3 ;
							end
							end

					St3 :	begin
							if( rg_chip_address <= 'h7FF ) begin
								nand_chip2.portA.request.put ( BRAMRequest{
																		write : True, 
																		address: rg_chip_address[10:0], 
																		datain : { 'd0 }, 
																		responseOnWrite : False} ) ;

								rg_chip_address <= rg_chip_address + 1 ;

								rg_erase_state <= St3 ;
							end

							else if( rg_chip_address > 'h7FF ) begin // else begin
								rg_chip_address <= 0 ; 

								rg_erase_state <= St4 ;
							end
							end

					St4 :	begin
							if( rg_chip_address <= 'h7FF ) begin
								nand_chip3.portA.request.put ( BRAMRequest{
																		write : True, 
																		address: rg_chip_address[10:0], 
																		datain : { 'd0 }, 
																		responseOnWrite : False} ) ;

								rg_chip_address <= rg_chip_address + 1 ;

								rg_erase_state <= St4 ;
							end

							else if( rg_chip_address > 'h7FF ) begin // else begin
								rg_chip_address <= 0 ; 

								rg_erase_state <= St5 ;
							end
							end

					St5 :	begin
							rg_t_ready_busy_l <= 1 ;
							
							rg_block_erase_ready <= False ;
							end
					endcase

					end // end of if

					rg_address_input_state <= St4 ;
				end

		St4 :	if( rg_read_ready == True ) begin // address of the page (to be read) is loaded, page-read process - to be started
					$display ( " **** TARGET **** ready/busy is made high " ) ;
					rg_read_ready <= False ;
					rg_t_ready_busy_l <= 1 ; // indicating that the 'nfc' can start reading the page from the flash device
				end
	endcase

endrule
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

rule rl_nand_idle ( wr_onfi_ce_l == 0 && wr_onfi_ale == 0 && wr_onfi_cle == 0 && wr_onfi_we_l == 1 && wr_onfi_re_l == 1 ) ;
	$display ( " **** TARGET **** idle " ) ;
	// idle - do nothing
endrule


rule rl_nand_standby ( wr_onfi_ce_l == 1 ) ;
	// standby - do nothing
endrule


rule rl_nand_write_protect ( wr_onfi_wp_l == 0 ) ;
	$display( " **** TARGET **** Nand Flash Device is Write Protected " ) ; // write does not happen when 'wr_onfi_wp_l' == 0
endrule

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

interface target_interface = fn_target_interface ( wr_onfi_ce_l, wr_onfi_cle, wr_onfi_ale, wr_onfi_we_l, wr_onfi_re_l, wr_onfi_wp_l, rg_data_to_nfc, wr_data_from_nfc, rg_t_ready_busy_l, rg_t_interrupt ) ;

endmodule

endpackage
