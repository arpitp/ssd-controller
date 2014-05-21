package NandFlashController ;

import BRAM :: * ;
import InterfaceNandFlashController :: * ;

interface NFC_Interface ;
	interface NandFlashInterface nfcB_interface  ;
	interface ONFiInterface onfi_interface ;
endinterface

typedef enum {
				St0, St1, St2, St3, St4, St5, St6, St6_1, St6_2, St7, St8, St9, St10
			 } State deriving( Bits, Eq ) ;

module mkNandFlashController ( NFC_Interface ) ;

// Wires and Regs related to the Nand Flash Interface 
	Wire#(Bit#(12))	wr_address_from_nvm <- mkDWire(0) ;	// address
	Wire#(Bit#(32))	wr_data_from_nvm 	<- mkDWire(0) ;	// data in
	Reg#(Bit#(32))	rg_data_to_nvm 		<- mkReg(0) ;	// data out
	Wire#(bit)		wr_nand_ce_l		<- mkDWire(1) ;	// active low
	Wire#(bit)		wr_nand_we_l		<- mkDWire(1) ;	// active low
	Wire#(bit)		wr_nand_oe_l 		<- mkDWire(1) ;	// active low
	Wire#(bit)		wr_nand_reset_l 	<- mkDWire(1) ;	// active low
	Reg#(bit)		rg_interrupt		<- mkReg(0) ;	// active high
	Reg#(bit)		rg_ready_busy_l		<- mkReg(1) ;	// active low

// Regs and Wires for Onfi Interface
	Reg#(Bit#(16))  rg_data_to_flash   	<- mkReg(0) ;	// data out \\ 16 bit bus 
	Wire#(Bit#(16)) wr_data_from_flash 	<- mkDWire(0) ;	// data in  \\ 16 bit bus 
	Reg#(bit) 		rg_onfi_ce_l 	   	<- mkReg(1) ;	// active low
	Reg#(bit)		rg_onfi_we_l	   	<- mkReg(1) ;	// active low
	Reg#(bit)		rg_onfi_re_l		<- mkReg(1) ;	// active low
	Reg#(bit)		rg_onfi_cle			<- mkReg(0) ;	// active high
	Reg#(bit)		rg_onfi_ale			<- mkReg(0) ;	// active high
	Reg#(bit)		rg_onfi_wp_l		<- mkReg(1) ;	// active low
	Wire#(bit)		wr_interrupt		<- mkDWire(0) ;	// active high
	Wire#(bit)		wr_ready_busy_l		<- mkDWire(1) ;	// active low

// Register Files
	Reg#(Bit#(8))  id_register0 	<- mkReg(0) ;	// ID Reg 0
	Reg#(Bit#(8))  id_register1 	<- mkReg(0) ;	// ID Reg 1
	Reg#(Bit#(8))  id_register2 	<- mkReg(0) ;	// ID Reg 2
	Reg#(Bit#(8))  id_register3 	<- mkReg(0) ;	// ID Reg 3
	Reg#(Bit#(25)) block_address 	<- mkReg(0) ; 	// Block Address Register
	Reg#(Bit#(8))  part_type_ECC 	<- mkReg(0) ; 	// Part Type and ECC Register 
	Reg#(bit) 	   buffer_number 	<- mkReg(0) ; 	// Buffer Number
	Reg#(Bit#(8))  status_register 	<- mkReg(0) ; 	// Status Register 
	Reg#(Bit#(8))  command_register <- mkReg(0) ;	// Command Register 

// Data Buffers 
	BRAM_Configure cfg_buffer0 = defaultValue ;
	BRAM2Port#( Bit#(10) , Bit#(32) ) buffer0 <- mkBRAM2Server (cfg_buffer0) ; // Address lines = 10 bits ,Data Width = 32bits.. 0.5K x 4Bytes = 2KB

	BRAM_Configure cfg_buffer1 = defaultValue ;
	BRAM2Port#( Bit#(10) , Bit#(32) ) buffer1 <- mkBRAM2Server (cfg_buffer1) ; // Address lines = 10 bits ,Data Width = 32bits.. 0.5K x 4Bytes = 2KB

// Necessary Registers
	Reg#(Bit#(12))  buffer_address 			<- mkReg(0) ; // Address to Buffer initialized to '0'
	Reg#(Bit#(16)) rg_temp_data_from_flash  <- mkReg(0) ;

	Reg#(Bool) rg_command_execution 	<- mkReg(False) ;
	Reg#(bit) rg_nand_buffer_control 	<- mkReg(0) ;

// STATE Registers 
	Reg#(State) rg_state0 			 <- mkReg(St1) ;
	Reg#(State) rg_state1 			 <- mkReg(St1) ;
	Reg#(State) rg_read_state 		 <- mkReg(St0) ;
	Reg#(State) rg_write_state 		 <- mkReg(St0) ;
	Reg#(State) rg_block_erase_state <- mkReg(St0) ;
	Reg#(State) rg_read_status_state <- mkReg(St1) ;
	Reg#(State) rg_read_ID_state 	 <- mkReg(St1) ;
	Reg#(State) rg_reset_state 		 <- mkReg(St1) ;


(* descending_urgency = "rl_disable, rl_write, rl_read" *)
	
	rule rl_disable (wr_nand_ce_l == 1 ) ;
		$display(" NAND FLASH CONTROLLER IN DISABLED STATE " ) ;
		rg_command_execution <= False ;
	endrule

	rule rl_write ( wr_nand_ce_l == 0 && wr_nand_we_l == 0 && rg_command_execution == False ) ;
		$display ( " NAND FLASH CONTROLLER IN WRITE STATE " ) ;
			
			if ( wr_address_from_nvm <= 'h7FF ) begin
				if ( buffer_number == 0 ) 						// Buffer Number 0 is in Control of the HOST ( i.e. NVMe ) 
					buffer0.portA.request.put ( BRAMRequest{
															write : True, 
															address: wr_address_from_nvm[9:0]  , 
															datain : wr_data_from_nvm, 
															responseOnWrite : False} ) ;

				else if ( buffer_number == 1 ) 					// Buffer Number 1 is in Control of the HOST ( i.e. NVMe ) 
					buffer1.portA.request.put ( BRAMRequest{
															write : True, 
															address: wr_address_from_nvm[9:0], 
															datain : wr_data_from_nvm, 
															responseOnWrite : False} ) ;
			end

			// if address is > 'h3FF and < 'hFF0 , then the address is invalid .. its reserved 

			// if ( wr_address_from_nvm == 'hFF0 )  // NO WRITE OPTION TO READ ID REGISTERS
			// if ( wr_address_from_nvm == 'hFF1 )  // NO WRITE OPTION TO READ ID REGISTERS
			// if ( wr_address_from_nvm == 'hFF2 )  // NO WRITE OPTION TO READ ID REGISTERS
			// if ( wr_address_from_nvm == 'hFF3 )  // NO WRITE OPTION TO READ ID REGISTERS	
		
			if ( wr_address_from_nvm == 'hFF4 ) begin
				block_address[7:0] <=  wr_data_from_nvm[7:0] ;
				$display(" **** NAND **** Block Address Part 1 written " ) ;
			end			
		
			if ( wr_address_from_nvm == 'hFF5 ) begin
				block_address[15:8] <=  wr_data_from_nvm[7:0] ;
				$display(" **** NAND **** Block Address Part 2 written " ) ;				
			end
			if ( wr_address_from_nvm == 'hFF6 ) begin
				block_address[24:16] <=  wr_data_from_nvm[8:0] ;
				$display(" **** NAND **** Block Address Part 3 written " ) ;
			end

			// if ( wr_address_from_nvm == 'hFF7 )  // NO WRITE OPTION TO PART TYPE AND ECC REGISTERS	

			if ( wr_address_from_nvm == 'hFF8 ) begin
				buffer_number <=  wr_data_from_nvm[0] ;			
				rg_nand_buffer_control <= ~wr_data_from_nvm[0] ;
				$display( "**** NAND ****  Buffer Control Changed " ) ;
			end

			// if ( wr_address_from_nvm == 'hFF9 )  // NO WRITE OPTION TO STATUS REGISTERS	 

			if ( wr_address_from_nvm == 'hFFA ) begin
				$display( " **** NAND **RL_nand_flash_controller_rl_command_execution**  Obtained the Command to be executed " ) ;
				command_register <=  wr_data_from_nvm[7:0] ;			
				rg_command_execution <= True ;
			end	
		// end
	endrule

	rule rl_read ( wr_nand_ce_l == 0 && wr_nand_oe_l == 0) ;
		$display ( " NAND FLASH CONTROLLER IN READ STATE " ) ;

			// Reading from Buffers
			if ( wr_address_from_nvm <= 'h7FF ) begin
				if ( buffer_number == 0 ) 			// Buffer Number 0 is in Control of the HOST ( i.e. NVMe ) 
					buffer0.portA.request.put ( BRAMRequest{
															write : False, 
															address: wr_address_from_nvm[9:0]  , 
															datain : ?, 
															responseOnWrite : False} ) ;

				else if ( buffer_number == 1 ) 		// Buffer Number 1 is in Control of the HOST ( i.e. NVMe ) 
					buffer1.portA.request.put ( BRAMRequest{
															write : False, 
															address: wr_address_from_nvm[9:0]  , 
															datain : ?, 
															responseOnWrite : False} ) ;
			end

			// if address is > 'h3FF and < 'hFF0 , then the address is invalid .. its reserved 

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
				$display( " **** NAND ****  Requested for Buffer Number Information " ) ;
			end
			if ( wr_address_from_nvm == 'hFF9 ) 	 
				rg_data_to_nvm <= {'d0 , status_register } ;		// Read Status Register
			if ( wr_address_from_nvm == 'hFFA ) 
				rg_data_to_nvm <= {'d0 , command_register } ;		// Read Command Register

		//end
	endrule

//************* Temporary register files **************//
// Reg#(Bit#(10)) rg_buffer_read_address <- mkReg(0) ;
// Reg#(Bit#(10)) rg_buffer_write_address <- mkReg(0) ;
// Reg#(Bit#(2)) page_count <- mkReg(0) ;

rule rl_command_execution ( rg_command_execution == True && wr_nand_ce_l == 0 )  ;
$display( " **** NAND **** In COMMAND EXECUTION STAGE " ) ;

	case( command_register ) matches
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		READ PAGE
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		'h00 :  begin
					rg_onfi_re_l  <= 1 ; // 
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // command cycle
					rg_onfi_cle   <= 1 ; //
					rg_onfi_ce_l  <= 0 ; //
				
					rg_data_to_flash <= {'d0 , command_register} ;

					rg_command_execution <= False ;
				end
	
		'h30 : case( rg_read_state ) matches 

			St0 :	begin
					rg_onfi_re_l  <= 1 ; // 
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // command cycle
					rg_onfi_cle   <= 1 ; //
					rg_onfi_ce_l  <= 0 ; //
				
					rg_data_to_flash <= {'d0 , command_register} ;

					rg_read_state <= St1 ;
					end

			St1 :   begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 0 ; // 
					rg_onfi_ale   <= 1 ; // address cycle 
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //
			 		
					rg_data_to_flash <= { block_address[4:0] , 11'd0 } ; // MT29F2G16A (x16) device
					
					rg_read_state <= St2 ;
					end

			St2 :	begin
					rg_data_to_flash <= { 'd0 , block_address[17:5] } ; // MT29F2G16A (x16) device
					$display ( " **** NAND **** address sent " ) ;

					rg_read_state <= St3 ;
					end

			St3 : 	begin
						// wait state
						$display ( " **** NAND **** wait state for ready/busy to get low " ) ;
						rg_read_state <= St4 ;
					end 

			St4 : 	begin
						$display ( " wr_ready_busy_l = %d ", wr_ready_busy_l ) ; 
					
						if( wr_ready_busy_l == 0 ) begin
							$display ( " **** NAND **** ready/busy got low " ) ;
							rg_read_state <= St5 ;
						end
					end

			St5 :	begin
					$display ( " wr_ready_busy_l = %d ", wr_ready_busy_l ) ;
					
					if( wr_ready_busy_l == 1 ) begin
						$display ( " **** NAND **** data output cycle initiated " ) ;
						rg_onfi_re_l  <= 0 ; // 
						rg_onfi_we_l  <= 1 ; // 
						rg_onfi_ale   <= 0 ; // data output cycle
						rg_onfi_cle   <= 0 ; // 
						rg_onfi_ce_l  <= 0 ; // 
						
						rg_read_state <= St6 ;
					end
					end
		
			St6 :	// wait state
					rg_read_state <= St6_1 ;

			St6_1 :	// wait state
					rg_read_state <= St7 ;

			St7 :	// we can write 32 bit data onto the buffer in 1 cycle but we get 16 bit data from the flash device each cycle
					// so we copy data(#16) in the 1st cycle & in the 2nd cycle, we write '2nd cycle data(#16) + copied data(#16)' = (#32) data onto the buffer
					begin
						$display ( " **** NAND **** 16 bit data received from target " ) ;
						
						rg_temp_data_from_flash <= wr_data_from_flash ; 
	
						rg_read_state <= St8 ;
					end

			St8 :	begin
					if( buffer_number == 0 ) begin
						buffer0.portB.request.put ( BRAMRequest{
															write : True, 
															address: buffer_address[9:0], 
															datain : { wr_data_from_flash, rg_temp_data_from_flash }, // 32 bits of data written onto the buffer 
															responseOnWrite : False} ) ;
						buffer_address <= buffer_address + 1 ;
					$display( " **** NAND **** 32 bit data loaded onto the buffer_0 " );
					end
					else if( buffer_number == 1 ) begin
						buffer1.portB.request.put ( BRAMRequest{
															write : True, 
															address: buffer_address[9:0], 
															datain : { wr_data_from_flash, rg_temp_data_from_flash }, 
															responseOnWrite : False} ) ;
						buffer_address <= buffer_address + 1 ;
					$display( " **** NAND **** 32 bit data loaded onto the buffer_1 " );
					end

					if( buffer_address < 'h3FF ) begin
						buffer_number <= 0 ;
						
						rg_read_state <= St7 ;
					end
					else if( buffer_address < 'h7FF ) begin
						buffer_number <= 1 ;

						rg_read_state <= St7 ;
					end
					else if( buffer_address >= 'h7FF ) begin // read process is complete 
						// buffer_address <= 0 ; // gives error in bluespec - so, we update in the next cycle
						buffer_number <= 0 ;
						$display ( " **** NAND **** page data received from target " ) ;

						rg_read_state <= St9 ;
					end 
					end

			St9 :	begin
					buffer_address <= 0 ;
					$display ( " **** NAND **** idle state initiated " ) ;
					
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 1 ; //
					rg_onfi_ale   <= 0 ; // idle
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //
					
					rg_interrupt <= 1 ;

					rg_read_state <= St10 ;
					end

			St10 :	rg_read_state <= St10 ; // rg_interrupt will be back to '0' coz of rl_disable_interrupt
										
			endcase
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		PROGRAM PAGE
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		'h80 :  begin
				rg_onfi_re_l  <= 1 ; //
				rg_onfi_we_l  <= 0 ; //
				rg_onfi_ale   <= 0 ; // command cycle
				rg_onfi_cle   <= 1 ; //
				rg_onfi_ce_l  <= 0 ; //
				
				rg_data_to_flash <= { 'd0 , command_register } ;

				rg_command_execution <= False ;
				end

		'h10 : case( rg_write_state ) matches

			St0 :	begin
					rg_onfi_re_l  <= 1 ; // 
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // command cycle
					rg_onfi_cle   <= 1 ; //
					rg_onfi_ce_l  <= 0 ; //
				
					rg_data_to_flash <= {'d0 , command_register} ;

					rg_write_state <= St1 ;
					end

			St1 :  	begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 1 ; // address cycle
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //
			 		
					rg_data_to_flash <= { block_address[4:0] , 11'd0 } ; // MT29F2G016A (x16) device

					rg_write_state <= St2 ;
					end

			St2 :	begin
						rg_data_to_flash <= { 'd0 , block_address[17:5] } ; // MT29F2G016A (x16) device
						$display ( " **** NAND **** address sent " ) ;

						rg_write_state <= St3 ;
					end

			St3 : 	begin
					// wait state
					$display ( " **** NAND **** wait state for ready/busy to get low " ) ;
					rg_write_state <= St4 ;
					end

			St4 : 	if( wr_ready_busy_l == 0 ) begin
						$display ( " **** NAND **** ready/busy got low " ) ;
						$display ( " **** NAND **** data input cycle initiated in next cycle" ) ;
						rg_write_state <= St5 ;
					end

			St5 :	begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // data input cycle
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //

					// we get 32 bit data from the buffer in 1 cycle but we can send only 16 bit data to the flash device each cycle
					// so, we dont increment the buffer address every cycle, instead we do it every alternate cycle
					// in the 1st cycle, 16 bits are sent and in the 2nd cycle, the next 16 bits are sent and then the address of buffer is incremented
					// we need to call 'buffer...portB.request.put(...)' in both the cycles,
					// so that the rule 'rl_portb_buffer..' will be fired in both the cycles and the both the 16 bit data from each cycle will be sent
					if( buffer_number == 0 )
						buffer0.portB.request.put ( BRAMRequest{
															write : False, 
															address: buffer_address[9:0], 
															datain : ?, 
															responseOnWrite : False} ) ; 
					else if( buffer_number == 1 )
						buffer1.portB.request.put ( BRAMRequest{
															write : False, 
															address: buffer_address[9:0], 
															datain : ?, 
															responseOnWrite : False} ) ; 
					rg_write_state <= St6 ;
					end

			St6 : 	begin
					if( buffer_number == 0 ) begin
						buffer0.portB.request.put ( BRAMRequest{
															write : False, 
															address: buffer_address[9:0], 
															datain : ?, 
															responseOnWrite : False} ) ;

						buffer_address <= buffer_address + 1 ;
					end
					else if( buffer_number == 1 ) begin
						buffer1.portB.request.put ( BRAMRequest{
															write : False, 
															address: buffer_address[9:0], 
															datain : ?, 
															responseOnWrite : False} ) ;
 
						buffer_address <= buffer_address + 1 ;
					end
					if( buffer_address < 'h3FF ) begin
						buffer_number <= 0 ;

						rg_write_state <= St5 ;
					end
					else if( buffer_address < 'h7FF ) begin
						buffer_number <= 1 ;

						rg_write_state <= St5 ;
					end
					else if( buffer_address >= 'h7FF ) begin // write process is complete
						// buffer_address <= 0 ; // gives error in bluespec - so, we updata in the next cycle
						buffer_number <= 0 ;

						rg_write_state <= St7 ;
					end
					end

			St7 :	begin
						buffer_address <= 0 ;
						
						$display ( " **** NAND **** write process is complete " ) ;
						$display ( " **** NAND **** waiting for ready/busy to get high " ) ;

						if( wr_ready_busy_l == 1) 
							rg_write_state <= St8 ;
					end

			St8 : 	begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 1 ; //
					rg_onfi_ale   <= 0 ; // idle
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //
					
					rg_interrupt <= 1 ;

					rg_write_state <= St9 ;
					end

			St9 :	rg_write_state <= St9 ; // rg_interrupt will be back to '0' coz of rl_disable_interrupt
					
			endcase
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		BLOCK ERASE
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		'h60 : 	begin
				rg_onfi_re_l  <= 1 ; //
				rg_onfi_we_l  <= 0 ; //
				rg_onfi_ale   <= 0 ; // command cycle
				rg_onfi_cle   <= 1 ; //
				rg_onfi_ce_l  <= 0 ; //
				
				rg_data_to_flash <= { 'd0 , command_register } ;
				end

		'hD0 : case( rg_block_erase_state ) matches

			St0 :	begin
					rg_onfi_re_l  <= 1 ; // 
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // command cycle
					rg_onfi_cle   <= 1 ; //
					rg_onfi_ce_l  <= 0 ; //
				
					rg_data_to_flash <= {'d0 , command_register} ;

					rg_block_erase_state <= St1 ;
					end
			
			St1 :  	begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 1 ; // address cycle
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //
			 		
					rg_data_to_flash <= { block_address[4:0] , 11'd0 } ; // MT29F2G016A (x16) device

					rg_block_erase_state <= St2 ;
					end

			St2 :	begin
						rg_data_to_flash <= { 'd0 , block_address[17:5] } ; // MT29F2G016A (x16) device

						rg_block_erase_state <= St3 ;
					end

			St3 :	// wait state
					rg_block_erase_state <= St4 ;

			St4 : 	if( wr_ready_busy_l == 0 )
						rg_block_erase_state <= St5 ;

			St5 :	if( wr_ready_busy_l == 1 ) begin

						rg_onfi_re_l  <= 1 ; //
						rg_onfi_we_l  <= 1 ; //
						rg_onfi_ale   <= 0 ; // idle
						rg_onfi_cle   <= 0 ; //
						rg_onfi_ce_l  <= 0 ; //
						
						rg_interrupt <= 1 ;

						rg_block_erase_state <= St6 ;
					end

			St6 :	rg_block_erase_state <= St6 ; // rg_interrupt will be back to '0' coz of rl_disable_interrupt


			endcase
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		READ STATUS
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		'h70 : case( rg_read_status_state ) matches // read status
			
			St1 : 	begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // command cycle
					rg_onfi_cle   <= 1 ; //
					rg_onfi_ce_l  <= 0 ; //
				
					rg_data_to_flash <= { 'd0 , command_register } ;

					rg_read_status_state <= St2 ;
					end

			St2 : 	// wait state
					rg_read_status_state <= St3 ;

			St3 : 	begin
						status_register <= wr_data_from_flash[7:0] ;
				
						rg_read_status_state <= St4 ;
					end

			St4 :	begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 1 ; //
					rg_onfi_ale   <= 0 ; // idle
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //
					
					rg_interrupt <= 1 ;

					rg_read_status_state <= St5 ;
					end

			St5 : 	rg_read_status_state <= St5 ; // rg_interrupt will be back to '0' coz of rl_disable_interrupt				

			endcase
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		READ ID
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		'h90 : case( rg_read_ID_state ) matches // read ID
			
			St1 : 	begin
					rg_onfi_re_l  <= 1 ; // 
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // command cycle
					rg_onfi_cle   <= 1 ; //
					rg_onfi_ce_l  <= 0 ; //
				
					rg_data_to_flash <= { 'd0 , command_register } ;

					rg_read_ID_state <= St2 ;
					end
			
			St2 :	// wait state
					rg_read_ID_state <= St3 ;

			St3 :	begin
					id_register0 <= wr_data_from_flash[7:0] ;
					id_register1 <= wr_data_from_flash[15:8] ;

					rg_read_ID_state <= St4 ;
					end

			St4 :	begin
					id_register2 <= wr_data_from_flash[7:0] ;
					id_register3 <= wr_data_from_flash[15:8] ;

					rg_read_ID_state <= St5 ;
					end

			St5 :	begin
					rg_onfi_re_l  <= 1 ; //
					rg_onfi_we_l  <= 1 ; //
					rg_onfi_ale   <= 0 ; // idle
					rg_onfi_cle   <= 0 ; //
					rg_onfi_ce_l  <= 0 ; //
					
					rg_interrupt <= 1 ;

					rg_read_ID_state <= St6 ;
					end

			St6 :	rg_read_ID_state <= St6 ; // rg_interrupt will be back to '0' coz of rl_disable_interrupt					
		
			endcase
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		RESET
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		'hFF : case( rg_reset_state ) matches // reset
			
			St1 : 	begin
					rg_onfi_re_l  <= 1 ; // 
					rg_onfi_we_l  <= 0 ; //
					rg_onfi_ale   <= 0 ; // command cycle
					rg_onfi_cle   <= 1 ; //
					rg_onfi_ce_l  <= 0 ; //
				
					rg_data_to_flash <= { 'd0 , command_register } ;

					rg_reset_state <= St2 ;
					end

			St2 :	if( wr_ready_busy_l == 0 )
						rg_reset_state <= St3 ;

			St3 : 	if( wr_ready_busy_l == 1 ) begin

						rg_onfi_re_l  <= 1 ; //
						rg_onfi_we_l  <= 1 ; //
						rg_onfi_ale   <= 0 ; // idle
						rg_onfi_cle   <= 0 ; //
						rg_onfi_ce_l  <= 0 ; //
						
						rg_interrupt <= 1 ;

						rg_reset_state <= St4 ;
					end

			St4 : 	rg_reset_state <= St4 ; // rg_interrupt will be back to '0' coz of rl_disable_interrupt

			endcase

		default :	$display ( " default " ) ;

	endcase

endrule
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

rule rl_portB_buffer0 ; // fired 1 cycle after 'buffer0.portB.request.put(...)' is called in "PROGRAM PAGE" state
	
	case( rg_state0 ) matches
	
		St1 :	begin
				let data <- buffer0.portB.response.get( ) ;
				rg_data_to_flash <= data[15:0] ;
				$display ( " **** NAND **** 1st 16 bit data sent from buffer0 " ) ;

				rg_state0 <= St2 ;
				end

		St2 : 	begin
				let data <- buffer0.portB.response.get( ) ;
				rg_data_to_flash <= data[31:16] ;
				$display ( " **** NAND **** 2nd 16 bit data sent from buffer0 " ) ;

				rg_state0 <= St1 ;
				end
	endcase
 
endrule

rule rl_portB_buffer1 ; // fired 1 cycle after 'buffer1.portB.request.put(...)' is called in "PROGRAM PAGE" state

	case( rg_state1 ) matches
	
		St1 :	begin
				let data <- buffer1.portB.response.get( ) ;
				rg_data_to_flash <= data[15:0] ;
				$display ( " **** NAND **** 1st 16 bit data sent from buffer1 " ) ;

				rg_state1 <= St2 ;
				end

		St2 : 	begin
				let data <- buffer1.portB.response.get( ) ;
				rg_data_to_flash <= data[31:16] ;
				$display ( " **** NAND **** 2nd 16 bit data sent from buffer1 " ) ;

				rg_state1 <= St1 ;
				end
	endcase

endrule

rule rl_disable_interrupt ( rg_interrupt == 1 ) ; // fired when 'rg_interrupt' = 1 so that it will be '0' in the next state 
	rg_interrupt <= 0 ;
endrule
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
rule rl_reading_from_nand_chip ;
	let data <- nand_chip.portA.response.get() ;
$display( " DATA RETRIEVED FROM NAND CHIP = %d " , data ) ;
	if ( rg_buffer_write_address == 'h3FF ) begin
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
// portB is used for Nand flash

//portA
rule rl_reading_data_from_buf0_portA ;
	let data0 <- buffer0.portA.response.get() ;
	rg_data_to_nvm <= data0 ;
endrule

rule rl_reading_data_from_buf1_portA ;
	let data1 <- buffer1.portA.response.get() ;
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
*/

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		TEST CASES - UNCOMMENT EACH CASE WHILE VERIFYING
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
Reg#(Bit#(32)) clock <- mkReg(0) ;

rule rl_cycle ;
	clock <= clock + 1 ;
	$display ( " ************************************************************************************************** clock = %d ", clock ) ;
endrule

//////////////////////////////////////// write - test commands /////////////////////////////////
rule rl_opcode1 ( clock == 'd2 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'h80 } ;

endrule

rule rl_addr1 ( clock == 'd4 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF4 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_addr2 ( clock == 'd5 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF5 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_addr3 ( clock == 'd6 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF6 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_data1 ( clock == 'd7 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 0 ;
wr_data_from_nvm <= { clock } ;

endrule

rule rl_data2 ( clock == 'd8 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 1 ;
wr_data_from_nvm <= { clock } ;

endrule

rule rl_data3 ( clock == 'd9 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 2 ;
wr_data_from_nvm <= { clock } ;

endrule

rule rl_data4 ( clock == 'd10 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 3 ;
wr_data_from_nvm <= { clock } ;

endrule

rule rl_data5 ( clock == 'd11 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 4 ;
wr_data_from_nvm <= { clock } ;

endrule

rule rl_opcode2 ( clock == 'd12 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'h10 } ;

endrule
*/
//////////////////////////////////////// read - test commands /////////////////////////////////
/*

rule rl_opcode1 ( clock == 'd2 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'h00 } ;

endrule

rule rl_addr1 ( clock == 'd4 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF4 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_addr2 ( clock == 'd5 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF5 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_addr3 ( clock == 'd6 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF6 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_opcode2 ( clock == 'd7 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'h30 } ;

endrule

//////////////////////////////// block erase - test commands ////////////////////////////////

rule rl_opcode1 ( clock == 'd2 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'h60 } ;

endrule

rule rl_addr1 ( clock == 'd4 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF4 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_addr2 ( clock == 'd5 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF5 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_addr3 ( clock == 'd6 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF6 ;
wr_data_from_nvm <= { 'd0 } ;

endrule

rule rl_opcode2 ( clock == 'd7 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'hD0 } ;

endrule

/////////////////////////////////////////////////////////////////////////////////////////////
*/
/*
/////////////////////////////////// read ID - test commands /////////////////////////////////

rule rl_opcode1 ( clock == 'd2 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'h90 } ;

endrule

rule rl_read_id_flash1 ( clock == 'd50 ) ; // choose clock accordingly

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF0 ;

endrule

rule rl_read_id_flash2 ( clock == 'd51 ) ; // choose clock accordingly - next cycle to the cycle chosen above

$display( " NAND Flash Device ID : %d ", rg_data_to_nvm ) ;

endrule

rule rl_read_id_flash3 ( clock == 'd52 ) ; // choose clock accordingly - next cycle to the cycle chosen above

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF1 ;

endrule

rule rl_read_id_flash4 ( clock == 'd53 ) ; // choose clock accordingly - next cycle to the cycle chosen above

$display( " NAND Flash Device ID : %d ", rg_data_to_nvm ) ;

endrule

rule rl_read_id_flash5 ( clock == 'd54 ) ; // choose clock accordingly - next cycle to the cycle chosen above

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF2 ;

endrule

rule rl_read_id_flash6 ( clock == 'd55 ) ; // choose clock accordingly - next cycle to the cycle chosen above

$display( " NAND Flash Device ID : %d ", rg_data_to_nvm ) ;

endrule

rule rl_read_id_flash7 ( clock == 'd56 ) ; // choose clock accordingly - next cycle to the cycle chosen above

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF3 ;

endrule

rule rl_read_id_flash8 ( clock == 'd57 ) ; // choose clock accordingly - next cycle to the cycle chosen above

$display( " NAND Flash Device ID : %d ", rg_data_to_nvm ) ;

endrule

/////////////////////////////////////////////////////////////////////////////////////////////
*/
/*
///////////////////////////////// read status - test commands ///////////////////////////////

rule rl_opcode1 ( clock == 'd2 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'h70 } ;

endrule

rule rl_read_status_flash1 ( clock == 'd50 ) ; // choose clock accordingly

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFF9 ;

endrule

rule rl_read_status_flash2 ( clock == 'd51 ) ; // choose clock accordingly - next cycle to the cycle chosen above

$display( " Status of NAND Flash Device : %d ", rg_data_to_nvm ) ; // check datasheet for what each 'status register' value refers to.. 

endrule

/////////////////////////////////////////////////////////////////////////////////////////////
*/
/*
/////////////////////////////////// reset - test commands ///////////////////////////////////

rule rl_opcode1 ( clock == 'd2 ) ;

wr_nand_we_l <= 0 ;
wr_address_from_nvm <= 'hFFA ;
wr_data_from_nvm <= { 'hFF } ;

endrule

/////////////////////////////////////////////////////////////////////////////////////////////
*/
/*
rule rl_always ;

wr_nand_ce_l <= 0 ;
$display ( " buffer address = %d ", buffer_address ) ;

endrule

rule rl_finish ( clock == 'd6000 ) ;

$finish ;

endrule
*/
//////////////////////////////////////////////////////////////////////////////////////////////

interface nfcB_interface = fn_nfcB_interface ( wr_address_from_nvm, wr_data_from_nvm, rg_data_to_nvm, wr_nand_ce_l, wr_nand_we_l, wr_nand_oe_l, wr_nand_reset_l, rg_interrupt, rg_ready_busy_l ) ;

interface onfi_interface = fn_onfi_interface ( rg_onfi_ce_l, rg_onfi_cle, rg_onfi_ale, rg_onfi_we_l, rg_onfi_re_l, rg_onfi_wp_l, rg_data_to_flash, wr_data_from_flash, wr_ready_busy_l, wr_interrupt ) ;

endmodule

endpackage 
