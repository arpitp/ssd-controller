// Request Handler and Completor 


package RequestHandler ;

import Vector :: * ;
import FIFO :: * ;
import FIFOF :: * ;
import Pcie_Controller :: * ;
import InterfaceRequestHandler :: * ;

//////////////////////////////////////////////////////////////////////
// typedefs ...............

typedef struct {
			
		Bit#(2) fmt ; // format 
		Bit#(5) header_type ; // type of the header
		Bit#(3) tc ; // traffic class 
		bit td ; 	 // tlp digest 
		bit ep ;	 // poisoned tlp 
		Bit#(2) attr ; // attribute 
		Bit#(10) length ;  // payload  length 
		Bit#(8) reserved ; // reserved bits 

		} First_DWord_Header deriving(Bits , Eq ) ;

typedef struct {

		Bit#(16) requester_ID ; 
		Bit#(8) tag ;
		Bit#(4) last_dw_be ;
		Bit#(4) first_dw_be ;		

		} Second_DWord_ReadWrite_Header deriving(Bits , Eq ) ;

typedef struct {

		Bit#(16) completer_ID ; 
		Bit#(3) status ;
		Bit#(1) bcm ;
		Bit#(12) byteCount ;		

		} Second_DWord_Completion_Header deriving(Bits , Eq ) ;

typedef struct {

		Bit#(16) requester_ID ; 
		Bit#(8) tag ;
		Bit#(1) reserved ;
		Bit#(7) lowerAddress ;		

		} Third_DWord_Completion_Header deriving(Bits , Eq ) ;

/////////////////////////////////////////////////////////////////////

interface Ifc_RequestHandler ;

		interface Receive_Interface receive_ifc ;
		interface Controller_Register_File_Interface crf_ifc ;
		interface Completor_Interface_sideA cisA_ifc ;
		interface TransmitCompletionData_Interface tcdA_ifc ;

endinterface 



module mkRequestHandler ( Ifc_RequestHandler/* Empty*/ ) ;


// Registers and Wires related to Receive interface 
// Regs 
 	Reg#(bit) rg_rx_tready_ <- mkReg(0) ;
	Reg#(bit) rg_rx_np_ok_ <- mkReg(0) ;

// Wires
	Wire#(bit) wr_rx_tlast <- mkDWire(0) ;
	Wire#(bit) wr_rx_tvalid <- mkDWire(0) ;
	Wire#(Bit#(32)) wr_rx_tdata <- mkDWire(0) ;
	Wire#(Bit#(10)) wr_rx_tuser <- mkDWire(0) ;
	Wire#(Bit#(4)) wr_rx_tkeep <- mkDWire(0) ;

//////////////////////////////////////////////////////////////////////////////////////
// Regs Related to Completion Data Transmit (to device) Interface
//////////////////////////////////////////////////////////////////////////////////////

	Reg#(Bit#(32)) rg_data_out <- mkReg(0) ;
	Reg#(bit) rg_data_valid <- mkReg(0) ;
	Reg#(bit) rg_last_DWord <- mkReg(0) ;

////////////////////////////////////////////////////////

// Registers and Wires related to Controller register file interface
// Regs

	Reg#(Bit#(32)) rg_address_rf_ <- mkReg(0) ; // 32 bit or 64 bit ??
	Reg#(Bit#(64)) rg_data_out_ <- mkReg(0) ; 
	Reg#(Bit#(4)) rg_byte_enable_ <- mkReg(0) ;
	Reg#(bit) rg_write_ <- mkReg(0) ;
	Reg#(bit) rg_read_ <- mkReg(0) ;

// Wires
	Wire#(Bit#(32)) wr_data_in <- mkDWire(0) ;

// Registers and Wires related to Completor interface 
// Regs 

	Reg#(Bit#(10)) rg_length_ <- mkReg(0) ;
	Reg#(Bit#(16)) rg_requestor_id_<- mkReg(0) ;
	Reg#(Bit#(8)) rg_tag_ <- mkReg(0) ;
	Reg#(bit) rg_completor_ <- mkReg(0) ;
	Reg#(Bit#(3)) rg_status_ <- mkReg(0) ;
	Reg#(Bit#(7)) rg_lower_address_ <- mkReg(0) ;

	Wire#(bit) wr_completor_ready <- mkDWire(0) ;

// Regs and Wires NVM -to- PCIe
// Wires
	Wire#(bit) wr_nvm_tready <- mkDWire(0) ; // driven by a method from nvm , whenever nvm is ready .

// Regs
	Reg#(Bit#(32)) rg_header_count <- mkReg(0) ; // counter register to keep track of the number of header DWords received 
	Reg#(Bit#(2)) rg_data_count <- mkReg(0) ;

// Internal Regs and Wires .....
	Wire#(bit) wr_write <- mkDWire (0) ;
	Wire#(bit) wr_read <- mkDWire (0) ;
	Reg#(Bool) rg_start_write <- mkReg(False) ;
	Reg#(Bool) rg_ready_for_TLP <- mkReg (True) ;
// Regs 
	Reg#(Bit#(64)) rg_addr_64b <- mkReg(0) ; // 64 bit address for the register file
	Reg#(Bit#(32)) rg_addr_32b <- mkReg(0) ; // 32 bit address for the register file
	Reg#(Bit#(64)) rg_data_buffer <- mkReg(0) ;
//	Reg#(Bit#(32)) rg_data_buffer2 <- mkReg(0) ;
	Reg#(Bool) rg_first_data <- mkReg(False) ;
	Reg#(Bool) rg_second_data <- mkReg(False) ; 
	Reg#(Bool) disable_write <- mkReg(False) ;
	Reg#(Bool) rg_start_read <- mkReg(False) ;
	Reg#(Bit#(10)) rg_read_data_count <- mkReg(0) ;
	Reg#(Bool) rg_completor_notified <- mkReg(True) ;
	Reg#(Bool) rg_notification_done <- mkReg(False) ;

	Reg#(Bool) rg_data_payload <- mkReg(False) ; // presence or absence of data pay load 
												 // presence of payload indicates a Write to Memory 
												 // absence of payload indicates a read from memory 

//////////////////////////////////////////////////////////////////////////////////
// Registers related to format of the TLP
// ANd last transaction Indications ..
/////////////////////////////////////////////////////////////////////////////////

	Reg#(Bool) memoryRead_3DW <- mkReg(False) ;
	Reg#(Bool) memoryRead_4DW <- mkReg(False) ;
	Reg#(Bool) memoryWrite_3DW <- mkReg(False) ;
	Reg#(Bool) memoryWrite_4DW <- mkReg(False) ;
	Reg#(Bool) completionWith_NoData <- mkReg(False) ;
	Reg#(Bool) completionWith_Data <- mkReg(False) ;
	Reg#(Bool) last_transaction <- mkReg(False) ;
	Reg#(Bool) rg_receiveCompletionData <- mkReg(False) ;
	Reg#(Bit#(8)) rg_free_tag <- mkReg(0) ; 
///////////////////////////////////////////////////////////////////////////////////
	
	Reg#(First_DWord_Header) fdh <- mkReg(	First_DWord_Header{ 		
																				fmt : 0,  
																				header_type : 0 , 
																				tc : 0 , 
																				td : 0 , 	  
																				ep : 0 ,	  
																				attr : 0 , 
																				length : 0 ,   
																				reserved : 0  
																				} ) ;

	Reg#(Second_DWord_ReadWrite_Header) sdh_ReadWrite <- mkReg(	Second_DWord_ReadWrite_Header{ 		
																				requester_ID  : 0,  
																				tag : 0 , 
																				last_dw_be  : 0 , 
																				first_dw_be : 0   
																				} ) ;

	Reg#(Second_DWord_Completion_Header) sdh_Completion <- mkReg(	Second_DWord_Completion_Header{ 		
																				completer_ID  : 0,  
																				status : 0 , 
																				bcm  : 0 , 
																				byteCount : 0   
																				} ) ;

	Reg#(Third_DWord_Completion_Header) tdh_Completion <- mkReg(	Third_DWord_Completion_Header{ 		
																				requester_ID  : 0,  
																				tag : 0 , 
																				reserved : 0 , 
																				lowerAddress : 0   
																				} ) ;
	
	rule rl_first_dword_header ( wr_nvm_tready == 1'b1 && wr_rx_tvalid == 1'b1 && rg_header_count == 32'd0 && rg_ready_for_TLP == True ) ;

		rg_ready_for_TLP <= False ;
		fdh <= 	First_DWord_Header{ 		
											fmt : 		wr_rx_tdata [30 : 29 ],  
											header_type :wr_rx_tdata [28 : 24 ] , 
											tc : 		wr_rx_tdata [22 : 20 ] , 
											td : 		wr_rx_tdata [15 ] , 	  
											ep : 		wr_rx_tdata [14 ] ,	  
											attr : 		wr_rx_tdata [13 : 12 ] , 
											length : 	wr_rx_tdata [9 : 0 ] ,   
											reserved : 	{wr_rx_tdata [31 ],wr_rx_tdata [23 ],wr_rx_tdata [19 : 16 ], wr_rx_tdata [11 : 10 ]}  
											}  ;
		
		rg_header_count <= 32'd1 ; // received the first header

		if(wr_rx_tdata[28:24] == 'h00) begin
			if(wr_rx_tdata[30:29] == 'b00) 
				memoryRead_3DW <= True ;
			else if(wr_rx_tdata[30:29] == 'b01) 
				memoryRead_4DW <= True ;
			else if(wr_rx_tdata[30:29] == 'b10) 
				memoryWrite_3DW <= True ;
			else if(wr_rx_tdata[30:29] == 'b11)  
				memoryWrite_4DW <= True ;
		end

		else if(wr_rx_tdata[28:24] == 'h0a) begin
			last_transaction <= False ;
			if(wr_rx_tdata[30:29] == 'b00)
				completionWith_NoData <= True ;
			else if(wr_rx_tdata[30:29] == 'b10) begin
				$display(" Completion With Data header 1  " ) ;				
				completionWith_Data <= True ;
			end
		end

		$display (" first DW = %b " , wr_rx_tdata ) ;
	endrule 

	rule rl_second_dword_header_read_write ( wr_nvm_tready == 1'b1 && wr_rx_tvalid == 1'b1 && rg_header_count == 32'd1 ) ;

		if(memoryRead_3DW || memoryRead_4DW || memoryWrite_3DW || memoryWrite_4DW )
		sdh_ReadWrite <= Second_DWord_ReadWrite_Header{ 		
											requester_ID  : 	wr_rx_tdata [31 : 16 ],  
											tag :				wr_rx_tdata [15 : 8 ] , 
											last_dw_be  : 		wr_rx_tdata [7 : 4 ] , 
											first_dw_be : 		wr_rx_tdata [3 : 0 ]    
											} ;

		if(completionWith_NoData || completionWith_Data) begin
		sdh_Completion <= Second_DWord_Completion_Header{ 		
											completer_ID	: 	wr_rx_tdata [31 : 16 ],  
											status 			:	wr_rx_tdata [15 : 13 ] , 
											bcm  			: 	wr_rx_tdata [12 ] , 
											byteCount 		: 	wr_rx_tdata [11 : 0 ]    
											} ;
		$display(" Completion With Data header 2  " ) ;	
		end

		rg_header_count <= 32'd2 ; // received the second header

		$display (" second DW = %b , format = %b" , wr_rx_tdata , fdh.fmt ) ;

	endrule

	rule rl_32bit_fmt ( (memoryRead_3DW || memoryWrite_3DW) && (wr_nvm_tready == 1'b1 && wr_rx_tvalid == 1'b1) && rg_header_count == 'd2) ;
		
		rg_header_count <= 32'd0 ;
		rg_addr_32b <= wr_rx_tdata ;

		if( memoryWrite_3DW ) begin
			rg_data_payload <= True ; // payload present
			memoryWrite_3DW <= False ;
			$display(" its Write Op " ) ; 
		end

		if( memoryRead_3DW ) begin // Start the read operation 
			memoryRead_3DW <= False ;
			rg_start_read <= True ;
			rg_read_data_count <= 0 ;
		end
			
	endrule


	rule rl_64bit_fmt ( (memoryRead_4DW || memoryWrite_4DW ) && (wr_nvm_tready == 1'b1 && wr_rx_tvalid == 1'b1 && 
									(rg_header_count == 32'd2 || rg_header_count == 32'd3  ) ) ) ;

		if ( rg_header_count == 32'd2 ) begin
			rg_addr_64b[63:32] <= wr_rx_tdata ;
			rg_header_count <= 32'd3 ;
		end

		else begin 
			rg_addr_64b[31:0] <= wr_rx_tdata ;
			rg_header_count <= 32'd0 ;
			if ( memoryWrite_4DW ) begin
				memoryWrite_4DW <= False ;
				rg_data_payload <= True ; // payload present 
			end

			if ( memoryRead_4DW ) begin
				memoryRead_3DW <= False ;
				/* else its a read operation ...*/
				/* save the request ID , tag .. to use it for completion */

				

				/* control for Read from meomory can be added here */
				/* start reading from memory .. once it is finished reading .. go to completion */
			end
		end
	endrule 

Reg#(Bit#(2)) rg_tag_to_device <- mkReg(0) ;

	rule rl_completion_header3( (completionWith_NoData || completionWith_Data)  && (wr_nvm_tready == 1'b1 && wr_rx_tvalid == 1'b1) && 																												rg_header_count == 'd2 ) ;

		tdh_Completion <= 	Third_DWord_Completion_Header{ 		
															requester_ID  : wr_rx_tdata [31:16],  
															tag : wr_rx_tdata [15:8] , 
															reserved : wr_rx_tdata [7] , 
															lowerAddress : wr_rx_tdata [6:0]  
															}  ;
		completionWith_NoData <= False ;
		rg_header_count <= 'd0 ;
		if (completionWith_Data)  begin
		$display(" Completion With Data header 3  " ) ;	
			let dWords_Sent = sdh_Completion.byteCount >> 2 ;
			if(fdh.length == dWords_Sent[9:0] ) 
				last_transaction <= True ; 			// Indicates that the Completer is performing its last transaction 			

			rg_receiveCompletionData <= True ;	
			completionWith_Data <= False ;	
		end
	
	endrule 

	rule rl_receiveCompletionData  ;
		if ( rg_receiveCompletionData ) begin
			$display(" Tag = %d " , tdh_Completion.tag[1:0] ) ;
			rg_tag_to_device <= tdh_Completion.tag[1:0] ;
			rg_data_out <= wr_rx_tdata ;
			rg_data_valid <= wr_rx_tvalid ;
			if ( wr_rx_tlast == 1'b1 ) begin
				rg_ready_for_TLP <= True ;
				rg_receiveCompletionData <= False ;
			end			
		end
		else begin
			rg_data_valid <= 0 ;
			rg_tag_to_device <= 0 ;
		end	
	endrule

	rule rl_transactionCompletionIndication ( last_transaction ) ;
		rg_last_DWord <= wr_rx_tlast ;						// Indicates the Last DWord in the Last Transaction ... 
		rg_free_tag <= tdh_Completion.tag ;
	endrule 

	// the length of the memory write is at max 2
	// rule to receive the data payload and trigger the write to memory location
	
	rule rl_data_in (wr_nvm_tready == 1'b1 && wr_rx_tvalid == 1'b1 && rg_data_payload == True) ;

		if(rg_data_count == 2'd0) begin
			rg_data_buffer[63:0] <= {32'd0,wr_rx_tdata}  ;
			if (wr_rx_tlast == 1'b1) begin 
				rg_ready_for_TLP <= True ;
				rg_data_payload <= False ;
				rg_start_write <= True ;  // Start the Write Operation 
			end
			else begin
				rg_data_count <= 2'd1 ;
			end
		end

		else if (rg_data_count == 2'd1) begin
			rg_data_buffer[63:32] <= wr_rx_tdata  ;

			if (wr_rx_tlast == 1'b1) begin 
				$display(" End" ) ;
				rg_ready_for_TLP <= True ;
				rg_data_payload <= False ;
				rg_data_count <= 2'd0 ;  // memory write can not be more than 64 bits at a time .. there is no need . 
				rg_start_write <= True ;
			end
		end

	endrule

	rule rl_control_reg_write (rg_start_write == True) ;
		rg_write_<= 1'b1 ; 
		rg_address_rf_ <= rg_addr_32b ;
		rg_data_out_ <= rg_data_buffer ;
		$display ( " data written = %b , Address = %b ",rg_data_buffer, rg_addr_32b ) ;
//		rg_byte_enable_ <= sdh.first_dw_be ;
		rg_start_write <= False ;
	endrule

	rule rl_disable_read_write;
		if(rg_start_write == False)rg_write_ <= 'b0;
		if(rg_start_read == False)rg_read_ <= 'b0 ;
	endrule

//	rule rl_display ;
//		$display ( " Write = %b , Read = %b  ",rg_write_ , rg_read_ ) ;
///	endrule

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	rule rl_control_reg_read (rg_start_read == True ) ;
//		rg_byte_enable_ <= 4'b1111 ;
		rg_read_ <= 1 ;
		rg_start_read <= False ;
		rg_address_rf_ <= rg_addr_32b ;
		rg_completor_notified <= False ;
	endrule

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	rule rl_completor_notification ( rg_completor_notified == False) ;
		if (wr_completor_ready == 1) begin
			rg_completor_notified <= True ;
			rg_completor_ <= 1;
			rg_length_ <= fdh.length;
			rg_requestor_id_ <= sdh_ReadWrite.requester_ID;
			rg_tag_ <= sdh_ReadWrite.tag;
	//		rg_status_ <= ; error reporting in the incoming TLP 
			rg_lower_address_ <= rg_addr_32b[6 : 0] ; 
			rg_notification_done <= True ;
//			$display("Completor is notified about read operation ... \n Address = %b " , rg_address_rf_  ) ;
		end
		
	endrule

	rule rl_notified (rg_notification_done == True) ;
		rg_completor_ <= 0 ;
		rg_notification_done <= False ;	
	endrule

// test section 
/*
	Reg#(Bit#(32)) rg_count <- mkReg(0) ;

	rule rl_count ;
		rg_count <= rg_count + 1 ;
	endrule 

	rule rl_init ;
		wr_nvm_tready <= 'b1 ;
//		wr_completor_ready <= 1; 
	endrule
*/
/*
	rule rl_write_transaction0(rg_count == 5) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b00000000_00000000_00000000_00000000 ;
	endrule
	
	rule rl_write_transaction1 ( rg_count == 6) ;
		wr_rx_tvalid <= 1 ;
	endrule
	
	rule rl_write_transaction2 ( rg_count == 7) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b10000000_00000000_00000000_10101010 ;
	endrule

	rule rl_write_transaction3 ( rg_count == 8) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b10000000_00000000_00000000_11111111;
	endrule	

	rule rl_write_transaction4 ( rg_count == 9) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b01000000_00000000_11100000_10101010 ;
		wr_rx_tlast <= 1 ;
	endrule
*/
/*
	rule rl_completion_txn0_part1 ( rg_count == 5) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b01001010_00000000_00000000_00000101 ; // Completion With Data .. length of payload = 5 Dwords  .
	endrule

	rule rl_completion_txn1_part1 ( rg_count == 6) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b00000000_00000000_00000000_00101000 ; // Completion With Data .. byteCount Remaining = 40 Bytes ( 10 Dwords are remaining , but only 5 are sent ) .
	endrule

	rule rl_completion_txn2_part1 ( rg_count == 7) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b00000000_00000000_00000010_00000000 ; // Completion With Data .. tag = 2 .
	endrule

	rule rl_completion_txn3_part1 ( rg_count > 7  && rg_count <= 12) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= rg_count ; // Completion With Data .. length of payload = 10 .
	endrule

	rule rl_last_part1 ( rg_count == 12 ) ;
		wr_rx_tlast <= 1 ;
	endrule

	rule rl_completion_txn0_part2 ( rg_count == 20) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b01001010_00000000_00000000_00000101 ; // Completion With Data .. length of payload = 5 Dwords .
	endrule

	rule rl_completion_txn1_part2 ( rg_count == 21) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b00000000_00000000_00000000_00010100 ; // Completion With Data .. byteCount = 20  ( 5 DWords remaining .. and 5 are sent . its last transaction )
	endrule

	rule rl_completion_txn2_part2 ( rg_count == 22) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= 'b00000000_00000000_00000010_00000000 ; // Completion With Data .. tag = 2 .
	endrule

	rule rl_completion_txn3_part2 ( rg_count > 22  && rg_count <= 27) ;
		wr_rx_tvalid <= 1 ;
		wr_rx_tdata <= rg_count ; // Completion With Data .. length of payload = 10 .
	endrule

	rule rl_last_part2 ( rg_count == 27 ) ;
		wr_rx_tlast <= 1 ;
	endrule

	rule rl_display ;
		$display(" completion data = %d , dataValid = %b , dataLast = %d tag_to_device = %d " ,rg_data_out , rg_data_valid ,rg_last_DWord ,rg_tag_to_device) ;
	endrule
	
	Reg#(bit) rg_rx_tvalid <- mkReg(0);
	Reg#(bit) rg_rx_tlast <- mkReg(0) ;
	Reg#(Bit#(32)) rg_rx_tdata <- mkReg(0) ;
	Reg#(bit) rg_device_rdy <- mkReg(0) ;


	rule rl_assignments ;
		rg_rx_tvalid <= wr_rx_tvalid ;
		rg_rx_tdata <= wr_rx_tdata ;
		rg_rx_tlast <= wr_rx_tlast ;
		rg_device_rdy <= wr_nvm_tready ;
	endrule

	rule rl_finish (rg_count == 30);
		$finish () ;
	endrule
*/
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

interface cisA_ifc = fn_completor_interface ( rg_length_ , rg_requestor_id_ , rg_tag_ , rg_completor_ , rg_status_ , rg_lower_address_, wr_completor_ready) ;

interface crf_ifc = fn_config_register_file_interface ( rg_address_rf_, rg_data_out_ ,rg_byte_enable_ ,rg_write_ ,rg_read_ ) ;

interface receive_ifc = fn_receive_interface (wr_rx_tlast ,rg_rx_tready_ ,rg_rx_np_ok_ ,wr_rx_tdata ,  wr_rx_tuser ,  wr_rx_tvalid,  wr_rx_tkeep) ;

interface tcdA_ifc = fn_transmitCompletionData_interface( rg_data_out , rg_data_valid , rg_last_DWord , rg_tag_to_device ) ;


endmodule 
endpackage 
