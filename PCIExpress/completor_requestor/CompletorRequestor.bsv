//
//
//

package CompletorRequestor ;

import Vector :: * ;
import FIFO :: * ;
import FIFOF :: * ;
import InterfaceCompletorRequestor :: * ;


interface Ifc_CompletorRequestor ;

	interface Transmit_Interface transmit_ifc ;
//	interface Controller_Reg_File_Read_Interface crfr_ifc ;
	interface Completor_Interface_sideB cisB_ifc ;
	interface Config_Interface_Part config_part_ifc ;  // this part of the ifc is used to get the Requester(or completer) ID for outgoing trsctn
//	interface NvmCommandCompletionSideB_Interface nvmCommandCompletionSideB_Interface ;

	interface ReadWriteCompletion_Interface readWriteCompletion_ifc ;

endinterface

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

		Bit#(16) completer_ID ;
		Bit#(3) status ;
		bit bcm ;
		Bit#(12) byte_count ;

		} Second_DWord_Completion_Header deriving(Bits , Eq ) ;

typedef struct {
		
		Bit#(16) requestor_ID ;
		Bit#(8) tag ;
		Bit#(4) lastBE ;
		Bit#(4) firstBE ;

		} Second_DWord_ReadWrite_Header deriving(Bits , Eq ) ;

typedef struct {

		Bit#(16) requester_ID ; 
		Bit#(8) tag ;
		bit reserved ;
		Bit#(7) lower_address ;		

		} Third_DWord_Header deriving(Bits , Eq ) ;

/////////////////////////////////////////////////////////////////////
(*synthesize*)
(*always_ready*)
(*always_enabled*)
module mkCompletorRequestor (Ifc_CompletorRequestor) ;

// Regs and Wires related to Config( part) interface
// Wire 

	Wire#(Bit#(8)) wr_cfg_bus_number <- mkDWire(0) ;
	Wire#(Bit#(5)) wr_cfg_device_number <- mkDWire(0) ;
	Wire#(Bit#(3)) wr_cfg_function_number <- mkDWire(0) ;

// Regs and Wires related to Reg File Interface
// Wires

	Wire#(bit) wr_read_data <- mkDWire(0) ;
	Wire#(Bit#(64)) wr_data_in <- mkDWire(0) ;

// Registers and Wires related to transmit interface
// Regs

	Reg#(bit) rg_tx_tlast_ <- mkReg(0) ; 
	Reg#(Bit#(32)) rg_tx_tdata_ <- mkReg(0) ;
	Reg#(bit)rg_tx_tvalid_ <- mkReg(0) ;  							
	Reg#(Bit#(4)) rg_tx_tuser <- mkReg(0) ;
	Reg#(Bit#(4)) rg_tx_tkeep_ <- mkReg(0) ;
	Reg#(bit) rg_tx_cfg_gnt_ <- mkReg(1) ;

// Wires
                 							
	Wire#(Bit#(6)) wr_tx_buf_av <- mkDWire(0) ;
	Wire#(bit) wr_tx_terr_drop <- mkDWire(0) ;
	Wire#(bit) wr_tx_cfg_req <- mkDWire(0) ; // by default the priority is given to configuration tlp's .. ref: page 63 endpoint guide
	Wire#(bit) wr_tx_tready <- mkDWire(0) ;  

// Regs and Wires related to Compltor interface
// Wires
	
	Wire#(Bit#(10)) 	 wr_length <- mkDWire(0) ;
	Wire#(Bit#(16))	 wr_requestor_id  <- mkDWire(0) ;
	Wire#(Bit#(8))	 wr_tag  <- mkDWire(0) ;
	Wire#(bit)	 	 wr_completor  <- mkDWire(0) ;
	Wire#(Bit#(3))	 wr_status  <- mkDWire(0) ;
	Wire#(Bit#(7))	 wr_lower_address  <- mkDWire(0) ;
	Reg#(bit) rg_completor_ready <- mkReg(1) ; // By default its ready .. ************* Note ***************

// Internal Registers and Wires for Counting and keeping track ... 
// Regs

	Reg#(Bit#(4)) rg_header_count  <- mkReg(0) ; 

////////////////////////////////////////////////////////////////
// Wires related to Read Write Completion Interface
///////////////////////////////////////////////////////////////

Wire#(Bit#(32)) wr_data_in_temp <- mkDWire(0) ;
Wire#(Bit#(64)) wr_address_in <- mkDWire(0) ;
Wire#(Bit#(2)) wr_requested_tag <- mkDWire(0) ;
Wire#(bit)  wr_send_completion_tlp  <- mkDWire(0) ;
Wire#(bit)  wr_send_write_tlp  <- mkDWire(0) ;
Wire#(bit)  wr_send_read_tlp  <- mkDWire(0) ;
Wire#(bit)  wr_send_valid_data  <- mkDWire(0) ;
Wire#(bit)  wr_64b_address <- mkDWire(1) ; // by default its 1  ..
Wire#(Bit#(10)) wr_payload_length <- mkDWire(0) ;

/////////////////////////////////////////////////////////////
// Internal regs and Wires
/////////////////////////////////////////////////////////////

	Vector #(4,Reg#(Bit#(32))) rg_data_buffer <- replicateM( mkReg(0)) ;
	Reg#(Bit#(64)) rg_address <- mkReg(0) ;
	Reg#(Bit#(10)) rg_payload_length <- mkReg(0) ;
	Reg#(Bool) rg_start_write_tlp <- mkReg(False) ;
	Reg#(Bool) rg_make_tlp <- mkReg(False) ;
	Reg#(Bit#(10)) rg_data_count <- mkReg(0) ;
	Reg#(Bool) rg_continue_data_tlp <- mkReg(False) ;
	Reg#(Bool) rg_make_data_tlp <- mkReg(False) ;
	Reg#(Bool) rg_make_completion_tlp <- mkReg(False) ;
	Reg#(Bool) rg_make_64bWrite_tlp <- mkReg(False) ;
	Reg#(Bool) rg_make_32bWrite_tlp <- mkReg(False) ;
	Reg#(Bool) rg_make_64bRead_tlp <- mkReg(False) ;
	Reg#(Bool) rg_make_32bRead_tlp <- mkReg(False) ;
	Reg#(Bool) busy  <- mkReg(False) ;
	Wire#(bit) wr_wait <- mkDWire(0) ;
	Wire#(bit) wr_data_valid <- mkDWire(0) ;
	Reg#(bit) rg_data_valid <- mkReg(0) ;
////////////////////////////////////////////////////////

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

	Reg#(Second_DWord_Completion_Header) sdh_completion <- mkReg(	Second_DWord_Completion_Header{				completer_ID : 0,
																												status : 0,
																												bcm : 0,
																												byte_count : 0 
																												} ) ;

	Reg#(Second_DWord_ReadWrite_Header) sdh_readWrite <- mkReg(	Second_DWord_ReadWrite_Header{				    requestor_ID : 0,
																												tag : 0,
																												lastBE : 0,
																												firstBE : 0 
																												} ) ;

	Reg#(Third_DWord_Header) tdh <- mkReg(	Third_DWord_Header{ 		
																				requester_ID  : 0,  
																				tag : 0 , 
																				reserved  : 0 , 
																				lower_address : 0   
																				} ) ;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Rule to form the headers for Sending Completion Packets 
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
rule rl_CompletionHeaders ( (wr_send_completion_tlp  == 1 || wr_completor == 1) && !busy ) ;  

			
			fdh <= 	First_DWord_Header{ 		
										fmt : 		2'h2,      
										header_type :5'h0a , // the format field together with the type field says its a completion tlp
										tc : 		3'h0 , 
										td : 		0 , 	  
										ep : 		0 ,	  
										attr : 		0 , 
									 // this signal will be raised for one clock cycle
										length : 	wr_payload_length ,   
										reserved :  0  
										}  ;

			sdh_completion  <=	Second_DWord_Completion_Header{
										completer_ID : {wr_cfg_bus_number, wr_cfg_device_number, wr_cfg_function_number}, // this number is 																								grabbed from configuration requests
										status : wr_status,
										bcm : 0,
										byte_count : 0 
										}  ;
		if(wr_completor == 1 ) 
			tdh <=	Third_DWord_Header{ 		
										requester_ID  : wr_requestor_id,  
										tag : wr_tag , 
										reserved  : 0 , 
										lower_address : wr_lower_address   
										}  ;

		if(wr_send_completion_tlp  == 1 )	begin
			busy <= True ;
			rg_make_tlp <= True ;
		end
//	rg_make_completion_tlp <= True ;
//	rg_header_count <= 4'd0 ;
	
endrule

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Rules to form headers for Writing Data .... In our case it would be to write Completions into the Completion Queues
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	rule rl_WriteHeaders ((wr_send_write_tlp == 1 ) && !busy) ;

	$display(" Write Header Made " ) ;
			fdh <= 	First_DWord_Header{ 		
										fmt : 		2'h2,      
										header_type :5'h00 , // the format field together with the type field says its a Write tlp
										tc : 		3'h0 , 
										td : 		0 , 	  
										ep : 		0 ,	  
										attr : 		0 , 
										length : 	wr_payload_length ,   
										reserved :  0  
										}  ;

			sdh_readWrite  <=	Second_DWord_ReadWrite_Header{
										requestor_ID :0 ,
										tag : 0,
										lastBE : 0,
										firstBE : 0 
										}  ;

	rg_make_tlp <= True ;
	busy <= True ;

	endrule

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Rules to form headers for Reading Data .... In our case it would be to get the Commands and Data 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	rule rl_ReadHeaders (wr_send_read_tlp == 1 && !busy ) ;

	$display(" Read Header Made " ) ;
			fdh <= 	First_DWord_Header{ 		
										fmt : 		2'h0,      
										header_type :5'h00 , // the format field together with the type field says its a Read tlp
										tc : 		3'h0 , 
										td : 		0 , 	  
										ep : 		0 ,	  
										attr : 		0 , 
										length : 	wr_payload_length ,   
										reserved :  0  
										}  ;

			sdh_readWrite  <=	Second_DWord_ReadWrite_Header{
										requestor_ID : {wr_cfg_bus_number, wr_cfg_device_number, wr_cfg_function_number} ,
										tag : {6'd0,wr_requested_tag},
										lastBE : 0,
										firstBE : 0 
										}  ;

	rg_make_tlp <= True ;
	busy <= True ;
	endrule


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Rule to make TLP out of the header information		
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

rule rl_make_tlp (rg_make_tlp ) ; // *********form the TLPs when the data is Ready***** not implemented this way 

	if(wr_tx_tready == 1) begin // if the core accepts the start of a TLP by asserting tready, it is guaranteed to accept the complete TLP with size up to the value contained in the Max_Payload_Size field of the PCI Express Device Capability Register ( offset 04H).

		if(rg_header_count == 4'd0 ) begin  
			rg_tx_tdata_ <= {1'b0, fdh.fmt , fdh.header_type , 1'd0 , fdh.tc , 4'd0 , fdh.td , fdh.ep ,fdh.attr, 2'd0 ,fdh.length} ;
			rg_header_count <= 4'd1 ;	
			rg_tx_tvalid_ <= 1 ;	
		end

		if(rg_header_count == 4'd1) begin
			if(wr_send_completion_tlp == 1) begin
				rg_tx_tdata_ <= {sdh_completion.completer_ID , sdh_completion.status , sdh_completion.bcm , sdh_completion.byte_count} ;
				rg_tx_tvalid_ <= 1 ;
			end
	
			else if(wr_send_write_tlp == 1 || wr_send_read_tlp == 1) begin
				rg_tx_tdata_ <= {sdh_readWrite.requestor_ID , sdh_readWrite.tag , sdh_readWrite.lastBE , sdh_readWrite.firstBE} ;
				rg_tx_tvalid_ <= 1 ;
			end
			rg_header_count <= 4'd2 ;
		end

		if(rg_header_count == 4'd2) begin
			if(wr_send_completion_tlp == 1) begin
				rg_tx_tdata_ <= {tdh.requester_ID , tdh.tag , 1'd0 , tdh.lower_address} ;
				rg_header_count <= 0 ;
				wr_send_valid_data <= 1 ;									// in the next ccycle valid data shud be ready 
//				rg_make_completion_tlp <= False ;
				rg_make_data_tlp <= True ;
				rg_make_tlp <= False ;
				rg_tx_tvalid_ <= 1 ; 
			end
			else if( wr_send_read_tlp == 1 && wr_64b_address == 0) begin 
				rg_tx_tdata_ <= wr_address_in[31:0]; 							// 32bit address  
				rg_header_count <= 0 ;
//				rg_make_32bRead_tlp <= False ;
				rg_make_tlp <= False ;
				rg_tx_tvalid_ <= 1 ;
				rg_tx_tlast_ <= 1 ;
				busy <= False ;
			end
			else if (  wr_send_write_tlp == 1 && wr_64b_address == 0 ) begin
				rg_tx_tdata_ <= wr_address_in[31:0]; 							// 32bit address  
				rg_header_count <= 0 ;
				rg_make_tlp <= False ;
				wr_send_valid_data <= 1 ;									// in the next ccycle valid data shud be ready 
				rg_make_data_tlp <= True ;
//				rg_make_32bWrite_tlp <= False ;
			end
			else if( (wr_send_write_tlp == 1 || wr_send_read_tlp == 1) && wr_64b_address == 1) begin
				rg_tx_tdata_ <= wr_address_in[31:0]; 							// 64 bit address  
				rg_header_count <= 'd3 ;
			end
//			rg_data_count <= 0 ; // reinitialise the count for transmitting
		end

		if(rg_header_count == 4'd3) begin

			if (  wr_send_write_tlp == 1 && wr_64b_address == 1) begin
				rg_tx_tdata_ <= wr_address_in[63:32]; 								// 64bit address  
				rg_header_count <= 0 ;
				rg_make_tlp <= False ;
				rg_make_data_tlp <= True ;
				wr_send_valid_data <= 1 ;									// in the next ccycle valid data shud be ready 
//				rg_make_64bWrite_tlp <= False ;
			end
			else if ( wr_send_read_tlp == 1 && wr_64b_address == 1) begin
				rg_tx_tdata_ <= wr_address_in[63:32]; 								// 64bit address  
				rg_header_count <= 0 ;
				busy <= False ;
				rg_tx_tvalid_ <= 1 ;
				rg_tx_tlast_ <= 1 ;
//				rg_make_64bRead_tlp <= False ;
				rg_make_tlp <= False ;
			end
		end	
	end

endrule

rule rl_pcie_not_busy ( !busy) ;
	rg_tx_tvalid_ <= 0 ;
	rg_tx_tlast_ <= 0 ;
endrule
///////////////////////////////////////////////////////////////////////////////////////////////////
// Making Data Payload Tlp 
///////////////////////////////////////////////////////////////////////////////////////////////////

Wire#(bit) wr_txn_discontinue <- mkDWire(0) ;
Reg#(bit) rg_txn_discontinue <- mkReg(0) ;
Reg#(Bool) txn_discontinue <- mkReg(False) ;

rule rl_data_tlp (rg_make_data_tlp) ;

	if( wr_tx_tready == 1 && wr_data_valid == 1 ) begin 					 // Destination Throttling .. page 67 endpoint Block plus ...
			wr_send_valid_data <= 1 ;

			if(wr_txn_discontinue == 1) begin                     //  the last DWord 
				$display("Transaction discontinued " ) ;
				rg_tx_tdata_ <= wr_data_in_temp ;
				rg_tx_tvalid_ <= 1 ;
				rg_tx_tlast_ <= 1 ;										
				rg_data_count <= rg_data_count + 1 ;
				rg_tx_tuser <= 4'b1000 ; 
				txn_discontinue <= True ;
			end

			else if((rg_data_count == fdh.length-1) && !txn_discontinue ) begin                     //  the last DWord 
				$display("second" ) ;
				rg_tx_tdata_ <= wr_data_in_temp ;
				rg_tx_tvalid_ <= 1 ;
				rg_tx_tlast_ <= 1 ;										
				rg_data_count <= rg_data_count + 1 ;
			end

			else if(rg_data_count == fdh.length || txn_discontinue == True ) begin
				$display("End") ;
				rg_make_data_tlp <= False ;
				busy <= False ;
				rg_tx_tvalid_ <= 0 ;
				rg_tx_tlast_ <= 0 ;									
				rg_data_count <= 0 ;
				rg_tx_tuser<= 4'b1000;
				txn_discontinue <= False ;
			end
	
			else begin
				$display(" first " ) ;
				rg_tx_tdata_ <= wr_data_in_temp ;
				rg_tx_tvalid_ <= 1 ;
				rg_tx_tlast_ <= 0 ;										
				rg_data_count <= rg_data_count + 1 ;
			end	
	end

	else if( wr_tx_tready == 0 )begin
		$display("Wait .. its Destination Throttling ") ;
		wr_send_valid_data <= 1 ;
		wr_wait <= 1 ;
	end

	else if( wr_data_valid == 0 ) begin
		$display("Wait .. its Source Throttling ") ;	
		wr_send_valid_data <= 1 ;	
		rg_tx_tvalid_ <= 0 ;
	end

endrule 
/*
////////////////////////////////////////////////////////////
// Rules for Testing 
////////////////////////////////////////////////////////////

Reg#(Bit#(32)) rg_clock <- mkReg(0) ;
Reg#(Bit#(10)) rg_count <- mkReg(0) ;
Reg#(Bit#(64)) rg_read_adr <- mkReg(64'h0000000500000009) ;
Reg#(Bit#(32)) rg_data_in_temp <- mkReg(0) ;
Reg#(bit) rg_tx_tready <- mkReg(0) ;

rule rl_clock ;
	rg_clock <= rg_clock + 1 ;
endrule 

rule rl_assignments ;
	rg_tx_tready <= wr_tx_tready ;
	rg_txn_discontinue <= wr_txn_discontinue ;
endrule


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
// the following set of rules ( Rules 1 to 3 ) are defined to simulate a test case to send Completion TLPs ///
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

//////////
//Rule 1//
//////////
rule rl_from_Request_handler ( rg_clock == 3 ) ;
	 wr_requestor_id <= 'd2;
	 wr_tag <= 'd4; 
	 wr_completor <= 1 ; 
	 wr_status <= 'd6;
	 wr_lower_address <= 'd7;
endrule

//////////
//Rule 2//
//////////
rule rl_send_completion_rquest_frm_nvm (rg_clock >= 4 && rg_clock < 10) ;
	wr_send_completion_tlp <= 1 ;
	wr_payload_length <= 'd2 ;
endrule

//////////
//Rule 3//
//////////
rule rl_send_data  ;
		if ( wr_send_valid_data == 1 ) begin
			if(rg_count == 0 ) begin
				if ( wr_wait == 0 ) begin
					rg_count <=  1 ;
					rg_data_in_temp <= rg_read_adr[31:0] ;
					rg_data_valid <= 1;
				end
			end

			if(rg_count == 1) begin	
				if ( wr_wait == 0 )	begin 
					rg_count <= 2 ;
					rg_data_in_temp <= rg_read_adr[63:32] ;
					rg_data_valid <= 1;
				end
			end	
		end	
endrule

rule rl_always ;
	wr_data_in_temp <= rg_data_in_temp;	
	wr_tx_tready <= 1 ;
	wr_data_valid <= rg_data_valid ;
endrule

*/
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The following Set of rules ( rule 4 to 9) are used to simulate test cases for Transmitting Write TLPs under various scinarios //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*
//////////
//Rule 4//
//////////
rule rl_from_Nvme_to_write ( rg_clock >= 5 && rg_clock < 14 ) ;
	wr_send_write_tlp <= 1 ;
endrule
*/
/*
////////// /////////////////////////////////////////////////////////////////////////
//Rule 5// // uncomment this to simulate Source Initiated transaction Discontinue //
////////// /////////////////////////////////////////////////////////////////////////
rule rl_disc ( rg_clock == 'd15) ;
	wr_txn_discontinue <= 1 ;
endrule
*/
/*
//////////
//Rule 6//
//////////
rule rl_send_data  ;
		if ( wr_send_valid_data == 1 ) begin
			if ( wr_wait == 0 ) begin
				rg_data_in_temp <= rg_clock ;
				rg_data_valid <= 1;
			end
		end
endrule

//////////
//Rule 7//
//////////
rule rl_always ;
	wr_data_in_temp <= rg_data_in_temp;	
	wr_tx_tready <= 1 ;
endrule
*/
//////////  //////////////////////////////////////////////////////////////////////////////////////
//Rule 8//  // Uncomment the code in the rule condition to simulate source initiated Throttling //
//////////  //////////////////////////////////////////////////////////////////////////////////////
//rule rl_valid_data /*(!( rg_clock == 10 || rg_clock == 13 ))*/ ;
//	wr_data_valid <= rg_data_valid ;
//endrule

//////////  ///////////////////////////////////////////////////////////////////////////////////////////
//Rule 9//  // Uncomment the code in the rule condition to simulate Destination initiated Throttling //
//////////  ///////////////////////////////////////////////////////////////////////////////////////////
//rule rl_tx_ready /*(!( rg_clock == 10 || rg_clock == 13 ))*/;
//	wr_payload_length <= 'd10 ;
//	wr_address_in <= 64'h0000000500000007 ;
//	wr_tx_tready <= 1 ; // Always Ready 
//endrule

/*
rule rl_display ;
	$display ( " clock = %d , header count = %d , data  count = %d , data to pcie = %h data valid = %b , data last = %b , COUNT = %d" , rg_clock, rg_header_count , rg_data_count , rg_tx_tdata_ , rg_tx_tvalid_ , rg_tx_tlast_ , rg_count) ;
endrule
*/

interface config_part_ifc = fn_config_part_interface (wr_cfg_bus_number, wr_cfg_device_number, wr_cfg_function_number) ; 

//interface crfr_ifc = fn_controller_reg_file_read_interface ( wr_read_data , wr_data_in , wr_32b_data) ;

interface transmit_ifc = fn_transmit_interface ( rg_tx_tlast_ ,  rg_tx_tdata_ , rg_tx_tvalid_ , rg_tx_tuser ,rg_tx_tkeep_ , wr_tx_tready,         wr_tx_buf_av , wr_tx_terr_drop , wr_tx_cfg_req, rg_tx_cfg_gnt_ ) ;

interface cisB_ifc = fn_completor_interface ( wr_length , wr_requestor_id , wr_tag , wr_completor , wr_status , wr_lower_address , rg_completor_ready ) ;

interface readWriteCompletion_ifc = fn_readWriteCompletion_ifc ( wr_data_in_temp , wr_address_in , wr_requested_tag , wr_send_completion_tlp , wr_send_write_tlp , wr_send_read_tlp , wr_send_valid_data , wr_payload_length , wr_64b_address , wr_data_valid , wr_wait ) ; 


endmodule
endpackage
