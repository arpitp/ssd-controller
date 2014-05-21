//
// Interface definitions for the CompletorRequestor moddule

package InterfaceCompletorRequestor ;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*	interface Controller_Reg_File_Read_Interface  ;

		method Action _read_data_m ( bit _read_data ) ;
		method Action _data_in_m (Bit#(64) _data_in ) ;
		method Action _32b_data_m (bit _32b_data ) ;
	
	endinterface

	function Controller_Reg_File_Read_Interface fn_controller_reg_file_read_interface ( Wire#(bit) wr_read_data , Wire#(Bit#(64)) wr_data_in,
																							Wire#(bit) wr_32b_data) ;

		
		return ( interface Controller_Reg_File_Read_Interface  ;

					method Action _read_data_m ( _read_data ) ;
						wr_read_data <= _read_data ;
					endmethod
					method Action _data_in_m ( _data_in ) ;
						wr_data_in <= _data_in ;
					endmethod
					method Action _32b_data_m ( _32b_data ) ;
						wr_32b_data <= _32b_data ;
					endmethod
				 endinterface ) ;
	endfunction
*/
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	interface Completor_Interface_sideB ;

		method Action _length_m (Bit#(10) _length) ;
		method Action _requestor_id_m (Bit#(16) _requestor_id) ;
		method Action _tag_m (Bit#(8) _tag) ;
		method Action _completor_m (bit _completor) ; 
		method Action _status_m (Bit#(3) _status) ;
		method Action _lower_address_m (Bit#(7) _lower_address) ;
		method bit completor_ready_ ( ) ;

	endinterface

	function Completor_Interface_sideB fn_completor_interface ( Wire#(Bit#(10)) wr_length , Wire#(Bit#(16)) wr_requestor_id , 							Wire#(Bit#(8)) wr_tag , Wire#(bit) wr_completor , Wire#(Bit#(3)) wr_status , Wire#(Bit#(7)) wr_lower_address, Reg#(bit) rg_completor_ready) ;

		return ( 	interface Completor_Interface_sideB ;

						method Action _length_m (_length) ;
							wr_length <= _length ;
						endmethod 
						method Action _requestor_id_m (_requestor_id) ;
							wr_requestor_id <= _requestor_id ;
						endmethod
						method Action _tag_m (_tag) ;
							wr_tag <= _tag ;
						endmethod
						method Action _completor_m (_completor) ;
							wr_completor <= _completor ;
						endmethod 
						method Action _status_m (_status) ;
							wr_status <= _status ;
						endmethod 
						method Action _lower_address_m (_lower_address) ;
							wr_lower_address <= _lower_address ;
						endmethod 
						method bit completor_ready_ ( ) ;
							return rg_completor_ready ;
						endmethod
						
					endinterface ) ;

	endfunction 
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	interface Config_Interface_Part ;

		method Action _cfg_bus_number_m (Bit#(8) _cfg_bus_number) ;
		method Action _cfg_device_number_m (Bit#(5) _cfg_device_number) ;
		method Action _cfg_function_number_m (Bit#(3) _cfg_function_number) ;		

	endinterface

	function Config_Interface_Part fn_config_part_interface (Wire#(Bit#(8)) wr_cfg_bus_number, Wire#(Bit#(5)) wr_cfg_device_number,                 																		Wire#(Bit#(3)) wr_cfg_function_number) ;

		return (	interface Config_Interface_Part  ;

						method Action _cfg_bus_number_m (_cfg_bus_number) ;
							wr_cfg_bus_number <= _cfg_bus_number ;
						endmethod

						method Action _cfg_device_number_m (_cfg_device_number) ;
							wr_cfg_device_number <= _cfg_device_number ;
						endmethod

						method Action _cfg_function_number_m (_cfg_function_number) ;
							wr_cfg_function_number <= _cfg_function_number ;
						endmethod		

					endinterface	) ;
	

	endfunction
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	interface NvmCommandCompletionSideB_Interface 	;
		method Action _completionData_m (Bit#(128) _completionData ) ;
		method Action _completionAddress_m (Bit#(64) _completionAddress ) ;
		method Action _completionEn_m (bit _completionEn) ;			
	endinterface 

	function NvmCommandCompletionSideB_Interface fn_nvmCommandCompletionSideB_interface (Wire#(Bit#(128)) wr_completionData ,
																			Wire#(Bit#(64))wr_completionAddress ,
																	Wire#(bit) wr_completionEn);

	
		return (	interface NvmCommandCompletionSideB_Interface 	;
						method Action _completionData_m (_completionData ) ;
							wr_completionData  <= _completionData  ;
						endmethod
						method Action _completionAddress_m (_completionAddress ) ;
							wr_completionAddress  <= _completionAddress  ;
						endmethod
						method Action _completionEn_m (_completionEn) ;		
							wr_completionEn <= _completionEn ;
						endmethod	
					endinterface ) ;

	endfunction

	interface ReadWriteCompletion_Interface ;
		
		method Action 	_data_in_temp_m (Bit#(32) _data_in_temp ) ;

		method Action  _address_in_m ( Bit#(64) _address_in)  ;

		method Action _requested_tag_m (Bit#(2) _requested_tag) ;

		method Action _send_completion_tlp_m ( bit _send_completion_tlp) ;

		method Action _send_write_tlp_m  (bit _send_write_tlp) ;

		method Action _send_read_tlp_m ( bit _send_read_tlp) ;

		method bit send_valid_data_ () ;

		method Action _payload_length_m ( Bit#(10) _payload_length )  ;

		method Action _64b_address_m ( bit _64b_address) ;

		method Action _data_valid_m ( bit _data_valid ) ;

		method bit wait_ () ;

	endinterface

	function ReadWriteCompletion_Interface fn_readWriteCompletion_ifc (Wire#(Bit#(32)) wr_data_in_temp ,Wire#(Bit#(64)) wr_address_in ,Wire#(Bit#(2)) wr_requested_tag , Wire#(bit) wr_send_completion_tlp ,Wire#(bit) wr_send_write_tlp ,Wire#(bit) wr_send_read_tlp ,Wire#(bit) wr_send_valid_data ,Wire#(Bit#(10)) wr_payload_length , Wire#(bit) wr_64b_address, Wire#(bit) wr_data_valid ,Wire#(bit) wr_wait) ;

		return ( interface ReadWriteCompletion_Interface ;
					method Action 	_data_in_temp_m ( _data_in_temp ) ;
						wr_data_in_temp <= _data_in_temp ;
					endmethod
					method Action  _address_in_m (_address_in)  ;
						wr_address_in <= _address_in ;
					endmethod
					method Action _requested_tag_m (_requested_tag) ;
						wr_requested_tag <= _requested_tag ;
					endmethod
					method Action _send_completion_tlp_m (_send_completion_tlp) ;
						wr_send_completion_tlp <= _send_completion_tlp ;
					endmethod
					method Action _send_write_tlp_m  (_send_write_tlp) ;
						wr_send_write_tlp <= _send_write_tlp ;
					endmethod
					method Action _send_read_tlp_m (_send_read_tlp) ;
						wr_send_read_tlp <= _send_read_tlp ;
					endmethod
					method bit send_valid_data_ () ;
						return wr_send_valid_data ;
					endmethod
					method Action _payload_length_m ( _payload_length )  ;
						wr_payload_length <= _payload_length ;
					endmethod
					method Action _64b_address_m (_64b_address) ;
						wr_64b_address <= _64b_address ;	
					endmethod
					method Action _data_valid_m ( bit _data_valid ) ;
						wr_data_valid <= _data_valid ; 
					endmethod
					method bit wait_ () ;
						return wr_wait ;
					endmethod
			
				endinterface ) ;

	endfunction

//////////////////////////////////////////////////
// Transmit Interface
//////////////////////////////////////////////////

	interface Transmit_Interface ;
		method bit tx_tlast_ () ;
		method Bit#(32) tx_tdata_ () ;
		method bit tx_tvalid_ () ;
		method Action _tx_tready_m ( bit _tx_tready) ;
		method Bit#(4) tx_tuser_ () ;
		method Action _tx_buf_av_m ( Bit#(6) _tx_buf_av ) ;
		method Action _tx_terr_drop_m ( bit _tx_terr_drop ) ;
		method Action _tx_cfg_req_m ( bit _tx_cfg_req) ;
		method bit tx_cfg_gnt_ () ;			
		method Bit#(4) tx_tkeep_ () ;
	endinterface

	function Transmit_Interface fn_transmit_interface ( Reg#(bit) rg_tx_tlast_ , Reg#(Bit#(32)) rg_tx_tdata_ , Reg#(bit)rg_tx_tvalid_ ,  							Reg#(Bit#(4)) rg_tx_tuser_ ,Reg#(Bit#(4)) rg_tx_tkeep_ , Wire#(bit) wr_tx_tready,                                							Wire#(Bit#(6)) wr_tx_buf_av ,Wire#(bit) wr_tx_terr_drop ,Wire#(bit) wr_tx_cfg_req,Reg#(bit) rg_tx_cfg_gnt_ ) ;

		return(	interface Transmit_Interface ;
					method bit tx_tlast_ () ;
						return rg_tx_tlast_ ;
					endmethod
		
					method Bit#(32) tx_tdata_ () ;
						return rg_tx_tdata_ ;
					endmethod
					
					method bit tx_tvalid_ () ;
						return rg_tx_tvalid_ ;
					endmethod

					method Action _tx_tready_m ( bit _tx_tready) ;
						wr_tx_tready <= _tx_tready ;
					endmethod
					
					method Bit#(4) tx_tuser_ () ;
						return rg_tx_tuser_ ;
					endmethod

					method Action _tx_buf_av_m ( Bit#(6) _tx_buf_av ) ;
						wr_tx_buf_av  <= _tx_buf_av ;
					endmethod

					method Action _tx_terr_drop_m ( bit _tx_terr_drop ) ;
						wr_tx_terr_drop <= _tx_terr_drop ;
					endmethod

					method Action _tx_cfg_req_m ( bit _tx_cfg_req) ;
						wr_tx_cfg_req <= _tx_cfg_req ;
					endmethod

					method bit tx_cfg_gnt_ () ;		
						return rg_tx_cfg_gnt_ ;
					endmethod	

					method Bit#(4) tx_tkeep_ () ;
						return rg_tx_tkeep_ ;
					endmethod 
				endinterface ) ; 

	endfunction
endpackage

