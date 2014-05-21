package InterfaceRequestHandler ;

		interface Controller_Register_File_Interface ;

			method Bit#(32) address_rf_ () ;
			method Bit#(64) data_out_ () ;
			method Bit#(4) byte_enable_ () ;
			method bit read_ () ;
			method bit write_ () ;
//			method Action _data_in_m (Bit#(32) _data_in) ;

		endinterface

	function Controller_Register_File_Interface fn_config_register_file_interface ( Reg#(Bit#(32)) rg_address_rf_,
   				Reg#(Bit#(64)) rg_data_out_ , Reg#(Bit#(4)) rg_byte_enable_ ,Reg#(bit) rg_write_ , Reg#(bit) rg_read_ ) ;
		
		return (	interface Controller_Register_File_Interface ;

						method Bit#(32) address_rf_ () ;
							return rg_address_rf_ ;
						endmethod
						method Bit#(64) data_out_ () ;
							return rg_data_out_ ;
						endmethod
						method Bit#(4) byte_enable_ () ;
							return rg_byte_enable_ ;
						endmethod
						method bit read_ () ;
							return rg_read_ ;
						endmethod
						method bit write_ () ;
							return rg_write_ ;
						endmethod
					endinterface  ) ;
	

	endfunction

		interface Completor_Interface_sideA  ;
	
			method Bit#(10) length_ () ;
			method Bit#(16) requestor_id_ ();
			method Bit#(8) tag_ () ;
			method bit completor_ ();
			method Bit#(3) status_ ();
			method Bit#(7) lower_address_ ();
			method Action _completor_ready_m (bit _completor_ready ) ;
	
		endinterface

	function Completor_Interface_sideA fn_completor_interface ( Reg#(Bit#(10)) rg_length_ , Reg#(Bit#(16))  rg_requestor_id_ ,Reg#(Bit#(8))  rg_tag_ , Reg#(bit) rg_completor_ ,Reg#(Bit#(3))  rg_status_ , Reg#(Bit#(7)) rg_lower_address_ , Wire#(bit) wr_completor_ready) ;

		return ( interface Completor_Interface_sideA  ;
	
					method Bit#(10) length_ () ;
						return rg_length_ ;
					endmethod
					method Bit#(16) requestor_id_ ();
						return rg_requestor_id_ ;
					endmethod
					method Bit#(8) tag_ () ;
						return rg_tag_ ;
					endmethod
					method bit completor_ ();
						return rg_completor_ ;
					endmethod
					method Bit#(3) status_ ();
						return rg_status_ ;
					endmethod
					method Bit#(7) lower_address_ ();
						return rg_lower_address_ ;
					endmethod
					method Action _completor_ready_m (bit _completor_ready ) ;
						wr_completor_ready <= _completor_ready ;
					endmethod
					
	
				endinterface ) ;


	endfunction

		interface TransmitCompletionData_Interface ;
			method Bit#(32) data_out_ () ;
			method bit data_valid () ;
			method bit last_DWord () ;
			method Bit#(2) tag_to_device_ () ;
		endinterface 

 	function TransmitCompletionData_Interface fn_transmitCompletionData_interface( Reg#(Bit#(32)) rg_data_out , Reg#(bit) rg_data_valid ,Reg#(bit) rg_last_DWord, Reg#(Bit#(2)) rg_tag_to_device ) ;

		return ( 		interface TransmitCompletionData_Interface ;
							method Bit#(32) data_out_ () ;
								return rg_data_out ;
							endmethod
							method bit data_valid () ;
								return rg_data_valid ;
							endmethod
							method bit last_DWord () ;
								return rg_last_DWord ;
							endmethod
							method Bit#(2) tag_to_device_ () ;
								return rg_tag_to_device ;
							endmethod
						endinterface ) ;
		
	endfunction
endpackage
