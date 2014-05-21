package InterfaceNandFlashTarget ;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		ONFi - TARGET INTERFACE
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
interface TargetInterface   ;
 
	method Bit#(16) data_to_nfc_ ( ) ;
	method Action _data_from_nfc_m ( Bit#(16) _data_from_nfc ) ;
	method Action _onfi_ce_l_m ( bit _onfi_ce_l ) ;
	method Action _onfi_we_l_m ( bit _onfi_we_l ) ;
	method Action _onfi_re_l_m ( bit _onfi_re_l ) ;
	method Action _onfi_wp_l_m ( bit _onfi_wp_l ) ;
	method Action _onfi_cle_m ( bit _onfi_cle ) ;
	method Action _onfi_ale_m ( bit _onfi_ale ) ;
	method bit t_interrupt_ ;
	method bit t_ready_busy_l_ ;

endinterface

function TargetInterface fn_target_interface ( Wire#(bit) wr_onfi_ce_l, Wire#(bit) wr_onfi_cle, Wire#(bit) wr_onfi_ale, Wire#(bit) wr_onfi_we_l, Wire#(bit) wr_onfi_re_l, Wire#(bit) wr_onfi_wp_l, Reg#(Bit#(16)) rg_data_to_nfc, Wire#(Bit#(16)) wr_data_from_nfc, Reg#(bit) rg_t_ready_busy_l, Reg#(bit) rg_t_interrupt ) ;

	return ( interface TargetInterface ;

				method Action _data_from_nfc_m ( _data_from_nfc ) ;
					wr_data_from_nfc <= _data_from_nfc ;
				endmethod

				method Bit#(16) data_to_nfc_ ( ) ;
					return rg_data_to_nfc ;
				endmethod

				method Action _onfi_ce_l_m ( _onfi_ce_l ) ;
					wr_onfi_ce_l <= _onfi_ce_l ;
				endmethod

				method Action _onfi_we_l_m ( _onfi_we_l ) ;
					wr_onfi_we_l <= _onfi_we_l ;
				endmethod

				method Action _onfi_re_l_m ( _onfi_re_l ) ;
					wr_onfi_re_l <= _onfi_re_l ;
				endmethod

				method Action _onfi_wp_l_m ( _onfi_wp_l ) ;
					wr_onfi_wp_l <= _onfi_wp_l ;
				endmethod

				method Action _onfi_cle_m ( _onfi_cle ) ;
					wr_onfi_cle <= _onfi_cle ;
				endmethod

				method Action _onfi_ale_m ( _onfi_ale ) ;
					wr_onfi_ale <= _onfi_ale ;
				endmethod

				method bit t_interrupt_ ( ) ;
					return rg_t_interrupt ;
				endmethod

				method bit t_ready_busy_l_ ( ) ;
					return rg_t_ready_busy_l ;
				endmethod

				endinterface ) ;

endfunction

endpackage

