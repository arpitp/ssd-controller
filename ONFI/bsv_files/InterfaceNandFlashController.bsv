package InterfaceNandFlashController ;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		NVM - NAND FLASH CONTROLLER INTERFACE
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
interface NandFlashInterface   ;

	method Action _address_from_nvm_m (Bit#(12) _address_from_nvm) ; 
	method Action _data_from_nvm_m ( Bit#(32) _data_from_nvm ) ;
	method Bit#(32) data_to_nvm_ ( ) ;
	method Action _nand_ce_l_m ( bit _nand_ce_l ) ;
	method Action _nand_we_l_m ( bit _nand_we_l ) ;
	method Action _nand_oe_l_m ( bit _nand_oe_l ) ;
	method Action _nand_reset_l_m ( bit _nand_reset_l ) ;
	method bit interrupt_ () ;
	method bit ready_busy_l_ () ;

endinterface

function NandFlashInterface fn_nfcB_interface ( Wire#(Bit#(12)) wr_address_from_nvm, Wire#(Bit#(32)) wr_data_from_nvm, Reg#(Bit#(32)) rg_data_to_nvm, Wire#(bit) wr_nand_ce_l, Wire#(bit) wr_nand_we_l, Wire#(bit) wr_nand_oe_l, Wire#(bit) wr_nand_reset_l, Reg#(bit) rg_interrupt, Reg#(bit) rg_ready_busy_l ) ;


	return ( interface NandFlashInterface   ;

				method Action _address_from_nvm_m ( _address_from_nvm ) ; 
					wr_address_from_nvm <= _address_from_nvm ;
				endmethod

				method Action _data_from_nvm_m (  _data_from_nvm ) ;
					wr_data_from_nvm <= _data_from_nvm ;
				endmethod

				method Bit#(32) data_to_nvm_ ( ) ;
					return rg_data_to_nvm ;
				endmethod 

				method Action _nand_ce_l_m (  _nand_ce_l ) ;
					wr_nand_ce_l <= _nand_ce_l ; 
				endmethod

				method Action _nand_we_l_m (  _nand_we_l ) ;
					wr_nand_we_l <= _nand_we_l ; 
				endmethod

				method Action _nand_oe_l_m (  _nand_oe_l ) ;
					wr_nand_oe_l <= _nand_oe_l ; 
				endmethod

				method Action _nand_reset_l_m ( _nand_reset_l ) ;
					wr_nand_reset_l <= _nand_reset_l ; 
				endmethod

				method bit interrupt_ () ;
					return rg_interrupt ;
				endmethod

				method bit ready_busy_l_ () ;
					return rg_ready_busy_l ;
				endmethod

			endinterface ) ;

endfunction

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//		NAND FLASH CONTROLLER - ONFi INTERFACE
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
interface ONFiInterface   ;
 
	method Action _data_from_flash_m ( Bit#(16) _data_from_flash ) ;
	method Bit#(16) data_to_flash_ ( ) ;
	method bit onfi_ce_l_ () ;
	method bit onfi_we_l_ () ;
	method bit onfi_re_l_ () ;
	method bit onfi_wp_l_ () ;
	method bit onfi_cle_ () ;
	method bit onfi_ale_ () ;
	method Action _interrupt_m ( bit _interrupt ) ;
	method Action _ready_busy_l_m ( bit _ready_busy_l ) ;

endinterface

function ONFiInterface fn_onfi_interface ( Reg#(bit) rg_onfi_ce_l, Reg#(bit) rg_onfi_cle, Reg#(bit) rg_onfi_ale, Reg#(bit) rg_onfi_we_l, Reg#(bit) rg_onfi_re_l, Reg#(bit) rg_onfi_wp_l, Reg#(Bit#(16)) rg_data_to_flash, Wire#(Bit#(16)) wr_data_from_flash, Wire#(bit) wr_ready_busy_l, Wire#(bit) wr_interrupt ) ;

	return ( interface ONFiInterface ;

				method Action _data_from_flash_m ( _data_from_flash ) ;
					wr_data_from_flash <= _data_from_flash ;
				endmethod

				method Bit#(16) data_to_flash_ ( ) ;
					return rg_data_to_flash ;
				endmethod

				method bit onfi_ce_l_ () ;
					return rg_onfi_ce_l ;
				endmethod

				method bit onfi_we_l_ () ;
					return rg_onfi_we_l ;
				endmethod

				method bit onfi_re_l_ () ;
					return rg_onfi_re_l ;
				endmethod

				method bit onfi_wp_l_ () ;
					return rg_onfi_wp_l ;
				endmethod

				method bit onfi_cle_ () ;
					return rg_onfi_cle ;
				endmethod

				method bit onfi_ale_ () ;
					return rg_onfi_ale ;
				endmethod

				method Action _interrupt_m ( _interrupt ) ;
					wr_interrupt <= _interrupt ;
				endmethod

				method Action _ready_busy_l_m ( _ready_busy_l ) ;
					wr_ready_busy_l <= _ready_busy_l ;
				endmethod
		
				endinterface ) ;

endfunction

endpackage
