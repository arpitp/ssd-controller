package InterModuleConnection ;

import Connectable :: * ;

import InterfaceNandFlashController :: * ;
import InterfaceNandFlashTarget :: * ;

instance Connectable #( ONFiInterface , TargetInterface ) ;

	module mkConnection #( ONFiInterface onfi_ifc , TargetInterface target_ifc ) ( Empty ) ;

		 (* no_implicit_conditions, fire_when_enabled *)

		rule rl_data_from_flash ;
			onfi_ifc._data_from_flash_m (target_ifc.data_to_nfc_) ;
		endrule 

		rule rl_interrupt ;
			onfi_ifc._interrupt_m (target_ifc.t_interrupt_) ;
		endrule

		rule rl_ready_busy ;
			onfi_ifc._ready_busy_l_m (target_ifc.t_ready_busy_l_) ;
		endrule

		rule rl_data_from_nfc ;
			target_ifc._data_from_nfc_m (onfi_ifc.data_to_flash_) ;
		endrule

		rule rl_onfi_ce ;
			target_ifc._onfi_ce_l_m (onfi_ifc.onfi_ce_l_) ;
		endrule

		rule rl_onfi_we ;
			target_ifc._onfi_we_l_m (onfi_ifc.onfi_we_l_) ;
		endrule

		rule rl_onfi_re ;
			target_ifc._onfi_re_l_m (onfi_ifc.onfi_re_l_) ;
		endrule

		rule rl_onfi_wp ;
			target_ifc._onfi_wp_l_m (onfi_ifc.onfi_wp_l_) ;
		endrule

		rule rl_onfi_cle ;
			target_ifc._onfi_cle_m (onfi_ifc.onfi_cle_) ;
		endrule

		rule rl_onfi_ale ;
			target_ifc._onfi_ale_m (onfi_ifc.onfi_ale_) ;
		endrule

	endmodule

endinstance

endpackage
