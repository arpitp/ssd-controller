/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
-- Copyright (c) 2013, Indian Institute of Technology Madras (IIT Madras)
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

package InterModuleConnection  ; 

import InterfaceNvmController :: * ;
import Interface_Pcie_model :: * ;
import InterfaceNandFlashController :: * ;
import InterfaceNandFlashTarget :: * ;
import Connectable :: * ;

/////////////////////////////////////////////

/////////////////////////////////////////////
// NVM Receive Completion Data Connection
/////////////////////////////////////////////

instance Connectable #(NvmReceiveCompletionData_Interface, Pcie_Tx) ;

	module mkConnection #(NvmReceiveCompletionData_Interface nvm_receive , Pcie_Tx pcie_transmit ) ( Empty ) ;

		 (* no_implicit_conditions, fire_when_enabled *)

		rule rl_completion_data ;
			nvm_receive._completionDataIn (pcie_transmit.completionDataOut_ ) ;
		endrule 

		rule rl_completion_data_valid ;
			nvm_receive._completionDataValid (pcie_transmit.completionDataValid) ;
		endrule

		rule rl_completion_last_dword ;
			nvm_receive._completionLastDWord (pcie_transmit.completionLastDWord  ) ;
		endrule

		rule rl_tag ;
			nvm_receive._tag (pcie_transmit.tag_  ) ;
		endrule

		rule rl_pcie_busy ;
			nvm_receive._pcie_busy_m ( pcie_transmit.pcie_busy_) ;
		endrule

		rule rl_read_grant ;
			nvm_receive._read_request_granted_m ( pcie_transmit.read_request_granted ) ;
		endrule

		rule rl_write_grant ;
			nvm_receive._write_request_granted_m ( pcie_transmit.write_request_granted ) ;
		endrule


	endmodule

endinstance
//////////////////////////////////////////////

//////////////////////////////////////////////
// NVM Transmit to Pcie Interface 
//////////////////////////////////////////////

instance Connectable #(NvmTransmitToPCIe_Interface,Pcie_Rx) ;

	module mkConnection #(NvmTransmitToPCIe_Interface nvm_transmit ,Pcie_Rx pcie_receive ) (Empty)  ;

		 (* no_implicit_conditions, fire_when_enabled *)

		rule rl_data_in ;
			pcie_receive._data_from_device_m (nvm_transmit.data_to_pcie ) ;
		endrule

		rule rl_address_in ;
			pcie_receive._address_from_device_m (nvm_transmit.address_to_pcie ) ;
		endrule	

		rule rl_requested_tag ;
			pcie_receive._requested_tag_m (nvm_transmit.requested_tag ) ;
		endrule

		rule rl_completion_tlp ;
			pcie_receive._send_completion_tlp_m (nvm_transmit.send_completion_tlp ) ;
		endrule

		rule rl_write_tlp ;
			pcie_receive._send_write_tlp_m (nvm_transmit.send_write_tlp ) ;
		endrule

		rule rl_read_tlp ;
			pcie_receive._send_read_tlp_m (nvm_transmit.send_read_tlp ) ;
		endrule

		rule rl_valid_data ;
			nvm_transmit._send_valid_data_m (pcie_receive.send_valid_data_ ) ;
		endrule

		rule rl_payload_length ;
			pcie_receive._payload_length_m (nvm_transmit.payload_length ) ;
		endrule

		rule rl_64bit ;
			pcie_receive._64b_address_m (nvm_transmit.b64_address ) ;
		endrule

		rule rl_data_valid ; 
			pcie_receive._data_valid_m (nvm_transmit.data_valid ) ;
		endrule

		rule rl_nvm_wait ; 
			pcie_receive._nvm_wait_m (nvm_transmit.nvm_wait_ ) ;
		endrule

		rule rl_wait ;
			nvm_transmit._wait_m (pcie_receive.wait_ ) ;
		endrule


	endmodule

endinstance

//////////////////////////////////////////////////////////////////////////////////////////////
// NVM to NAND Flash Interface Connection 
//////////////////////////////////////////////////////////////////////////////////////////////

instance Connectable #(NandFlashController_Interface , NandFlashInterface ) ;
	module mkConnection #(NandFlashController_Interface nfcA_interface , NandFlashInterface nfcB_interface ) (Empty) ;
		 (* no_implicit_conditions, fire_when_enabled *)

		rule rl_address ;
			nfcB_interface._address_from_nvm_m ( nfcA_interface.address_to_nand_ ) ;
		endrule

		rule rl_data_to_nand ;
			nfcB_interface._data_from_nvm_m ( nfcA_interface.data_to_nand_ ) ;
		endrule

		rule rl_data_from_nand ;
			nfcA_interface._data_from_nand_m ( nfcB_interface.data_to_nvm_ ) ;
		endrule

		rule rl_nand_chip_enable ;
			nfcB_interface._nand_ce_l_m ( nfcA_interface.nand_chip_enable_) ;
		endrule

		rule rl_nand_write_enable ;
			nfcB_interface._nand_we_l_m( nfcA_interface.nand_write_enable_) ;
		endrule

		rule rl_nand_output_enable ;
			nfcB_interface._nand_oe_l_m	( nfcA_interface.nand_output_enable_) ;
		endrule

		rule rl_nand_reset ;
			nfcB_interface._nand_reset_l_m ( nfcA_interface.nand_reset_ ) ;
		endrule

		rule rl_interrupt ;
			nfcA_interface._interrupt_m ( nfcB_interface.interrupt_ ) ;
		endrule

		rule rl_ready ;
			nfcA_interface._ready_busy_m (nfcB_interface.ready_busy_l_) ;
		endrule 

	endmodule
	
endinstance

////



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

