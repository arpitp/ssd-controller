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

package TestbenchNvm ;

//import NvmController :: * ;
import NvmController_imp :: * ;
import PCIe_model :: * ;

import NandFlashTarget :: * ;
import NandFlashController :: * ;

import InterModuleConnection :: * ;
import Connectable :: * ;

	module mkTestbenchNvm ( Empty ) ;

	Ifc_Controller nvm_controller <- mkNvmController_imp ;
	PCIe_model_interface pcie_model <- mkPCIe_model ;
	Target_Interface nand_flash_target0 <- mkNandFlashTarget ;
	Target_Interface nand_flash_target1 <- mkNandFlashTarget ;
	NFC_Interface nand_flash_controller0 <- mkNandFlashController ;
	NFC_Interface nand_flash_controller1 <- mkNandFlashController ;	

	// Nfc to Nand Flash Target 
	mkConnection ( nand_flash_controller0.onfi_interface , nand_flash_target0.target_interface ) ;

	mkConnection ( nand_flash_controller1.onfi_interface , nand_flash_target1.target_interface ) ;

	// Nvm Recieve Completion Data 
	mkConnection ( nvm_controller.nvmReceiveCompletionData_interface , pcie_model.pcie_transmit) ;

	// Nvm Transmit to Pcie 
	mkConnection ( nvm_controller.nvmTransmitToPCIe_interface , pcie_model.pcie_receive) ;

	// Nvm to Nfc
	mkConnection ( nvm_controller.nfc_interface0 , nand_flash_controller0.nfcB_interface ) ;
		
	mkConnection ( nvm_controller.nfc_interface1 , nand_flash_controller1.nfcB_interface ) ;

	endmodule

endpackage
