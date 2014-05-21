package TestBenchNFC ;

// import NvmController :: * ; // UNCOMMENT WHILE CONNECTING NVME
import NandFlashTarget :: * ;
import NandFlashController :: * ;

import InterModuleConnection :: * ;
import Connectable :: * ;

	module mkTestBenchNFC ( Empty ) ;

	// Ifc_Controller nvm_controller <- mkNvmController ;
	Target_Interface nand_flash_target <- mkNandFlashTarget ;
	NFC_Interface nand_flash_controller <- mkNandFlashController ;

	// Nfc to Nand Flash Target 
	mkConnection ( nand_flash_controller.onfi_interface , nand_flash_target.target_interface ) ;

	//************************** UNCOMMENT WHILE CONNECTING NVME ***************************//
	// Nvm to Nand Flash
	// mkConnection ( nvm_controller.nfc_interface , nand_flash_controller.nfcB_interface ) ;
	
	endmodule

endpackage

