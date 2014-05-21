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


package InterfaceNvmController ;

	interface ControllerConfigurationWrite ;
		
		method Action _dword_number_m ( Bit#(32) _dword_number ) ;
		method Action _byte_enable_m (  Bit#(4) _byte_enable ) ;
		method Action _regFiledata_in_m (Bit#(64) _regFiledata_in ) ;
		method Action _read_m ( bit _read ) ;
		method Action _write_m ( bit _write ) ;
		
	endinterface

	function ControllerConfigurationWrite fn_controller_registerFileWrite_interface ( Wire#(Bit#(32)) wr_dword_number , 
																						Wire#(Bit#(4)) wr_byte_enable ,    																								Wire#(Bit#(64)) wr_regFiledata_in , 
																								Wire#(bit) wr_read , Wire#(bit) wr_write ) ;

		return ( 	interface ControllerConfigurationWrite ;
		
						method Action _dword_number_m ( _dword_number ) ;
							wr_dword_number <= _dword_number ;
						endmethod
						method Action _byte_enable_m ( _byte_enable ) ;
							wr_byte_enable <= _byte_enable ;
						endmethod
						method Action _regFiledata_in_m ( _regFiledata_in ) ;
							wr_regFiledata_in <= _regFiledata_in ;
						endmethod
						method Action _read_m ( _read ) ;
							wr_read <= _read ;
						endmethod
						method Action _write_m ( _write ) ;
							wr_write <= _write ;
						endmethod
				
					endinterface ) ; 
	

	endfunction
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	interface NvmInterruptSideA_Interface ;
		method bit vector_rdy () ;
		method Bit#(5) vector_number () ;
		method Bit#(5) aggregation_threshold () ;
	endinterface 

	function NvmInterruptSideA_Interface fn_nvmInterruptSideA_ifc ( Reg#(bit) rg_vector_rdy , Reg#(Bit#(5)) rg_vector_number, 																		Reg#(Bit#(5)) rg_aggregation_threshold ) ;

		return (
				interface NvmInterruptSideA_Interface ;
					method bit vector_rdy () ;
						return rg_vector_rdy ;
					endmethod
					method Bit#(5) vector_number () ;
						return rg_vector_number ;
					endmethod
					method Bit#(5) aggregation_threshold () ;
						return rg_aggregation_threshold ;
					endmethod
				endinterface ) ;
	endfunction 
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	interface NvmReceiveCompletionData_Interface ;
		method Action _completionDataIn ( Bit#(32) completionDataIn ) ;
		method Action _completionDataValid ( bit completionDataValid ) ;
		method Action _completionLastDWord ( bit completionLastDWord ) ;
		method Action _tag ( Bit#(2) tag ) ;
		method Action _pcie_busy_m ( Bool _pcie_busy ) ;
		method Action _read_request_granted_m ( Bool _read_request_granted ) ;
		method Action _write_request_granted_m ( Bool _write_request_granted ) ;
	endinterface


	function NvmReceiveCompletionData_Interface fn_nvmReceiveCompletionData_interface (Wire#(Bit#(32))  wr_completionDataIn ,Wire#(bit) wr_completionDataValid ,Wire#(bit)  wr_completionLastDWord ,Wire#(Bit#(2))  wr_tag , Wire#(Bool) wr_pcie_busy, Wire#(Bool) wr_read_request_granted ,Wire#(Bool) wr_write_request_granted ) ;
		
		return ( 	interface NvmReceiveCompletionData_Interface ;
						method Action _completionDataIn ( completionDataIn ) ;
							wr_completionDataIn <= completionDataIn ;
						endmethod
						
						method Action _completionDataValid ( completionDataValid ) ;
							wr_completionDataValid <= completionDataValid ;
						endmethod 
						method Action _completionLastDWord ( completionLastDWord ) ;
							wr_completionLastDWord <= completionLastDWord ;
						endmethod
						method Action _tag ( tag ) ;
							wr_tag <= tag ;
						endmethod
						method Action _pcie_busy_m ( _pcie_busy ) ;
							wr_pcie_busy <= _pcie_busy ;
						endmethod
						method Action _read_request_granted_m ( _read_request_granted ) ;
							wr_read_request_granted <= _read_request_granted ;
						endmethod
						method Action _write_request_granted_m (_write_request_granted ) ;
							wr_write_request_granted <= _write_request_granted ;
						endmethod
					endinterface ) ;

	endfunction 
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	interface NvmTransmitToPCIe_Interface ;
		method Bit#(32) data_to_pcie () ;
		method Bit#(64) address_to_pcie () ;
		method Bit#(2) requested_tag () ;
		method Bit#(10) payload_length () ;
		method bit send_completion_tlp () ;
		method bit send_write_tlp () ;
		method bit send_read_tlp () ;
		method bit b64_address () ;
		method Action _send_valid_data_m ( bit _send_valid_data) ;
		method bit data_valid () ;
		method bit nvm_wait_ () ;
		method Action _wait_m (bit _wait) ;

	endinterface 

	function  NvmTransmitToPCIe_Interface  fn_nvmTransmitToPCIe_interface(	Reg#(Bit#(32)) rg_data_to_pcie ,	Reg#(Bit#(64))  rg_address_to_pcie , 	Reg#(Bit#(2)) rg_requested_tag , 	Reg#(bit) rg_send_completion_tlp ,	Reg#(bit)  rg_send_write_tlp ,	Reg#(bit)  rg_send_read_tlp , Wire#(bit) wr_send_valid_data , 	Reg#(Bit#(10)) rg_payload_length , 	Reg#(bit) rg_64b_address , Reg#(bit) rg_data_valid ,Reg#(bit) rg_nvm_wait , Wire#(bit) wr_wait  ) ;

		return ( interface NvmTransmitToPCIe_Interface ;
					method Bit#(32) data_to_pcie () ;
						return rg_data_to_pcie ;
					endmethod
					method Bit#(64) address_to_pcie () ;
						return rg_address_to_pcie ;
					endmethod
					method Bit#(2) requested_tag () ;
						return rg_requested_tag  ;
					endmethod 
					method Bit#(10) payload_length () ;
						return rg_payload_length  ;
					endmethod 
					method bit send_completion_tlp () ;
						return  rg_send_completion_tlp ;
					endmethod
					method bit send_write_tlp () ;
						return rg_send_write_tlp  ;
					endmethod
					method bit send_read_tlp () ;
						return rg_send_read_tlp ;
					endmethod
					method bit b64_address () ;
						return  rg_64b_address ;
					endmethod
					method Action _send_valid_data_m ( _send_valid_data) ;
						wr_send_valid_data <= _send_valid_data ;
					endmethod 
					method bit data_valid () ;
						return  rg_data_valid ;
					endmethod
					method bit nvm_wait_ () ;
						return rg_nvm_wait ;
					endmethod
					method Action _wait_m ( _wait) ;
						wr_wait <= _wait ;
					endmethod

				endinterface 
							) ;

	endfunction

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	interface NandFlashController_Interface ;
		method Bit#(12) address_to_nand_ () ;
		method Bit#(32) data_to_nand_ () ;
		method Action _data_from_nand_m (Bit#(32) _data_from_nand) ;
		method bit nand_chip_enable_ () ;
		method bit nand_write_enable_ () ;
		method bit nand_output_enable_ () ;
		method bit nand_reset_ () ;
		method Action _interrupt_m (bit _interrupt ) ;
		method Action _ready_busy_m (bit _ready_busy_l ) ;
	endinterface


	function  NandFlashController_Interface fn_nfc_interface0 (Reg#(Bit#(12)) rg_address_to_nand0 ,Reg#(Bit#(32)) rg_data_to_nand0 ,Wire#(Bit#(32)) wr_data_from_nand0 ,Reg#(bit) rg_nand_ce_l0 ,Reg#(bit) rg_nand_we_l0 ,Reg#(bit) rg_nand_oe_l0 ,Reg#(bit) rg_nand_reset_l0 ,Wire#(bit) wr_interrupt0 ,Wire#(bit) wr_ready_busy_l0 ) ;

		return (	interface NandFlashController_Interface ;
						method Bit#(12) address_to_nand_ () ;
							return rg_address_to_nand0 ;
						endmethod
						method Bit#(32) data_to_nand_ () ;
							return rg_data_to_nand0 ;
						endmethod
						method Action _data_from_nand_m ( _data_from_nand) ;
							wr_data_from_nand0 <= _data_from_nand ;
						endmethod
						method bit nand_chip_enable_ () ;
							return rg_nand_ce_l0 ;
						endmethod
						method bit nand_write_enable_ () ;
							return rg_nand_we_l0 ;
						endmethod
						method bit nand_output_enable_ () ;
							return rg_nand_oe_l0 ;
						endmethod
						method bit nand_reset_ () ;
							return rg_nand_reset_l0 ;
						endmethod
						method Action _interrupt_m ( _interrupt ) ;
							wr_interrupt0 <= _interrupt ;
						endmethod
						method Action _ready_busy_m ( _ready_busy_l ) ;
							wr_ready_busy_l0 <= _ready_busy_l ;
						endmethod
					endinterface ) ;

	endfunction 



	function  NandFlashController_Interface fn_nfc_interface1 (Reg#(Bit#(12)) rg_address_to_nand1 ,Reg#(Bit#(32)) rg_data_to_nand1 ,Wire#(Bit#(32)) wr_data_from_nand1 ,Reg#(bit) rg_nand_ce_l1 ,Reg#(bit) rg_nand_we_l1 ,Reg#(bit) rg_nand_oe_l1 ,Reg#(bit) rg_nand_reset_l1 ,Wire#(bit) wr_interrupt1 ,Wire#(bit) wr_ready_busy_l1 ) ;

		return (	interface NandFlashController_Interface ;
						method Bit#(12) address_to_nand_ () ;
							return rg_address_to_nand1 ;
						endmethod
						method Bit#(32) data_to_nand_ () ;
							return rg_data_to_nand1 ;
						endmethod
						method Action _data_from_nand_m ( _data_from_nand) ;
							wr_data_from_nand1 <= _data_from_nand ;
						endmethod
						method bit nand_chip_enable_ () ;
							return rg_nand_ce_l1 ;
						endmethod
						method bit nand_write_enable_ () ;
							return rg_nand_we_l1 ;
						endmethod
						method bit nand_output_enable_ () ;
							return rg_nand_oe_l1 ;
						endmethod
						method bit nand_reset_ () ;
							return rg_nand_reset_l1 ;
						endmethod
						method Action _interrupt_m ( _interrupt ) ;
							wr_interrupt1 <= _interrupt ;
						endmethod
						method Action _ready_busy_m ( _ready_busy_l ) ;
							wr_ready_busy_l1 <= _ready_busy_l ;
						endmethod
					endinterface ) ;

	endfunction 






////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/* Please Make Correction before Use ********************
	interface Nvm_PciInterconnect_Interface  ;

		method Bit#(64) address_to_pcie () ;
		method bit send_read_tlp () ;
		method Action _command_In_m ( Bit#(512) _command_In ) ;
		method Action _commandEn_m (Bool _commandEn ) ;
		 
	endinterface

	function Nvm_PciInterconnect_Interface fn_fifo_to_nvm_interconnect ( Reg#(Bit#(64)) rg_address_to_pcie ,Wire#(Bit#(512)) wr_command_In , 
																		Reg#(bit) send_read_tlp , Wire#(Bool) wr_commandEn) ;
		return
				(interface Nvm_PciInterconnect_Interface ;

					method bit  send_read_tlp () ;
						return rg_addr_en;
					endmethod

					method Action _commandEn_m ( _commandEn) ;
						wr_commandEn <= _commandEn ;
					endmethod

					method Bit#(64) address_to_pcie () ;
						return rg_address_to_pcie ;
					endmethod	
		
					method Action _data_in_m (_dat_in) ;
							wr_data_in <= _dat_in ;
					endmethod

				endinterface) ;
	endfunction
*/
 	
endpackage
