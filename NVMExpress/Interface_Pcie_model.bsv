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

package Interface_Pcie_model ;

	interface Pcie_Rx ;
		
		method Action 	_data_from_device_m (Bit#(32) _data_from_device ) ;

		method Action  _address_from_device_m ( Bit#(64) _address_from_device)  ;

		method Action _requested_tag_m (Bit#(2) _requested_tag) ;

		method Action _send_completion_tlp_m ( bit _send_completion_tlp) ;

		method Action _send_write_tlp_m  (bit _send_write_tlp) ;

		method Action _send_read_tlp_m ( bit _send_read_tlp) ;

		method bit send_valid_data_ () ;

		method Action _payload_length_m ( Bit#(10) _payload_length )  ;

		method Action _64b_address_m ( bit _64b_address) ;

		method Action _data_valid_m ( bit _data_valid ) ;

		method Action _nvm_wait_m ( bit _nvm_wait ) ;

		method bit wait_ () ;
	


	endinterface

	function Pcie_Rx fn_pcie_receive (Wire#(Bit#(32)) wr_data_from_device ,Wire#(Bit#(64)) wr_address_from_device ,Wire#(Bit#(2)) wr_requested_tag , Wire#(bit) wr_send_completion_tlp ,Wire#(bit) wr_send_write_tlp ,Wire#(bit) wr_send_read_tlp ,Wire#(bit) wr_send_valid_data ,Wire#(Bit#(10)) wr_payload_length , Wire#(bit) wr_64b_address, Wire#(bit) wr_data_valid ,Wire#(bit) wr_nvm_wait, Wire#(bit) wr_wait) ;

		return ( interface Pcie_Rx ;
					method Action 	_data_from_device_m ( _data_from_device ) ;
						wr_data_from_device <= _data_from_device ;
					endmethod
					method Action  _address_from_device_m (_address_from_device)  ;
						wr_address_from_device <= _address_from_device ;
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

					method Action _nvm_wait_m ( bit _nvm_wait ) ;
						wr_nvm_wait <= _nvm_wait ;
					endmethod
					method bit wait_ () ;
						return wr_wait ;
					endmethod

			
				endinterface ) ;

	endfunction


		interface Pcie_Tx ;
			method Bit#(32) completionDataOut_ () ;
			method bit completionDataValid () ;
			method bit completionLastDWord () ;
			method Bit#(2) tag_ () ;
			method Bool pcie_busy_ () ;
			method Bool read_request_granted () ;
			method Bool write_request_granted () ;
		endinterface 

 	function Pcie_Tx fn_pcie_transmit ( Reg#(Bit#(32)) rg_completionDataOut , Reg#(bit) rg_completionDataValid ,Reg#(bit) rg_completionLastDWord, Reg#(Bit#(2)) rg_tag ,Reg#(Bool)  rg_pcie_busy , Reg#(Bool) rg_read_request_granted ,Reg#(Bool) rg_write_request_granted ) ;

		return ( 		interface Pcie_Tx ;
							method Bit#(32) completionDataOut_ () ;
								return rg_completionDataOut ;
							endmethod
							method bit completionDataValid () ;
								return rg_completionDataValid ;
							endmethod
							method bit completionLastDWord () ;
								return rg_completionLastDWord ;
							endmethod
							method Bit#(2) tag_ () ;
								return rg_tag ;
							endmethod
							method Bool pcie_busy_ () ;
								return rg_pcie_busy ;
							endmethod
							method Bool read_request_granted () ;
								return rg_read_request_granted  ;
							endmethod
							method Bool write_request_granted () ;
								return rg_write_request_granted  ;
							endmethod

						endinterface ) ;
		
	endfunction

endpackage
