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

