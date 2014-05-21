/*
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
-- Copyright (c) 2013,2014  Indian Institute of Technology Madras (IIT Madras)
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


package NvmPciTestConnection ;

import InterfaceNvmController :: * ;

import InterfacePciInterconnectTest :: * ;

import Connectable:: *;


instance Connectable #(PciInterconnect, Nvm_PciInterconnect_Interface);
   module mkConnection #(PciInterconnect pci,
                         Nvm_PciInterconnect_Interface  nvm)
                       (Empty);

      (* no_implicit_conditions, fire_when_enabled *)

	rule connect ;
	pci._start_read(nvm.address_out_ ) ;
	endrule

	rule rl_valid1 (nvm.addr_en_) ;
		pci._addr_valid(nvm.addr_en_) ;
	endrule

	rule rl_valid2 (pci.data_valid_) ;
		nvm._dat_en_m(pci.data_valid_) ;
	endrule

	rule connect2 ;
	nvm._data_in_m(pci.data_out_);
    endrule
      
   endmodule

endinstance

endpackage
