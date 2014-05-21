-------------------------------------------------------------------------------
-- 
-- Copyright (c) 2013-2014, Indian Institute of Technology Madras (IIT Madras)
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
-- 3. Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
-- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
-- IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
-- OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
-- OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- 
-------------------------------------------------------------------------------


The repository contains the Bluespec and Verilog RTL of 
the NVMExpress and Lightsor Controller 
developed by Computer Architecture and Systems Lab, Department of Computer Science and Engineering, 
Indian Institute of Techonology Madras.

The code was compiled using Bluespec-2013.05.beta2 version of the compiler. 

This IP has not been fully tested or integrated in an SoC but unit tests have been done
to test the function interfaces and behaviour. We will shortly check in testbenches and
describe a minimal testing process using tools like Modelsim.

The code is licensed under the 3 part BSD license.

The core SSD controller will be released in two variants, an NVMe variant which implements
the 1.1 version of the NVMExpress standard and an enhanced variant which support the newly'proposed 
Lighstor standard. The Lightsor variant can be though of as a superset of the NVMe standard with signficantly
high functionality. The command set will be compatible with the SNIA NVM API. Lightsor aslo uses RapidIO as the
interconnect instead of PCIe.

Both variants will be released as a full SoC with dual/quad core CPU for control plane functions.
Release platform will be a Xilinx FPGA platform with Xilinx PCIe and NAND modules being used. We may also
release an ONFI 4.0 version of the NAND controller at a later date (Logical only, will need to use 3rd party 
DFI 3.x compatible PHY).

Status (as of May 2014)
Basic NVMe 1.0 compatible IP with 8 channel support has been checked in. Verification is underway.
The code is being released so that other research groups can start using our work rather than wait for a
full release. Since there exists no open source SSD controller with commercial grade features, we feel 
even the code at this stage is useful.

Planned changes - NVME 1.1 feature upgrade, 16/32 channel
support, performance optimizations, ARM/RISC-V CPU addition, encryption/compression/de-dup engines.

We are also planning to move the NAND controller off the main block and define a serial protocol over 
SRIO to allow connection of multiple NAND modules to the main controller. 

G S Madhusudan

