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

////////////////////////////////////////////////////////////////////////////////
// Name of the Module 	: NVM Express Controller
// Coded by 			: M.Shanmukh Abhishek
//				: Keerthi Kiran H Pujar(Multi-Channel aspect of NVMe Controller)
// Last Modified 	 	: April 29th, 2014
//
// Module Description 	: This module contains the high levelmodules of the NVMexpress Controller.
//						Functionality at a glance
// 						1. Command Fetch
//						2. Command Arbitration
//						3. ASQ and ISQ Command Execution
//						4. Command Completion
//						5. Status update
//						6. Error Logs
//						7. NAND Flash Controller Access
// References			: NVMe 1.0c Spec , 2011
////////////////////////////////////////////////////////////////////////////////


package NvmController;

/*
* Importing Bluespec Libraries
*/

import Vector :: *;
import Arbiter :: *;
import FIFO ::*;
import FIFOF :: *;
import BRAM :: *;

/*
* Importing Supporting Pacakages
* The package "Controller_reg" has the Register file definition
* Package "InterfaceNvmController has the Interface definition for this module
*/

import Controller_reg :: *;
import InterfaceNvmController :: *;

typedef Bit#(64) Address_Out_Type;
typedef Bit#(512) SqCommandType;


/*
* The following Structure is used has been defined for use with the "Abort Command"
* The command to be aborted is uniquely identified by the Submission Q ID (sqID field) and the Command ID (cID field)
* Hence this structure is defined
*/
typedef struct {
	Bit#(16) sqID;
	Bit#(16) cID;
} CommandType deriving(Bits,Eq);


/*
* The following Structure is used to define the State Machine for the NAND Flash Controller Access
* The NAND Flash requires two OPCODES for a command
* The Address is to be sent in Three Consecutive Cycles
*/
typedef enum {
	DEFAULT,
	OPCODE_PART1,
	OPCODE_PART2,
	ADDRESS_PART1,
	ADDRESS_PART2,
	ADDRESS_PART3
} Nand_control_states deriving(Bits ,Eq);

/*
* The following Structure is used to define the State Machine to fecilitate Data Transfer between the PCIe Controller and NVMe
* REQUEST_DATA_FROM_PCIe is equivalent to Request to Read from PCIe
* REQUEST_DATA_TO_PCIe   is equivalent to Request to Write to PCIe
*/
typedef enum {
	DO_NOTHING,
	REQUEST_DATA_FROM_PCIe ,
	REQUEST_DATA_TO_PCIe,
	READ_DATA_FROM_PCIe,
	WRITE_DATA_TO_PCIe
} Data_transfer_states deriving (Bits, Eq);

/*
* The following Structure is used to define the State Machine which "Fetches Command from the Main Memory"
*/
typedef enum {
	FETCH_IDLE ,
	FETCHING_COMMAND ,
	COMMAND_ENQUEUING
} Command_Fetch_States deriving (Bits, Eq);

/*
* The following Structure is used to define the State Machine which "Executes the commands"
* This state machine starts as soon as the command is ready in the internalexecQ..
*/
typedef enum {
	EXECUTION_IDLE,
	EXECUTE_ASQ_COMMAND,
	ASQ_COMPLETION ,
	ASQ_READ_DATA_STRUCTURE,
	ISQ_CHECK_ABORT,
	//PRE_EXECUTE_ISQ_LKUP,
	//PRE_EXECUTE_ISQ_DISPATCH,
	EXECUTE_ISQ,
	//EXECUTE_ISQ_CHANNEL0,
	//EXECUTE_ISQ_CHANNEL1,
	//EXECUTE_ISQ_CHANNEL2,
	//EXECUTE_ISQ_CHANNEL3,
	//EXECUTE_ISQ_CHANNEL4,
	//EXECUTE_ISQ_CHANNEL5,
	//EXECUTE_ISQ_CHANNEL6,
	//EXECUTE_ISQ_CHANNEL7,
	ISQ_COMPLETION,
	INITIATE_WRITE_TO_NAND0,
	INITIATE_READ_FROM_NAND0,
	INITIATE_WRITE_TO_NAND1,
	INITIATE_READ_FROM_NAND1
} Command_Execution_States deriving (Bits, Eq);

/*
* The following Structure is used to define the State Machine which "Acquires Command or Data from the PCIe"
* The data payload from the PCFIe is either "Command" or"data (which is to be written to NAND) "
* the "tag field" is used to distinguish it between the two
* This state machine is about identifying the type of payload and storing it appropiately
*/
typedef enum {
	ACQUIRE_IDLE,
	ACQUIRING_COMMAND,
	ACQUIRING_DATA,
	ACQUIRED_DATA,
	ACQUIRED_COMMAND
} Command_and_data_acquire_states deriving (Bits, Eq);

/*
* The following Structure is used to define the State Machine which "Requests PCIe Controller for sending Read / Write TLPs"
* There is a possibility for Simultaneous requests ,inside the NVMe Controller, to access PCIe Controller
* Request may arise from :
*	1. Fetch Module --> asking PCIe to send Read TLP for Reading Command from Main Memory
*	2. Date Transfer Module --> asking the PCIe to send Read TLP to read data or send Write TLP to write data
*	3. Completions module  --> asking PCIe  to send " Write TLP " to send completions to the Main Memory
* This module takes various requests and gives access to one on the basis of assigned priority
* Priotiy is described near the STate Machine Description
*/
typedef enum {
	PCIe_REQ_IDLE,
	PCIe_REQ_WRITE_DATA,
	PCIe_REQ_WRITE_COMPLETION,
	PCIe_SEND_READ_COMMAND_INFO,
	PCIe_SEND_READ_DATA_INFO,
	PCIe_SEND_WRITE_DATA_INFO,
	PCIe_SEND_WRITE_COMPLETION_INFO,
	PCIe_WAIT_STATE
} Pcie_Request_States deriving (Bits, Eq);


/*
* NVMe Interface Declaration
* NVMe has fives interfaces on total
* Four interfaces towards PCIe
* One interface to NAND Flash Controller
* The detailed definition of the Interfaces can be found in "InterfaceNvmController.bsv"
*/
interface Ifc_Controller;
	interface ControllerConfigurationWrite controller_config_write;
	interface NvmInterruptSideA_Interface nvmInterruptSideA_interface;
	interface NvmReceiveCompletionData_Interface nvmReceiveCompletionData_interface;
	interface NvmTransmitToPCIe_Interface nvmTransmitToPCIe_interface;
	interface NandFlashController_Interface nfc_interface0;
	interface NandFlashController_Interface nfc_interface1;
endinterface

(*synthesize*)

////////////////////////////////////////////////////////////////////////////////
// Uncomment the following two lines if you are Simulating in Questa Sim
//(*always_ready*)
//(*always_enabled*)
////////////////////////////////////////////////////////////////////////////////

/*
* Main Module Definition
*/
`define No_SQueues 17
`define No_CQueues 17
`define No_ISQueues 16
`define No_ICQueues 16

module mkNvmController_imp (Ifc_Controller);

////////////////////////////////////////////////////////////////////////////////
// Controller Configuration Write Interface Related Regs and Wires
////////////////////////////////////////////////////////////////////////////////

// Supporting Registers .. these are not part of the interface definition
Reg#(Bit#(64)) rg_regFiledata_out <- mkReg(0); // this is an intermediate Register just to transfer the reg data to PCIe
Reg#(bit) rg_read_regFiledata <- mkReg(0); // this register is used to notify the Completor that the data is ready to be read ** NOT USED **

// Wires .. these are part of the interface
Wire#(Bit#(32)) wr_regFileAddress <- mkDWire(0); 	// Address of the Register in the Reg File
Wire#(Bit#(4)) wr_byte_enable <- mkDWire(0);		// Byte Enable sent from PCIe
Wire#(Bit#(64)) wr_regFiledata_in <- mkDWire(0); 	// register data .. This is used for writing into Reg File
Wire#(bit) wr_read <- mkDWire(0);					// Indicates its a Read Reg File Operation
Wire#(bit) wr_write <- mkDWire(0);					// Indicates its a Write Reg File operation


/////////////////////////////////////////////////////////////////////
// Regs and Wire related to PCIe Completor Requestor Interface
////////////////////////////////////////////////////////////////////

//Regs .. part of interface
Reg#(Bit#(32)) rg_data_to_pcie <- mkReg(0);		// Data to be sent to PCIe
Reg#(Bit#(64)) rg_address_to_pcie <- mkReg(0);	// Address being sent to PCIe .. ITs always 64bit .. No 32 bit support
Reg#(Bit#(2)) rg_requested_tag <- mkReg(0);		// The tag being requested . '01 implies read command '10 implies read data
Reg#(Bit#(10)) rg_payload_length <- mkReg(0);	// Length of payload in DWords
Reg#(bit)  rg_send_completion_tlp <- mkReg(0);	// request to send Completion TLP
Reg#(bit)  rg_send_write_tlp <- mkReg(0);		// request to send Write TLP
Reg#(bit)  rg_send_read_tlp <- mkReg(0);		// request to send Read TLP
Reg#(bit)  rg_64b_address <- mkReg(1); 			// Indicates its a 64b addreess being sent all the address are 64 bit .. by default ...
Reg#(bit) rg_data_valid <- mkReg(0);			// Indicates that the data being sent is valid
Reg#(bit) rg_nvm_wait <- mkReg(0);				// NVM side Wait Assertion .. If required

// Wires part of interface
Wire#(bit) wr_data_valid <- mkDWire(0);			// Idicates data valid
Wire#(bit) wr_send_valid_data <- mkDWire(0);	// Indication from the PCIe to send valid data
Wire#(bit) wr_wait <- mkDWire(0);				// Wait assertion from the PCIe controller
Wire#(Bool) wr_pcie_busy <- mkDWire(False);		// Indicates that the PCIe is busy or ready to receive requests

////////////////////////////////////////////////////////////////////////////////
// registers ansd wires for sending read request to pcie and getting the command from pcie
////////////////////////////////////////////////////////////////////////////////

// Internal Registers for Acquiring data and Command
// The registers are used in relation to State machine "Acquire Command And Data"
Reg#(bit) rg_command_ready <- mkReg(0);				// Indicates that the command has been acquired and it ready in command buffer
Reg#(Bit#(12)) rg_addr_for_write_bufr <- mkReg(0);		// Address for the Data buffer
Reg#(Bit#(4)) rg_addr_for_cmd_bufr <- mkReg(0);			// Address for Command buffer
Vector#(16,Reg#(Bit#(32))) rg_command_buf <- replicateM(mkReg(0));	// Command buffer declared as a vector of DWord sized Regs

// Wires in use
Wire#(bit) wr_commandEn <- mkDWire(0);			// Indicated that the command is ready .. indication to Fetch unit
Wire#(bit) wr_write_data_ready <- mkDWire(0);	// Indiactes data ready .. Internal
Wire#(Bit#(512)) wr_command_In <- mkDWire(0);	// The command obtained into buffers is concatenated into this wire.



////////////////////////////////////////////////////////////////////////////////
// Registers and Wires Realated to Receive Completion data interface
////////////////////////////////////////////////////////////////////////////////

// wires fir the interface
Wire#(Bit#(32)) wr_completionDataIn <- mkDWire(0);		// Completion data .. this is obtained as payload to Request Handler in PCIe
Wire#(bit)  wr_completionDataValid <- mkDWire(0);		// Indicates that the data sent is valid
Wire#(bit)  wr_completionLastDWord <- mkDWire(0);		// Indiactes that this DWord is the last DWord for the associated tag
Wire#(Bit#(2))  wr_tag <- mkDWire(0);					// Tag associated with this transaction


////////////////////////////////////////////////////////////////////////////////
// Registers Related to Interrupt Interface
////////////////////////////////////////////////////////////////////////////////

Reg#(bit) rg_vector_rdy <- mkReg(0);					// Indiactes that the interrupt Vector is ready
Reg#(Bit#(5)) rg_vector_number <- mkReg(0);			// Indicates the vector number
Reg#(Bit#(5)) rg_aggregation_threshold <- mkReg(0);	// LSB 5bits of the Value of Aggregation Threshold set by the HOST CPU .


////////////////////////////////////////////////////////////////////////////////
// Registers for Q management
////////////////////////////////////////////////////////////////////////////////

Vector#(`No_SQueues,Reg#(Bit#(16))) sqhdbl <- replicateM(mkReg(0));		// SQ Head Door Bell Registers
Vector#(`No_CQueues,Reg#(Bit#(16))) cqtdbl <- replicateM(mkReg(0));		// CQ Tail Door Bell Registers
Vector#(`No_SQueues,Reg#(Bool)) rg_SqEmpty <- replicateM(mkReg(False));	// Register to indicate SQ Empty Condition
Vector#(`No_SQueues,Reg#(Bool)) rg_SqFull  <- replicateM(mkReg(False));	// Register to indicate SQ Full  Condition
Vector#(`No_CQueues,Reg#(Bool)) rg_CqEmpty <- replicateM(mkReg(False));	// Register to indicate CQ Empty Condition
Vector#(`No_CQueues,Reg#(Bool)) rg_CqFull  <- replicateM(mkReg(False));	// Register to indicate CQ Full  Condition


////////////////////////////////////////////////////////////////////////////////
// Registers for checking ASQ and ISQ request Eligibility
////////////////////////////////////////////////////////////////////////////////
Reg#(Bool) rg_EligibleForRequestAsq <- mkReg(False);  // Checks if the ASQ is eligible for requesting
Vector #(`No_ISQueues , Reg#(Bool)) rg_EligibleForRequestIsq <- replicateM(mkReg(False));  // Checks if the ISQs are eligible for requesting


////////////////////////////////////////////////////////////////////////////////
// Admin Q realted Registers
////////////////////////////////////////////////////////////////////////////////
Reg#(Address_Out_Type) rg_base_address_asq <- mkReg(0);  // Register for base address of ASQ
Reg#(Bit#(64)) rg_offset_asq <- mkReg(0);  // Offset register
Reg#(Bit#(32)) rg_count_asq  <- mkReg(0);  // used for calculating next address in Q


////////////////////////////////////////////////////////////////////////////////
// I/O Completion Q realted ragisters
////////////////////////////////////////////////////////////////////////////////
Vector#(`No_ICQueues,Reg#(bit)) rg_Icq_en <- replicateM(mkReg(0));  // Only two Icq
Vector#(`No_ICQueues,Reg#(Bit#(16))) rg_CQ_size <- replicateM(mkReg(0));  // Size of Cq to be created
Vector#(`No_ICQueues,Reg#(Bit#(64))) rg_CQ_base_address <- replicateM(mkReg(0));  // Size of Cq to be created

////////////////////////////////////////////////////////////////////////////////
// Q Identifiers/
////////////////////////////////////////////////////////////////////////////////
Vector#(`No_SQueues,Reg#(Bit#(16))) rg_cqIDofSQ <- replicateM(mkReg(0));  // CQ Identifiers for associated SQs
Reg#(Bit#(16)) rg_cqID <- mkReg(0);  // CQ Identifier for sending Completions

////////////////////////////////////////////////////////////////////////////////
// Interrupt Vector related
////////////////////////////////////////////////////////////////////////////////
Vector#(`No_SQueues, Reg#(Bit#(5))) rg_InterruptVector <- replicateM(mkReg(0));  // This Vector holds the Interrupt Vector Numbers for as many as NINE Submission Queues
Vector#(`No_SQueues, Reg#(bit)) rg_InterruptEnable <- replicateM(mkReg(0));  // Vector to check if Interrupts are enabled to associated SQ

////////////////////////////////////////////////////////////////////////////////
// Registers for CQ Entry
////////////////////////////////////////////////////////////////////////////////
Reg#(Bit#(32)) rg_CQ_DWord0 <- mkReg(0);  // Command specific .. Depends on command
Reg#(Bit#(32)) rg_CQ_DWord1 <- mkReg(0);  // Reserved
Reg#(CQ_DWord2) rg_CQ_DWord2 <- mkReg(CQ_DWord2 {
	sqID : 0,			// Submission Q ID
	sqHeadPointer : 0	// SQ Head pointer
});

Reg#(CQ_DWord3) rg_CQ_DWord3 <- mkReg(CQ_DWord3 {  // STATUS FILED DEFINITIONS
	status_field_DNR : 0 ,	// Do Not Retry .. with same command
	status_field_M   : 0 ,	// More .. Indicates more error logs present
	status_field_res : 0 ,	// reserved
	status_field_SCT : 0 ,	// Status Code Type
	status_field_SC  : 0 ,	// Status Code
	phase_tag		 : 0 ,	// Phase tag info
	commandID		 : 0 	// Command ID
});

////////////////////////////////////////////////////////////////////////////////
// Wires Related to CQ Status
////////////////////////////////////////////////////////////////////////////////
Wire#(bit) wr_update_admin_command_status <- mkDWire(0);	// update admin status
Wire#(bit) wr_update_nvm_command_status <- mkDWire(0);		// update for NVM status
Wire#(bit) wr_admin_success <- mkDWire(0);					// Indicates Admin Cmd success
Wire#(bit) wr_nvm_success <- mkDWire(0);					// Indicates Nvm Cmd success
Wire#(bit) wr_admin_max_Q_size_exceeded <- mkDWire(0);		// Indicates max Q size exceeded
Wire#(bit) wr_admin_invalid_QID <- mkDWire(0);				// Indicates invalid Q ID

////////////////////////////////////////////////////////////////////////////////
// I/O Submission Q related registers
////////////////////////////////////////////////////////////////////////////////
Vector#(`No_ISQueues,Reg#(bit)) rg_Isq_en <- replicateM(mkReg(0));  // Only `No_ISQueues Isq s
Vector#(`No_ISQueues,Reg#(Bit#(16))) rg_ISQ_size <- replicateM(mkReg(0));  // Size of Sq to be created
Vector#(`No_ISQueues,Reg#(Bit#(64))) rg_ISQ_base_address <- replicateM(mkReg(0));  // Size of Sq to be created

Vector#(`No_ISQueues,Reg#(Bit#(64))) rg_offset_isq <- replicateM(mkReg(0));  // Offset register
Vector#(`No_ISQueues,Reg#(Bit#(16))) rg_count_isq  <- replicateM(mkReg(0));  // used for calcting next adr in Q


////////////////////////////////////////////////////////////////////////////////
// internal Command Queue
////////////////////////////////////////////////////////////////////////////////
FIFOF #(SqCommandType) ff_internalexecQ <- mkSizedFIFOF(5);  // Internal FIFO for holding 5 commands
FIFOF #(SqCommandType) ff_internalexecQ_isq <- mkSizedFIFOF(5);  // for ISQ Commands ** presently Not Used **

////////////////////////////////////////////////////////////////////////////////
// Admin Command related registers
////////////////////////////////////////////////////////////////////////////////

// Registers to define the command
Reg#(Bit#(8)) rg_opcode 		<- mkReg(0); 			// Opcode field in command
Reg#(Bit#(2)) rg_fuse  			<- mkReg(0); 			// Indicates fused command
Reg#(Bit#(6)) rg_reserved0	 		<- mkReg(0);		// Reserved
Reg#(Bit#(16)) rg_command_id 	<- mkReg(0); 			// Command ID Info
Reg#(Bit#(32)) rg_nsid	 		<- mkReg(0); 			// NameSpace IDentifier
Reg#(Bit#(32)) rg_reserved1	 		<- mkReg(0); 		// Reseerved space
Reg#(Bit#(32)) rg_reserved2	 		<- mkReg(0); 		// Reserved Space
Reg#(Bit#(64)) rg_mptr 			<- mkReg(0); 			// Meta data Pointer
Reg#(Bit#(64)) rg_prp1 			<- mkReg(0); 			// Physical Region Page Pointer
Reg#(Bit#(64)) rg_prp2 			<- mkReg(0); 			// Physical Region Page Pointer List
Reg#(Bit#(32)) rg_cdw10 		<- mkReg(0); 			// DWord 10 .. Command Specific
Reg#(Bit#(32)) rg_cdw11 		<- mkReg(0); 			// DWord 11 .. Command Specific
Reg#(Bit#(32)) rg_cdw12 		<- mkReg(0); 			// DWord 12 .. Command Specific
Reg#(Bit#(32)) rg_cdw13 		<- mkReg(0); 			// DWord 13 .. Command Specific
Reg#(Bit#(32)) rg_cdw14 		<- mkReg(0); 			// DWord 14 .. Command Specific
Reg#(Bit#(32)) rg_cdw15 		<- mkReg(0); 			// DWord 15 .. Command Specific

////////////////////////////////////////////////////////////////////////////////
// Identify Data Structure(DS) related registers and Wires
////////////////////////////////////////////////////////////////////////////////
// Internal Regs for Reading Data Structures
Reg#(Bool) rg_Start_read_Data_Structure <- mkReg(False); 	// control Signal to Start reading Data Structure
Reg#(Bit#(13)) rg_read_Bram_Count <- mkReg(0);				// BRAM Address count
Reg#(bit) rg_CNS <- mkReg(0);								// Indicate weather its a Controller DS or Namespace DS

////////////////////////////////////////////////////////////////////////////////
// Registers related to completion Interface and Rules for sending completions
////////////////////////////////////////////////////////////////////////////////
// Regs ..
Reg#(Bool) rg_sendAdminCommandCompletion <- mkReg(False);	// Control signal to send Admin Command Completion
Reg#(Bool) rg_sendIOCommandCompletion <- mkReg(False);		// Control signal to send NVM Command Completion
Reg#(Bool) rg_disableSendCompletion <- mkReg(False);		// Disable sending completionssss ....

////////////////////////////////////////////////////////////////////////////////
// Registers for Command Aborting
////////////////////////////////////////////////////////////////////////////////

Reg#(Maybe#(CommandType)) abort_command_list[5];		// List of Commands to be aborted Keerthi:may not be directly useful
for(Integer i = 0; i < 5; i = i + 1) begin				// A maximum of Five commands can be aborted at a time..
	abort_command_list[i] <- mkReg(tagged Invalid);		// Valid => command is yet to be aborted
end														// Invalid => new command can be added to be aborted

Reg#(Bool) abort_all <- mkReg(False);								// Abort all
Reg#(Bit#(16)) rg_command_sqID_to_be_processed <- mkReg(0);			// This is the sqID of the Command that has to be executed

Reg#(Bool) rg_ASQCommand_checkAbort <- mkReg(False);				// Check for ASQ abort
Reg#(Bool) rg_ISQCommand_checkAbort <- mkReg(False);				// Check for ISQ abort

Wire#(bit) wr_command_got_aborted <- mkDWire(0);					// Indicates that the command was aborted
Wire#(bit) wr_abort_command_limit_exceeded <- mkDWire(0);			// Indicates Command Limit exceeded
Reg#(Bit#(3)) rg_no_of_outstanding_commands_to_abort <- mkReg(0);	// Indicates the Number of Outstanding Commands left to be aborted

////////////////////////////////////////////////////////////////////////////////
// Registers and Wire for supporting Data Transfer State Machine and PCIe Request State Machine
////////////////////////////////////////////////////////////////////////////////
Wire#(bit) wr_req_read_command <- mkDWire(0);			// Indicates a Request to read command
Reg#(bit) rg_req_read_command_accepted <- mkReg(0);		// Read Command Accepted

Wire#(bit) wr_req_read_data <- mkDWire(0);				// Indicates a Request to read data
Reg#(bit) rg_req_read_data_accepted <- mkReg(0);		// Read data Accepted

Wire#(bit) wr_req_write_data <- mkDWire(0);				// Indicates a Request to write data
Reg#(bit) rg_req_write_data_accepted <- mkReg(0);		// Write data Accepted
Reg#(bit) send_data_done <- mkReg(0);					// Indicates Data Sent

Wire#(bit) wr_req_write_completion <- mkDWire(0);		// Indicates a Request to write completion
Reg#(bit) rg_req_write_completion_accepted <- mkReg(0);	// Write completion Accepted
Reg#(bit) send_completion_done <- mkReg(0);				// Indicates completion sent

////////////////////////////////////////////////////////////////////////////////
// Controller Registers Definition
////////////////////////////////////////////////////////////////////////////////
Reg#(Controller_capabilities) cap <- mkReg(Controller_capabilities {
	reserved1  	: 0 ,		// Reserved
	mpsmax  	: 4'd5, 	// max page size
	mpsmin  	: 4'd0,		// Min page size
	reserved2  	: 0 ,		// Reserved
	css  		: 4'b0001,	//
	reserved3  	: 0 ,		// Reserved
	dstrd  		: 0,		// DoorBell Stride Value
	to  		: 8'd10,	// 10 * 500 ms
	reserved4  	: 0 ,		// Reserved
	ams  		: 0, 		// Only Round Robin supported
	cqr  		: 1, 		// Physically contiguous memory for SQ CQs
	mqes  		: 16'd9 	// max Q size = 10
});

Reg#(Version) version <- mkReg(Version {
	mjr : 16'h0001,		// version Major
	mnr : 16'h0000		// version Minor
});

Reg#(Bit#(32)) mask_set <- mkReg(0);		// Interrupt Vectr Mask Set
Reg#(Bit#(32)) mask_clear <- mkReg(0);		// Interrupt Vectr Mask Clr

Reg#(Controller_configuration) cc <- mkReg(Controller_configuration {
	reserved1	: 0,	 	// Resrvd
	iocqes 		: 0,		// IO Completion Q Entry Size
	iosqes 		: 0,		// IO Submission Q Entry Size
	shn 		: 0,		// Shutdown Notification
	ams 		: 0,		// Arbitration Mechnsm
	mps 		: 0,		// Max Page Size
	css 		: 0,		// IO Command Set Selected
	reserved2 	: 0,		// Reserved
	en 		: 0 			// Controller Enable
});

Reg#(Controller_status) csts <- mkReg(Controller_status {
	reserved	:0,		// Reserved
	shst		:0,		// Shutdwn Status
	cfs		:0,			// Controller Fatal Status
	rdy		:0			// Controller Ready
	});

Reg#(AQA) aqa <- mkReg(AQA {
	reserved1	:0,		// Reserved
	acqs		:0,		// Admin CQ Size
	reserved2	:0,		// reserved
	asqs		:5		// Admin SQ Size
	});

Reg#(ASQ) asq <- mkReg(ASQ {
	asqb		:0,		// Admin SQ BAse Adrs
	reserved 	:0		// Reserved
	});

Reg#(ACQ) acq <- mkReg(ACQ {
	acqb		:0,		// Admin CQ Base Address
	reserved 	:0		// Reserved
	});

Vector#(`No_SQueues,Reg#(SQTDBL)) sqtdbl <- replicateM(mkReg(SQTDBL {
	reserved	:0,		// Reserved
	sqt 		:0		// SQ TAil DoorBell Value
}));

Vector#(`No_SQueues,Reg#(CQHDBL)) cqhdbl <- replicateM(mkReg(CQHDBL {
	reserved	:0,		// Reserved
	cqh 		:0		// CQ Head Door Bell Value
}));

////////////////////////////////////////////////////////////////////////////////
// End of Controller registers
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
// Registers to Store the Features of the Controller
////////////////////////////////////////////////////////////////////////////////
Reg#(Arbitration) arbitration <- mkReg(Arbitration {
	hpw	: 0,	// high Priority Weight
	mpw	: 0,	// Medium Priority Weight
	lpw	: 0,	// Low Priority Weight
	ab	: 0		// Arbitration Burst
});		// * Only round Robin Supported *

Reg#(Bit#(32)) powerManagement	<- mkReg(0);			// Power Management Feature
Reg#(Bit#(32)) lbaRangeType		<- mkReg(0);			// LBA RANGE Feature *Used as File System*
Reg#(Bit#(32)) temperatureThreshold <- mkReg(0);		// temperature threshold limit
Reg#(Bit#(32)) errorRecovery		<- mkReg(0);		// error recovery Feature
Reg#(Bit#(32)) volatileWriteCache		<- mkReg(0);	// cache not supported

Reg#(NumberOfQs_Requested) numberR <- mkReg(NumberOfQs_Requested {
	ncqr	: 0,	// Number of IO CQs Requested
	nsqr	: 0		// Number of IO SQs Requested
});

Reg#(NumberOfQs_Allocated) numberA <- mkReg(NumberOfQs_Allocated {
	ncqa	: 1,	// Number of IO CQs Allocated
	nsqa	: 8		// Number of IO SQs Allocated
});

Reg#(InterruptCoalescing) interrupt <- mkReg(InterruptCoalescing {
	int_time	: 0,	// Int Aggregation time
	thr		: 0			// Int Aggregation Threshold
});

Reg#(InterruptVectorConfiguration) interruptVectorConfig <- mkReg(InterruptVectorConfiguration{
	cd		: 0,	// Coalesceing Disable
	iv		: 0		// Interupt Vector
});

Reg#(Bit#(32)) writeAtomicity <- mkReg(0);				// Write Atomicity
Reg#(Bit#(32)) asynchronousEventConfig <- mkReg(0);		// Asyn Event Configuration
Reg#(Bit#(32)) softwareProgressMarker <- mkReg(0);		// Software Progress Marker

// Internal Registers for features

Reg#(Bool) rg_getFeatures <- mkReg(False);  // Internal Register to enable get Features

////////////////////////////////////////////////////////////////////////////////
// Registers for Facilitating MSI interrupts
////////////////////////////////////////////////////////////////////////////////
Reg#(Bit#(32)) mask_reg <- mkReg(0);		// Internal Register for Interrupt Mask
Reg#(Bit#(32)) status_reg <- mkReg(0);		// Interrupt Status register

Reg#(Bit#(32)) prev_mask_reg <- mkReg(0);	// Stores Previous value .. used for pos-edge detect
Reg#(Bit#(32)) prev_status_reg <- mkReg(0);	// Stores Previous value .. used for pos-edge detect
Reg#(Bit#(32)) prev_mask_set <- mkReg(0);	// Stores Previous value .. used for pos-edge detect
Reg#(Bit#(32)) prev_mask_clear <- mkReg(0);	// Stores Previous value .. used for pos-edge detect

Wire#(Bit#(32)) pos_edge_on_mask_set <- mkDWire(0);
Wire#(Bit#(32)) pos_edge_on_mask_clear <- mkDWire(0);
Wire#(Bit#(32)) pos_edge_on_status_reg <- mkDWire(0);
Wire#(Bit#(32)) neg_edge_on_mask_reg <- mkDWire(0);

Wire#(Bit#(32)) event_d <- mkDWire(0);

Wire#(bit) wr_clear_m <- mkDWire(0);
Wire#(bit) wr_set_m <- mkDWire(0);

Reg#(bit) rg_clear_m <- mkReg(0);
Reg#(bit) rg_set_m <- mkReg(0);

Reg#(Bit#(5)) vectr <- mkReg(0);
Reg#(Bit#(1)) vectr_rdy <- mkReg(0);

Reg#(Bit#(5)) vectr_count[31];
for(Integer i = 0; i < 31; i = i + 1) begin
	vectr_count[i] <- mkReg(0);
end

Reg#(Bit#(5)) aggr_threshold <- mkReg(3);


////////////////////////////////////////////////////////////////////////////////
// Module Instantiations
////////////////////////////////////////////////////////////////////////////////
Arbiter_IFC#(`No_SQueues) rr_arb <- mkArbiter(False);  // fixed = True i.e. it gives the current client the priority  Keerthi:may be


////////////////////////////////////////////////////////////////////////////////
// BRAMs for Identify Controller Data Structure and Identify Namespace Data Structure
////////////////////////////////////////////////////////////////////////////////


/*
* Buffer for Controller Data Structure
* The contents are initialized by using the Text file Named "IdentifyControllerDataStructur.txt"
*/
BRAM_Configure cfg_controller_ds = defaultValue;
cfg_controller_ds.loadFormat = tagged Hex "IdentifyControllerDataStructure.txt";

BRAM1Port#(Bit#(12) , Bit#(32)) controller_data_structure <- mkBRAM1Server(cfg_controller_ds);


/*
* Buffer for NameSpace Data Structure
* The contents are initialized by using the Text file Named "IdentifyNamespaceDataStructur.txt"
*/
BRAM_Configure cfg_namespace_ds = defaultValue;
cfg_controller_ds.loadFormat = tagged Hex "IdentifyNamespaceDataStructure.txt";

BRAM1Port#(Bit#(12) , Bit#(32)) namespace_data_structure <- mkBRAM1Server(cfg_namespace_ds);

////////////////////////////////////////////////////////////////////////////////
// BRAM for LBA Range Data Structures
////////////////////////////////////////////////////////////////////////////////

/*
* Buffer for LBA Range Data Structure
* The contents are initialized by using the Text file Named "LBARangeDataStructur.txt"
*/
BRAM_Configure cfg_lbaRange_ds = defaultValue;
cfg_lbaRange_ds.loadFormat = tagged Hex "LBARangeDataStructure.txt";

BRAM1Port#(Bit#(12) , Bit#(32)) lbaRange_data_structure <- mkBRAM1Server(cfg_lbaRange_ds);

////////////////////////////////////////////////////////////////////////////////
// BRAm for Write Data Buffer
////////////////////////////////////////////////////////////////////////////////

BRAM_Configure cfg_write_data_buffer = defaultValue;
BRAM1Port#(Bit#(12) , Bit#(32)) write_data_buffer <- mkBRAM1Server(cfg_write_data_buffer);

////////////////////////////////////////////////////****************************


////////////////////////////////////////////////////////////////////////////////
// Rule for Controller Initialization
// When the Host sets the cc.en bit to 1 .. Controller responds  to it by setting the controler status to ready
////////////////////////////////////////////////////////////////////////////////
rule rl_Initialization;
	if(cc.en == 1) begin
		csts.rdy <= 1;
//			$display("Controller Enabled");
	end
	else csts.rdy <= 0;
endrule


//////////////////////////////////////////////////////////////////////////////////
// Rule for Admin and I/O Queue management										//
// Q Empty Condition --> Tail = Head											//
// Q Full Condition  --> Tail = Head -1											//
// One location is always vacant												//
//////////////////////////////////////////////////////////////////////////////////

rule rl_SQmanagement;
	for (Integer r = 0; r < `No_SQueues; r = r + 1) begin

		if (sqtdbl[r].sqt == sqhdbl[r]) begin  // sqhdbl is the SQ Head Door Bell pointer used by the Controller
			rg_SqEmpty[r] <= True;  // Q Empty Condition
			rg_SqFull[r] <= False;
//				$display(" SQ %d is  empty " , r);
		end

		else if (sqtdbl[r].sqt == (sqhdbl[r] - 1)) begin
//				$display(" SQ %d is  FULL  " , r);
			rg_SqEmpty[r] <= False;
			rg_SqFull[r] <= True;  // Q Full Condition
		end

		else if ((sqtdbl[r].sqt > sqhdbl[r]) || (sqtdbl[r].sqt < (sqhdbl[r] - 1))) begin
			rg_SqEmpty[r] <= False;
			rg_SqFull[r] <= False;
//				$display(" SQ %d is  not empty " , r);
		end

	end
endrule

rule rl_CQmanagement;
	for (Integer r = 0; r < `No_CQueues; r = r + 1) begin

		if (cqhdbl[r].cqh == cqtdbl[r]) begin  // sqhdbl is the SQ Head Door Bell pointer used by the Controller
			rg_CqEmpty[r] <= True;  // Q Empty Condition
			rg_CqFull[r] <= False;
//				$display(" SQ %d is  empty " , r);
		end

		else if (cqtdbl[r] == (cqhdbl[r].cqh - 1)) begin
//				$display(" SQ %d is  FULL  ", r);
			rg_CqEmpty[r] <= False;
			rg_CqFull[r] <= True;  // Q Full Condition
		end

		else if ((cqtdbl[r] > cqhdbl[r].cqh) || (cqtdbl[r] < (cqhdbl[r].cqh - 1))) begin
			rg_CqEmpty[r] <= False;
			rg_CqFull[r] <= False;
//				$display(" SQ %d is  not empty " , r);
		end

	end
endrule


//////////////////////////////////////////////////////////////////////////////////
// Rule to check for the eligibility to request 								//
// If the particular Q is not empty then that Q is Eligible for Request			//
// Ofcourse The Q has to be Created first 										//
// Admin Q need not be created													//
// NVM IO SQ has to be created and then checked for empty or not				//
//////////////////////////////////////////////////////////////////////////////////

// Rule for Admin
rule rl_check_asq;
	if (!rg_SqEmpty[0] && cc.en == 1) begin  // SQ Tail Door Bell is updated and controller enabled
		rg_EligibleForRequestAsq <= True;
//			$display(" ********** NVMe ******** SQ 0 is eligible for request ***************** ");
	end
	else begin
		rg_EligibleForRequestAsq <= False;
//			$display(" ********** NVMe ******** SQ 0 is NOT eligible for request ************* ");
	end
endrule: rl_check_asq

////////////////////////////////////////////////////////////////////////////////

// rule for IO SQ
rule rl_check_isq;
	for (Integer r = 0; r < `No_ISQueues; r = r + 1) begin
		if ((!rg_SqEmpty[r+1]) && (rg_Isq_en[r] == 1) && (cc.en == 1)) begin		// SQ Tail Door Bell is updated and controller enabled
																											// And Queue is created
		rg_EligibleForRequestIsq[r] <= True;
//			$display(" ********** NVMe ********** SQ %d is eligible for Request ************* ", r+1);
		end
		else
			rg_EligibleForRequestIsq[r] <= False;
	end
endrule: rl_check_isq


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Rules for Requesting the Arbiter 																						//
// If the SQ is eligible for requesting then the following Rules are used to Request the Arbiter for granting the Access	//
// Keeps requesting the arbiter as long as it is eligible to request														//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// rule for ASQ
rule rl_asq_req (rg_EligibleForRequestAsq);
		rr_arb.clients[0].request;
//			$display(" ********** NVMe *********** SQ 0 is now requesting  ************** ");
endrule : rl_asq_req


// Rules for ISQs
for (Integer r = 0; r < `No_ISQueues; r = r + 1) begin
	rule  rl_isq_req (rg_EligibleForRequestIsq[r]);
//				$display(" ********** NVMe ********** SQ %d is now requesting *************  ", r+1);
			rr_arb.clients[r+1].request;
	endrule : rl_isq_req
end
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// The following Registers are Used for implementing Command Fetch State Machine
////////////////////////////////////////////////////////////////////////////////

Reg#(Command_Fetch_States) rg_fetch_state <- mkReg(FETCH_IDLE);
Reg#(bit) rg_asq_grant <- mkReg(0);
Reg#(bit) rg_isq_grant <- mkReg(0);
FIFOF#(Bit#(16)) sqID_fifo <- mkSizedFIFOF(5);
Reg#(Bit#(64)) base_address <- mkReg(0);
Reg#(Bit#(64)) offset <- mkReg(0);
Reg#(bit) enq_isq <- mkReg(0); // 1 => ASQ , 0 => ISQ
Reg#(Bit#(512)) rg_command_in <- mkReg(0);
Reg#(Bool) rg_waiting_for_command <- mkReg(False);
Reg#(Bool) next_grant <- mkReg(True);
Wire#(Bool) wr_read_request_granted <- mkDWire(False);
Wire#(Bool) wr_write_request_granted <- mkDWire(False);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Command Fetch State Machine																													 //
// State Machine Description on the basis of rules :																							 //
//																																				 //
//						RULE					STATE_REPRESENTATION 		FUNCTIONALITY									 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//						rl_fetch_idle					Idle state of fetch						Following Command Information is gathered:		 //
//																									sqID										 //
//																									base Address of the Queue 					 //
//																									offset (present location of command is		 //
//																									determined as Effective Address = BA + Offset//
//	---------------------------------------------------------------------------------------------------------------------------------------------//
//						rl_fetching_command				Command Fetch							NVMe Starts Requesting PCIe for Reading Command  //
//																								Waits for the PCIe Request State Machine to grant//
//																								Jumps to next State When the Comand is ready	 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//						rl_command_enqueuing			Command En-Queue						Command Obtained from the Acquire Command State  //
//																								is EnQueued										 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


rule rl_fetch_idle (rg_fetch_state == FETCH_IDLE && next_grant == False);
	$display(" ########### STATE INFO ######### COMMAND FETCH STATE #### IDLE ");

	if (rg_command_sqID_to_be_processed == 0) begin
		rg_fetch_state <= FETCHING_COMMAND;
		sqID_fifo.enq (rg_command_sqID_to_be_processed);  // ENQUEING THE SQ ID INFORMATION
		base_address <= { asq.asqb , asq.reserved };
		offset <= rg_offset_asq;
		enq_isq <= 0;
	end

	else begin
		rg_fetch_state <= FETCHING_COMMAND;
		sqID_fifo.enq (rg_command_sqID_to_be_processed);  // ENQUEING THE SQ ID INFORMATION
		base_address <= rg_ISQ_base_address[rg_command_sqID_to_be_processed - 1];
		offset <= rg_offset_isq[rg_command_sqID_to_be_processed - 1];
		enq_isq <= 1;
	end

endrule


rule rl_fetching_command (rg_fetch_state == FETCHING_COMMAND);
	$display(" ########### STATE INFO ######### COMMAND FETCH STATE #### FETCHING COMMAND ");

	if(rg_waiting_for_command == False) begin
		wr_req_read_command <= 1; // Requesting to READ COMMAND
		if (rg_req_read_command_accepted == 1) begin
			rg_waiting_for_command <= True;
			rg_address_to_pcie <= base_address + offset;
		end
	end

	else begin
		if(wr_commandEn == 1) begin
			rg_waiting_for_command <= False;
			rg_command_in <= wr_command_In;
			rg_fetch_state <= COMMAND_ENQUEUING;
		end
	end
endrule

rule rl_command_enqueuing (rg_fetch_state == COMMAND_ENQUEUING);
	$display(" ########### STATE INFO ######### COMMAND FETCH STATE #### COMMAND ENQUEUING ");
	if(enq_isq == 0) begin
		ff_internalexecQ.enq(rg_command_in);
		rg_fetch_state <= FETCH_IDLE;
		next_grant <= True;
	end

	else if (enq_isq == 1) begin
//			$display(" @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ SQ ID ENQUEUING = %d @@@@@@@@@@@@@@@@@@@" ,rg_command_sqID_to_be_processed);
		ff_internalexecQ.enq(rg_command_in);
		rg_fetch_state <= FETCH_IDLE;
		next_grant <= True;
	end

endrule

////////////////////////////////////////////////////////////////////////////////
// Command Fetch State Machine Done
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
// Rule to compute the next address of the Admin Sq Command						//
// Functionality : 																//
//					1. Calcultes the next Address in the Submission Queue 0		//
//					2. Updates the SQ Head Doorbell								//
//																				//
//////////////////////////////////////////////////////////////////////////////////


rule rl_asq_grant (rr_arb.clients [0].grant && next_grant == True);

	$display("************** ASQ got the Grant ************ ");

	// SQ ID information
	let lv_sqID = 0;
	rg_command_sqID_to_be_processed <= lv_sqID;

	next_grant <= False;

	let asq_size = {0,aqa.asqs};
	if(rg_count_asq == 0) begin
		rg_count_asq <= rg_count_asq + 1;
		rg_offset_asq <= 0;
	end

	else if(rg_count_asq == asq_size + 1) begin  // end of ASQ
		rg_count_asq <= 0;
		rg_offset_asq <= 0;
	end

	else begin
		rg_offset_asq <= rg_offset_asq + 64;  // fetch the next command after 16 Dwords
		rg_count_asq <= rg_count_asq + 1;
	end

	let maxASQSize = {4'b0000 , aqa.asqs};

	if(sqhdbl[0] ==  maxASQSize)
		sqhdbl[0] <= 0;
	else
		sqhdbl[0] <= (sqhdbl[0] + 1);

endrule

//////////////////////////////////////////////////////////////////////////////////
// Rule to compute the next address of the IO Sq Command						//
// Functionality : 																//
//					1. Calcultes the next Address in the IO Submission Queue 	//
//					2. Updates the SQ Head Doorbell								//
//																				//
//////////////////////////////////////////////////////////////////////////////////


for (Integer r = 0; r < `No_ISQueues; r = r + 1) begin
	rule rl_isq_grant (rr_arb.clients [r+1].grant && next_grant == True);

		// SQ ID information
		next_grant <= False;
		$display("********** SQ %d got the grant ********", r+1);
		let lv_sqID = fromInteger(r+1);
		rg_command_sqID_to_be_processed <= lv_sqID;

		if(rg_count_isq[r] == 0) begin
			rg_count_isq[r] <= rg_count_isq[r] + 1;
			rg_offset_isq[r] <= 0;
		end

		else if(rg_count_isq[r] == rg_ISQ_size[r] + 1) begin  // end of ASQ
			rg_count_isq[r] <= 0;
			rg_offset_isq[r] <= 0;
		end

		else begin
			rg_offset_isq[r] <= rg_offset_isq[r] + 64;  // fetch the next command after 16 Dwords
			rg_count_isq[r] <= rg_count_isq[r] + 1;
		end

		let maxISQSize = rg_ISQ_size[lv_sqID - 1];

		if(sqhdbl[lv_sqID] ==  maxISQSize) begin
			sqhdbl[lv_sqID] <= 0;
		end
		else begin
			sqhdbl[lv_sqID] <= (sqhdbl[lv_sqID] + 1);
		end

	endrule
end

Reg#(Command_Execution_States) rg_execution_state <- mkReg(EXECUTION_IDLE);
Reg#(Bit#(16)) command_sqID <- mkReg(0);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Command Execution State Machine																												 //
// State Machine Description on the basis of rules :																							 //
//																																				 //
//							RULE						STATE_REORESENTATION 					FUNCTIONALITY									 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//						rl_execution_idle				Idle state of Command					1. Determines if it is a Admin or NVM Command	 //
//														 Execution								2. Opcode Decode								 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//					rl_asq_command_execution			ASQ Command Execution					1. Admin Command Execution						 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//				        rl_asq_completion          		ASQ Command Completion					1. Admin Command Completion						 //
//																								2. It requests PCIe to send Write TLP			 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
// Remaining Rules related to Command Execution State Machine Are Explained later................................................................//
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

rule rl_execution_idle (rg_execution_state == EXECUTION_IDLE);
	$display(" ########### STATE INFO ######### COMMAND EXEC STATES #### EXECUTION IDLE ");

		if (ff_internalexecQ.notEmpty) begin
			let lv_sqID = sqID_fifo.first ();
			if (lv_sqID == 0) begin
				let admin_command = ff_internalexecQ.first ();
				$display(" THE OPDODE FOR THE ASQ COMMAND IS = %d ", admin_command[7:0]);
				rg_opcode	<= admin_command[7:0];
				rg_fuse 	<= admin_command[9:8];
				rg_reserved0 <= admin_command[15:10];
				rg_command_id <= admin_command[31:16];		// DWord 0

				rg_nsid		<= admin_command[63:32];		// Dword 1

				rg_reserved1 <= admin_command[95:64];		// Dword 2

				rg_reserved2 <= admin_command[127:96];		// Dword 3

				rg_mptr		<= admin_command[191:128];		// Dword 4 & 5

				rg_prp1		<= admin_command[255:192];		// Dword 6 & 7

				rg_prp2		<= admin_command[319:256];		// Dword 8 & 9

				rg_cdw10    <= admin_command[351:320];		// Dword 10

				rg_cdw11    <= admin_command[383:352];		// Dword 11

				rg_cdw12    <= admin_command[415:384];		// Dword 12

				rg_cdw13    <= admin_command[447:416];		// Dword 13

				rg_cdw14    <= admin_command[479:448];		// Dword 14

				rg_cdw15    <= admin_command[511:480];		// Dword 15

				rg_execution_state <= EXECUTE_ASQ_COMMAND;
			end
			else begin
				let nvm_command = ff_internalexecQ.first();
				rg_cqID <= rg_cqIDofSQ[lv_sqID];		// CQ ID for this SQ ID
				command_sqID <= lv_sqID;
				rg_opcode	<= nvm_command[7:0];
				rg_fuse 	<= nvm_command[9:8];
				rg_reserved0 <= nvm_command[15:10];
				rg_command_id <= nvm_command[31:16];	// DWord 0

				rg_nsid		<= nvm_command[63:32];		// Dword 1

				rg_reserved1 <= nvm_command[95:64];		// Dword 2

				rg_reserved2 <= nvm_command[127:96];	// Dword 3

				rg_mptr		<= nvm_command[191:128];	// Dword 4 & 5

				rg_prp1		<= nvm_command[255:192];	// Dword 6 & 7

				rg_prp2		<= nvm_command[319:256];	// Dword 8 & 9

				rg_cdw10    <= nvm_command[351:320];	// Dword 10

				rg_cdw11    <= nvm_command[383:352];	// Dword 11

				rg_cdw12    <= nvm_command[415:384];	// Dword 12

				rg_cdw13    <= nvm_command[447:416];	// Dword 13

				rg_cdw14    <= nvm_command[479:448];	// Dword 14

				rg_cdw15    <= nvm_command[511:480];	// Dword 15

				rg_execution_state <= ISQ_CHECK_ABORT;
			end

		sqID_fifo.deq();
		ff_internalexecQ.deq();
		end

endrule

rule rl_asq_command_execution (rg_execution_state == EXECUTE_ASQ_COMMAND);

	$display(" ########### STATE INFO #########  COMMAND EXEC STATES #### ASQ EXECUTION IN PROGRESS ");
////////////////////////////////////////////////////////////////////////////////
// Opcode : 00h
// Delete I/O Submission Q Command
////////////////////////////////////////////////////////////////////////////////

		if (rg_opcode == 8'h00) begin		// Delete I/O Submission Q
		$display(" Delete I/O Submission Q ");
		wr_update_admin_command_status <= 1;		// Ready to Update the Completion Status
				let qID = rg_cdw10[15:0];			// The QID for the associated SQ to be Deleted
				if (qID < numberA.nsqa) begin
					rg_Isq_en[qID] <= 0;			// Disable the Q
					wr_admin_success <= 1;
				end

				else begin
					wr_admin_invalid_QID <= 1;
				end
				rg_execution_state <= ASQ_COMPLETION;
				rg_CQ_DWord0 <= 0;
		end

////////////////////////////////////////////////////////////////////////////////
// Opcode : 01h
// Create I/O Submission Q Command
////////////////////////////////////////////////////////////////////////////////

		else if(rg_opcode == 8'h01) begin		// Create I/O Submission Q
		$display(" Create I/O Submission Q ");
		wr_update_admin_command_status <= 1;		// Ready to Update the Completion Status
			if (rg_fuse == 2'b00) begin		// Indicates Normal Operation

				let qID = rg_cdw10[15:0];		// The QID for the asociated SQ to be created
				if(qID < numberA.nsqa) begin
					if (rg_cdw10[31:16] < cap.mqes) begin
//						$display(" SQ %d is enabled .. it can contain commands now " , qID); // SQ 2 is ISQ 1 ... SQ0 is ASQ
//						$display(" @@@@@@@@SIZE OF THE CREATED QUEUE = %d @@@@@@@@@@", rg_cdw10[31:16]);
						rg_Isq_en[qID - 1] <= 1; 					// Enabling the Q with particular qID. i.e Creating it
						rg_ISQ_size[qID-1] <= rg_cdw10[31:16];		// Size of the Q
						rg_ISQ_base_address[qID-1] <= rg_prp1;		// NEED TO SEE MORE CDW11.PC ***** CAREFUL *****
						wr_admin_success <= 1;						// Command Executed Successfuly
						rg_cqIDofSQ[qID] <= rg_cdw11[31:16];		// Identifier of CQ to utilize for completons for this SQID..
					end
					else begin
						wr_admin_max_Q_size_exceeded <= 1;  // Command Execution FAILED
					end
				end
				else begin
					wr_admin_invalid_QID <= 1;  // Command Execution FAILED
				end
			end // end of fuse if
			rg_execution_state <= ASQ_COMPLETION;
			rg_CQ_DWord0 <= 0;
		end // end of opcode if

////////////////////////////////////////////////////////////////////////////////
// Opcode : 04h
// Delete I/O Completion Q Command
////////////////////////////////////////////////////////////////////////////////


		else if (rg_opcode == 8'h04) begin  // Delete I/O Completion Q
		$display(" Delete I/O Completion Q ");
		wr_update_admin_command_status <= 1;  // Ready to Update the Completion Status
				let qID = rg_cdw10[15:0];  // The QID for the asociated CQ to be Deleted

				if (qID < numberA.ncqa) begin
					rg_Icq_en[qID] <= 0;  // Disable the Q
					wr_admin_success <= 1;
				end

				else begin
					wr_admin_invalid_QID <= 1;
				end
				rg_execution_state <= ASQ_COMPLETION;
				rg_CQ_DWord0 <= 0;
		end

////////////////////////////////////////////////////////////////////////////////
// Opcode : 05h
// Create I/O Completion Q Command
////////////////////////////////////////////////////////////////////////////////

		else if(rg_opcode == 8'h05) begin  // Create I/O Completion Q

		$display(" Create I/O Completion Q ");
		wr_update_admin_command_status <= 1;  // Ready to Update the Completion Status

			if (rg_fuse == 2'b00) begin  // Indicates Normal Operation

				let qID = rg_cdw10[15:0];  // The QID for the asociated CQ to be created
				if(qID < numberA.ncqa) begin
					if (rg_cdw10[31:16] < cap.mqes) begin
						rg_Icq_en[qID] <= 1;  // Enabling the Q with particular qID. i.e Creating it
						rg_CQ_size[qID] <= rg_cdw10[31:16];  // Size of the Q
						rg_CQ_base_address[qID] <= rg_prp1;  // NEED TO SEE MORE CDW11.PC ***** CAREFUL *****
						wr_admin_success <= 1;

		// Interrupts for the CQs
						rg_InterruptVector[qID] <= rg_cdw11[20:16];  // Using only 5 bits out of the 16 bits specified. bcoz SP6 endpoint core suports only 32 vectors
						rg_InterruptEnable[qID] <= rg_cdw11[1];

					end
					else begin
						wr_admin_max_Q_size_exceeded <= 1;
					end
				end
				else begin
					wr_admin_invalid_QID <= 1;
				end

			end // end of fuse if
			rg_execution_state <= ASQ_COMPLETION;
			rg_CQ_DWord0 <= 0;
		end // end of opcode if

////////////////////////////////////////////////////////////////////////////////
// Opcode : 06h
//  Identify Command
////////////////////////////////////////////////////////////////////////////////

		else if(rg_opcode == 8'h06) begin
			$display(" Identify Command ");
			rg_CNS <= rg_cdw10[0];
			rg_CQ_DWord0 <= 0;
			$display(" address update @ opcode 06 ");
			rg_execution_state <= ASQ_READ_DATA_STRUCTURE;
		end

////////////////////////////////////////////////////////////////////////////////
// Ocode : 08h
// Abort Command
////////////////////////////////////////////////////////////////////////////////

		else if (rg_opcode == 8'h08) begin
			$display(" ABORT Command ");

			/*
			The Value of  5 is the max value of oustanding abort commands supported
			This value is specified in the identify controller data structure . page 55 NVMe Spec
			*/
			if (rg_no_of_outstanding_commands_to_abort == 'd5) begin
				wr_update_admin_command_status <= 1;
				wr_abort_command_limit_exceeded <= 1;
				rg_execution_state <= ASQ_COMPLETION;
			end
			else begin
				rg_no_of_outstanding_commands_to_abort <= rg_no_of_outstanding_commands_to_abort + 1;
				wr_update_admin_command_status <= 1;
				wr_admin_success <= 1;
				rg_execution_state <= ASQ_COMPLETION;
				if (abort_command_list[0] matches tagged Invalid) begin
					abort_command_list[0] <= tagged Valid CommandType {
						sqID : rg_cdw10[15:0],
						cID  : rg_cdw10[31:16]
					};
				end
				else if (abort_command_list[1] matches tagged Invalid) begin
					abort_command_list[1] <= tagged Valid CommandType{
						sqID : rg_cdw10[15:0],
						cID  : rg_cdw10[31:16]
					};
				end
				else if (abort_command_list[2] matches tagged Invalid) begin
					abort_command_list[2] <= tagged Valid CommandType {
						sqID : rg_cdw10[15:0],
						cID  : rg_cdw10[31:16]
					};
				end
				else if (abort_command_list[3] matches tagged Invalid) begin
					abort_command_list[3] <= tagged Valid CommandType {
						sqID : rg_cdw10[15:0],
						cID  : rg_cdw10[31:16]
					};
				end
				else if (abort_command_list[4] matches tagged Invalid) begin
					abort_command_list[4] <= tagged Valid CommandType {
						sqID : rg_cdw10[15:0],
						cID  : rg_cdw10[31:16]
					};
				end
			end
		end

////////////////////////////////////////////////////////////////////////////////
// Opcode : 09h
// SET FEATURES Command
////////////////////////////////////////////////////////////////////////////////

		else if (rg_opcode == 8'h09) begin  // SET Features

			$display(" SET FEATURES command ");
			let featureID = rg_cdw10[7:0];

			if(featureID == 8'h03) begin
				lbaRangeType[5:0] <= rg_cdw11[5:0];
				// The data buffer has to be obtained from the address specified in the PRP field
			end

			else begin
				rg_execution_state <= ASQ_COMPLETION;
				wr_update_admin_command_status <= 1;
				wr_admin_success <= 1;
				if(featureID == 8'h01) begin

					arbitration <=  Arbitration {
						hpw	: rg_cdw11[31:24],
						mpw	: rg_cdw11[23:16],
						lpw	: rg_cdw11[15:8],
						ab	: rg_cdw11[2:0]
						};
				end
				if(featureID == 8'h02) begin
					powerManagement[4:0] <= rg_cdw11[4:0];
				end

				if(featureID == 8'h04) begin
					temperatureThreshold[15:0] <= rg_cdw11[15:0];
				end
				if(featureID == 8'h05) begin
					errorRecovery[15:0] <= rg_cdw11[15:0];
				end
				if(featureID == 8'h06) begin
	 				volatileWriteCache[0] <= rg_cdw11[0];
				end
				if(featureID == 8'h07) begin
					numberR <= NumberOfQs_Requested {
						ncqr	: rg_cdw11[31:16],
						nsqr	: rg_cdw11[15:0]
					};
				end
				if(featureID == 8'h08) begin
					interrupt <= InterruptCoalescing {
						int_time	: rg_cdw11[15:8],
						thr		: rg_cdw11[7:0]
						};
				end
				if(featureID == 8'h09) begin
					interruptVectorConfig <= InterruptVectorConfiguration {
						cd		: rg_cdw11[16],
						iv		: rg_cdw11[15:0]
					};
				end
				if(featureID == 8'h0a) begin
					writeAtomicity[0] <= rg_cdw11[0];
				end
				if(featureID == 8'h0b) begin
					asynchronousEventConfig[15:0] <= rg_cdw11[15:0];
				end
				if(featureID == 8'h80) begin
					softwareProgressMarker[7:0] <= rg_cdw11[7:0];
				end
			end
			rg_CQ_DWord0 <= 0;
		end

////////////////////////////////////////////////////////////////////////////////
// Opcode : 0Ah
// GET FEATURES Command
////////////////////////////////////////////////////////////////////////////////

		else if (rg_opcode == 8'h0a) begin  // Get Features Command
		$display(" GET FETURES Command ");
			let featureID = rg_cdw10[7:0];


			if(featureID == 8'h03) begin
				rg_getFeatures <= True;
				rg_CQ_DWord0 <= lbaRangeType;  // There are three data structures .. each data structure is in a 4kb BRAM ...
				rg_execution_state <= ASQ_READ_DATA_STRUCTURE;
			end

			else begin
				rg_execution_state <= ASQ_COMPLETION;
				wr_update_admin_command_status <= 1;
				wr_admin_success <= 1;
				if(featureID == 8'h01) begin
					rg_CQ_DWord0 <= {arbitration.hpw,arbitration.mpw,arbitration.lpw,5'd0,arbitration.ab};
				end
				if(featureID == 8'h02) begin
					rg_CQ_DWord0 <= powerManagement;
				end

				if(featureID == 8'h04) begin
					rg_CQ_DWord0 <= temperatureThreshold;
				end
				if(featureID == 8'h05) begin
					rg_CQ_DWord0 <= errorRecovery;
				end
				if(featureID == 8'h06) begin
					rg_CQ_DWord0 <= volatileWriteCache;
				end
				if(featureID == 8'h07) begin
					rg_CQ_DWord0 <= {numberA.ncqa , numberA.nsqa};
				end
				if(featureID == 8'h08) begin
					rg_CQ_DWord0 <= {16'd0,interrupt.int_time,interrupt.thr};
				end
				if(featureID == 8'h09) begin
					rg_CQ_DWord0 <= {15'd0,interruptVectorConfig.cd, interruptVectorConfig.iv};
				end
				if(featureID == 8'h0a) begin
					rg_CQ_DWord0 <= writeAtomicity;
				end
				if(featureID == 8'h0b) begin
					rg_CQ_DWord0 <= asynchronousEventConfig;
				end
				if(featureID == 8'h80) begin
					rg_CQ_DWord0 <= softwareProgressMarker;
				end
			end
		end
endrule

Reg#(Bit#(2)) rg_completion_count <- mkReg(0);
Reg#(Bool) rg_sending_completion <- mkReg(False);

rule rl_asq_completion (rg_execution_state == ASQ_COMPLETION);

	$display(" ########### STATE INFO ######### COMMAND EXEC STATES #### ASQ COMPLETION ");

	rg_Start_read_Data_Structure <= False; // Done with Reading the Data Structures

	if (rg_sending_completion == False) begin
		wr_req_write_completion <= 1;
			if (rg_req_write_completion_accepted == 1) begin
				rg_sending_completion <= True;
				rg_address_to_pcie <= {acq.acqb,12'd0};  // The base address of the Admin Completion Queue


		//		if(rg_InterruptEnable[0] == 1) begin				// The interrupt vector would be sent ONLY after the Completion TLp has been completely sent .. This is taken care in the PCIe Controller ... This is just to indicate that the vector is READY .. After Sending the TLP over the PCIe LAne the Completor Will generate another control signal to activate MSI interupt
//					let vector_number = rg_InterruptVector[0];
					if(mask_reg[0] == 0) begin
						status_reg[0] <= 1;
						$display("++++++++++++++++++++++Status Register made HIGH ++++++++++++++++++++++++");
					end
				//end
			end
	end
	else begin
		if(wr_send_valid_data == 1 && wr_wait == 0) begin
			wr_data_valid <= 1;
			if(rg_completion_count == 'b00) begin
				$display(" CQ DWORD ---- 0 sent ");
				rg_data_to_pcie <= 	rg_CQ_DWord0;
				rg_completion_count <= 'b01;
			end

			else if(rg_completion_count == 'b01) begin
				$display(" CQ DWORD ---- 1 sent ");
				rg_data_to_pcie <= 	rg_CQ_DWord1;
				rg_completion_count <= 'b10;
			end

			else if(rg_completion_count == 'b10) begin
				$display(" CQ DWORD ---- 2 sent ");
				rg_data_to_pcie <= {rg_CQ_DWord2.sqID,rg_CQ_DWord2.sqHeadPointer};
				rg_completion_count <= 'b11;
			end

			else if(rg_completion_count == 'b11) begin
				$display(" CQ DWORD ---- 3 sent ");
				rg_data_to_pcie <= {
					rg_CQ_DWord3.status_field_DNR,
					rg_CQ_DWord3.status_field_M,
					rg_CQ_DWord3.status_field_res,
					rg_CQ_DWord3.status_field_SCT,
					rg_CQ_DWord3.status_field_SC,
					rg_CQ_DWord3.phase_tag,
					rg_CQ_DWord3.commandID
				};
				rg_completion_count <= 'b00;

				send_completion_done <= 1;
				rg_execution_state <= EXECUTION_IDLE;
				rg_sending_completion <= False;
			end

		end
	end

endrule

/*
ACQUIRE_IDLE,
ACQUIRING_COMMAND,
ACQUIRING_DATA,
ACQUIRED_DATA,
ACQUIRED_COMMAND
*/

Reg#(Command_and_data_acquire_states) rg_acquire_state <- mkReg(ACQUIRE_IDLE);
Reg#(bit) rg_buffer_full <- mkReg(0); // To indicate that the data has been filled into buffers

rule rl_acquire_idle (rg_acquire_state == 	ACQUIRE_IDLE);
	$display(" ############## STATE INFO ####### ACQUIRE STATES #### ACQUIRE IDLE ");
	wr_commandEn <= 0;
	rg_buffer_full <= 0;
	if(wr_tag == 'd1)  begin
		rg_acquire_state <= ACQUIRING_COMMAND;
	end

	else if(wr_tag == 'd2) begin
		rg_acquire_state <= ACQUIRING_DATA;
	end
endrule

rule rl_acquiring_command (	rg_acquire_state == ACQUIRING_COMMAND);
	$display(" ############## STATE INFO ####### ACQUIRE STATES #### ACQUIRING COMMAND ");
		if (wr_tag == 'd1 && wr_completionDataValid == 1) begin
			$display(" Receiving Command ");
			rg_command_buf[rg_addr_for_cmd_bufr] <= wr_completionDataIn;

			if(wr_completionLastDWord == 1) begin
				$display(" Last DWord Received ");
				rg_addr_for_cmd_bufr <= 0;
				rg_command_ready <= 1;
				rg_acquire_state <= ACQUIRED_COMMAND;
			end
			else begin
				rg_addr_for_cmd_bufr <= rg_addr_for_cmd_bufr + 1;
			end

		end

		else if(wr_tag == 'd2) begin
			rg_acquire_state <= ACQUIRING_DATA;
		end
		else if(wr_tag == 'd0) begin
			rg_acquire_state <= ACQUIRE_IDLE;
		end
endrule

rule rl_acquiring_data (rg_acquire_state == ACQUIRING_DATA);
	$display(" ############## STATE INFO ####### ACQUIRE STATES #### ACQUIRING DATA ");
	if(wr_tag == 'd2 && wr_completionDataValid == 1) begin

		write_data_buffer.portA.request.put(BRAMRequest {
			write : True,
			address: rg_addr_for_write_bufr,
			datain :  wr_completionDataIn,
			responseOnWrite : False
		});
		if(wr_completionLastDWord == 1) begin
			rg_addr_for_write_bufr <= 0;
			rg_acquire_state <= ACQUIRED_DATA;
		end
		else begin
			rg_addr_for_write_bufr <= rg_addr_for_write_bufr + 1;
		end
	end

	else if(wr_tag == 'd1) begin
		rg_acquire_state <= ACQUIRING_COMMAND;
	end
	else if(wr_tag == 'd0) begin
		rg_acquire_state <= ACQUIRE_IDLE;
	end

	endrule

rule rl_acquired_command (rg_acquire_state == ACQUIRED_COMMAND);
	$display(" ############## STATE INFO ####### ACQUIRE STATES #### ACQUIRED COMMAND ");
	wr_commandEn <= 1;
	wr_command_In <= {rg_command_buf[15],rg_command_buf[14],rg_command_buf[13],rg_command_buf[12],rg_command_buf[11],rg_command_buf[10],rg_command_buf[9],rg_command_buf[8],rg_command_buf[7],rg_command_buf[6],rg_command_buf[5],rg_command_buf[4],rg_command_buf[3],rg_command_buf[2],rg_command_buf[1],rg_command_buf[0]};
	rg_command_ready <= 0;
	$display(" opcode = %d " , rg_command_buf[0]);
	rg_acquire_state <= ACQUIRE_IDLE;
endrule

rule rl_acquired_data (	rg_acquire_state == ACQUIRED_DATA);
	$display(" ############## STATE INFO ####### ACQUIRE STATES #### ACQUIRED DATA ");
	rg_buffer_full <= 1;
	rg_acquire_state <= ACQUIRE_IDLE;
endrule

/******************************************************************************/
/******************************************************************************/

/*
PCIe_REQ_IDLE,
PCIe_REQ_WRITE_DATA,
PCIe_REQ_WRITE_COMPLETION,
PCIe_SEND_INFO
*/

Reg#(Pcie_Request_States) rg_pcie_req_state <- mkReg(	PCIe_REQ_IDLE);

rule rl_req_idle (rg_pcie_req_state == PCIe_REQ_IDLE);

$display(" ########### STATE INFORMATION ###### PCIe REQ STATES #### PCIe_REQ_IDLE ");
	send_data_done <= 0;
	send_completion_done <= 0;

	rg_send_read_tlp <= 0;
	rg_send_write_tlp <= 0;
	rg_requested_tag <= 'd0;
	rg_payload_length <= 'd0;

	if (wr_pcie_busy == False) begin
		if (wr_req_write_completion == 1) begin
			rg_req_write_completion_accepted <= 1;
			rg_pcie_req_state <= PCIe_SEND_WRITE_COMPLETION_INFO;
		end

		else if (wr_req_write_data == 1) begin
			rg_req_write_data_accepted <= 1;
			rg_pcie_req_state <= PCIe_SEND_WRITE_DATA_INFO;
		end

		else if (wr_req_read_data == 1) begin
			rg_req_read_data_accepted <= 1;
			rg_pcie_req_state <= PCIe_SEND_READ_DATA_INFO;
		end

		else if (wr_req_read_command == 1) begin
			rg_req_read_command_accepted <= 1;
			rg_pcie_req_state <= PCIe_SEND_READ_COMMAND_INFO;
		end

		else begin
			rg_req_read_command_accepted <= 0;
			rg_req_read_data_accepted <= 0;
			rg_req_write_data_accepted <= 0;
			rg_req_write_completion_accepted <= 0;
		end
	end

	else begin
		rg_req_read_command_accepted <= 0;
		rg_req_read_data_accepted <= 0;
		rg_req_write_data_accepted <= 0;
		rg_req_write_completion_accepted <= 0;
	end

endrule

rule rl_pcie_req_wait_state (rg_pcie_req_state == PCIe_WAIT_STATE);
	rg_pcie_req_state <= PCIe_REQ_IDLE;
endrule

rule rl_pcie_send_read_command_info (rg_pcie_req_state == PCIe_SEND_READ_COMMAND_INFO);
	$display(" ########### STATE INFORMATION ###### PCIe REQ STATES #### PCIe_SEND_READ_COMMAND_INFO ");
	//		rg_req_read_command_accepted <= 0;
	rg_send_read_tlp <= 1;
	rg_requested_tag <= 'd1;
	rg_payload_length <= 'd16;
	rg_pcie_req_state <= PCIe_WAIT_STATE;
endrule

rule rl_pcie_send_read_data_info (rg_pcie_req_state == PCIe_SEND_READ_DATA_INFO);
	$display(" ########### STATE INFORMATION ###### PCIe REQ STATES #### PCIe_SEND_READ_DATA_INFO ");
	//		rg_req_read_data_accepted <= 0;
	rg_send_read_tlp <= 1;
	rg_requested_tag <= 'd2;
	rg_payload_length <= 'd1023;
	rg_pcie_req_state <= PCIe_WAIT_STATE;
endrule

rule rl_pcie_send_write_data_info (rg_pcie_req_state ==  PCIe_SEND_WRITE_COMPLETION_INFO);
	$display(" ########### STATE INFORMATION ###### PCIe REQ STATES #### PCIe_SEND_WRITE_COMPLETION_INFO ");
	rg_send_write_tlp <= 1;
	rg_requested_tag <= 'd0;
	rg_payload_length <= 'd4;
	rg_pcie_req_state <= PCIe_REQ_WRITE_COMPLETION;
endrule

rule rl_pcie_send_write_completion (rg_pcie_req_state == PCIe_SEND_WRITE_DATA_INFO);
	$display(" ########### STATE INFORMATION ###### PCIe REQ STATES #### PCIe_SEND_WRITE_DATA_INFO ");
	rg_send_write_tlp <= 1;
	rg_requested_tag <= 'd0;
	rg_payload_length <= 'd1023;
	rg_pcie_req_state <= PCIe_REQ_WRITE_DATA;
endrule

rule rl_send_compeltion (rg_pcie_req_state == PCIe_REQ_WRITE_COMPLETION);
	$display(" ########### STATE INFORMATION ###### PCIe REQ STATES #### PCIe_REQ_WRITE_COMPLETION ");
	if(send_completion_done == 1) begin
		rg_pcie_req_state <= PCIe_REQ_IDLE;
	end
endrule

rule rl_send_data (rg_pcie_req_state == PCIe_REQ_WRITE_DATA);
	$display(" ########### STATE INFORMATION ###### PCIe REQ STATES #### PCIe_REQ_WRITE_DATA ");
	if(send_data_done == 1) begin
		rg_pcie_req_state <= PCIe_REQ_IDLE;
	end
endrule


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Command Execution State Machine																												 //
// State Machine Description on the basis of rules :																							 //
//																																				 //
//				RULE									STATE DESCRIPTION									FUNCTIONALITY						 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//			rl_isq_check_abort							ISQ CHECK FOR ABORT							1. Checks for the command to be aborted		 //
//																									2. If it is tagged valid then there is a 	 //
//																										command to be aborted					 //
//																									3. Command to be aborted is identified uniquely//
//																										by sqID and commandID					 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//			rl_isq_completion							ISQ COMPLETION								Isq Completion								 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//			rl_isq_execution							ISQ COMMAND EXECUTION						1. Write Command							 //
//																									2. Read Command								 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//			rl_initiate_write_to_nand					INITIATE WRITE TO NAND						starts Nand Control State machine to write	 //
// ----------------------------------------------------------------------------------------------------------------------------------------------//
//			rl_initiate_read_to_nand					INITIATE READ TO NAND						starts Nand Control State machine to read	 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

rule rl_isq_check_abort (rg_execution_state == ISQ_CHECK_ABORT);

	$display(" ########################## STATE INFO ############### CHECKING ISQ ABORT ######################## ");

	if(abort_command_list[0] matches tagged Valid .commandType &&&
		(commandType.sqID == command_sqID  && commandType.cID == rg_command_id)) begin

		$display(" Command Aborted ");
		wr_update_nvm_command_status <= 1;
		wr_command_got_aborted <= 1;
		abort_command_list[0] <= tagged Invalid;
		rg_no_of_outstanding_commands_to_abort <= rg_no_of_outstanding_commands_to_abort - 1;
		rg_execution_state <= ISQ_COMPLETION;
	end

	else if(abort_command_list[1] matches tagged Valid .commandType &&&
		(commandType.sqID == command_sqID  && commandType.cID == rg_command_id)) begin

		$display(" Command Aborted ");
		wr_update_nvm_command_status <= 1;
		wr_command_got_aborted <= 1;
		abort_command_list[1] <= tagged Invalid;
		rg_no_of_outstanding_commands_to_abort <= rg_no_of_outstanding_commands_to_abort - 1;
		rg_execution_state <= ISQ_COMPLETION;
	end

	else if(abort_command_list[2] matches tagged Valid .commandType &&&
		(commandType.sqID == command_sqID  && commandType.cID == rg_command_id)) begin

		$display(" Command Aborted ");
		wr_update_nvm_command_status <= 1;
		wr_command_got_aborted <= 1;
		abort_command_list[2] <= tagged Invalid;
		rg_no_of_outstanding_commands_to_abort <= rg_no_of_outstanding_commands_to_abort - 1;
		rg_execution_state <= ISQ_COMPLETION;
	end

	else if(abort_command_list[3] matches tagged Valid .commandType &&&
		(commandType.sqID == command_sqID  && commandType.cID == rg_command_id)) begin

		$display(" Command Aborted ");
		wr_update_nvm_command_status <= 1;
		wr_command_got_aborted <= 1;
		abort_command_list[3] <= tagged Invalid;
		rg_no_of_outstanding_commands_to_abort <= rg_no_of_outstanding_commands_to_abort - 1;
		rg_execution_state <= ISQ_COMPLETION;
	end

	else if(abort_command_list[4] matches tagged Valid .commandType &&&
		(commandType.sqID == rg_command_sqID_to_be_processed  && commandType.cID == rg_command_id)) begin

		$display(" Command Aborted ");
		wr_update_nvm_command_status <= 1;
		wr_command_got_aborted <= 1;
		abort_command_list[4] <= tagged Invalid;
		rg_no_of_outstanding_commands_to_abort <= rg_no_of_outstanding_commands_to_abort - 1;
		rg_execution_state <= ISQ_COMPLETION;
	end

	else begin
		$display(" ######### COMMAND NOT ABORTED ################");
		rg_execution_state <= EXECUTE_ISQ;
	end

endrule

rule rl_isq_completion (rg_execution_state == ISQ_COMPLETION);

	if(rg_sending_completion == False) begin
		wr_req_write_completion <= 1;
		if (rg_req_write_completion_accepted == 1) begin
			rg_sending_completion <= True;
			rg_address_to_pcie <= rg_CQ_base_address[rg_cqID]; // The I/O Completion Queue Address

			// The register rg_cqID must be updated while executing the particular Command in the associated SQ
			$display(" address update @ IO Command Completion ");
			if(rg_InterruptEnable[rg_cqID] == 1) begin
				rg_vector_rdy <= 1;
				rg_vector_number <= rg_InterruptVector[rg_cqID];
				let vector_number = rg_InterruptVector[rg_cqID];
				if(mask_reg[vector_number] == 0) begin
					status_reg[vector_number] <= 1;
				end
			end
		end
	end


	else begin
		if(wr_send_valid_data == 1 && wr_wait == 0) begin
			wr_data_valid <= 1;
			if(rg_completion_count == 'b00) begin
				$display(" CQ DWORD ---- 0 sent ");
				rg_data_to_pcie <= 	rg_CQ_DWord0;
				rg_completion_count <= 'b01;
			end

			else if(rg_completion_count == 'b01) begin
				$display(" CQ DWORD ---- 1 sent ");
				rg_data_to_pcie <= 	rg_CQ_DWord1;
				rg_completion_count <= 'b10;
			end

			else if(rg_completion_count == 'b10) begin
				$display(" CQ DWORD ---- 2 sent ");
				rg_data_to_pcie <= {rg_CQ_DWord2.sqID,rg_CQ_DWord2.sqHeadPointer };
				rg_completion_count <= 'b11;
			end

			else if(rg_completion_count == 'b11) begin
				$display(" CQ DWORD ---- 3 sent ");
				rg_data_to_pcie <= {
					rg_CQ_DWord3.status_field_DNR,
					rg_CQ_DWord3.status_field_M,
					rg_CQ_DWord3.status_field_res,
					rg_CQ_DWord3.status_field_SCT,
					rg_CQ_DWord3.status_field_SC,
					rg_CQ_DWord3.phase_tag,
					rg_CQ_DWord3.commandID
				};
				rg_completion_count <= 'b00;

				send_completion_done <= 1;
				rg_execution_state <= EXECUTION_IDLE;
				rg_sending_completion <= False;
			end

		end
	end
endrule


/*

Reg#(bit) lookup_successful <- mkReg(0);


Reg#(int) temp <- mkReg(0);
Reg#(int) nfc_num <- mkReg(0);
Reg#(Bit#(64))	logical_addr[1024];
Reg#(int) nfc[1024];
Reg#(int) nfc_temp <- mkReg(0);

rule rl_pre_execute_isq_lkup (rg_execution_state == PRE_EXECUTE_ISQ_LKUP);

		for (int i=0;i<1024;i=i+1) begin
				if (logical_addr[i] != 64'b0) begin
					if(logical_addr[i] == {rg_cdw11 , rg_cdw10}) begin
						nfc_num <= nfc[i];
						lookup_successful <= 1;
						rg_execution_state <= PRE_EXECUTE_ISQ_DISPATCH;
					end
				end

				if (lookup_successful == 0) begin
					logical_addr[i] <= {rg_cdw11 , rg_cdw10};
					nfc[i] <= temp;
					nfc_temp <= temp;
					if (temp == 7)
					temp <= 0;
					else
					temp <= temp + 1;

					lookup_successful <= 0;
					rg_execution_state <= PRE_EXECUTE_ISQ_DISPATCH;
				end
		end
endrule

rule rl_pre_isq_execution (rg_execution_state == PRE_EXECUTE_ISQ_DISPATCH);
	if (lookup_successful == 1) begin
		case (nfc_num)
			0:rg_execution_state <= EXECUTE_ISQ_CHANNEL0;
			1:rg_execution_state <= EXECUTE_ISQ_CHANNEL1;
			2:rg_execution_state <= EXECUTE_ISQ_CHANNEL2;
			3:rg_execution_state <= EXECUTE_ISQ_CHANNEL3;
			4:rg_execution_state <= EXECUTE_ISQ_CHANNEL4;
			5:rg_execution_state <= EXECUTE_ISQ_CHANNEL5;
			6:rg_execution_state <= EXECUTE_ISQ_CHANNEL6;
			7:rg_execution_state <= EXECUTE_ISQ_CHANNEL7;
			default:rg_execution_state <= EXECUTE_ISQ;
			endcase
		lookup_successful <= 0;
	end
	else begin
		case (nfc_temp)
			0:rg_execution_state <= EXECUTE_ISQ_CHANNEL0;
			1:rg_execution_state <= EXECUTE_ISQ_CHANNEL1;
			2:rg_execution_state <= EXECUTE_ISQ_CHANNEL2;
			3:rg_execution_state <= EXECUTE_ISQ_CHANNEL3;
			4:rg_execution_state <= EXECUTE_ISQ_CHANNEL4;
			5:rg_execution_state <= EXECUTE_ISQ_CHANNEL5;
			6:rg_execution_state <= EXECUTE_ISQ_CHANNEL6;
			7:rg_execution_state <= EXECUTE_ISQ_CHANNEL7;
			default:rg_execution_state <= EXECUTE_ISQ;
		endcase
	end
endrule

*/



////////////////////////////////////////////////////////////////////////////////
// Registers for NVM to NAND Interface Signals
////////////////////////////////////////////////////////////////////////////////

Reg#(Bit#(64)) rg_flush_data_from <- mkReg(0);
Reg#(Bit#(64)) rg_write_data_from <- mkReg(0);
Reg#(Bit#(64)) rg_transfer_read_data_to <- mkReg(0);
Reg#(Bit#(64)) rg_logical_block_address <- mkReg(0);
Reg#(Bool) rg_initiate_flush <- mkReg(False);
Reg#(Bool) rg_initiate_write0 <- mkReg(False);
Reg#(Bool) rg_initiate_read0 <- mkReg(False);
Reg#(Bool) rg_initiate_write1 <- mkReg(False);
Reg#(Bool) rg_initiate_read1 <- mkReg(False);

Reg#(Bool) rg_request_pcie_to_send_data <- mkReg(False);
Wire#(bit) wr_data_received <- mkDWire(0);
Reg#(Bool) rg_write_to_nand_successful0 <- mkReg(False);
Reg#(Bool) rg_read_from_nand_successful0 <- mkReg(False);

Reg#(int) rg_nfc_temp <- mkReg(0);
////////////////////////////////////////////////////////////////////////////////

rule rl_isq_execution (rg_execution_state == EXECUTE_ISQ);

	$display(" ******* NVM Command Execution ****** ");

	if (rg_nfc_temp == 1)
		rg_nfc_temp <= 0;
	else
		rg_nfc_temp <= rg_nfc_temp + 1;

	if(rg_opcode == 'h00) begin // Flush
			rg_flush_data_from <= rg_prp1;
			rg_logical_block_address <= {rg_cdw11 , rg_cdw10};
		//	rg_initiate_flush <= True;
		end

		if(rg_opcode == 'h01) begin  // Write
		$display(" ****NVM EXPRESS ************* *************** WRITE COMMAND ******* ************************");
		$display(" ************** Data is to be taken from location = %d ", rg_prp1);
			rg_write_data_from <= rg_prp1;
			rg_logical_block_address <= {rg_cdw11 , rg_cdw10};
			if(wr_data_received == 0)
				rg_request_pcie_to_send_data <= True;
			else if (wr_data_received == 1) begin
				$display(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TRANSITION FROM WRITE TO >>>> INITIATE WRITE >>>>>>>. ");
				rg_request_pcie_to_send_data <= False;
				if (rg_nfc_temp == 0) begin
					rg_execution_state <= INITIATE_WRITE_TO_NAND0;
					rg_initiate_write0 <= True;
				end
				else begin
					rg_execution_state <= INITIATE_WRITE_TO_NAND1;
					rg_initiate_write1 <= True;
				end
			end
		end

		if(rg_opcode == 'h02) begin // Read
		$display(" ******* NVM EXPRESS **************************** READ COMMAND ********************************");
		$display(" Transfer the Read Data to memory location %d " , rg_prp1);
			rg_transfer_read_data_to <= rg_prp1;
			rg_logical_block_address <= {rg_cdw11 , rg_cdw10};
			if (rg_nfc_temp == 0) begin
				rg_execution_state <= INITIATE_READ_FROM_NAND0;
				rg_initiate_read0 <= True;
			end
			else  begin
				rg_execution_state <= INITIATE_READ_FROM_NAND1;
				rg_initiate_read1 <= True;
			end
		end

endrule

rule rl_initiate_write_to_nand0 (rg_execution_state == INITIATE_WRITE_TO_NAND0);

	$display("######  NVM EXPRESS  CHANNEL0 #####STATE INFO ########### INITIATE WRITE ######################## ");
	if(rg_write_to_nand_successful0 == True) begin
		$display(" @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ NAND DATA SUCCESSFULLY SENT - CHANNEL0 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ");
		rg_write_to_nand_successful0 <= False;
		wr_nvm_success <= 1;
		wr_update_nvm_command_status <= 1;
		rg_execution_state <= ISQ_COMPLETION;
	end
endrule

rule rl_initiate_read_from_nand0 (rg_execution_state == INITIATE_READ_FROM_NAND0);

	$display("######  NVM EXPRESS  CHANNEL0 #####STATE INFO ########### INITIATE READ ######################## ");
	if(rg_read_from_nand_successful0 == True) begin
		$display(" @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ NAND DATA SUCCESSFULLY READ - CHANNEL0 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ");
		rg_read_from_nand_successful0 <= False;
		wr_nvm_success <= 1;
		wr_update_nvm_command_status <= 1;
		rg_execution_state <= ISQ_COMPLETION;
	end
endrule

//CHANNEL1 related

Reg#(Bool) rg_write_to_nand_successful1 <- mkReg(False);
Reg#(Bool) rg_read_from_nand_successful1 <- mkReg(False);

rule rl_initiate_write_to_nand1 (rg_execution_state == INITIATE_WRITE_TO_NAND1);
	$display("######  NVM EXPRESS CHANNEL1 #####STATE INFO ########### INITIATE WRITE ######################## ");
	if(rg_write_to_nand_successful1 == True) begin
		$display(" @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ NAND DATA SUCCESSFULLY SENT - CHANNEL1 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ");
		rg_write_to_nand_successful1 <= False;
		wr_nvm_success <= 1;
		wr_update_nvm_command_status <= 1;
		rg_execution_state <= ISQ_COMPLETION;
	end
endrule

rule rl_initiate_read_from_nand1 (rg_execution_state == INITIATE_READ_FROM_NAND1);
	$display("######  NVM EXPRESS  CHANNEL1 #####STATE INFO ########### INITIATE READ ######################## ");
	if(rg_read_from_nand_successful1 == True) begin
		$display(" @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ NAND DATA SUCCESSFULLY READ - CHANNEL1 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ");
		rg_read_from_nand_successful1 <= False;
		wr_nvm_success <= 1;
		wr_update_nvm_command_status <= 1;
		rg_execution_state <= ISQ_COMPLETION;
	end
endrule

////////////////////////////////////////////////////////////////////////////////
// Rules For Admin Command Completions
////////////////////////////////////////////////////////////////////////////////


rule rl_command_status_update (wr_update_admin_command_status == 1 || wr_update_nvm_command_status == 1);

	if(wr_update_admin_command_status == 1) begin
		$display(" Sending Admin Command Completion ");
		rg_CQ_DWord2 <= CQ_DWord2 {
			sqID : 0 ,
			sqHeadPointer : sqhdbl[0]
		};
	end

	else if (wr_update_nvm_command_status == 1) begin
		rg_CQ_DWord2 <= CQ_DWord2 {
			sqID : command_sqID,
			sqHeadPointer : sqhdbl[command_sqID]
		};
	end

	if(wr_admin_success == 1 || wr_nvm_success == 1) begin
		rg_CQ_DWord3 <= CQ_DWord3 {
			status_field_DNR : 0 , // NA
			status_field_M   : 0 , // No more Error log info
			status_field_res : 0 , // Reserved
			status_field_SCT : 0 , // Generic Command Status
			status_field_SC  : 0 , // Successful Completion
			phase_tag		 : 0 , // Phase Tag set to 0
			commandID		 : rg_command_id  // Command ID provided by the HOST S/W
		};
	end

	else if(wr_admin_max_Q_size_exceeded == 1) begin
		rg_CQ_DWord3 <= CQ_DWord3 {
			status_field_DNR : 1 , // Do Not Retry With The Same Command
			status_field_M   : 0 , // No more Error log info
			status_field_res : 0 , // Reserved
			status_field_SCT : 1 , // Command Specific Status
			status_field_SC  : 2 , // Maximum Queue Size Exceeded
			phase_tag		 : 0 , // Phase Tag set to 0
			commandID		 : rg_command_id  // Command ID provided by the HOST S/W
		};

	end

	else if(wr_admin_invalid_QID == 1) begin
		rg_CQ_DWord3 <= CQ_DWord3 {
			status_field_DNR : 1 , // Do Not Retry With The Same Command
			status_field_M   : 0 , // No more Error log info
			status_field_res : 0 , // Reserved
			status_field_SCT : 1 , // Command Specific Status
			status_field_SC  : 1 , // Invalid Queue Identifier
			phase_tag		 : 0 , // Phase Tag set to 0
		 	commandID		 : rg_command_id  // Command ID provided by the HOST S/W
		};

	end

	else if(wr_abort_command_limit_exceeded == 1) begin
		rg_CQ_DWord3 <= CQ_DWord3 {
			status_field_DNR : 1 , // Do Not Retry With The Same Command
			status_field_M   : 0 , // No more Error log info
			status_field_res : 0 , // Reserved
			status_field_SCT : 1 , // Command Specific Status
			status_field_SC  : 3 , // Abort Command Limit Exceeded
			phase_tag		 : 0 , // Phase Tag set to 0
		 	commandID		 : rg_command_id  // Command ID provided by the HOST S/W
		};

	end

endrule

////////////////////////////////////////////////////////////////////////////////
// Rules to send Control Signals to NAND Flash Controller (TN-29-06 Micron NAND Flash Controller Spartan-3)
////////////////////////////////////////////////////////////////////////////////

// Registers and Wires for NAND Flash Interface
Reg#(Bit#(12))	rg_address_to_nand0 	<- mkReg(0);	// address
Reg#(Bit#(32))	rg_data_to_nand0 	<- mkReg(0);	// data out
Wire#(Bit#(32))	wr_data_from_nand0 	<- mkDWire(0);	// data in
Reg#(bit)		rg_nand_ce_l0		<- mkReg(1);	// active low
Reg#(bit)		rg_nand_we_l0		<- mkReg(1);	// active low
Reg#(bit)		rg_nand_oe_l0 		<- mkReg(1);	// active low
Reg#(bit)		rg_nand_reset_l0 	<- mkReg(1);	// active low
Wire#(bit)		wr_interrupt0		<- mkDWire(0);	// active high
Wire#(bit)		wr_ready_busy_l0		<- mkDWire(1);	// active low

// Internal Registers for NAND flash interface Control Signals


Reg#(Nand_control_states) rg_nand_state0 <- mkReg(DEFAULT);
Reg#(Data_transfer_states) rg_data_transfer_state0 <- mkReg(DO_NOTHING);

Reg#(bit) rg_r0 <- mkReg(0);
Reg#(bit) rg_w0 <- mkReg(0);

Reg#(Bool) rg_start_data_transfer_to_nand0 <- mkReg(False);

rule rl_initiate_read (rg_initiate_read0 == True);
	$display(" **************SENDING READ REQUEST TO NAND FLASH - CHANNEL0 ************ ");
	if (wr_ready_busy_l0 == 0) begin
//		$display(" **************SENDING READ REQUEST TO NAND FLASH************ ");
		rg_r0 <= 1; // READ
		rg_nand_state0 <= OPCODE_PART1;
		rg_initiate_read0 <= False;
	end
endrule

rule rl_initiate_write (rg_initiate_write0 == True);
	if (wr_ready_busy_l0 == 0) begin
		$display(" **************SENDING WRITE REQUEST TO NAND FLASH - CHANNEL0 ************ ");
		rg_w0 <= 1; // WRITE
		rg_nand_state0 <= OPCODE_PART1;
		rg_initiate_write0 <= False;
	end

endrule

rule rl_opcode1 (rg_nand_state0 == OPCODE_PART1);
	$display(" ***************OPCODE 1 SENT*****************");
	rg_nand_ce_l0 <= 0;
	rg_nand_we_l0 <= 0;
	rg_nand_oe_l0 <= 1;
	rg_address_to_nand0 <= 'hFFA;
	if(rg_r0 == 1)
		rg_data_to_nand0 <= 'h00;
	else if(rg_w0 == 1)
		rg_data_to_nand0 <= 'h80;

	rg_nand_state0 <= ADDRESS_PART1;
endrule

rule rl_addr1 (rg_nand_state0 == ADDRESS_PART1);
	$display(" ***************ADDRESS 1 SENT*****************");
	rg_nand_ce_l0 <= 0;
	rg_nand_we_l0 <= 0;
	rg_nand_oe_l0 <= 1;
	rg_address_to_nand0 <= 'hFF4;
	rg_data_to_nand0 <= {'d0,rg_logical_block_address[7:0]};
	rg_nand_state0 <= ADDRESS_PART2;
endrule

rule rl_addr2 (rg_nand_state0 == ADDRESS_PART2);
	$display(" ***************ADDRESS 2 SENT*****************");
	rg_nand_ce_l0 <= 0;
	rg_nand_we_l0 <= 0;
	rg_nand_oe_l0 <= 1;
	rg_address_to_nand0 <= 'hFF5;
	rg_data_to_nand0 <= {'d0,rg_logical_block_address[15:8] };
	rg_nand_state0 <= ADDRESS_PART3;
endrule

rule rl_addr3 (rg_nand_state0 == ADDRESS_PART3);
	$display(" ***************ADDRESS 3 SENT*****************");
	rg_nand_ce_l0 <= 0;
	rg_nand_we_l0 <= 0;
	rg_nand_oe_l0 <= 1;
	rg_address_to_nand0 <= 'hFF6;
	rg_data_to_nand0 <= {'d0 ,rg_logical_block_address[24:16] };

	if(rg_r0 == 1) begin
		rg_nand_state0 <= OPCODE_PART2;
	end else if (rg_w0 == 1) begin
		rg_nand_state0 <= DEFAULT;
		rg_addr_for_write_bufr <= 0;
		rg_start_data_transfer_to_nand0 <= True;
	end
endrule

rule rl_opcode2 (rg_nand_state0 == OPCODE_PART2);
	$display(" ***************OPCODE 2 SENT*****************");
	rg_nand_ce_l0 <= 0;
	rg_nand_we_l0 <= 0;
	rg_nand_oe_l0 <= 1;
	rg_address_to_nand0 <= 'hFFA;
	if(rg_r0 == 1) begin
		rg_r0 <= 0;
		rg_data_to_nand0 <= 'h30;
		rg_data_transfer_state0 <= DO_NOTHING;
	end
	else if (rg_w0 == 1) begin
		rg_data_to_nand0 <= 'h10;
		rg_w0 <= 0;
	end
	rg_write_to_nand_successful0 <= True;
	rg_nand_state0 <= DEFAULT;
endrule

rule rl_default (rg_nand_state0 == DEFAULT);

	$display(" *********** NAND CONTROL SIGNAL STATE CHANNEL0 = DEFAULT ********** ");
	if (rg_r0 == 1) begin
		rg_nand_ce_l0 <= 0;
		rg_nand_we_l0 <= 1;
		rg_nand_oe_l0 <= 0; // READ ENABLE
	end
	else if (rg_w0 == 1) begin
		rg_nand_ce_l0 <= 0;
		rg_nand_we_l0 <= 0;
		rg_nand_oe_l0 <= 1; // WRITE ENABLE
	end
	else begin
		rg_nand_ce_l0 <= 0;
		rg_nand_we_l0 <= 1;
		rg_nand_oe_l0 <= 1;	// NO ENABLE
	end
endrule

Reg#(Bool) rg_supply_initial_address <- mkReg(False);
Reg#(Bool) rg_waiting_for_data <- mkReg(False);

rule rl_request_data_frm_pcie0 (rg_data_transfer_state0 == REQUEST_DATA_FROM_PCIe);
	$display(" ************DATA TRANSFER STATE = REQUEST_DATA_FROM_PCIe ************ ") ;

	if (rg_waiting_for_data == False) begin
		wr_req_read_data <= 1;
		if(rg_req_read_data_accepted == 1) begin
			rg_waiting_for_data <= True;
			rg_address_to_pcie <= rg_write_data_from;
		end
	end

	else begin
		rg_waiting_for_data <= False;
		rg_data_transfer_state0 <= READ_DATA_FROM_PCIe;
		rg_supply_initial_address <= True;
	end
endrule

Reg#(Bool) rg_sending_data_to_pcie <- mkReg(False);

rule rl_request_data_to_pcie (rg_data_transfer_state0 == REQUEST_DATA_TO_PCIe);
	$display(" ************DATA TRANSFER STATE = REQUEST_DATA_TO_PCIe ************ ");

	if(rg_sending_data_to_pcie == False) begin
		wr_req_write_data <= 1;
		if(rg_req_write_data_accepted == 1) begin
			rg_sending_data_to_pcie <= True;
			rg_address_to_pcie <= rg_transfer_read_data_to;
		end
	end

	else begin
		rg_sending_data_to_pcie <= False;
		rg_data_transfer_state0 <= WRITE_DATA_TO_PCIe;
		rg_supply_initial_address <= True;
	end
endrule

//Reg#(Bool) read_data_from_buffer <- mkReg(False);

rule rl_read_data_from_pcie (rg_data_transfer_state0 == READ_DATA_FROM_PCIe);
	$display(" ************DATA TRANSFER STATE = READ_DATA_FROM_PCIe************ ");
	if(rg_buffer_full == 1) begin
		wr_data_received <= 1;
		rg_data_transfer_state0 <= DO_NOTHING;
//				read_data_from_buffer <= True;
	end
endrule

rule rl_data_transfer_to_nand0 (rg_start_data_transfer_to_nand0 == True);
	$display(" @@@@@@@@@@@@@@@@@@@@@@@@@ RULE IS SENDING DATA TO NAND - CHANNEL0 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ");
	rg_addr_for_write_bufr <= rg_addr_for_write_bufr + 1;
	write_data_buffer.portA.request.put(BRAMRequest{
		write : False,
		address: rg_addr_for_write_bufr ,
		datain :  ? ,
		responseOnWrite : False
	});
	if(rg_addr_for_write_bufr == 'h3FF) begin
//				rg_nand_state0 <= OPCODE_PART2;
		rg_start_data_transfer_to_nand0 <= False;
	end
endrule

rule rl_reading_internal_buffer0;
	let data_out <- write_data_buffer.portA.response.get();

	$display(" ---------------------> sending data to nand flash controller buffer \n data  >>>>> %d  ..... address = %d >>>>>> " , rg_data_to_nand0 , rg_address_to_nand0);

	if (rg_supply_initial_address == True) begin
		rg_supply_initial_address <= False;
		rg_address_to_nand0 <= 0;
		rg_data_to_nand0 <= data_out;
	end

	else begin
		rg_address_to_nand0 <= rg_address_to_nand0 + 1;
		rg_data_to_nand0 <= data_out;
	end

	if (rg_address_to_nand0 == 'h3FE) begin
		$display(" >>>>>>>>>>>>>>>>>>>>>>>>>>>> SENDING LAST DATA >>>>>>>>>>>>>>>>>>>>>>>>> ");
		rg_nand_state0 <= OPCODE_PART2;
	end
endrule


rule rl_write_data_to_pcie (rg_data_transfer_state0 == WRITE_DATA_TO_PCIe);
	$display(" ************DATA TRANSFER STATE = WRITE_DATA_TO_PCIe************ ");

	if (wr_send_valid_data == 1) begin
		if (wr_wait == 0) begin
			wr_data_valid <= 1;
			if (rg_supply_initial_address == True) begin
				rg_supply_initial_address <= False;
				rg_address_to_nand0 <= 0;
				rg_data_to_pcie <= wr_data_from_nand0;
			end

			else begin
				rg_address_to_nand0 <= rg_address_to_nand0 + 1;
				rg_data_to_pcie <= wr_data_from_nand0;
			end

			if (rg_address_to_nand0 == 'h3FE) begin
				 rg_read_from_nand_successful0 <= True;
				rg_data_transfer_state0 <= DO_NOTHING;
				send_data_done <= 1;
			end
		end
	end
endrule

rule rl_data_transfer_do_nothing (rg_data_transfer_state0 == DO_NOTHING);
	$display(" ************DATA TRANSFER STATE = DO NOTHING NOTHING NOTHING************ ");
	send_data_done <= 0;
	if (rg_request_pcie_to_send_data == True)
		rg_data_transfer_state0 <=  REQUEST_DATA_FROM_PCIe;
endrule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
rule rl_data_valid;
	rg_data_valid <= wr_data_valid;
endrule
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
Wire#(bit) wr_interrupt_internal <- mkDWire(0);

rule rl_interrupt (wr_interrupt0 == 1);
	$display(" *********&&&&&&&&&&	@@@@@@@@@@@@@@@@@@@@@@@*** INTERRUPT RECEIVED ***@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&********* ");
	rg_r0 <= 1;
	rg_data_transfer_state0 <= REQUEST_DATA_TO_PCIe;
	rg_nand_state0 <= DEFAULT;
endrule


// Registers and Wires for NAND Flash Interface
Reg#(Bit#(12))	rg_address_to_nand1 	<- mkReg(0);	// address
Reg#(Bit#(32))	rg_data_to_nand1 	<- mkReg(0);	// data out
Wire#(Bit#(32))	wr_data_from_nand1 	<- mkDWire(0);	// data in
Reg#(bit)		rg_nand_ce_l1		<- mkReg(1);	// active low
Reg#(bit)		rg_nand_we_l1		<- mkReg(1);	// active low
Reg#(bit)		rg_nand_oe_l1 		<- mkReg(1);	// active low
Reg#(bit)		rg_nand_reset_l1 	<- mkReg(1);	// active low
Wire#(bit)		wr_interrupt1		<- mkDWire(0);	// active high
Wire#(bit)		wr_ready_busy_l1		<- mkDWire(1);	// active low

// Internal Registers for NAND flash interface Control Signals


Reg#(Nand_control_states) rg_nand_state1 <- mkReg(DEFAULT);
Reg#(Data_transfer_states) rg_data_transfer_state1 <- mkReg(DO_NOTHING);

Reg#(bit) rg_r1 <- mkReg(0);
Reg#(bit) rg_w1 <- mkReg(0);

Reg#(Bool) rg_start_data_transfer_to_nand1 <- mkReg(False);

rule rl_initiate_read1 (rg_initiate_read1 == True);
	$display(" **************SENDING READ REQUEST TO NAND FLASH - CHANNEL1 ************ ");
	if (wr_ready_busy_l1 == 0) begin
//		$display(" **************SENDING READ REQUEST TO NAND FLASH************ ");
		rg_r1 <= 1; // READ
		rg_nand_state1 <= OPCODE_PART1;
		rg_initiate_read1 <= False;
	end
endrule

rule rl_initiate_write1 (rg_initiate_write1 == True);
	if (wr_ready_busy_l1 == 0) begin
		$display(" **************SENDING WRITE REQUEST TO NAND FLASH - CHANNEL1 ************ ");
		rg_w1 <= 1; // WRITE
		rg_nand_state1 <= OPCODE_PART1;
		rg_initiate_write1 <= False;
	end
endrule

rule rl_opcode1_1 (rg_nand_state1 == OPCODE_PART1);
	$display(" ***************OPCODE 1 SENT*****************");
	rg_nand_ce_l1 <= 0;
	rg_nand_we_l1 <= 0;
	rg_nand_oe_l1 <= 1;
	rg_address_to_nand1 <= 'hFFA;
	if(rg_r1 == 1)
		rg_data_to_nand1 <= 'h00;
	else if(rg_w1 == 1)
		rg_data_to_nand1 <= 'h80;

	rg_nand_state1 <= ADDRESS_PART1;
endrule

rule rl_addr1_1 (rg_nand_state1 == ADDRESS_PART1);
	$display(" ***************ADDRESS 1 SENT*****************");
	rg_nand_ce_l1 <= 0;
	rg_nand_we_l1 <= 0;
	rg_nand_oe_l1 <= 1;
	rg_address_to_nand1 <= 'hFF4;
	rg_data_to_nand1 <= {'d0,rg_logical_block_address[7:0]};
	rg_nand_state1 <= ADDRESS_PART2;
endrule

rule rl_addr2_1 (rg_nand_state1 == ADDRESS_PART2);
	$display(" ***************ADDRESS 2 SENT*****************");
	rg_nand_ce_l1 <= 0;
	rg_nand_we_l1 <= 0;
	rg_nand_oe_l1 <= 1;
	rg_address_to_nand1 <= 'hFF5;
	rg_data_to_nand1 <= {'d0,rg_logical_block_address[15:8] };
	rg_nand_state1 <= ADDRESS_PART3;
endrule

rule rl_addr3_1 (rg_nand_state1 == ADDRESS_PART3);
	$display(" ***************ADDRESS 3 SENT*****************");
	rg_nand_ce_l1 <= 0;
	rg_nand_we_l1 <= 0;
	rg_nand_oe_l1 <= 1;
	rg_address_to_nand1 <= 'hFF6;
	rg_data_to_nand1 <= {'d0 ,rg_logical_block_address[24:16] };

	if(rg_r1 == 1)
		rg_nand_state1 <= OPCODE_PART2;
	else if (rg_w1 == 1) begin
		rg_nand_state1 <= DEFAULT;
		rg_addr_for_write_bufr <= 0;
		rg_start_data_transfer_to_nand1 <= True;
	end
endrule

rule rl_opcode2_1 (rg_nand_state1 == OPCODE_PART2);
	$display(" ***************OPCODE 2 SENT*****************");
	rg_nand_ce_l1 <= 0;
	rg_nand_we_l1 <= 0;
	rg_nand_oe_l1 <= 1;
	rg_address_to_nand1 <= 'hFFA;
	if(rg_r1 == 1) begin
		rg_r1 <= 0;
		rg_data_to_nand1 <= 'h30;
		rg_data_transfer_state1 <= DO_NOTHING;
	end
	else if (rg_w1 == 1) begin
		rg_data_to_nand1 <= 'h10;
		rg_w1 <= 0;
	end
	rg_write_to_nand_successful1 <= True;
	rg_nand_state1 <= DEFAULT;
endrule

rule rl_default1 (rg_nand_state1 == DEFAULT);
	$display(" *********** NAND CONTROL SIGNAL STATE CHANNEL1 = DEFAULT ********** ");
	if (rg_r1 == 1) begin
		rg_nand_ce_l1 <= 0;
		rg_nand_we_l1 <= 1;
		rg_nand_oe_l1 <= 0; // READ ENABLE
	end
	else if (rg_w1 == 1) begin
		rg_nand_ce_l1 <= 0;
		rg_nand_we_l1 <= 0;
		rg_nand_oe_l1 <= 1; // WRITE ENABLE
	end
	else begin
		rg_nand_ce_l1 <= 0;
		rg_nand_we_l1 <= 1;
		rg_nand_oe_l1 <= 1;	// NO ENABLE
	end
endrule

Reg#(Bool) rg_supply_initial_address1 <- mkReg(False);
Reg#(Bool) rg_waiting_for_data1 <- mkReg(False);

rule rl_request_data_frm_pcie1 (rg_data_transfer_state1 == REQUEST_DATA_FROM_PCIe);
	$display(" ************DATA TRANSFER STATE = REQUEST_DATA_FROM_PCIe ************ ") ;

	if (rg_waiting_for_data1 == False) begin
		wr_req_read_data <= 1;
		if(rg_req_read_data_accepted == 1) begin
			 rg_waiting_for_data1 <= True;
		  	 rg_address_to_pcie <= rg_write_data_from;
		end
	end

	else begin
		rg_waiting_for_data1 <= False;
		rg_data_transfer_state1 <= READ_DATA_FROM_PCIe;
		rg_supply_initial_address1 <= True;
	end
endrule

Reg#(Bool) rg_sending_data_to_pcie1 <- mkReg(False);

rule rl_request_data_to_pcie1 (rg_data_transfer_state1 == REQUEST_DATA_TO_PCIe);
	$display(" ************DATA TRANSFER STATE = REQUEST_DATA_TO_PCIe ************ ");

	if(rg_sending_data_to_pcie1 == False) begin
		wr_req_write_data <= 1;
		if(rg_req_write_data_accepted == 1) begin
			rg_sending_data_to_pcie1 <= True;
			rg_address_to_pcie <= rg_transfer_read_data_to;
		end
	end

	else begin
		rg_sending_data_to_pcie <= False;
		rg_data_transfer_state1 <= WRITE_DATA_TO_PCIe;
		rg_supply_initial_address1 <= True;
	end
endrule

Reg#(Bool) read_data_from_buffer <- mkReg(False);

rule rl_read_data_from_pcie1 (rg_data_transfer_state1 == READ_DATA_FROM_PCIe);
	$display(" ************DATA TRANSFER STATE = READ_DATA_FROM_PCIe************ ");
	if(rg_buffer_full == 1) begin
		wr_data_received <= 1;
		rg_data_transfer_state1 <= DO_NOTHING;
//				read_data_from_buffer <= True;
	end
endrule

rule rl_data_transfer_to_nand1 (rg_start_data_transfer_to_nand1 == True);
	$display(" @@@@@@@@@@@@@@@@@@@@@@@@@ RULE IS SENDING DATA TO NAND - CHANNEL1 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ");
	rg_addr_for_write_bufr <= rg_addr_for_write_bufr + 1;
		write_data_buffer.portA.request.put(BRAMRequest{
			write : False,
			address: rg_addr_for_write_bufr,
			datain :  ?,
			responseOnWrite : False
		});
	if(rg_addr_for_write_bufr == 'h3FF) begin
//				rg_nand_state0 <= OPCODE_PART2;
		rg_start_data_transfer_to_nand1 <= False;
	end
endrule

rule rl_reading_internal_buffer1;
	let data_out <- write_data_buffer.portA.response.get();
	$display(" ---------------------> sending data to nand flash controller buffer \n data  >>>>> %d  ..... address = %d >>>>>> " , rg_data_to_nand0 , rg_address_to_nand0);

	if (rg_supply_initial_address1 == True) begin
		rg_supply_initial_address1 <= False;
		rg_address_to_nand1 <= 0;
		rg_data_to_nand1 <= data_out;
	end

	else begin
		rg_address_to_nand1 <= rg_address_to_nand1 + 1;
		rg_data_to_nand1 <= data_out;
	end

	if (rg_address_to_nand1 == 'h3FE) begin
		$display(" >>>>>>>>>>>>>>>>>>>>>>>>>>>> SENDING LAST DATA >>>>>>>>>>>>>>>>>>>>>>>>> ");
		rg_nand_state1 <= OPCODE_PART2;
	end
endrule


rule rl_write_data_to_pcie1 (rg_data_transfer_state1 == WRITE_DATA_TO_PCIe);
	$display(" ************DATA TRANSFER STATE = WRITE_DATA_TO_PCIe************ ");

	if (wr_send_valid_data == 1) begin
		if (wr_wait == 0) begin
			wr_data_valid <= 1;
			if (rg_supply_initial_address1 == True) begin
				rg_supply_initial_address1 <= False;
				rg_address_to_nand1 <= 0;
				rg_data_to_pcie <= wr_data_from_nand1;
			end

			else begin
				rg_address_to_nand1 <= rg_address_to_nand1 + 1;
				rg_data_to_pcie <= wr_data_from_nand0;
			end

			if (rg_address_to_nand1 == 'h3FE) begin
				 rg_read_from_nand_successful1 <= True;
				rg_data_transfer_state1 <= DO_NOTHING;
				send_data_done <= 1;
			end
		end
	end
endrule

rule rl_data_transfer_do_nothing1 (rg_data_transfer_state1 == DO_NOTHING);
	$display(" ************DATA TRANSFER STATE = DO NOTHING NOTHING NOTHING************ ");
	send_data_done <= 0;
	if (rg_request_pcie_to_send_data == True)
		rg_data_transfer_state1 <=  REQUEST_DATA_FROM_PCIe;
endrule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
rule rl_data_valid1;
	rg_data_valid <= wr_data_valid;
endrule
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Wire#(bit) wr_interrupt_internal <- mkDWire(0);

rule rl_interrupt1 (wr_interrupt1 == 1);
	$display(" *********&&&&&&&&&&	@@@@@@@@@@@@@@@@@@@@@@@*** INTERRUPT RECEIVED ***@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&********* ");
	rg_r1 <= 1;
	rg_data_transfer_state1 <= REQUEST_DATA_TO_PCIe;
	rg_nand_state1 <= DEFAULT;
endrule




////////////////////////////////////////////////////////////////////////////////
// Rule For Reading the Data Structure from BRAMs and sending them to PCIe Controller
////////////////////////////////////////////////////////////////////////////////

rule rl_read_data_structure (rg_execution_state ==  ASQ_READ_DATA_STRUCTURE);

	if(rg_sending_data_to_pcie == False) begin
		wr_req_write_data <= 1;
		if(rg_req_write_data_accepted == 1) begin
			rg_sending_data_to_pcie <= True;
			rg_address_to_pcie <= rg_prp1;
			rg_Start_read_Data_Structure <= True;
		end
	end

	else begin
		if (rg_Start_read_Data_Structure && wr_send_valid_data == 1) begin
			if (wr_send_valid_data == 1) begin
				if (wr_wait == 0) begin
					rg_read_Bram_Count <= rg_read_Bram_Count + 1;
				end

				if (rg_CNS == 0 && !rg_getFeatures) begin
					controller_data_structure.portA.request.put(BRAMRequest{
						write : False,
						address: rg_read_Bram_Count[11:0],
						datain : ?,
						responseOnWrite : False
					});
				end

				if (rg_CNS == 1 && !rg_getFeatures) begin
					namespace_data_structure.portA.request.put(BRAMRequest{
						write : False,
						address: rg_read_Bram_Count[11:0],
						datain : ?,
						responseOnWrite : False
					});
				end

				if (rg_getFeatures) begin
					lbaRange_data_structure.portA.request.put(BRAMRequest{
						write : False,
						address: rg_read_Bram_Count[11:0],
						datain : ?,
						responseOnWrite : False
					});
				end
			end
		end


		else if (rg_Start_read_Data_Structure && wr_send_valid_data == 0) begin  // End of 4KB file .. End is actualy detected by the PCIe Controller
			rg_sending_data_to_pcie <= False;
			wr_update_admin_command_status <= 1;	// Update the Status in  Completion
			wr_admin_success <= 1;					// Update it as "Succesful Completion .. Hurray !! "
			rg_getFeatures <= False;				// Done With Get Features
			rg_execution_state <= ASQ_COMPLETION;	// Completion of the Command Execution
			send_data_done <= 1;
		end
	end

endrule

////////////////////////////////////////////////////////////////////////////////
// Rule for Sending the data read the BRAMS to the PCIe Controller ... THE DMA Engine
////////////////////////////////////////////////////////////////////////////////

rule rl_dma_to_pcie_from_cds;
	wr_data_valid <= 1; // Sending Valid data
	let bram_data_from_cds <- controller_data_structure.portA.response.get();
	rg_data_to_pcie <= bram_data_from_cds;
	$display(" dma data = %b", rg_data_to_pcie);
endrule

rule rl_dma_to_pcie_from_nds;
	wr_data_valid <= 1; // Sending Valid data
	let bram_data_from_nds <- namespace_data_structure.portA.response.get();
	rg_data_to_pcie <= bram_data_from_nds;
	$display(" dma data = %b", rg_data_to_pcie);
endrule

rule rl_dma_to_pcie_from_lbaRange;
	wr_data_valid <= 1; // Sending Valid data
	let bram_data_from_lbaRange <- lbaRange_data_structure.portA.response.get();
	rg_data_to_pcie <= bram_data_from_lbaRange;
	$display(" dma data = %b", rg_data_to_pcie);
endrule


// ***** Problem *** the BRAM is taking the first address twice 0 0 1 2 3 .. is the sequence obtained *******

////////////////////////////////////////////////////////////////////////////////
// Rule for Writing into the Controller registers
////////////////////////////////////////////////////////////////////////////////

rule rl_write_controller_register_file (wr_write == 1);

		if (wr_regFileAddress == 32'h0c) begin
			mask_set <= wr_regFiledata_in [31:0];
		end

		if (wr_regFileAddress == 32'h10) begin
			mask_clear <= wr_regFiledata_in [31:0];
		end

		if(wr_regFileAddress== 32'h14) begin
			cc <= Controller_configuration {
				reserved1	: 0,
				iocqes 		: 0,
				iosqes 		: 0,
				shn 		: wr_regFiledata_in [15:14],
				ams 		: wr_regFiledata_in [13:11],
				mps 		: wr_regFiledata_in [10:7],
				css 		: wr_regFiledata_in [6:4],
				reserved2 	: wr_regFiledata_in [3:1],
				en 		    : wr_regFiledata_in [0]
			};
//				$display(" Controller Configured with %b ", wr_regFiledata_in);
		end

		else if (wr_regFileAddress == 32'h24) begin
			aqa <= AQA {
				reserved1	:0,
				acqs		:wr_regFiledata_in [27:16],
				reserved2	:0,
				asqs		:wr_regFiledata_in [11:0]
			};
		end

		else if (wr_regFileAddress == 32'h28) begin
			asq <= ASQ {
				asqb		:wr_regFiledata_in [63:12],
				reserved 	:0
			} ;
		end

		else if (wr_regFileAddress == 32'h30) begin
			acq <= ACQ {
				acqb		:wr_regFiledata_in [63:12],
				reserved 	:0
			} ;
		end

		else
		for (Integer y = 0; y < `No_SQueues; y = y + 1)	begin
			 if (wr_regFileAddress == 32'h1000 + ((2*fromInteger(y))*(4 << cap.dstrd))) begin
				sqtdbl[y] <= SQTDBL {
					reserved	:wr_regFiledata_in[31:16],
					sqt 		:wr_regFiledata_in[15:0]
				};

			$display(" SQTail door bell updated %b ", wr_regFiledata_in);
			end

			else if (wr_regFileAddress == 32'h1000 + ((2*fromInteger(y) + 1)*(4 << cap.dstrd))) begin
				cqhdbl[y] <= CQHDBL {
					reserved	:wr_regFiledata_in[31:16],
					cqh 		:wr_regFiledata_in[15:0]
				};
			end
		end

endrule

////////////////////////////////////////////////////////////////////////////////
// Rules for Interrupt generation
////////////////////////////////////////////////////////////////////////////////


rule rl_prev_reg;
	prev_mask_reg <= mask_reg;
	prev_status_reg <= status_reg;
	prev_mask_set <= mask_set;
	prev_mask_clear <= mask_clear;
endrule

rule rl_edge_detect;
	pos_edge_on_mask_set <= (mask_set) & (~prev_mask_set);
	pos_edge_on_mask_clear <= (mask_clear) & (~prev_mask_clear);
	pos_edge_on_status_reg <= (status_reg) & (~prev_status_reg);
	neg_edge_on_mask_reg <= (prev_mask_reg) & (~mask_reg);
endrule

rule rl_clear_mask (rg_clear_m == 1 && rg_set_m == 0);
//	$display("Mask register bit clear operation done");
	mask_reg <= mask_reg & (~pos_edge_on_mask_clear);
	mask_clear <= 0;
endrule

rule rl_set_mask (rg_set_m == 1 && rg_clear_m == 0);
//	$display("Mask register bit set operation done");
	mask_reg <= mask_reg | pos_edge_on_mask_set ;
	mask_set <= 0;
endrule

rule rl_assignments;
	rg_clear_m <= wr_clear_m;
	rg_set_m <= wr_set_m;
endrule

rule rl_event_detect;
	event_d <= (status_reg & neg_edge_on_mask_reg) | (~(mask_reg) & pos_edge_on_status_reg) | (neg_edge_on_mask_reg & pos_edge_on_status_reg);
endrule

rule rl_vect_num;
	if(event_d[0] == 1) begin  // Admin Submission Queue interrupt is not aggregated
		vectr <= 0;
		vectr_rdy <= 1;
		status_reg[0] <= 0;
	end
	else if(event_d[1] == 1) begin
		status_reg[1] <= 0;
		if(vectr_count[0] == aggr_threshold) begin
			vectr <= 1;
			vectr_rdy <= 1;
			vectr_count[0] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 1 .... %d " , vectr_count[0]);
			vectr_count[0] <= vectr_count[0] + 1;
		end
	end
	else if(event_d[2] == 1) begin
		status_reg[2] <= 0;
		if(vectr_count[1] == aggr_threshold) begin
			vectr <= 2;
			vectr_rdy <= 1;
			vectr_count[1] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 2 .... %d " , vectr_count[1]);
			vectr_count[1] <= vectr_count[1] + 1;
		end
	end	else if(event_d[3] == 1) begin
		status_reg[3] <= 0;
		if(vectr_count[2] == aggr_threshold) begin
			vectr <= 3;
			vectr_rdy <= 1;
			vectr_count[2] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 3 .... %d " , vectr_count[2]);
			vectr_count[2] <= vectr_count[2] + 1;
		end
	end	else if(event_d[4] == 1) begin
		status_reg[4] <= 0;
		if(vectr_count[3] == aggr_threshold) begin
			vectr <= 4;
			vectr_rdy <= 1;
			vectr_count[3] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 4 .... %d " , vectr_count[3]);
			vectr_count[3] <= vectr_count[3] + 1;
		end
	end	else if(event_d[5] == 1) begin
		status_reg[5] <= 0;
		if(vectr_count[4] == aggr_threshold) begin
			vectr <= 5;
			vectr_rdy <= 1;
			vectr_count[4] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 5 .... %d " , vectr_count[4]);
			vectr_count[4] <= vectr_count[4] + 1;
		end
	end	else if(event_d[6] == 1) begin
		status_reg[6] <= 0;
		if(vectr_count[5] == aggr_threshold) begin
			vectr <= 6;
			vectr_rdy <= 1;
			vectr_count[5] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 6 .... %d " , vectr_count[5]);
			vectr_count[5] <= vectr_count[5] + 1;
		end
	end	else if(event_d[7] == 1) begin
		status_reg[7] <= 0;
		if(vectr_count[6] == aggr_threshold) begin
			vectr <= 7;
			vectr_rdy <= 1;
			vectr_count[6] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 7 .... %d " , vectr_count[6]);
			vectr_count[6] <= vectr_count[6] + 1;
		end
	end	else if(event_d[8] == 1) begin
		status_reg[8] <= 0;
		if(vectr_count[7]== aggr_threshold) begin
			vectr <= 8;
			vectr_rdy <= 1;
			vectr_count[7] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 8 .... %d " , vectr_count[7]);
			vectr_count[7] <= vectr_count[7] + 1;
		end
	end	else if(event_d[9] == 1) begin
		status_reg[9] <= 0;
		if(vectr_count[8]== aggr_threshold) begin
			vectr <= 9;
			vectr_rdy <= 1;
			vectr_count[8] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 9 .... %d " , vectr_count[8]);
			vectr_count[8] <= vectr_count[8] + 1;
		end
	end	else if(event_d[10] == 1) begin
		status_reg[10] <= 0;
		if(vectr_count[9]== aggr_threshold) begin
			vectr <= 10;
			vectr_rdy <= 1;
			vectr_count[9] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 10 .... %d " , vectr_count[9]);
			vectr_count[9] <= vectr_count[9] + 1;
		end
	end	else if(event_d[11] == 1) begin
		status_reg[11] <= 0;
		if(vectr_count[10]== aggr_threshold) begin
			vectr <= 11;
			vectr_rdy <= 1;
			vectr_count[10] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 11 .... %d " , vectr_count[10]);
			vectr_count[10] <= vectr_count[10] + 1;
		end
	end	else if(event_d[12] == 1) begin
		status_reg[12] <= 0;
		if(vectr_count[11]== aggr_threshold) begin
			vectr <= 12;
			vectr_rdy <= 1;
			vectr_count[11] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 12 .... %d " , vectr_count[11]);
			vectr_count[11] <= vectr_count[11] + 1;
		end
	end	else if(event_d[13] == 1) begin
		status_reg[13] <= 0;
		if(vectr_count[12]== aggr_threshold) begin
			vectr <= 13;
			vectr_rdy <= 1;
			vectr_count[12] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 13 .... %d " , vectr_count[12]);
			vectr_count[12] <= vectr_count[12] + 1;
		end
	end	else if(event_d[14] == 1) begin
		status_reg[14] <= 0;
		if(vectr_count[13]== aggr_threshold) begin
			vectr <= 14;
			vectr_rdy <= 1;
			vectr_count[13] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 14 .... %d " , vectr_count[13]);
			vectr_count[13] <= vectr_count[13] + 1;
		end
	end	else if(event_d[15] == 1) begin
		status_reg[15] <= 0;
		if(vectr_count[14]== aggr_threshold) begin
			vectr <= 15;
			vectr_rdy <= 1;
			vectr_count[14] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 15 .... %d " , vectr_count[14]);
			vectr_count[14] <= vectr_count[14] + 1;
		end
	end	else if(event_d[16] == 1) begin
		status_reg[16] <= 0;
		if(vectr_count[15]== aggr_threshold) begin
			vectr <= 16;
			vectr_rdy <= 1;
			vectr_count[15] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 16 .... %d " , vectr_count[15]);
			vectr_count[15] <= vectr_count[15] + 1;
		end
	end	else if(event_d[17] == 1) begin
		status_reg[17] <= 0;
		if(vectr_count[16]== aggr_threshold) begin
			vectr <= 17;
			vectr_rdy <= 1;
			vectr_count[16] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 17 .... %d " , vectr_count[16]);
			vectr_count[16] <= vectr_count[16] + 1;
		end
	end	else if(event_d[18] == 1) begin
		status_reg[18] <= 0;
		if(vectr_count[17]== aggr_threshold) begin
			vectr <= 18;
			vectr_rdy <= 1;
			vectr_count[17] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 18 .... %d " , vectr_count[17]);
			vectr_count[17] <= vectr_count[17] + 1;
		end
	end	else if(event_d[19] == 1) begin
		status_reg[19] <= 0;
		if(vectr_count[18]== aggr_threshold) begin
			vectr <= 19;
			vectr_rdy <= 1;
			vectr_count[18] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 19 .... %d " , vectr_count[18]);
			vectr_count[18] <= vectr_count[18] + 1;
		end
	end	else if(event_d[20] == 1) begin
		status_reg[20] <= 0;
		if(vectr_count[19]== aggr_threshold) begin
			vectr <= 20;
			vectr_rdy <= 1;
			vectr_count[19] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 20 .... %d " , vectr_count[19]);
			vectr_count[19] <= vectr_count[19] + 1;
		end
	end	else if(event_d[21] == 1) begin
		status_reg[21] <= 0;
		if(vectr_count[20]== aggr_threshold) begin
			vectr <= 21;
			vectr_rdy <= 1;
			vectr_count[20] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 21 .... %d " , vectr_count[20]);
			vectr_count[20] <= vectr_count[20] + 1;
		end
	end	else if(event_d[22] == 1) begin
		status_reg[22] <= 0;
		if(vectr_count[21]== aggr_threshold) begin
			vectr <= 22;
			vectr_rdy <= 1;
			vectr_count[21] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 22 .... %d " , vectr_count[21]);
			vectr_count[21] <= vectr_count[21] + 1;
		end
	end	else if(event_d[23] == 1) begin
		status_reg[23] <= 0;
		if(vectr_count[22]== aggr_threshold) begin
			vectr <= 23;
			vectr_rdy <= 1;
			vectr_count[22] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 23 .... %d " , vectr_count[22]);
			vectr_count[22] <= vectr_count[22] + 1;
		end
	end	else if(event_d[24] == 1) begin
		status_reg[24] <= 0;
		if(vectr_count[23]== aggr_threshold) begin
			vectr <= 24;
			vectr_rdy <= 1;
			vectr_count[23] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 24 .... %d " , vectr_count[23]);
			vectr_count[23] <= vectr_count[23] + 1;
		end
	end	else if(event_d[25] == 1) begin
		status_reg[25] <= 0;
		if(vectr_count[24]== aggr_threshold) begin
			vectr <= 25;
			vectr_rdy <= 1;
			vectr_count[24] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 25 .... %d " , vectr_count[24]);
			vectr_count[24] <= vectr_count[24] + 1;
		end
	end	else if(event_d[26] == 1) begin
		status_reg[26] <= 0;
		if(vectr_count[25]== aggr_threshold) begin
			vectr <= 26;
			vectr_rdy <= 1;
			vectr_count[25] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 26 .... %d " , vectr_count[25]);
			vectr_count[25] <= vectr_count[25] + 1;
		end
	end	else if(event_d[27] == 1) begin
		status_reg[27] <= 0;
		if(vectr_count[26]== aggr_threshold) begin
			vectr <= 27;
			vectr_rdy <= 1;
			vectr_count[26] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 27 .... %d " , vectr_count[26]);
			vectr_count[26] <= vectr_count[26] + 1;
		end
	end	else if(event_d[28] == 1) begin
		status_reg[28] <= 0;
		if(vectr_count[27]== aggr_threshold) begin
			vectr <= 28;
			vectr_rdy <= 1;
			vectr_count[27] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 28 .... %d " , vectr_count[27]);
			vectr_count[27] <= vectr_count[27] + 1;
		end
	end	else if(event_d[29] == 1) begin
		status_reg[29] <= 0;
		if(vectr_count[28]== aggr_threshold) begin
			vectr <= 29;
			vectr_rdy <= 1;
			vectr_count[28] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 29 .... %d " , vectr_count[28]);
			vectr_count[28] <= vectr_count[28] + 1;
		end
	end	else if(event_d[30] == 1) begin
		status_reg[30] <= 0;
		if(vectr_count[29]== aggr_threshold) begin
			vectr <= 30;
			vectr_rdy <= 1;
			vectr_count[29] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 30 .... %d " , vectr_count[29]);
			vectr_count[29] <= vectr_count[29] + 1;
		end
	end	else if(event_d[31] == 1) begin
		status_reg[31] <= 0;
		if(vectr_count[30]== aggr_threshold) begin
			vectr <= 31;
			vectr_rdy <= 1;
			vectr_count[30] <= 0;
		end
		else begin
			vectr_rdy <= 0;
			$display(" Aggregated vector number 31 .... %d " , vectr_count[30]);
			vectr_count[30] <= vectr_count[30] + 1;
		end
	end
	else begin
		vectr <= 0;
		vectr_rdy <= 0;
	end
endrule

rule rl_display;
//	$display(" ************************************************ clock = %d", clock);
	if(pos_edge_on_mask_set != 0)
		$display(" POS edge on mask set register detected");
	if(pos_edge_on_status_reg != 0)
		$display("+++++++++++++++++++++++++++++++++++++++++++++++++++++++ POS edge on status register detected");
	if(pos_edge_on_mask_clear != 0)
		$display(" POS edge on mask clear register detected");
	if(neg_edge_on_mask_reg != 0)
		$display(" NEG edge on mask register detected ");

	if(vectr_rdy == 1)
		$display(" VECTOR IS READY ..... Vector Number = %d", vectr);
endrule


////////////////////////////////////////////////////////////////////////////////
// Rule for Reading from the Controller Register File
////////////////////////////////////////////////////////////////////////////////
Reg#(Bool) rg_send_read_data <- mkReg(False);


(* descending_urgency = " rl_write_controller_register_file, rl_read_controller_register_file, rl_fetching_command , rl_asq_completion , rl_isq_completion , rl_request_data_frm_pcie0 , rl_request_data_to_pcie ,rl_write_data_to_pcie, rl_read_data_structure, rl_req_idle " *)


rule rl_read_controller_register_file (wr_read == 1);
	rg_send_completion_tlp <= 1;
	rg_send_read_data <= True;

	if(wr_regFileAddress == 32'h00) begin
		rg_regFiledata_out <= {8'd0, cap.mpsmax, cap.mpsmin, 7'd0, cap.css, 1'd0, cap.dstrd, cap.to, 5'd0, cap.ams, cap.cqr, cap.mqes };
		rg_payload_length <= 'd2;
	end

	if (wr_regFileAddress == 32'h08) begin
		rg_regFiledata_out <= {32'd0, version.mjr , version.mnr };
		rg_payload_length <= 'd1;
	end

	if (wr_regFileAddress == 32'h0c) begin
		rg_regFiledata_out <= {32'd0, mask_reg}; // Returns the Internal Mask register value
		rg_payload_length <= 'd1;
	end

	if (wr_regFileAddress == 32'h10) begin
		rg_regFiledata_out <= {32'd0, mask_reg}; // Returns the internal Mask Register Value
		rg_payload_length <= 'd1;
	end

	if (wr_regFileAddress == 32'h14) begin
		rg_regFiledata_out <= {32'd0, 8'd0, cc.iocqes, cc.iosqes, cc.shn, cc.ams, cc.mps, cc.css, 3'd0, cc.en};
		rg_payload_length <= 'd1;
	end

	if (wr_regFileAddress == 32'h1c) begin
		rg_regFiledata_out <= {32'd0, 28'd0, csts.shst, csts.cfs, csts.rdy};
		rg_payload_length <= 'd1;
	end

	if (wr_regFileAddress == 32'h24) begin
		rg_regFiledata_out <= {32'd0, 4'd0, aqa.acqs, 4'd0, aqa.asqs};
		rg_payload_length <= 'd1;
	end

	if (wr_regFileAddress == 32'h28) begin
		rg_regFiledata_out <= {asq.asqb, 12'd0};
		rg_payload_length <= 'd2;
	end

	if (wr_regFileAddress == 32'h30) begin
		rg_regFiledata_out <= {acq.acqb, 12'd0};
		rg_payload_length <= 'd2;
	end

	if (wr_regFileAddress > 32'h37 && wr_regFileAddress <= 32'hfff) begin
		rg_regFiledata_out <= 64'd0;
		rg_payload_length <= 'd2;
	end

endrule

for (Integer y = 0; y < `No_SQueues; y = y + 1)	begin
	rule rl_read_SQdoorBell ((wr_regFileAddress == 32'h1000 + (2*fromInteger(y))*(4 << cap.dstrd)) && wr_read == 1);
		rg_read_regFiledata <= 1; // this is active as long the data is read from the reg file ...
		rg_regFiledata_out <= {32'd0, 16'd0, sqtdbl[y].sqt};
		rg_payload_length <= 'd1;
	endrule
end

for (Integer y = 0; y < `No_SQueues; y = y + 1)	begin
	rule rl_read_CQdoorBell ((wr_regFileAddress == 32'h1000 + (2*fromInteger(y) + 1)*(4 << cap.dstrd)) && wr_read == 1);
		rg_read_regFiledata <= 1; // this is active as long the data is read from the reg file ...
		rg_regFiledata_out <= {32'd0, 16'd0, cqhdbl[y].cqh};
		rg_payload_length <= 'd1;
	endrule
end

////////////////////////////////////////////////////////////////////////////////
// the Following Rule Sends the Data read from the Reg File to the PCIe Controller ... Which is eventually sent to CPU
////////////////////////////////////////////////////////////////////////////////
Reg#(bit) rg_read_count <- mkReg(0);

rule rl_send_read_data (rg_send_read_data);
	if (wr_send_valid_data == 1) begin
		if(rg_read_count == 0) begin
			if (wr_wait == 0) begin
				rg_read_count <=  1;
				rg_data_to_pcie <= rg_regFiledata_out[31:0];
				wr_data_valid <= 1;
			end
		end

		if(rg_read_count == 1) begin
			if (wr_wait == 0)	begin
				rg_read_count <= 0;
				rg_data_to_pcie <= rg_regFiledata_out[63:32];
				wr_data_valid <= 1;
			end
		end
	end
	else begin
		rg_send_read_data <= False;
		rg_send_completion_tlp <= 0;
	end
endrule


////////////////////////////////////////////////////////////////////////////////
// Rules for testing the NVM ADMIN Commands
////////////////////////////////////////////////////////////////////////////////

Reg#(Bit#(32)) rg_clock <- mkReg(0);

rule rl_count;
	rg_clock <= rg_clock + 1;
	$display(" ***************************************************************************************************CLOCK = %d ", rg_clock);
endrule
/*
rule rl_display;
	$display(" SQ 0  Head pointer = %d \n SQ 0 Tail Pointer = %d", sqhdbl[0],sqtdbl[0].sqt);
	$display(" SQ 2  Head pointer = %d \n SQ 2 Tail Pointer = %d", sqhdbl[2],sqtdbl[2].sqt);
endrule
*/
rule rl_enable_controller(rg_clock == 32'd10);
	wr_write <= 1;
	wr_regFileAddress <= 32'h14;
	wr_regFiledata_in <= 64'd1; // enable the controller
endrule

rule rl_update_SQ_Tail_pointer (rg_clock == 32'd12);
	wr_write <= 1;
	wr_regFileAddress <= 32'h1000;
	wr_regFiledata_in <= 64'd2; // SQT = 2 @ SQ 0 ...
endrule

rule rl_update_SQ_Tail_1_pointer (rg_clock == 32'd14);
	wr_write <= 1;
	wr_regFileAddress <= 32'h1010; // Address of SQ 2 Tail Doorbell
	wr_regFiledata_in <= 64'd2; // SQT = 1 @ SQ 2 ...
endrule
/*
rule rl_update_SQ_Tail_1_pointer1 (rg_clock == 32'd114);
	wr_write <= 1;
	wr_regFileAddress <= 32'h1010; // Address of SQ 2 Tail Doorbell
	wr_regFiledata_in <= 64'd2; // SQT = 1 @ SQ 2 ...
endrule
*/
/*
rule rl_interrupt_frm_nand (rg_clock == 32'd4000);
	wr_interrupt_internal <= 1;
endrule
*/
rule rl_finish (rg_clock == 32'd8000);
	$finish;
endrule


////////////////////////////////////////////////////////////////////////////////
// interface definition
////////////////////////////////////////////////////////////////////////////////

//interface nvm_interconnect = fn_fifo_to_nvm_interconnect (rg_address_to_pcie , wr_command_In ,rg_send_completion_tlp, wr_commandEn);

interface nvmInterruptSideA_interface = fn_nvmInterruptSideA_ifc(
	vectr_rdy,
	vectr,
	rg_aggregation_threshold
);

interface controller_config_write = fn_controller_registerFileWrite_interface(
	wr_regFileAddress,
	wr_byte_enable,
	wr_regFiledata_in,
	wr_read,
	wr_write
);

interface nvmReceiveCompletionData_interface = fn_nvmReceiveCompletionData_interface(
	wr_completionDataIn,
	wr_completionDataValid,
	wr_completionLastDWord,
	wr_tag, wr_pcie_busy,
	wr_read_request_granted,
	wr_write_request_granted
);

interface nvmTransmitToPCIe_interface = fn_nvmTransmitToPCIe_interface(
	rg_data_to_pcie,
	rg_address_to_pcie,
	rg_requested_tag,
	rg_send_completion_tlp,
	rg_send_write_tlp,
	rg_send_read_tlp,
	wr_send_valid_data,
	rg_payload_length,
	rg_64b_address,
	rg_data_valid,
	rg_nvm_wait,
	wr_wait
);

interface nfc_interface0 = fn_nfc_interface0(
	rg_address_to_nand0,
	rg_data_to_nand0,
	wr_data_from_nand0,
	rg_nand_ce_l0,
	rg_nand_we_l0,
	rg_nand_oe_l0,
	rg_nand_reset_l0,
	wr_interrupt0,
	wr_ready_busy_l0
);

interface nfc_interface1 = fn_nfc_interface1(
	rg_address_to_nand1,
	rg_data_to_nand1,
	wr_data_from_nand1,
	rg_nand_ce_l1,
	rg_nand_we_l1,
	rg_nand_oe_l1,
	rg_nand_reset_l1,
	wr_interrupt1,
	wr_ready_busy_l1
);

endmodule
endpackage

