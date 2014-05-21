package InterruptRequestor ;

import ConfigReg :: * ;
import InterfaceInterruptRequestor :: * ;

	interface InterruptRequestor_Interface ;
		interface ConfigurationInterrupt_Interface configInterrupt_interface ;
		interface NvmInterruptSideB_Interface nvmInterruptSideB_interface ;
		interface CompletorInterruptSideB_Interface completorInterruptSideB_interface ;
	endinterface 

module mkInterruptRequestor( InterruptRequestor_Interface ) ;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Intrrupt Related Configuration Interface Registers and Wire are defined below 	
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	Reg#(bit) rg_cfg_interrupt <- mkReg(0) ;
	Reg#(bit) rg_cfg_interrupt_assert <- mkReg(0) ;
	Reg#(Bit#(8)) rg_cfg_interrupt_di <- mkReg(0) ;

	Wire#(bit) wr_cfg_interrupt_rdy <- mkDWire(0) ;
	Wire#(Bit#(8)) wr_cfg_interrupt_do <- mkDWire(0) ;
	Wire#(Bit#(3)) wr_cfg_interrupt_mmenable <- mkDWire(0) ;
	Wire#(bit) wr_cfg_interrupt_msienable <- mkDWire(0) ; 

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// NVM side Interface Related Registers and Wires 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	Wire#(bit) wr_vector_rdy <- mkDWire(0) ;
	Wire#(Bit#(5)) wr_vector_number <- mkDWire(0) ;
	Wire#(Bit#(5)) wr_aggregation_threshold <- mkDWire(0) ;
	

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Completor Module Side Interface Related wire
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	Wire#(bit) wr_interrupt <- mkDWire(0) ;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Internal Registers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	ConfigReg#(Bit#(5)) rg_interrupt_count <- mkConfigReg(0) ;
	Reg#(Bit#(5)) rg_interrupt_threshold <- mkReg(0) ;
	Reg#(Bool) rg_initiate_interrupt <- mkReg(False) ;
	Reg#(Bit#(5)) rg_interrupt_CORE_threshold <- mkReg(0) ;
	Reg#(Bit#(5)) rg_aggregation_threshold <- mkReg(0) ;
	Wire#(bit) wr_cfg_interrupt <- mkDWire(0) ;

	Reg#(Bit#(5)) rg_vectorQ[32] ;
	for (Integer i = 0 ; i < 32 ; i = i + 1 ) begin
		rg_vectorQ[i] <- mkReg(0) ;
	end


	Reg#(Bit#(32)) rg_count <- mkReg(0) ;


	rule rl_vectorQ ((wr_vector_rdy == 1) ) ;
		rg_vectorQ[rg_interrupt_count] <= (wr_vector_number) ;	
		rg_interrupt_count <= rg_interrupt_count + 1 ;
//		$display(" interrupt count = %d " , rg_interrupt_count ) ;
	endrule 

	rule rl_set_core_threshold ;
		rg_interrupt_CORE_threshold <= (1<<wr_cfg_interrupt_mmenable) ;
//		$display(" interrupt CORE threshold = %d " , rg_interrupt_CORE_threshold ) ;
	endrule
	
	rule rl_set_aggre_threshold (wr_vector_rdy == 1) ;
		rg_aggregation_threshold <= wr_aggregation_threshold ;
//		$display(" interrupt aggregation threshold = %d " , rg_aggregation_threshold ) ;
	endrule

	rule rl_threshold_set ;
		if(rg_aggregation_threshold <= rg_interrupt_CORE_threshold )
			rg_interrupt_threshold <= rg_aggregation_threshold ;
		else
			rg_interrupt_threshold <= rg_interrupt_CORE_threshold  ;
//		$display(" interrupt threshold = %d " , rg_interrupt_threshold ) ;
	endrule

	rule rl_MultiVector_MSI (wr_interrupt == 1) ;
		$display(" ==== comparision ==== " ) ;
		$display(" interrupt count = %d .... int thrs = %d " ,rg_interrupt_count ,  rg_interrupt_threshold ) ;
		if(rg_interrupt_count == rg_interrupt_threshold) begin 
			rg_interrupt_count <= 0 ;
			rg_initiate_interrupt <= True ;
		end
	
	endrule

	rule rl_initiate_interrupt (rg_initiate_interrupt) ;
		$display("interrupt initiated at %d", rg_count) ;
		rg_cfg_interrupt_di[4:0] <= rg_vectorQ[rg_interrupt_count] ;
		wr_cfg_interrupt <= 1 ; 						// This is an internal Wire .. the register assignment is done in separte rule
		if( wr_cfg_interrupt_rdy == 1 ) begin
			$display("Interrupt Sent") ;
			if(rg_interrupt_count == rg_interrupt_threshold -1) begin
				rg_interrupt_count <= 0 ;
				rg_initiate_interrupt <= False ;
			end
	
			else if( rg_interrupt_count != (rg_interrupt_threshold - 1) ) begin
				rg_interrupt_count <= rg_interrupt_count + 1 ;
			end
		end 	
		$display(" vector number = %d " , rg_vectorQ[rg_interrupt_count]) ;
	endrule

	rule rl_config_interrupt ;
		rg_cfg_interrupt <= wr_cfg_interrupt ;
	endrule

//////////////////////////////
// Interface Definitions ... 
//////////////////////////////

	interface configInterrupt_interface = fn_configInterrupt_ifc ( rg_cfg_interrupt , rg_cfg_interrupt_assert , rg_cfg_interrupt_di , wr_cfg_interrupt_rdy , wr_cfg_interrupt_do, wr_cfg_interrupt_mmenable, wr_cfg_interrupt_msienable ) ;

	interface nvmInterruptSideB_interface = fn_nvmInterruptSideB_ifc ( wr_vector_rdy ,  wr_vector_number, wr_aggregation_threshold ) ;

	interface completorInterruptSideB_interface = fn_completorInterruptSideB_ifc ( wr_interrupt ) ;

/////////////////////////////////////////////////////
// Test Setup
///////////////////////////////////////////////////// 
/*
	rule rl_count ;
		rg_count <= rg_count + 1 ;
		wr_cfg_interrupt_msienable <= 1 ;
		$display("cycle = %d", rg_count) ;
	endrule

	rule rl_int_grant ;
		if (rg_cfg_interrupt == 1 ) begin
			wr_cfg_interrupt_rdy <= 1 ;
		end	
	endrule

	rule rl_msi_enable ;
		wr_cfg_interrupt_mmenable <= 3'b010 ;
	endrule

	rule rl_vector_from_nvm (rg_count == 3) ;
		$display("vector ready");
		wr_vector_rdy <= 1 ;
		wr_vector_number <= 5'd1 ;
		wr_aggregation_threshold <= 5'd3 ;
	endrule

	rule rl_vector_from_nvm1 (rg_count == 9) ;
		$display("vector ready");
		wr_vector_rdy <= 1 ;
		wr_vector_number <= 5'd3 ;
		wr_aggregation_threshold <= 5'd3 ;
	endrule

	rule rl_vector_from_nvm2 (rg_count == 15) ;
		$display("vector ready");
		wr_vector_rdy <= 1 ;
		wr_vector_number <= 5'd4 ;
		wr_aggregation_threshold <= 5'd3 ;
	endrule

	rule rl_interrupt_gen (rg_count == 6 || rg_count == 12 || rg_count == 18) ;
		wr_interrupt <= 1 ;
	endrule

	Reg#(bit) rg_cfg_interrupt_rdy <- mkReg(0) ;
	Reg#(bit) rg_cfg_interrupt_msienable <- mkReg(0) ;	
	Reg#(bit) rg_vector_rdy <- mkReg(0) ;
	Reg#(Bit#(5))	rg_vector_number <- mkReg(0) ;
	Reg#(bit) rg_interrupt <- mkReg(0) ;

	rule rl_assignments ;
		rg_cfg_interrupt_rdy <= wr_cfg_interrupt_rdy ;
		rg_cfg_interrupt_msienable <= wr_cfg_interrupt_msienable ;
		rg_vector_rdy <= wr_vector_rdy ;
		rg_vector_number <= wr_vector_number ;
		rg_interrupt <= wr_interrupt ;
	endrule

	rule rl_finish (rg_count == 50 ) ;
		$finish ;
	endrule
*/
endmodule

endpackage
