package InterfaceInterruptRequestor ;

	interface ConfigurationInterrupt_Interface ;
		method bit cfg_interrupt () ;
		method bit cfg_interrupt_assert () ;
		method Bit#(8) cfg_interrupt_di () ;
		method Action _cfg_interrupt_rdy_m ( bit _cfg_interrupt_rdy ) ; 
		method Action _cfg_interrupt_do_m ( Bit#(8) _cfg_interrupt_do ) ;
		method Action _cfg_interrupt_mmenable_m ( Bit#(3) _cfg_interrupt_mmenable ) ;
		method Action _cfg_interrupt_msienable_m ( bit _cfg_interrupt_msienable ) ;
	endinterface	

	function ConfigurationInterrupt_Interface fn_configInterrupt_ifc ( Reg#(bit) rg_cfg_interrupt , Reg#(bit) rg_cfg_interrupt_assert , 								Reg#(Bit#(8)) rg_cfg_interrupt_di , Wire#(bit) wr_cfg_interrupt_rdy , Wire#(Bit#(8)) wr_cfg_interrupt_do, 									Wire#(Bit#(3)) wr_cfg_interrupt_mmenable, Wire#(bit) wr_cfg_interrupt_msienable ) ;

		return ( 
				interface ConfigurationInterrupt_Interface;
					method bit cfg_interrupt () ;
						return  rg_cfg_interrupt;
					endmethod
					method bit cfg_interrupt_assert () ;
						return  rg_cfg_interrupt_assert;
					endmethod
					method Bit#(8) cfg_interrupt_di () ;
						return  rg_cfg_interrupt_di;
					endmethod
					method Action _cfg_interrupt_rdy_m ( _cfg_interrupt_rdy ) ; 
						wr_cfg_interrupt_rdy <= _cfg_interrupt_rdy ;
					endmethod
					method Action _cfg_interrupt_do_m (  _cfg_interrupt_do ) ;
						wr_cfg_interrupt_do <= _cfg_interrupt_do ;
					endmethod					
					method Action _cfg_interrupt_mmenable_m (  _cfg_interrupt_mmenable ) ;
						wr_cfg_interrupt_mmenable <= _cfg_interrupt_mmenable ;
					endmethod
					method Action _cfg_interrupt_msienable_m (  _cfg_interrupt_msienable ) ;		
						wr_cfg_interrupt_msienable <= _cfg_interrupt_msienable  ;
					endmethod
				endinterface ) ;
	endfunction 

	interface NvmInterruptSideB_Interface ;
		method Action _vector_rdy_m ( bit _vector_rdy ) ;
		method Action _vector_number_m ( Bit#(5) _vector_number ) ;
		method Action _aggregation_threshold_m ( Bit#(5) _aggregation_threshold ) ;
	endinterface

	function NvmInterruptSideB_Interface fn_nvmInterruptSideB_ifc ( Wire#(bit) wr_vector_rdy , Wire#(Bit#(5)) wr_vector_number,        																						Wire#(Bit#(5)) wr_aggregation_threshold ) ;

		return ( 
				interface NvmInterruptSideB_Interface ;
					method Action _vector_rdy_m ( _vector_rdy ) ;
						wr_vector_rdy <= _vector_rdy ;
					endmethod 
					method Action _vector_number_m ( Bit#(5) _vector_number ) ;
						wr_vector_number <= _vector_number ;
					endmethod 
					method Action _aggregation_threshold_m ( Bit#(5) _aggregation_threshold ) ;
						wr_aggregation_threshold <=  _aggregation_threshold ;
					endmethod 
				endinterface ) ;

	endfunction 	

	interface CompletorInterruptSideB_Interface ;
		method Action _interrupt_m ( bit _interupt ) ; 
	endinterface

	function CompletorInterruptSideB_Interface fn_completorInterruptSideB_ifc ( Wire#(bit) wr_interrupt ) ;

		return (
				interface CompletorInterruptSideB_Interface ;
					method Action _interrupt_m (  _interrupt ) ; 
						wr_interrupt <= _interrupt ;
					endmethod
				endinterface ) ;			
	endfunction

endpackage
