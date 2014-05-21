package Pcie_Controller ;

	interface Transaction_Interface ;
//		interface Common_Interface common ;
		interface Transmit_Interface transmit ;
		interface Receive_Interface receive ;
	endinterface
	
	interface Pcie_Interface ;
		interface Transaction_Interface transactions ;
//		interface Configuration_Interface configuration ;
//		interface Error_reporting_Interface error ;
	endinterface

//////////////////////////////////
// Common Interface Definition .
//////////////////////////////////
/*
	interface Common_Interface ;
					
		method Action _user_lnk_up( bit _user_lnk_up ) ;
		method Action _flow_control(Bit#(8) _fc_ph ,Bit#(8) _fc_ph ,Bit#(12) _fc_pd ,Bit#(8) _fc_nph,Bit#(12) _fc_npd,
									   Bit#(8) _fc_cplh, Bit#(12) _fc_cpld  ) ;
		method Bit#(3) fc_sel_ () ;

	endinterface 
 
	function Common_Interface fn_common_interface ( Wire#(bit) wr_user_lnk_up , Reg#(Bit#(3)) rg_fc_sel_ , Wire#(Bit#(8)) wr_fc_ph ,
													Wire#(Bit#(12)) wr_fc_pd , Wire#(Bit#(8)) wr_fc_nph , Wire#(Bit#(12)) wr_fc_npd ,
													Wire#(Bit#(8)) wr_fc_cplh , Wire#(Bit#(12)) wr_fc_cpld ) ;
		return
			(	interface Common_Interface ;
					
					method Action _user_lnk_up( bit _user_lnk_up ) ;
						wr_user_lnk_up <= _user_lnk_up ;
					endmethod 

					method Action _flow_control(Bit#(8) _fc_ph ,Bit#(8) _fc_ph ,Bit#(12) _fc_pd ,Bit#(8) _fc_nph,Bit#(12) _fc_npd,
												Bit#(8) _fc_cplh, Bit#(12) _fc_cpld  ) ;
						wr_fc_ph <= _fc_ph ;
						wr_fc_ph <= _fc_ph ;
						wr_fc_pd <= _fc_pd ;
						wr_fc_nph <= _fc_nph ;
						wr_fc_npd <= _fc_npd ;
						wr_fc_cplh <= _fc_cplh ;
						wr_fc_cpld <= _fc_cpld ;

					endmethod

					method Bit#(3) fc_sel_ () ;
						return rg_fc_sel_ ;
					endmethod

				endinterface ) ;
	endfunction 
	*/
//////////////////////////////////////////////////
// Transmit Interface
//////////////////////////////////////////////////


	interface Transmit_Interface ;
		method bit tx_tlast_ () ;
		method Bit#(32) tx_tdata_ () ;
		method bit tx_tvalid_ () ;
		method Action _tx_tready_m ( bit _tx_tready) ;
		method Bit#(3) tx_tuser_ () ;
		method Action _tx_buf_av_m ( Bit#(6) _tx_buf_av ) ;
		method Action _tx_terr_drop_m ( bit _tx_terr_drop ) ;
		method Action _tx_cfg_req_m ( bit _tx_cfg_req) ;
		method bit tx_cfg_gnt_ () ;			
		method Bit#(4) tx_tkeep_ () ;
	endinterface

	function Transmit_Interface fn_transmit_interface ( Reg#(bit) rg_tx_tlast_ , Reg#(Bit#(32)) rg_tx_tdata_ , Reg#(bit)rg_tx_tvalid_ ,  							Reg#(Bit#(3)) rg_tx_tuser_ ,Reg#(Bit#(4)) rg_tx_tkeep_ , Wire#(bit) wr_tx_tready,                                							Wire#(Bit#(6)) wr_tx_buf_av ,Wire#(bit) wr_tx_terr_drop ,Wire#(bit) wr_tx_cfg_req,Reg#(bit) rg_tx_cfg_gnt_ ) ;

		return(	interface Transmit_Interface ;
					method bit tx_tlast_ () ;
						return rg_tx_tlast_ ;
					endmethod
		
					method Bit#(32) tx_tdata_ () ;
						return rg_tx_tdata_ ;
					endmethod
					
					method bit tx_tvalid_ () ;
						return rg_tx_tvalid_ ;
					endmethod

					method Action _tx_tready_m ( bit _tx_tready) ;
						wr_tx_tready <= _tx_tready ;
					endmethod
					
					method Bit#(3) tx_tuser_ () ;
						return rg_tx_tuser_ ;
					endmethod

					method Action _tx_buf_av_m ( Bit#(6) _tx_buf_av ) ;
						wr_tx_buf_av  <= _tx_buf_av ;
					endmethod

					method Action _tx_terr_drop_m ( bit _tx_terr_drop ) ;
						wr_tx_terr_drop <= _tx_terr_drop ;
					endmethod

					method Action _tx_cfg_req_m ( bit _tx_cfg_req) ;
						wr_tx_cfg_req <= _tx_cfg_req ;
					endmethod

					method bit tx_cfg_gnt_ () ;		
						return rg_tx_cfg_gnt_ ;
					endmethod	

					method Bit#(4) tx_tkeep_ () ;
						return rg_tx_tkeep_ ;
					endmethod 
				endinterface ) ; 

	endfunction

	interface Receive_Interface ;
		method Action _rx_tlast_m (bit _rx_tlast) ;
		method Action _rx_tdata_m (Bit#(32) _rx_tdata) ;
		method Action _rx_tuser_m (Bit#(10) _rx_tuser) ;
		method Action _rx_tkeep_m (Bit#(4) _rx_tkeep) ;
		method Action _rx_tvalid_m (bit _rx_tvalid) ;
		method bit rx_tready_ () ;
		method bit rx_np_ok_ () ;		
	endinterface

	function Receive_Interface fn_receive_interface (Wire#(bit) wr_rx_tlast , Reg#(bit) rg_rx_tready_ , Reg#(bit) rg_rx_np_ok_ , 
								Wire#(Bit#(32)) wr_rx_tdata , Wire#(Bit#(10)) wr_rx_tuser , Wire#(bit) wr_rx_tvalid, Wire#(Bit#(4)) wr_rx_tkeep);

	return(	interface Receive_Interface ;
				method Action _rx_tlast_m (bit _rx_tlast) ;
					 wr_rx_tlast <= _rx_tlast ;
				endmethod

				method Action _rx_tdata_m (Bit#(32) _rx_tdata) ;
					wr_rx_tdata <= _rx_tdata ;
				endmethod

				method Action _rx_tuser_m (Bit#(10) _rx_tuser) ;
					wr_rx_tuser <= _rx_tuser ;
				endmethod

				method Action _rx_tkeep_m (Bit#(4) _rx_tkeep) ;
					wr_rx_tkeep <= _rx_tkeep ;
				endmethod 

				method Action _rx_tvalid_m (bit _rx_tvalid) ;
					wr_rx_tvalid <= _rx_tvalid ;
				endmethod

				method bit rx_tready_ () ;
					return rg_rx_tready_ ;
				endmethod

				method bit rx_np_ok_ () ; 
					return rg_rx_np_ok_ ;
				endmethod 
			endinterface ) ;
	endfunction
				  

endpackage
