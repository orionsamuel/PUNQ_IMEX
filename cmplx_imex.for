c==========================================================
c==========================================================
c	subroutine para pegar o "x" (do complex) e convertê-los
c	para o esquema do IMEX
c	
	subroutine cmplx_imex(k, l, i, n_l, por,
     1int_active, n_active_blocks, active_blocks,x,n_prop)
c
	real por(n_prop*n_l),x(k,l), 
     1int_active(n_prop*n_active_blocks+1)
	real active_blocks(n_prop*n_l), reserv_total(n_prop*n_l)
	integer i, n_prop

	kkk=0
	kactive=0
	
	do ll=1, n_prop*n_l
		kkk=kkk+1
		if(active_blocks(ll).EQ.0) then
			reserv_total(kkk)=0
			por(ll)=.0
		else
			kactive = kactive + 1
			int_active(kactive) = kkk
			reserv_total(kkk)=x(i,kactive)
			por(ll)=x(i,kactive)
		endif
			
	enddo
	
      return
      end
c