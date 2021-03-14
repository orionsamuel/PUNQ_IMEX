c=====================================================
c	subroutine para encher a matrix x com as suas variaveis
c	converte os vetores para o esquema do complex - So O INICIAL!
c
      subroutine inp_cmplx1(k,l,m,n_l, n_active_blocks,
     1por,inf_por,sup_por,x,g,h, active_blocks,reserv_total, n_prop)
	real por(n_prop*n_l), inf_por(n_prop*n_l), sup_por(n_prop*n_l),
     1x(k,l), g(m), h(m), active_blocks(n_prop*n_l) 
     
	real reserv_total(n_prop*n_l), int_active(n_prop*n_active_blocks)
	real inf_por_reserv_total(n_prop*n_l),
     1sup_por_reserv_total(n_prop*n_l)
     	integer n_prop
		
	kactive=0
	kkk=0
	do ll=1, n_prop*n_l
	
		kkk=kkk+1
		if(active_blocks(ll).EQ.0) then
			reserv_total(kkk)=0
			inf_por_reserv_total(kkk)=0
			sup_por_reserv_total(kkk)=0
		else
			kactive = kactive + 1
			int_active(kactive) = kkk
			reserv_total(kkk)=por(ll)
			inf_por_reserv_total(kkk)=inf_por(ll)
			sup_por_reserv_total(kkk)=sup_por(ll)
		endif
			
	enddo
		
	do jp=1, kactive
		
			x(1,jp) = reserv_total(int_active(jp))
			g(jp) = inf_por_reserv_total(int_active(jp))
			h(jp) = sup_por_reserv_total(int_active(jp))
	
	write(1002,*) x(1,jp)
	enddo


	return
      end