c===========================================================
c	subroutine para encher a matrix x com as suas variáveis
c	converte os vetores para o esquema do complex - EXCETO INICIAL
c===========================================================
	subroutine inp_cmplx2(k,l,n_l,por,ic,
     .n_active_blocks,active_blocks,reserv_total,int_active,
     .kactive, n_prop)
c
	real por(n_prop*n_l), active_blocks(n_prop*n_l)
	
	real reserv_total(n_prop*n_l), int_active(n_prop*n_active_blocks)
	integer n_prop
	n_prop=2
	kactive=0
	kkk=0
	
	do ll=1, n_prop*n_l
		kkk=kkk+1
		if(active_blocks(ll).EQ.0) then
			reserv_total(kkk)=0
		else
			kactive = kactive + 1
			int_active(kactive) = kkk
			reserv_total(kkk)=por(ll)
		endif
	enddo

	return
	end