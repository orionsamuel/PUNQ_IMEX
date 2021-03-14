c===============================================================
c	subroutine inp_x_inund: para encher uma matriz x_inund com
c	suas veriaveis 	. Convert os vetores para o esquema do complex
c================================================================
	subroutine inp_x_inund(k, l, n_l, n_inund,
     1	por, x_inund, n_prop)
	real por(n_prop*n_l), x_inund(40*k,n_prop*n_l)
c
	
	do ll=1, n_prop*n_l
			
		jpor = ll
		x_inund(n_inund,jpor) = por(ll)
		
	enddo
	
	return
	end