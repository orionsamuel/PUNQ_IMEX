c=======================================================
c=======================================================
c	subroutine zera_mat
c
	subroutine zera_cube(dados, xx)
	integer xx
	real(4) dados(xx) 
	
	do kk=1,xx
		dados(kk)=0.0	
	enddo

	return
	end