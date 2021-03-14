c=======================================================
c=======================================================
c	subroutine zera_mat
c
	subroutine zera_mat(dados,xx,yy)
	integer xx, yy
	real(4) dados(xx,*)
	do jj=1, yy
		do kk=1,xx
			dados(kk,jj)=0.
		enddo
	enddo
	return
	end