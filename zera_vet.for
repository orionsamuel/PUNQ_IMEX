c=========================================================
c	subroutine zera_vet
c========================================================
	subroutine zera_vet(vetor,xx)
	integer xx
	real(4) vetor(xx)
	do ii=1, xx
		vetor(ii)=0.
	enddo
	return
	end