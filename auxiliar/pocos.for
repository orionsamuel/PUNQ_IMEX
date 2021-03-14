	real inf(1400)


	open(10,file='inf_por.dat',status='unknown')
	open(11,file='pocos.dat',status='unknown')	
	
	n_l=1400
	do i=1, n_l
		read(10,*) inf(i)
		if(i.LE.700)then
		
			if(inf(i).NE.0.01)then
				write(11,*) i , inf
			endif
		endif
		
		if(i.GT.700)then
			if(inf(i).NE.0.5)then
				write(11,*) i , inf
			endif
		endif
	enddo
	
	close(10)
	close(11)
	stop
	end