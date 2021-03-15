	real permx(700)

	open(1,file='permx_truth.dat',status='unknown')
	open(2,file='permz.dat', status='unknown')
	
	do i=1, 700

		read(1,*) permx(i)
		write(2,*) 0.31*(permx(i)) + 3.12

	enddo

	stop
	end