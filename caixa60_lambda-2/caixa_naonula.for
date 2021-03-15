	real inf_por(1400), sup_por(1400), truth(1400)
	real inf_por_naonulo(976),sup_por_naonulo(976), truth_naonulo(976)
		
	open(1,file='inf_por.dat',status='unknown')
	open(2,file='sup_por.dat',status='unknown')
	open(3,file='truth_por_permh.dat',status='unknown')
	open(4,file='inf_truth_sup.dat',status='unknown')


	cont=1
	do i=1, 1400
		read(1,*) inf_por(i)
		read(2,*) sup_por(i)
		read(3,*) truth(i)	
		if(inf_por(i).NE.0.0)then
			inf_por_naonulo(cont)=inf_por(i)
			sup_por_naonulo(cont)=sup_por(i)
			truth_naonulo(cont)=truth(i)
			cont=cont+1
		endif
	enddo
	close(1)
	close(2)
	close(3)

	write(4,*) 'contador  ','inf  ','truth  ','sup'

	do i=1, 976
		write(4,*) i, inf_por_naonulo(i), truth_naonulo(i),
	1	sup_por_naonulo(i)
	enddo
	close(4)
	
	stop
	end