	integer pl	!Pula linha
	integer it	!Identifica iteracao
	character*45 nome(4)	!Nomes entre os modelos do documentacao
	integer mode(5000)	!Identificacao do modelo
	real, allocatable:: func(:)	!valor da funcao objeto
	character*4 funcao !Guarda o nome func=
	real, allocatable:: par(:,:)	!Guarda os parametros dos modelos
	integer num_p, itmax
	character*10 pare, lixo
	real, allocatable:: media_por(:), dp(:)	!media e desvio padrao por bloco
	real, allocatable:: media_permh(:), dp_permh(:) !media e dp para permh
	real sum_por	!soma porosidade
	integer n_inund
	real inund
	integer ativo(1400)

	num_p=976	 !Numero de parametros do complex: poro e permh	
	pl=100735	 !Pula os modelos nao iterados
	mod=5000	 !Espaco nas matrizes e vetores
	inund=1.3e-3 !Niveo de inundacao


	open(1,file='DOCUMENTACAO.DAT',status='unknown')
	
	open(2, file='modelos_iterados_poro.DAT',status='unknown')
	
	open(3, file='modelos_iterados_perm.DAT',status='unknown')

	write(*,*) 'Lendo o documentacao...'		
	do i=1, pl
		read(1,*)
	enddo
	
	allocate(par(mod,num_p), media_por(5000), dp(5000), func(5000))
	allocate(media_permh(mod), dp_permh(mod))

	cont=0
10	read(1,*)
	read(1,*)
	read(1,*) lixo, itmax
		read(1,*) nome(1), it
		
		if(nome(1).EQ."status:")then	!detecta o fim do documentacao
			itmax=itmax-1
			go to 12
		endif
		
		read(1,*) nome(2), mode(it), funcao, func(it) 
		read(1,*) nome(3)
				
			if(nome(3).EQ."corrected point")then
				write(2,*) nome(1), it
				write(2,*) nome(2), mode(it), funcao, func(it) 
				write(2,*) nome(3)

				write(3,*) nome(1), it
				write(3,*) nome(2), mode(it), funcao, func(it) 
				write(3,*) nome(3)
				
				read(1,*) (par(mode(it),j),j=1,num_p)
			
				write(2,*) (par(mode(it),j),j=1,num_p/2)

				write(3,*) (par(mode(it),j),j=489,num_p)
			
			do i=1, 197 
				read(1,*)
			enddo
c			if(cont.EQ.6)then
c				write(*,*)
c				write(*,*) cont
c				pause
c			endif
			cont=cont+1		
			
			go to 10
			
		else
		
11				read(1,*) nome(2), mode(it), funcao, func(it) 
				read(1,*) nome(3)

				write(2,*) nome(1), it
				write(2,*) nome(2), mode(it), funcao, func(it) 
				write(2,*) nome(3)
				
				write(3,*) nome(1), it
				write(3,*) nome(2), mode(it), funcao, func(it) 
				write(3,*) nome(3)

			if(nome(3).EQ."corrected point")then
				read(1,*) (par(mode(it),j),j=1,num_p)
			
				write(2,*) (par(mode(it),j),j=1,num_p/2)

				write(3,*) (par(mode(it),j),j=489,num_p)
				
			
			do i=1, 197 
				read(1,*)
			enddo
				else
				go to 11
			
			endif					
			go to 10
			endif
		
12	write(*,*) 'Calculando a media e o desvio padrao por bloco'		

	open(4,file='media_dp_poro.dat',status='unknown')

c	MEDIA E DESVIO PADRAO PARA POROSIDADE	
	sum_por=0.0
	n_inund=0.0
	do j=1, num_p/2
		do it=1,itmax
			if(abs(func(it)).LE.inund)then
				sum_por=sum_por+par(mode(it),j)
				n_inund=n_inund+1
			endif
		enddo
		media_por(j)=sum_por/n_inund
		write(1000,*) media_por(j)
		sum_por=0.0
		n_inund=0
	enddo
	
	n_inund=0
	sum_por=0.0
	do j=1, num_p/2
		do it=1, num_p
			if(abs(func(it)).LE.inund)then
				sum_por=sum_por+(par(mode(it),j)-media_por(j))**2
				n_inund=n_inund+1
			endif		
		enddo
		dp(j)=sqrt(sum_por/n_inund)
		sum_por=0.0
		n_inund=0
		write(4,*) j, media_por(j), dp(j)
	enddo

	


	open(5,file='media_dp_perm.dat',status='unknown')

c	MEDIA E DESVIO PADRAO PARA PERMEABILIDADE	
	sum_por=0.0
	n_inund=0
	do j=489, num_p
		do it=1,itmax
			if(abs(func(it)).LE.inund)then
				sum_por=sum_por+par(mode(it),j)
				n_inund=n_inund+1
			endif
		enddo
		media_permh(j)=sum_por/n_inund
		sum_por=0.0
		n_inund=0
	enddo
	
	n_inund=0
	sum_por=0.0
	do j=489, num_p
		do it=1, num_p
			if(abs(func(it)).LE.inund)then
				sum_por=sum_por+(par(mode(it),j)-media_permh(j))**2
				n_inund=n_inund+1
			write(1001,*) par(mode(it),489), mode(it) 
			endif
		enddo
		dp_permh(j)=sqrt(sum_por/n_inund)
		sum_por=0.0
		n_inund=0
		write(5,*) j-488, media_permh(j), dp_permh(j)	
	enddo

	write(*,*) 'Fechando arquivos'

	close(1)
	close(2)
	close(3)
	close(4)
	close(5)
	
	write(*,*) 'Escrevendo nos arquivos para os mapas...'

	open(6,file='MEDIA_POR_IMEX.DAT',status='unknown')

	open(7,file='DP_POR_IMEX.DAT',status='unknown')

	open(8,file='MEDIA_PERMH_IMEX.DAT',status='unknown')

	open(9,file='DP_PERMH_IMEX.DAT',status='unknown')

	open(20,file='ACTIVE_BLOCKS.DAT',status='unknown')
	
	do i=1, 1400
		read(20,*) ativo(i)
	enddo
	close(20)
		
	cont=1
	do i=1, 700
		if(ativo(i).EQ.1)then
			write(6,*) media_por(cont)
			write(7,*) dp(cont)
			cont=cont+1
		else
			write(6,*) 0.0			
			write(7,*) 0.0	
		endif
	enddo	
		
	cont=489
	do i=701, 1400
		if(ativo(i).EQ.1)then
			write(8,*) media_permh(cont)
			write(9,*) dp_permh(cont)
		cont=cont+1
		else
			write(8,*) 0.0
			write(9,*) 0.0
		endif
	enddo	
	
	close(6)
	close(7)
	close(8)
	close(9)

	write(*,*) 'FIM'
	stop
	end