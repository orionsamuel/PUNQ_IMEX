c	PROGRAMA PARA O CALCULO DA MEDIA E DO DESVIO PADRAO DOS MODELOS DO COMPLEX,
c	LENDO DADOS A PARTIR DO ARQUIVO MODCOMPLX

	integer Nmod, Npar	!Numero de modelos e numero de parametros de cada modelo
	real, allocatable:: por(:,:), permh(:,:)
	real, allocatable:: media_por(:), dp_por(:)
	real, allocatable:: media_permh(:), dp_permh(:)
	real sum_prop	!Guarda a soma das propriedades
	real ativo(1400)

	Nmod=977
	Npar=976

	allocate(por(Nmod,Npar), permh(Nmod,Npar))
	allocate(media_por(Nmod), dp_por(Nmod))
	allocate(media_permh(Nmod), dp_permh(Nmod))
	
	open(50,file='MODCOMPLX.DAT',status='unknown')

	write(*,*) 'Lendo dados do COMPLEX...'

	do i=1, Nmod
		read(50,*)
		read(50,*) (por(i,j),j=1,Npar)
	enddo

	do i=1, Nmod
		do j=489, Npar
		permh(i,j)=por(i,j)
		enddo
		write(2000,*) permh(i,489)
	enddo
	close(50)

	write(*,*) 'Calculando media e desvio padrao...'

c	MEDIA E DESVIO PADRAO PARA POROSIDADE	
	
	open(4,file='MEDIA_DP_PORO.DAT',status='unknown')
	sum_prop=0.0
	do j=1, Npar/2
		do i=1,Nmod
			sum_prop=sum_prop+por(i,j)
		enddo
		media_por(j)=sum_prop/Nmod
		sum_prop=0.0
	enddo
	

	sum_prop=0.0
	do j=1, Npar/2
		do i=1, Nmod
			sum_prop=sum_prop+(por(i,j)-media_por(j))**2
		enddo
		dp_por(j)=sqrt(sum_prop/Nmod)
		sum_prop=0.0
		write(4,*) j, media_por(j), dp_por(j)
	enddo

c	MEDIA E DESVIO PADRAO PARA PERMEABILIDADE	
	
	open(5,file='MEDIA_DP_PERMH.DAT',status='unknown')

	sum_prop=0.0
	do j=489, Npar
		do i=1, Nmod
			sum_prop=sum_prop+permh(i,j)
		enddo
		media_permh(j)=sum_prop/Nmod
		sum_prop=0.0
	enddo
	
	sum_prop=0.0
	do j=489, Npar
		do i=1, Nmod
			sum_prop=sum_prop+(permh(i,j)-media_permh(j))**2
		enddo
		dp_permh(j)=sqrt(sum_prop/Nmod)
		sum_prop=0.0
		write(5,*) j-488, media_permh(j), dp_permh(j)	
	enddo

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
			write(7,*) dp_por(cont)
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

