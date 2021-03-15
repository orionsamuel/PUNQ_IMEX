c	PROGRAMA PARA LER O MODELO VERDADEIRO COM RUIDO NAS OBSERVACOES, LER 
C	A SAIDA DO COMPLEX PARA O MELHOR E O PIOR MODELO E IMPRIMIR UM ARQUIVO
C	DE ONDE PODE-SE CONSTRUIR GRAFICOS COMPARANDO O MODELO ORIGINAL
C	COM RUIDO E O MELHOR E O PIOR MODELO DO COMPLEX.

C	OBS: NAO ESQUECER DE ADICIONAR UMA LINHA NO INICIO DOS ARQUIVOS 
C	PARA O MELHOR E O PIOR MODELO DO COMPLEX

	integer Nmed, Npocos	!Numero de medidas e de pocos
	real, allocatable:: m_wct(:,:), m_bhp(:,:), m_gor(:,:) !medidas das propriedades com ruido
	real, allocatable:: dia(:,:), gas(:,:),agua(:,:),pres(:,:) !propriedades do modelo tentativa
	real, allocatable:: gas_p(:,:),agua_p(:,:),pres_p(:,:)!propriedades do pior modelo
	
	character*10, allocatable:: dat(:,:)
	integer prod(6)
	real, allocatable:: troca(:,:)

	prod(1)=4
	prod(2)=5
	prod(3)=11
	prod(4)=12
	prod(5)=15
	prod(6)=1

	Nmed=83
	Npocos=6

	allocate(m_wct(Nmed,Npocos+1), m_bhp(Nmed,Npocos+1), 
	1m_gor(Nmed,Npocos+1),troca(Nmed,Npocos))
	
	allocate(dia(Nmed,Npocos), dat(Nmed,Npocos), gas(Nmed,Npocos),
	1agua(Nmed,Npocos),pres(Nmed,Npocos))

	allocate(gas_p(Nmed,Npocos),
	1agua_p(Nmed,Npocos),pres_p(Nmed,Npocos))

	open(1,file='MODEL_RUIDO_WCT.DAT',status='unknown')
	open(2,file='MODEL_RUIDO_BHP.DAT',status='unknown')
	open(3,file='MODEL_RUIDO_GOR.DAT',status='unknown')

c	LENDO ARQUIVO COM AS OBSERVACOES DO MODELO VERDADEIRO COM RUIDO	
	do i=1, Nmed 
		read(1,*) (m_wct(i,j),j=1,Npocos)
		m_wct(i,7)=m_wct(i,1)
		read(2,*) (m_bhp(i,j),j=1,Npocos)
		m_bhp(i,7)=m_bhp(i,1)
		read(3,*) (m_gor(i,j),j=1,Npocos)
		m_gor(i,7)=m_gor(i,1)
	enddo

	close(1)
	close(2)
	close(3)
	

C	LENDO ARQUIVO COM OS DADOS CALCULADOS PELO MELHOR E O PIOR MODELO DO COMPLEX

	open(4,file='MELHOR_MODELO.DAT',status='unknown')
	open(5,file='PIOR_MODELO.DAT',status='unknown')

	do j=1, Npocos
		do pula=1, 8
			read(4,*)
			read(5,*)
		enddo	
	
		do i=1, Nmed
			read(4,*) dia(i,j), dat(i,j), gas(i,j), 
	1		agua(i,j), pres(i,j)
			read(5,*) dia(i,j), dat(i,j), gas_p(i,j), 
	1		agua_p(i,j), pres_p(i,j)
			dia(i,j)=dia(i,j)/365
		enddo
	enddo	
	
	open(10,file='TRUTH_RUIDO_MELHOR_PIOR.DAT',status='unknown')

	do j=1,Npocos
		write(10,40) 'prod',prod(j)
40		format(a4,i2)
		
		write(10,20) 'tempo(anos),', 'tempo(data),','gor_truth,',
	1 'wct_truth,','bhp_truth,','gor_melhor,',   
     1 'wct_melhor,','bhp_melhor,',',gor_pior,','wct_pior,','bhp_pior'
20		format(a12,1x,a12,3x,a10,1x,a10,3x,a10,4x,a10,1x,
	1a10,1x,a10,1x,a9,1x,a9,1x,a9)
		
		do i=1, Nmed
			write(10,30) dia(i,j),dat(i,j),m_gor(i,j+1),m_wct(i,j+1),
	1m_bhp(i,j+1),gas(i,j),agua(i,j),pres(i,j),
	1gas_p(i,j),agua_p(i,j),pres_p(i,j)
30			format(1(f4.1,',',8x,a10),',',1(1x,f11.5),','1(1x,e11.5),
	1',',(1x,f15.5),',',1(1x,f11.5),',',1(4x,e11.5),',',1(3x,f15.5),
     1',',1(1x,f11.5),',',1(4x,e11.5),',',1(3x,f15.5))
		enddo
	enddo

	close(10)
	write(*,*) 'CONCLUIDO!'
	stop
	end