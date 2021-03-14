      Program AjusteHist_Complex1 
c-----------------------------------------------------------------------------
c
c     Objetivo: Ajuste histórico. 
c
c     Programador: Walter Medeiros
c
c     Natal, 28 de outubro de 1997
c
c	Modificado por: Aderson do Nascimento
c	Adaptado para o problema de ajuste histórico.
c
c	Natal, 12 de novembro de 2002
c
c	modificado novamente por Aderson do Nascimento (20/07) para 
c	ajuste de histórico do PUNQS3
c-----------------------------------------------------------------------------
c
c     LD = parameter do numero de parametros a serem invertidos
c     KD=*** pelo menos (N+1) = parameter do numero de pontos do complex
c     ND=LD e MD=LD implica ausencia de parametros implicitos
c
c
c	Modificado novamente por Aderson do Nascimento
c	em 16/07/2003
c	Nesta nova versão, em vez de utilizar o modelo toy1_2d,
c	utilizaremos o modelo do PUNQ
c	No modelo do PUNQ, os poços produtores medem:
c	WCUT, GOR e pressão de fundo de poço (BHP)
c	
c	Nesta versão, as variáveis observadas e calculadas receben os índice
c	(subscritos) _obs e _cal. Com isso, eliminou-se aquele problemas de
c	cahmar o valor teórico como sendo o calculado e o do modelo como
c	sendo o experimental. Este probleminha, que causava certa confusão, foi
c	eliminado neste versão.
c	Note que agora, nós estamos invertendo a porosidade!!!
c	A Porosidade se relaciona (neste exemplo) com a permeabilidade
c	através da seguinte relação empírica (Veja Barker et al, SPE Journal
c	2001, ou SPE 74707):
c
c	log(k_h) = 9.02*por + 0.77
c	k_v=0.31*k_h + 3.12	
c-----------------------------------------------------------------------------
	USE DFPORT
      CHARACTER*30INPARDAT, INFDAT, SUPDAT, TRUTHMODEL,
     .MODCOMPLX, DOCUMENTA, ESTCOMPLX, ESTINUND, MODINUND, PERMXTRUTH,
     .PERMZTRUTH,	PORODAT, POROTRUTH, PERMXDAT, PERMZDAT,
     .PUNQS3DAT, ACTIVEBLK, PUNQS3_RPT_WCUT_DAT,
     .PUNQS3_RPT_BHP_DAT,PUNQS3_RPT_GOR_DAT, PUNQS3_RPT_WCUT_OUT,
     .PUNQS3_RPT_BHP_OUT,PUNQS3_RPT_GOR_OUT,
     .PUNQS3_MELHOR_DAT, PUNQS3_RPT_MELHOR_DAT,
     .PUNQS3_RPT_MELHOR_OUT, PUNQS3_PIOR_DAT, PUNQS3_RPT_PIOR_DAT,
     .PUNQS3_RPT_PIOR_OUT, ERRORDAT, POROMELHOR, PERMXMELHOR,
     .PERMZMELHOR, POROPIOR, PERMXPIOR, PERMZPIOR,HOTSTARTDAT,
     .HOTSTARTOUT
c
      character*300 cmd
      character*80 DIR
      LOGICAL MODINUND_FLAG, MODCOMPLX_FLAG, HOTSTART_DAT_FLAG,
     .	HOTSTART_OUT_FLAG
c
      integer Npocos, Nmed_wcut, Nmed_bhp, Nmed_gor, Npor,
     .n_l, n_inund, gamma, flag, p, q, ic, N_pairs,
     .n_active_blocks, i, jpor, NF,n_prop
c
      real Func, wcutobs, wcutcal, bhpobs, bhpcal, gorobs,
     .gorcal, por_min, por_max, sum_std, sum_por, gs
	real(4) tempo_total
c
      REAL std_k, std_wcut, std_bhp, std_gor, lambda_por_glb, delta
c
      real ruido,inund, variacao
      real, allocatable:: active_blocks(:), por_real(:),
     .permx(:), permz(:), inf_por(:), sup_por(:),
	1 permx_real(:),por_truth(:), permH_truth(:) 
c
      real, allocatable:: wcut_obs(:,:), wcut_cal(:,:),
     .bhp_obs(:,:), bhp_cal(:,:), gor_obs(:,:), gor_cal(:,:),
     .por(:), x(:,:), r(:,:)
c
      real, allocatable:: f(:), g(:), h(:), xc(:), x_real_total(:),
     .avg_por(:), std_por(:), tempo_bhp(:), tempo_gor(:),
     .tempo_wcut(:), x_inund(:,:), int_active(:), reserv_total(:),
     .int_real_active(:),inf_por_reserv_total(:),
     .sup_por_reserv_total(:), x_real(:)
     

C
	character*10, allocatable:: yymmdd_wcut(:), yymmdd_bhp(:),
     .yymmdd_gor(:)

      CHARACTER*10 shutin_dates(28)
	integer(4) iseed, iy
c
	data shutin_dates /'1968-01-02','1969-01-01','1970-01-01',
     .'1971-01-01','1972-01-02','1972-01-15','1973-01-02',
     .'1973-01-15','1974-01-02','1974-01-15','1975-01-02',
     .'1975-01-15','1976-01-02','1976-01-15','1977-01-02',
     .'1977-01-15','1978-01-02','1978-01-15','1979-01-02',
     .'1979-01-15','1980-01-02','1980-01-15','1981-01-02',
     .'1981-01-15','1982-01-02','1982-01-15','1983-01-02',
     .'1983-01-15'/
c
c	LEITURA DOS DADOS DE ENTRADA
c
	tempo_total = SECNDS(0.0)
	write(*,*) 'Lendo arquivos de dados:'
	open(0,file='DIR.DAT',status='old')
	read(0,*) DIR
	close(0)
	NF = INDEX(DIR,' ') - 1
	write(*,*)'INITIAL.DAT'
	open(1, file=DIR(:NF)//'INITIAL.DAT', STATUS='OLD')
	READ(1,*)
      READ(1,*)
      READ(1,*)
      READ(1,*)
      READ(1,*) INPARDAT
      READ(1,*)
      READ(1,*) INFDAT
      READ(1,*)
      READ(1,*) SUPDAT
	READ(1,*)
	read(1,*) ACTIVEBLK
	read(1,*)
      READ(1,*) TRUTHMODEL
	READ(1,*)
      READ(1,*) DOCUMENTA
	read(1,*)
	read(1,*) HOTSTART_DAT_FLAG
	read(1,*) HOTSTARTDAT
	read(1,*)
	read(1,*) HOTSTART_OUT_FLAG
	read(1,*) HOTSTARTOUT
	read(1,*)
	read(1,*) POROTRUTH
	read(1,*) PERMXTRUTH
	read(1,*) PERMZTRUTH
	close(1)
c
	write(*,*)'STATS.DAT'
	open(20, file=DIR(:NF)//'STATS.DAT',status='old')
	READ(20,*)
      READ(20,*)
      READ(20,*)
      READ(20,*)
      READ(20,*) ESTCOMPLX
      READ(20,*)
      READ(20,*) ESTINUND
	close(20)
c
	write(*,*)'PARAMOD.DAT'
	open(30,file=DIR(:NF)//'PARAMOD.DAT',status='old')
	READ(30,*)
      READ(30,*)
      READ(30,*)
      READ(30,*)
      READ(30,*) PORODAT
      READ(30,*)
      READ(30,*) PERMXDAT
	READ(30,*)
	READ(30,*) PERMZDAT
      READ(30,*)
	read(30,*) POROMELHOR	
	read(30,*) POROPIOR
	read(30,*)
	read(30,*) PERMXMELHOR
	read(30,*) PERMXPIOR
	read(30,*)
	read(30,*) PERMZMELHOR
	read(30,*) PERMZPIOR
	close(30)
c
	write(*,*)'IMEXFILES.DAT'
	open(50,file=DIR(:NF)//'IMEXFILES.DAT',status='old')
	READ(50,*)
      READ(50,*)
      READ(50,*)
      READ(50,*)
	read(50,*) PUNQS3DAT	
	read(50,*)
	read(50,*) PUNQS3_RPT_WCUT_DAT
	read(50,*) PUNQS3_RPT_BHP_DAT
	read(50,*) PUNQS3_RPT_GOR_DAT
	read(50,*)
	read(50,*) PUNQS3_RPT_WCUT_OUT
	read(50,*) PUNQS3_RPT_BHP_OUT
	read(50,*) PUNQS3_RPT_GOR_OUT
	read(50,*)
	read(50,*) PUNQS3_MELHOR_DAT
	read(50,*) PUNQS3_PIOR_DAT
	close(50)
c
	write(*,*)'SAIDAS.DAT'
	open(70,file=DIR(:NF)//'SAIDAS.DAT',status='old')
	READ(70,*)
      READ(70,*)
      READ(70,*)
      READ(70,*)
	read(70,*) ERRORDAT
	read(70,*)
	read(70,*) MODCOMPLX_FLAG, MODCOMPLX	
	read(70,*)
	read(70,*) MODINUND_FLAG, MODINUND
	close(70)
c     
c     leitura dos dados de entrada
c     INPAR.DAT---dados de entrada e campo de porosidade
c
	n_prop=2
	write(*,*)'INPAR.DAT'
	open(2,file=DIR(:NF)//INPARDAT,status='OLD')
	read(2,*)
	read(2,*) n_l
	Npor=n_prop*n_l
	read(2,*)
	read(2,*) Npocos, Nmed_wcut, Nmed_bhp, Nmed_gor
	read(2,*)
 	read(2,*) n_active_blocks
	close(2)
	
c	dando valores as matrizes dos infimos e supremos de porosidade
	open(3,file=DIR(:NF)//INFDAT,status='unknown')
	open(4,file=DIR(:NF)//SUPDAT,status='unknown')

	open(60, file='PORO_TRUTH.DAT',status='unknown')
	open(61, file='PERMX_TRUTH.DAT',status='unknown')
	
	allocate (inf_por(n_prop*n_l), sup_por(n_prop*n_l),
	1por_truth(n_l),permH_truth(n_l))

	call zera_cube(inf_por, n_prop*n_l)
	call zera_cube(sup_por, n_prop*n_l)
c	call zera_cube(por_truth,n_prop*n_l)
c	call zera_cube(permH_truth, n_prop*n_l)


	do ii=1,n_l
c	camada 1		
		if(ii.LE.140)then
c			inf_por(ii)=0.01
c			sup_por(ii)=.30
		
		read(3,*) inf_por(ii)
		read(4,*) sup_por(ii)	
		endif
c	camada 3	
		if((ii.GT.280).and.(ii.LE.420)) then
c			inf_por(ii)=0.01
c			sup_por(ii)=.30
		
			read(3,*) inf_por(ii)	
			read(4,*) sup_por(ii)
		endif	
c	camada 5
		if((ii.GT.560).and.(ii.LE.n_l)) then
c			inf_por(ii)=0.01
c			sup_por(ii)=.30
	
			read(3,*) inf_por(ii)
			read(4,*) sup_por(ii)
		endif	
c	camada 2
		if((ii.GT.140).and.(ii.LE.280)) then
c			inf_por(ii)=0.01
c			sup_por(ii)=.17
			
			read(3,*) inf_por(ii)
			read(4,*) sup_por(ii)
		endif
c	camada 4
		if((ii.GT.420).and.(ii.LE.560)) then
c			inf_por(ii)=0.01
c			sup_por(ii)=.22
	
			read(3,*) inf_por(ii)
			read(4,*) sup_por(ii)
		endif
	enddo
	
	do ii=1,n_l

c	permeabilidade horizontal			
c			inf_por(ii+n_l)=0.5
c			sup_por(ii+n_l)=1000		
	

			read(3,*) inf_por(ii+n_l)
			read(4,*) sup_por(ii+n_l)
				
	enddo
	close(3)
	close(4)


c	variacao=0.7
c	
c	
c	do ii=1, n_l
c			
c		read(60,*) por_truth(ii)
c		read(61,*) permH_truth(ii)
c	
c	enddo
c
c	close(60)
c	close(61)
c
c	
c	
	do ii=1,n_l
c					
c			inf_por(ii) = por_truth(ii) - por_truth(ii)*variacao			
c			if(inf_por(ii).LT.0.01)then
c				inf_por(ii)=0.01
c			endif
c				
c			sup_por(ii) = por_truth(ii) + por_truth(ii)*variacao
cc			if(sup_por(ii).GT.0.3)then
c				sup_por(ii)=0.3
c				endif
		
c			inf_por(ii+n_l) = permH_truth(ii) -
c	1		 permH_truth(ii)*variacao
c			if(inf_por(ii+n_l).LT.0.5)then
c				inf_por(ii+n_l)=0.5
c			endif
c	
c		sup_por(ii+n_l) = permH_truth(ii) + 
c	1	permH_truth(ii)*variacao
c		if(sup_por(ii+n_l).GT.1000)then
c			sup_por(ii+n_l)=1000
c		endif
	write(1000,*) inf_por(ii) ,sup_por(ii) ,inf_por(ii+n_l) ,
	1sup_por(ii+n_l)
c
	enddo
c		


c	Abrindo o arquivo com as observações  de WCUT, BHP e GOR
c	
	allocate (tempo_wcut(Nmed_wcut),wcut_obs(Nmed_wcut,Npocos))
	allocate (tempo_bhp(Nmed_bhp),bhp_obs(Nmed_bhp,Npocos))
	allocate (tempo_gor(Nmed_gor),gor_obs(Nmed_gor,Npocos))
	allocate (yymmdd_wcut(Nmed_wcut),yymmdd_bhp(Nmed_bhp),
     1yymmdd_gor(Nmed_gor))
	call zera_vet(tempo_wcut,Nmed_wcut)
	call zera_vet(tempo_bhp,Nmed_bhp)
	call zera_vet(tempo_gor,Nmed_gor)
	call zera_mat(wcut_obs,Nmed_wcut,Npocos)
	call zera_mat(bhp_obs,Nmed_gor,Npocos)
	call zera_mat(gor_obs,Nmed_gor,Npocos)
	open(6,file=DIR(:NF)//TRUTHMODEL,status='OLD')
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
			do ii=1,Nmed_wcut
				read(6,*) tempo_wcut(ii), yymmdd_wcut(ii),
     1			(wcut_obs(ii,kk),kk=1, Npocos)
			enddo
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
			do ii=1,Nmed_bhp
				read(6,*) tempo_bhp(ii), yymmdd_bhp(ii), 
	1			(bhp_obs(ii,kk),kk=1, Npocos)
			enddo
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
		read(6,*)
			do ii=1,Nmed_gor
				read(6,*) tempo_gor(ii), yymmdd_gor(ii),
	1			(gor_obs(ii,kk),kk=1, Npocos)
			enddo
	close(6)

c
c	Le o arquivo active_blocks.dat
c
	
	open(5,file=DIR(:NF)//ACTIVEBLK,status='OLD')
	allocate (active_blocks(n_prop*n_l))
	call zera_cube(active_blocks,n_prop*n_l)
	
	do ii=1, n_prop*n_l
		read(5,*) active_blocks(ii)
	enddo
	close(5)
	

c
c	lendo o campo de porosidades verdadeiro.
c	Calcula e escreve a permeabilidade nos arquivos correspondentes
c	A Porosidade se relaciona (neste exemplo) com a permeabilidade
c	através da seguinte relação empírica (Veja Barker et al, SPE Journal
c	2001, ou SPE 74707):
c
c	log(k_h) = 9.02*por + 0.77
c	k_v=0.31*k_h + 3.12	
c	Se o block é um bloco nulo (de acordo com o arquivo ACTIVEBLK),
c	a permeabilidade(x e y) e a porosidade são iguais a zero.
c
	open(8,file=DIR(:NF)//POROTRUTH,status='unknown')
	open(31,file=DIR(:NF)//PERMXTRUTH,status='unknown')
	open(32,file=DIR(:NF)//PERMZDAT,status='unknown')
	allocate (por(n_prop*n_l), permz(n_l))
	call zera_cube(por, n_prop*n_l)
	call zera_cube(permz, n_l)
	
	cont=0
	do ii=1,n_l
		read(8,*) por(ii)
		read(31,*) por(ii+n_l)	
			cont=cont+1		
			permz(cont) = 0.31*por(ii+n_l) + 3.12
		
			write(32,*) permz(cont)	
	enddo
	
	close(8)

	CLOSE(31)

	CLOSE(32)
	
	
c     CUIDADO: de agora em diante k tem funcao especifica!
c
	l=2*n_active_blocks
      k=l+1
      n=l
      m=l
      k0=7
	
c	l=2*l
c	n=2*n
c	m=2*m

c
      open(unit=k0,file=DIR(:NF)//DOCUMENTA,status='unknown')
c      print*,' De os valores de delta e itmax, beta e gama'
c	read(*,*)delta, itmax, beta, gamma
c	print*,'De o valor do "nivel de inundacao"'
c	read(*,*) inund
c	print*,'De o valor da contaminacao (ruido) do modelo verdadeiro'
c	print*,'para gerar o primeiro modelo do COMPLEX'
c	read(*,*) ruido
c	print*,'Entre com a semente (inteira e positiva!)'
c	read(*,*) iseed
c	print*, 'Entre com std_k, std_wcut, std_bhp,'
c	print*, 'std_gor, lambda_por_glb'
c	read(*,*)std_k, std_wcut, std_bhp, std_gor, lambda_por_glb
	
	alpha=1.3
	delta=0.001
	itmax=10000
	beta=5.0e-4
	gamma=2
	inund=1.5e-3
	ruido=.0
	iseed=240
	std_k=2.0
	std_wcut=.1
	std_bhp=5000.
	std_gor=500.
	lambda_por_glb=0.0
c
c	Escreve o vetor por num vetor por_real
c
	allocate (por_real(n_prop*n_l))
	call zera_cube(por_real,n_prop*n_l)
	
	do ii=1, n_prop*n_l
		por_real(ii)=por(ii)
		
	enddo
	
c	
c	Contamina as observações e cria arquivo inicial de porosidade p/ o COMPLEX
c
	call contamina_obs(iseed, n_l,
     1	Nmed_wcut, Nmed_bhp, Nmed_gor, 
     1	Npocos, wcut_obs, bhp_obs, gor_obs,
     1	yymmdd_bhp, yymmdd_wcut, yymmdd_gor,
     1	shutin_dates, por, ruido, n_prop, inf_por, sup_por)
	
c
c	Atribuindo os valores do reservatório verdadeiro a matriz x_real(k,n)
c
	allocate (x_real_total(n_prop*n_l))
	allocate (int_real_active(n_prop*n_l))
	call zera_vet(x_real_total,n_prop*n_l)
	jjj=0
	jjjactive=0
	
	do lll=1, n_prop*n_l
			jjj= jjj + 1
		if(active_blocks(lll).eq.0) then
			x_real_total(jjj)=0
			
			else
			jjjactive = jjjactive + 1
			int_real_active(jjjactive) = jjj
			
			x_real_total(jjj) = por_real(lll)
			
		endif
	
	enddo
		
c
	allocate(x_real(k))
	call zera_vet(x_real,k)
	do kk=1, jjjactive
			x_real(kk)=x_real_total(int_real_active(kk))
	enddo
	
      iy=iseed+290
	allocate(r(k,n))
	call zera_mat(r,k,n)
      do ii=1,n
         do jj=1,k
            r(jj,ii)=ran(iy)
         enddo
      enddo
	
c
c	converte o vetor por para esquema do complex - MODELO INICIAL
c
	
	allocate(x(k,l))
	allocate(x_inund(40*k,n_prop*n_l))
	call zera_mat(x_inund,40*k,n_prop*n_l)
	allocate(f(k))
	call zera_vet(f,k)
	allocate(g(m),h(m))
	call zera_vet(g,m)
	call zera_vet(h,m)
	call zera_mat(x,k,l)
	allocate(reserv_total(n_prop*n_l),
	1int_active(n_prop*n_active_blocks+1))
	call zera_vet(reserv_total,n_prop*n_l)
	call zera_vet(int_active,n_prop*n_active_blocks+1)
	allocate(xc(n))
	call zera_vet(xc,n)
		
c
	write(*,*)'Organizando porosidades num vetor x do COMPLEX...'
c
	allocate(inf_por_reserv_total(n_prop*n_l), 
	1sup_por_reserv_total(n_prop*n_l))
     	

	write(*,*)'Modelo inicial...'
	call inp_cmplx1(k,l,m,n_l, n_active_blocks,
     1por,inf_por,sup_por,x,g,h, active_blocks,reserv_total,n_prop)

c
		
	write(*,*)'Modelos restantes...'
	call inp_cmplx2(k,l,n_l,por,ic,
     1n_active_blocks,active_blocks,reserv_total,int_active,
     1kactive, n_prop)

c
c	chama complex
c
	write(*,*)'Chamando o COMPLEX...'
      call const_max(Npocos, n_active_blocks, int_active,
     1	DIR, NF, MODINUND_FLAG,
	1	HOTSTARTDAT, HOTSTART_DAT_FLAG,HOTSTARTOUT,HOTSTART_OUT_FLAG,
     1	PORODAT, PERMXDAT, PERMZDAT, PUNQS3DAT,
	1	PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_BHP_DAT, PUNQS3_RPT_GOR_DAT,
	1	PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_OUT,
     1	ERRORDAT,Nmed_wcut, Nmed_bhp, Nmed_gor, Npor, active_blocks,
	1	wcut_obs, wcut_cal, bhp_obs, n_l,
     1	bhp_cal, gor_obs, gor_cal,
     1	por, inund, x_inund, n_inund,
     1	k0, kactive, k, l, m,n,itmax,
     1	std_k, std_wcut, std_bhp, std_gor, N_pairs,lambda_por_glb,
     1	alpha,beta,gamma,delta,x,r,f,iev1,iev2,xc,g,h,it,flag, n_prop)
c

      print*,'status:',flag
      print*,'it:',it
	write(k0,*)'status:',flag
	write(k0,*) 'it:',it
	write(k0,*)'if flag=0, a maximum was founded satisfying'
	write(k0,*)'convergence criteria and maximum number'
	write(k0,*)'of iteractions'
	write(*,*)
	write(k0,*)'if flag=1, the maximum number of iteractions'
	write(k0,*)'was exceded'
	
	
c
c	Estatística dos modelos que estao dentro de um certo nivel de 
c	inundacao
c	Registro da estatística das porosidades dos modelos
c	que estao abaixo dum certo nivel de inundacao.
c	Em cada bloco da malha do reservatório
c
*********************************************************************
c 	call estat_inund(jpor, por_min, n_inund,DIR,NF,ESTINUND,
c	1	active_blocks, por_max, x_inund, sum_por, avg_por,
c	1	g, h, k,l,m, n_l, sum_std, std_por,
c     1	int_real_active,x_real_total,n_active_blocks,n_prop)
c	
*********************************************************************
c
c	Estatística no COMPLEX
c
c	Registro da estatística das porosidades do COMPLEX
c	em cada bloco da malha do reservatório
c
*********************************************************************
c	call estat_complex(k, jpor, por_min, int_active,
c    1	DIR,NF,ESTCOMPLX,
c	1	n_active_blocks, active_blocks, por_max, x, sum_por, avg_por,
c	1	g, h,l,m, n_l, sum_std, std_por,x_real, n_prop)
*********************************************************************
c
c	Registro dos valores da função objeto em cada ponto e
c	do x do complex
c
	if(MODCOMPLX_FLAG) then
		open(72,file=DIR(:NF)//MODCOMPLX,status='unknown')
		do jjj=1,k
			write(72,1030) jjj, -f(jjj), (x(jjj,kkk),kkk=1,n)
		enddo
	endif
1030	format('modelo ',i5,1x,'E=',e10.5,1x,/,10(f8.4,1x))
	close(72)
	
c
c	MELHOR MODELO
      write(k0,*)'MELHOR MODELO - COMPLEX ',iev2 
      write(k0,*)'Funcao Objeto = ',abs(f(iev2))
	call cmplx_imex(k, l, iev2, n_l, por,
     1int_active, n_active_blocks, active_blocks,x, n_prop)
c
c	Escrevendo o melhor modelo nos arquivos
c	correspondentes e no documentacao.dat
c
	write(*,*)'Escrevendo o melhor e o pior modelo nos arquivos' 
	write(*,*) 'correspondentes e no documentacao.dat'
c
	
	open(34,file=DIR(:NF)//POROMELHOR,status='unknown')
	open(35,file=DIR(:NF)//PERMXMELHOR,status='unknown')
	open(36,file=DIR(:NF)//PERMZMELHOR,status='unknown')
	
	do ii=1, n_l
		write(34,*) por(ii)
c		write(35,*) 10**(9.02*por(ii)+0.77) !permx
		write(35,*) por(ii+n_l)
		write(36,*) 0.31*por(ii+n_l) + 3.12     !permz
		write(k0,1111) por(ii), por(ii+n_l), !permx
	1	 0.31*por(ii+n_l) + 3.12     !permz


	
	enddo
	close(34)
	close(35)
	close(36)

1111	format(10(f10.4,1x))
	
	
c
c	chamando o IMEX DO MELHOR MODELO
	write(*,*) 
	write(*,*) 'CHAMANDO O IMEX DO MELHOR MODELO'
	cmd='d:\flavio\CMG\mx200410.exe -wait -f '//DIR(:NF)//
	1PUNQS3_MELHOR_DAT//
     1'-dd > lixao.out'
	call system(cmd)


c	PIOR MODELO
      write(k0,*)'PIOR MODELO - COMPLEX ', iev1
      write(k0,*)'Funcao Objeto = ',abs(f(iev1))
	call cmplx_imex(k, l, iev1, n_l, por,
     1int_active, n_active_blocks, active_blocks,x,n_prop)
c
c
c	Escrevendo o pior modelo no arquivo pior.dat e no documentacao.dat
c
	open(37,file=DIR(:NF)//POROPIOR,status='unknown')
	open(38,file=DIR(:NF)//PERMXPIOR,status='unknown')
	open(39,file=DIR(:NF)//PERMZPIOR,status='unknown')
	
	do ii=1, n_l
	
	write(37,*) por(ii)
c	write(38,*) 10**(9.02*por(ii)+0.77) !permx
	write(38,*) por(ii+n_l) !permx
	write(39,*) 0.31*por(ii+n_l) + 3.12     !permz

	write(k0,1111) por(ii), por(ii+n_l), !permx
	1 0.31*por(ii+n_l) + 3.12     !permz
			
	enddo
	close(37)
	close(38)
	close(39)
c
c	chamando o IMEX DO PIOR MODELO
	write(*,*) 
	write(*,*) 'CHAMANDO O IMEX DO PIOR MODELO'
	cmd='d:\flavio\CMG\mx200410.exe -wait -f '//DIR(:NF)//
	1PUNQS3_PIOR_DAT//
     1'-dd > lixao.out'
	call system(cmd)
c
	write(k0,*)'PARAMETROS DE ENTRADA DA SIMULACAO'
	write(k0,*) 'alpha',alpha
	write(k0,*)'delta=',delta
	write(k0,*)'itmax=',itmax
	write(k0,*)'beta=',beta
	write(k0,*)'gamma=',gamma
	write(k0,*)'inund=',inund
	write(k0,*)'ruido=',ruido
	write(k0,*)'semente=',iseed
	write(k0,*)'std_k=',std_k
	write(k0,*)'std_wcut=',std_wcut
	write(k0,*)'std_bhp=',std_bhp
	write(k0,*)'std_gor=',std_gor
	write(k0,*)'N_pairs=',N_pairs
	write(k0,*)'lambda_por_glb=',lambda_por_glb
	
	
c............................................LIBERANDO A MEMÓRIA
	
	tempo_total = SECNDS(s)

	
	WRITE (*,*) 'Tempo total de execucao ',tempo_total/3600., 'horas'
	write(k0,*)'Tempo total de execucao ',tempo_total/3600., 'horas'
	close(unit=k0)
	close(71)
	
	write(*,*)
	write(*,*) 'TERMINEI!!!!!  '
	pause

	deallocate (inf_por,sup_por,tempo_wcut,wcut_obs,
     1yymmdd_wcut,yymmdd_bhp,yymmdd_gor,
     1tempo_bhp,bhp_obs,tempo_gor,gor_obs,active_blocks,
     1por,permx,permz,x_real_total,x_real,por_real,x,g,h,r,f,
     1reserv_total,int_active,inf_por_reserv_total,
     1sup_por_reserv_total,xc)
	
	
	
C	tempo_total = SECNDS(s)

	
C	PRINT *, 'Tempo total de execucao ',tempo_total/3600., 'horas'
C	write(k0,*)'Tempo total de execucao ',tempo_total/3600., 'horas'
C	close(unit=k0)
C	close(71)
      stop
      end