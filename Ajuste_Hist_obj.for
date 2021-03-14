	subroutine func_comp(k, l, x, i, n_l,
     1	DIR, NF, PORODAT, PERMXDAT, PERMZDAT, PUNQS3DAT,
	1	PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_BHP_DAT, PUNQS3_RPT_GOR_DAT,
	1	PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_OUT,
     1	ERRORDAT, Nmed_wcut, Nmed_bhp, Nmed_gor,
     1	Npocos, Npor, por, active_blocks, int_active, n_active_blocks,
     1	wcut_obs, bhp_obs, gor_obs, std_k, std_wcut, std_bhp, std_gor,
     1	N_pairs,lambda_por_glb, Func, n_prop, modelo)
c-----------------------------------------------------------------------------
c
c     Objetivo: calcular a funcao objeto para o PUNQS3
c
c     Parametros de Entrada:
c
c     NDM_WCUT = dimension de Nmed_wcut
c	NDM_BHP = dimension de Nmed_bhp
c	NDM_GOR = dimension de Nmed_gor
c     NDC = dimension de Npocos
c	Npoco = numero de poços
c     Npor = numero de porosidade
c     Nmed_wcut = numero de medidas de WCUT
c	Nmed_bhp = numero de medidas de BHP
c	Nmed_gor = numero de medidas de GOR
c     wcut_obs(j,k)= WCUT observado
c	wcut_cal(j,k) = WCUT calculado (tentativa)
c     bhp_obs(j,k)= BHP observado
c	bhp_cal(j,k) = BHP calculado (tentativa)
c     gor_obs(j,k)= GOR observado
c	gor_cal(j,k) = GOR calculado (tentativa)
c	std_k = desvio padrão na permeabilidade
c	std_wcut = desvio padrão no WCUT
c	std_bhp = desvio padrão no BHP
c	std_gor = desvio padrão no GOR
c	lambda_por_glb = multiplicador de Lagrange para a suavidade global
c	N_pirs =  número de pares vizinhos suaves
c
c     Parametros de Saida:
c
c     Func = funcao objeto 
c     f(i) = valor da função objeto do modelo i
c     Programador: Walter Medeiros
c
c     Natal, 28 de outubro de 1997
c	
c	Modificado por Aderson do Nascimento,
c	paara calcular a função objeto de curvas de Water Cut SC
c	Natal, 13 de novembro de 2002
c
c	Modificado por Aderson do Nascimento
c	para incorporar o vínculo de suavidade global.
c	Natal, 8 de abril de 2003
c
c	Modificado novamente por Aderson do Nascimento
c	em 16/07/2003
c	Nesta nova versão, em vez de utilizar o modelo toy1_2d,
c	utilizaremos o modelo do PUNQ
c	No modelo do PUNQ, os poços produtores medem:
c	WCUT, GOR e pressão de fundo de poço (BHP)
c-----------------------------------------------------------------------------
	real por(n_prop*n_l), x(k,l)
	character*30 PORODAT, PERMXDAT, PERMZDAT,PUNQS3DAT,
     1PUNQS3_RPT_WCUT_DAT, PUNQS3_RPT_WCUT_OUT, PUNQS3_RPT_BHP_DAT,
     1PUNQS3_RPT_BHP_OUT, PUNQS3_RPT_GOR_DAT, PUNQS3_RPT_GOR_OUT,
     1ERRORDAT
	character*300 cmd
	character*80 DIR
	real tempo_wcut(Nmed_wcut),tempo_bhp(Nmed_bhp),
     1tempo_gor(Nmed_gor), wcut_obs(Nmed_wcut,Npocos),
     1bhp_obs(Nmed_bhp,Npocos), gor_obs(Nmed_gor,Npocos),
     1wcut_cal(Nmed_wcut,Npocos), bhp_cal(Nmed_bhp,Npocos),
     1gor_cal(Nmed_gor,Npocos), active_blocks(n_prop*n_l),
	1int_active(n_prop*n_active_blocks+1)
	integer i, NF, n_c, n_ll, modelo
	real aux, lambda_por_glb
c
c	Transforma do complex para o formato do imex
c
c
	n_c=10
	n_ll=14
	write(*,*)'Transformando do COMPLEX para o IMEX...'
	call cmplx_imex(k, l, i, n_l, por,
     1int_active, n_active_blocks, active_blocks,x,n_prop)
c
	open(31,file=DIR(:NF)//PORODAT,status='unknown')
	open(32,file=DIR(:NF)//PERMXDAT,status='unknown')
	open(33,file=DIR(:NF)//PERMZDAT,status='unknown')
	
	do ll=1, n_l
	
			write(31,*) por(ll)
c			write(32,*) 10**(9.02*por(ll)+0.77) !permx
c			write(33,*) 0.37*((por(ll,mm,kk)**(2.834))*43680.6) + 0.01			
			write(32,*) por(ll+700) !permx
			write(33,*) 0.31*(por(ll+700)) + 3.12 !permz
		
	enddo
	close(31)
	close(32)
	close(33)
		
c				CHAMANDO O IMEX
c
	write(*,*)'Calculando o problema direto...'
	cmd='d:\flavio\CMG\mx200410.exe -wait -f '//DIR(:NF)//PUNQS3DAT//
     1'-dd > lixao.out'
	call system(cmd)
	write(*,*)'******'
	write(*,*)'MODELO=', i
	write(*,*)'******'
	modelo = i

c
c				CHAMANDO O REPORT
c
	write(*,*)'Chamando o report.exe para extrair as observacoes'
	write(*,*) 'de WCUT, BHP e GOR...'
	cmd='d:\flavio\CMG\report.exe -f '//DIR(:NF)//
	1PUNQS3_RPT_WCUT_DAT//'
     1-o '//DIR(:NF)//PUNQS3_RPT_WCUT_OUT//' > lixao.out'
	call system(cmd)
	cmd='d:\flavio\CMG\report.exe -f '//DIR(:NF)//
	1PUNQS3_RPT_BHP_DAT//'
     1-o '//DIR(:NF)//PUNQS3_RPT_BHP_OUT//' > lixao.out'
	call system(cmd)
	cmd='d:\flavio\CMG\report.exe -f '//DIR(:NF)//
	1PUNQS3_RPT_GOR_DAT//'
     1-o '//DIR(:NF)//PUNQS3_RPT_GOR_OUT//' > lixao.out'
	call system(cmd)
c	
c				NOW READING THE OUTPUT FILE FROM THE REPORT COMMAND
c
c	PARA WCUT...
c
	write(*,*)'Lendo saida do report.exe'
	call zera_mat(wcut_cal,Nmed_wcut,Npocos)
	call zera_vet(tempo_wcut,Nmed_wcut)
	NmedLoc_wcut=Nmed_wcut
		open(51,file=DIR(:NF)//PUNQS3_RPT_WCUT_OUT,status='unknown')
				read(51,*)
				read(51,*)
				read(51,*)
				read(51,*)
				read(51,*)
				read(51,*)
				icountMed=0
					do ii=1,NmedLoc_wcut
						read(51,*,END=10) tempo_wcut(ii),
	1					(wcut_cal(ii,kk), kk=1,Npocos)
						icountMed=icountMed+1
					end do
		close(51)
  10	if(icountMed.LT.NmedLoc_wcut) then
			write(*,*) 'arquivo com numero de medidas pequeno...'
			NmedLoc_wcut=icountMed
			open(71,file=DIR(:NF)//ERRORDAT,status='unknown')
	write(71,*)'MODELO',i,'POSSUI APENAS',icountMed,'MEDIDAS de WCUT'
				close(51)

	end if
c
c
c	PARA BHP...
c
	NmedLoc_bhp=Nmed_bhp
	call zera_mat(bhp_cal,Nmed_bhp,Npocos)
	call zera_vet(tempo_bhp,Nmed_bhp)
		open(52,file=DIR(:NF)//PUNQS3_RPT_BHP_OUT,status='OLD')
				read(52,*)
				read(52,*)
				read(52,*)
				read(52,*)
				read(52,*)
				read(52,*)
				icountMed=0
					do ii=1,NmedLoc_bhp
						read(52,*,END=11) tempo_bhp(ii),
	1					(bhp_cal(ii,kk), kk=1,Npocos)
						icountMed=icountMed+1
					end do
		close(52)
  11	if(icountMed.LT.NmedLoc_bhp) then
			write(*,*) 'arquivo com numero de medidas pequeno...'
			NmedLoc_bhp=icountMed
			open(711,file=DIR(:NF)//ERRORDAT,status='unknown')
	write(21,*)'MODELO',i,'COM APENAS',icountMed,'MEDIDAS de BHP'
				close(52)

	end if
c
c	PARA GOR...
c
	NmedLoc_gor=Nmed_wcut
	call zera_mat(gor_cal,Nmed_gor,Npocos)
	call zera_vet(tempo_gor,Nmed_gor)
		open(53,file=DIR(:NF)//PUNQS3_RPT_GOR_OUT,status='OLD')
				read(53,*)
				read(53,*)
				read(53,*)
				read(53,*)
				read(53,*)
				read(53,*)
				icountMed=0
					do ii=1,NmedLoc_gor
						read(53,*,END=12) tempo_gor(ii),
	1					(gor_cal(ii,kk), kk=1,Npocos)
						icountMed=icountMed+1
					end do
		close(53)
  12	if(icountMed.LT.NmedLoc_gor) then
			write(*,*) 'arquivo com numero de medidas pequeno...'
			NmedLoc_gor=icountMed
			open(71,file=DIR(:NF)//ERRORDAT,status='unknown')
	write(71,*)'MODELO',i,'COM APENAS',icountMed,'MEDIDAS de GOR'
				close(53)

	end if
c
      F_tot = 0.0
	F_wcut = 0.0
	F_bhp = 0.0
	F_cal = 0.0
	F_gor = 0.0
c
c     para cada POÇO...
C	MINIMIZANDO MODELO COM OBSERVAÇÕES de WCUT
 	do ipp=1, Npocos
		do iqq=1, NmedLoc_wcut
			wcutobs = wcut_obs(iqq,ipp)
			wcutcal = wcut_cal(iqq,ipp)
			F_wcut = F_wcut + (wcutobs-wcutcal)**2
			enddo
	enddo
	WRITE(*,*) 'Fwcut=', F_wcut
c
c     para cada POÇO...
C	MINIMIZANDO MODELO COM AS OBSERVAÇÕES DE BHP
 	do ipp=1, Npocos
		do iqq=1, NmedLoc_bhp
			bhpobs = bhp_obs(iqq,ipp)
			bhpcal = bhp_cal(iqq,ipp)
			F_bhp = F_bhp + (bhpobs-bhpcal)**2
C			WRITE(*,*) 'Fbhp=', F_bhp
C			PAUSE
		enddo
	enddo
	WRITE(*,*) 'Fbhp=', F_bhp
c
c     para cada POÇO...
C	MINIMIZANDO MODELO COM AS OBSERVAÇÕES DE GOR
 	do ipp=1, Npocos
		do iqq=1, NmedLoc_gor
			gorobs = gor_obs(iqq,ipp)
			gorcal = gor_cal(iqq,ipp)
			F_gor = F_gor + (gorobs-gorcal)**2
		
		enddo
	enddo
	WRITE(*,*) 'Fgor=', F_gor
C	Agora o vínculo de suavidade global
c
	open(45,file='C:\temp_flavio001\DIF_PARES.DAT',status='unknown')
	F_glb = 0.0
	N_pairs=0.0
	j=0
100	do jj=1, n_l
		
		if(mod(jj,10).EQ.0.AND.mod(jj,140).NE.0) then
c			write(*,*) 'ponta jj= ', jj
c			pause
			if(jj.NE.n_ll) then
				if(active_blocks(jj).EQ.1.
     1			AND.active_blocks(jj+n_c).EQ.1) then
				F_glb = F_glb +
     1			((por(jj) - por(jj+n_c))**2)+
	1			((log(por(jj+n_l)) - log(por(jj+n_c+n_l)))**2)
				N_pairs = N_pairs + 1
				write(45,*) (por(jj) - por(jj+n_c))**2
				write(45,*)(log(por(jj+n_l)) - 
	1			log(por(jj+n_c+n_l)))**2
				endif
			endif
		endif		
		
		if(jj.EQ.131.or.jj.EQ.271.or.jj.EQ.411.or.jj.EQ.551.or.jj.EQ.
	1691)then
			j=1
		endif

		if(jj.EQ.141.or.jj.EQ.281.or.jj.EQ.421.or.jj.EQ.561)then
			j=0
	
		endif
c		if(j.EQ.0)then
c		write(*,*) 'j=' , j, 'jj=' ,jj
c		pause
c		endif
		if(j.EQ.1) then
									
c			write(*,*) 'Linha de baixo j=' , j, 'jj=' ,jj
c			pause
			

c			if(jj.NE.n_c) then
				if(active_blocks(jj).EQ.1.
     1 				AND.active_blocks(jj+1).EQ.1) then
c				write(*,*) 'Linha de baixo com bloco ativo, jj= ', jj
c				pause
							F_glb = F_glb +
     1 						((por(jj) - por(jj+1))**2)+
	1					((log(por(jj+n_l)) - log(por(jj+1+n_l)))**2)
							N_pairs = N_pairs + 1
					write(45,*) (por(jj) - por(jj+1))**2
					write(45,*) (log(por(jj+n_l)) - 
	1				log(por(jj+1+n_l)))**2
					endif
				endif
				
			if(mod(jj,10).NE.0.AND.mod(jj,140).NE.0.and.j.EQ.0) then
						
c						if(active_blocks(jj).EQ.0)then
c						write(*,*) 'Pelo meio jj= ', jj
c						write(*,*) 'Bloco nulo'
c						pause
c						endif
						
						if((active_blocks(jj).EQ.1).
     1					AND.(active_blocks(jj+1).EQ.1.
     1					AND.active_blocks(jj+n_c).EQ.1)) then
							F_glb = F_glb +
     .						((por(jj) - por(jj+1))**2) +
     .						((por(jj) - por(jj+n_c))**2)+
     .					((log(por(jj+n_l)) - log(por(jj+1+n_l)))**2) +
     .					((log(por(jj+n_l)) - log(por(jj+n_c+n_l)))**2)
					
					write(45,*) (por(jj) - por(jj+1))**2
					write(45,*) (por(jj) - por(jj+n_c))**2
					write(45,*) (log(por(jj+n_l)) -
	1				 log(por(jj+1+n_l)))**2
					write(45,*) (log(por(jj+n_l)) - 
	1				log(por(jj+n_c+n_l)))**2
							N_pairs = N_pairs + 2
c						write(*,*) 'Pelo meio jj= ', jj
c						write(*,*) 'O da frente e o de baixo ativos'
c						pause
						endif
						if((active_blocks(jj).EQ.1).
     1					AND.(active_blocks(jj+n_c).EQ.0.
     1					AND.active_blocks(jj+1).EQ.1)) then
							F_glb = F_glb +
     .						((por(jj) - por(jj+1))**2)+
     .					((log(por(jj+n_l)) - log(por(jj+1+n_l)))**2)
							N_pairs=N_pairs+1
					write(45,*) (por(jj) - por(jj+1))**2
					write(45,*) (log(por(jj+n_l)) - 
	1				log(por(jj+1+n_l)))**2

c						write(*,*) 'Pelo meio jj= ', jj
c						write(*,*) 'o da frente ativo'
c						pause
						endif
						
						if((active_blocks(jj).EQ.1).
     1					AND.(active_blocks(jj+n_c).EQ.1.
     1					AND.active_blocks(jj+1).EQ.0)) then
							F_glb = F_glb +
     .					((por(jj) - por(jj+n_c))**2)+
     .					((log(por(jj+n_l)) - log(por(jj+n_c+n_l)))**2)
						N_pairs=N_pairs+1
				write(45,*) (por(jj) - por(jj+n_c))**2
				write(45,*) (log(por(jj+n_l)) - 
	1			log(por(jj+n_c+n_l)))**2
c						write(*,*) 'Pelo meio jj= ', jj
c						write(*,*) 'o de baixo ativo'
c						pause
			endif
					
		endif
	enddo
	close(45)
	open(65, file='C:\temp_flavio001\SUAVIDADE.DAT',status='unknown')
	 
	WRITE(*,*) 'F_GLB =', F_glb	
	WRITE(*,*) 'N_PAIRS', N_pairs							
	f_norm_bhp=(std_wcut**2)/((std_bhp**2)*(NmedLoc_bhp*Npocos))
	f_norm_gor=(std_wcut**2)/((std_gor**2)*(NmedLoc_gor*Npocos))
	f_norm_suave=(std_wcut**2)/((std_k**2)*N_pairs)
c
c	aux=lambda_por_glb*f_norm_suave*F_glb
	
	write(*,*) 'WCT',F_wcut/(NmedLoc_wcut*Npocos)
	write(*,*) 'BHP',F_bhp*f_norm_bhp
	write(*,*) 'GOR',F_gor*f_norm_gor
	write(*,*) 'GLB',lambda_por_glb*f_norm_suave*F_glb
	
	WRITE(65,*) 'Modelo',i,'Dif_blocos',F_glb ,f_norm_suave,  
	1lambda_por_glb*f_norm_suave*F_glb 
	
	F_tot = F_wcut/(NmedLoc_wcut*Npocos) +
     1		F_bhp*f_norm_bhp +
	1		F_gor*f_norm_gor + 
     1		lambda_por_glb*f_norm_suave*F_glb
	Func= F_tot
	write(*,*) 'FUNCAO_OBETIVO', Func
	pause
	return
      end
	
	
