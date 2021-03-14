	subroutine contamina_obs(iseed, n_l,
     1	Nmed_wcut, Nmed_bhp, Nmed_gor, 
     1	Npocos, wcut_obs, bhp_obs, gor_obs,
     1	yymmdd_bhp, yymmdd_wcut, yymmdd_gor,
     1	shutin_dates, por, ruido,n_prop, inf_por, sup_por)
c
c	Criando os resultados contaminados com ruído gaussiano para
c	WCUT, BHP e GOR. Os ruídos são criados seguindo as diretivas
c	do PUNQ http://www.nitg.tno.tno.nl/punq/cases/punqs3/Introduction.html
c
c	PARA o WCUT...
c	The WCUT noise level is 2% before and 5% after water breakthrough
c	Aqui, considero que o water breakthough começa quando WCUT > 0.01
c
	real tempo_wcut(Nmed_wcut),tempo_bhp(Nmed_bhp),
     1tempo_gor(Nmed_gor), wcut_obs(Nmed_wcut,Npocos),
     1bhp_obs(Nmed_bhp,Npocos), gor_obs(Nmed_gor,Npocos), 
     1por(n_prop*n_l), inf_por(n_prop*n_l), sup_por(n_prop*n_l)

	character*10 shutin_dates(28),yymmdd_bhp(Nmed_bhp),
     1yymmdd_wcut(Nmed_wcut), yymmdd_gor(Nmed_gor)

	integer aux	
c
	real noise
	

	open(43,file='C:\temp_flavio001\MODEL_RUIDO_WCT.DAT',
	1status='unknown')
	open(44,file='C:\temp_flavio001\MODEL_RUIDO_BHP.DAT',
	1status='unknown')
	open(45,file='C:\temp_flavio001\MODEL_RUIDO_GOR.DAT',
	1status='unknown')

!	noise=0.3
	idum=-iseed
	do ii=1, Nmed_wcut
		do kk=1, Npocos
c			do tt=1, 28
c			if(yymmdd_wcut(ii).NE.shutin_dates(tt)) then
					
				if(wcut_obs(ii,kk).LE.0.01) then
					
				wcut_obs(ii,kk)=wcut_obs(ii,kk)+0.02*gasdev(idum)*
	1			wcut_obs(ii,kk)
				
				endif
				if(wcut_obs(ii,kk).GT.0.01) then
				wcut_obs(ii,kk)=wcut_obs(ii,kk)+0.05*gasdev(idum)*
	1			wcut_obs(ii,kk)
				endif
				

C			endif
c			if(yymmdd_wcut(ii).EQ.shutin_dates(tt)) then
c				wcut_obs(ii,kk)=0.0
c		
c			endif
c			enddo
		enddo
	enddo
	
	do ii=1, Nmed_wcut
		write(43,2001) (wcut_obs(ii,kk),kk=1,Npocos)
2001		format(6(1x,e12.5))
	enddo	
c
c	According to the PUNQ project, the noise level in the SHUTIN pressure
c	PARA o BHP...
c	is 1bar, whilst the noise level in the flowing pressure is 3bar
c	(1.0bar=100 kPa)
c
	idum=-iseed+1
	do ii=1, Nmed_bhp
		
		do kk=1, Npocos
		aux=1
			do tt=1,28
				if(aux.EQ.1)then

					if(yymmdd_bhp(ii).EQ.shutin_dates(tt)) then
					bhp_obs(ii,kk)=bhp_obs(ii,kk) +sign(100,idum)
						if(bhp_obs(ii,kk).LT.0.0)then
							bhp_obs(ii,kk)=abs(bhp_obs(ii,kk))
						endif
					!noise*gasdev(idum)*
	1				!bhp_obs(ii,kk) 
										
											
					endif
				
					if(yymmdd_bhp(ii).NE.shutin_dates(tt)) then
					bhp_obs(ii,kk)=bhp_obs(ii,kk) +sign(300,idum)
						if(bhp_obs(ii,kk).LT.0.0)then
							bhp_obs(ii,kk)=abs(bhp_obs(ii,kk))
						endif
					!noise*gasdev(idum)*
	1				!bhp_obs(ii,kk)
					
										 
						endif
				aux=0
					endif
		
			enddo
		enddo
	enddo
	
	do ii=1, Nmed_bhp
		write(44,2000) (bhp_obs(ii,kk),kk=1,Npocos)
2000		format(6(2x,f14.4))
	enddo	

c
c	PARA o GOR...
c	The noise level for the GOR is 10% before and 25% after gas breakthrough
	idum=-iseed+2
	do ii=1, Nmed_gor
		do kk=1, Npocos
C			do tt=1,28
C				if(yymmdd_gor(ii).NE.shutin_dates(tt)) then
					gor_obs(ii,kk)=gor_obs(ii,kk) +
     1				0.25*gasdev(idum)*gor_obs(ii,kk)

C				endif
C				if(yymmdd_gor(ii).EQ.shutin_dates(tt)) then
C					gor_obs(ii,kk)=0.
C				endif
C			enddo
		enddo
	enddo

	do ii=1, Nmed_gor
		write(45,2000) (gor_obs(ii,kk),kk=1,Npocos)

	enddo	
	CLOSE(43)
	CLOSE(44)
	CLOSE(45)
	
	
c**********************
c	contaminando o campo de porosidades verdadeiro para gerar o 
c	primeiro modelo do COMPLEX
c	
c	Alterado por Flavio em 15/07/2004, para gerar o primeiro modelo. De forma que
c	a porosidade de cada camada seja agora a media dos blocos que possuem pocos.
c	Cada bloco perfurado com poco teve o seu valor de porosidade fixado como verdadeiro.  
	
	open(40,file='C:\temp_flavio001\POR_INICIAL.DAT',
	1status='unknown')
	open(41,file='C:\temp_flavio001\PERMX_INICIAL.DAT',
	1status='unknown')
	do ii=1, n_l

C	PONTO MEDIO ENTRE INF E SUP PERTURBADO DE UM RUIDO		
c	por(ii)=(sup_por(ii)-inf_por(ii))/2+inf_por(ii)
c	por(ii+n_l)=(sup_por(ii+n_l)-inf_por(ii+n_l))/2+inf_por(ii+n_l)

		if(por(ii).EQ.0.0)then
			goto 15
		else

c	MEDIA DOS VALORES DOS BLOCOS ATRAVESSADOS POR POCOS*********
			if(ii.LE.140)then
				por(ii)= 0.16213
				por(ii+n_l)= 385.5
			endif
			if(ii.GT.140.and.ii.LE.280)then
				por(ii)=0.0813
				por(ii+n_l)=33.852
			endif
			if(ii.GT.280.and.ii.LE.420)then
				por(ii)=0.16053
				por(ii+n_l)= 300.17
			endif
			if(ii.GT.420.and.ii.LE.560)then
				por(ii)=0.16215
				por(ii+n_l)=331.4
			endif
			if(ii.GT.560.and.ii.LE.700)then
				por(ii)=0.2327
				por(ii+n_l)=651.63
			endif
********************************************************************

			por(ii)=por(ii)*(1 + ruido*(ran(iseed)-0.5))
			por(ii+n_l)=por(ii+n_l)*(1+ruido*(ran(iseed)-0.5))
		
		
c		if(por(ii).LT.inf_por(ii))then
c			por(ii)=inf_por(ii)
c		endif
c		
c		if(por(ii).GT.sup_por(ii))then
c			por(ii)=sup_por(ii)
c		endif
c
c		if(por(ii+n_l).LT.inf_por(ii+n_l))then
c			por(ii+n_l)=inf_por(ii+n_l)
c		endif
c
c		if(por(ii+n_l).GT.sup_por(ii+n_l))then
c			por(ii+n_l)=sup_por(ii+n_l)
c		endif
			
15	endif

	enddo

c	ATRIBUINDO OS VALORES VERDADEIROS DE POROSIDADE PARA OS BLOCOS COM POCOS

	por(58)= 0.09016
	por(198)=0.098
	por(338)=0.09114
	por(478)=0.196
	por(618)=0.2254

	por(59)= 0.1568
	por(199)=0.098
	por(339)=0.1274
	por(479)=0.1274

	por(85)= 0.1666
	por(225)=0.049
	por(365)=0.0784
	por(505)=0.1372
	por(645)=0.2352

	por(105)=0.0588
	por(245)=0.0882
	por(385)=0.1176
	por(525)=0.1568
	por(665)=0.2548

	por(109)=0.2744
	por(249)=0.0882
	por(389)=0.1666
	por(529)=0.1568

	por(116)=0.0588
	por(256)=0.0686
	por(396)=0.1862
	por(536)=0.1078

	por(n_l+58)=29.0178
	por(n_l+198)=68.6784
	por(n_l+338)=75.4992
	por(n_l+478)=234.906
	por(n_l+618)=722.064

	por(n_l+59)=567.616
	por(n_l+199)=23.814
	por(n_l+339)=227.556
	por(n_l+479)=144.942

	por(n_l+85)=239.512
	por(n_l+225)=15.4546
	por(n_l+365)=48.8824
	por(n_l+505)=247.548
	por(n_l+645)=704.424

	por(n_l+105)=12.7792
	por(n_l+245)=69.9622
	por(n_l+385)=337.022
	por(n_l+525)=474.712
	por(n_l+665)=583.198

	por(n_l+109)=756.07
	por(n_l+249)=34.1628
	por(n_l+389)=261.758
	por(n_l+529)=379.652
	
	por(n_l+116)=18.6494
	por(n_l+256)=25.1762
	por(n_l+396)=517.636
	por(n_l+536)=153.762
	
	do ii=1, n_l
		write(40,*) por(ii)
		write(41,*) por(ii+n_l)
	
	enddo

	close(40)
	close(41)

	return
	end