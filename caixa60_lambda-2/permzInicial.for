c	ESTE PROGRAMA RETIRA OS VALORES DE POROSIDADE E PERMEABILIDADE DOS
C	BLOCOS NULOS DO RESERVATORIO, DANDO COMO SAIDA ARQUIVOS ONDE ESSES
C	VALORES NAO ESTAO PRESENTES

c	CALCULA TAMBEM A DIFERENCA ENTRE O MODELO VERDADEIRO E O MELHOR MODELO
C	DO COMPLEX, SALVANDO ESSA DIFERENCA EM ARQUIVOS

	real por(700), por_naonulo(488)		!verdadeiro
	real permh(700), permh_naonulo(488)	!verdadeiro
	real permz(700), permz_naonulo(488)	!verdadeiro

	real porcal(700), por_naonulocal(488)		!calculado
	real permhcal(700), permh_naonulocal(488)	!calculado	
	real permzcal(700), permz_naonulocal(488)	!calculado

	real sum_por,sum_permh,sum_permz,sum_porcal,
	1sum_permhcal,sum_permzcal

	real media_por,media_permh,media_permz,media_porcal,
	1media_permhcal,media_permzcal

	real r_por,r_permh,r_permz

	real aux_por,aux1_por,aux_permh,aux1_permh,aux_permz,aux1_permz

	real dif_por(700), sum_pormenos, sum_pormais
	real media_pormenos, media_pormais
	integer cont_pormenos, cont_pormais 

	real dif_permh(700), sum_permhmenos, sum_permhmais
	real media_permhmenos, media_permhmais
	integer cont_permhmenos, cont_permhmais

	real dif_permz(700), sum_permzmenos, sum_permzmais
	real media_permzmenos, media_permzmais
	integer cont_permzmenos, cont_permzmais


	open(1,file='poro_truth.dat',status='unknown')
	open(2,file='permx_truth.dat',status='unknown')
	open(3,file='permz_truth.dat',status='unknown')

	open(4,file='poromelhor.dat',status='unknown')
	open(5,file='permxmelhor.dat',status='unknown')
	open(6,file='permzmelhor.dat',status='unknown')


	cont=1
	do i=1, 700
		read(1,*) por(i)
		read(2,*) permh(i)
		read(3,*) permz(i)
		
		read(4,*) porcal(i)
		read(5,*) permhcal(i)
		read(6,*) permzcal(i)

		if(permh(i).NE.0.0)then
			por_naonulo(cont)=por(i)
			permh_naonulo(cont)=permh(i)
			permz_naonulo(cont)=permz(i)
			por_naonulocal(cont)=porcal(i)
			permh_naonulocal(cont)=permhcal(i)
			permz_naonulocal(cont)=permzcal(i)
			cont=cont+1
		endif
	enddo

	
	close(1)
	close(2)
	close(3)
	close(4)
	close(5)
	close(6)

c	CALCULO DA DIFERENCA ENTRE O MODELO VERDADEIRO E O MELHOR MODELO
		
	do i=1, 700
		
		dif_por(i)=abs(por(i)-porcal(i))
		dif_permh(i)=abs(permh(i)-permhcal(i))
		dif_permz(i)=abs(permz(i)-permzcal(i))
	
	enddo



	open(7,file='poro_Truth_X_Melhor.dat',status='unknown')
	open(8,file='permh_Truth_X_Melhor.dat',status='unknown')
	open(9,file='permz_Truth_X_melhor.dat',status='unknown')

	write(7,*) 'cont ',' poro_truth	 ', 'poro_melhor'
	write(8,*) 'cont ',' permh_truth ', 'permh_melhor'
	write(9,*) 'cont ',' permz_truth ', 'permz_melhor'

	
	
	do i=1, 488
		write(7,*) 1, por_naonulo(i), por_naonulocal(i)
		write(8,*) 2, permh_naonulo(i), permh_naonulocal(i)
		write(9,*) 3, permz_naonulo(i), permz_naonulocal(i)
	enddo


	close(7)
	close(8)
	close(9)

C	do i=1, 488
C		
C		dif_por(i)=(por_naonulocal(i)-por_naonulo(i))/por_naonulo(i)
C		dif_permh(i)=(permh_naonulocal(i)-permh_naonulo(i))/
C	1	permh_naonulo(i)
C		dif_permz(i)=(permz_naonulocal(i)-permz_naonulo(i))/
C	1	permz_naonulo(i)
C	enddo
C
C	cont_pormenos=0
C	cont_pormais=0
C	sum_pormenos=0.0
C	sum_pormais=0.0
C
C	do i=1, 488
C		if(dif_por(i).LT.0.0)then
C			sum_pormenos=sum_pormenos+abs(dif_por(i))
C			cont_pormenos=cont_pormenos+1
C		endif
C		if(dif_por(i).GT.0.0)then
C			sum_pormais=sum_pormais+(dif_por(i))
C			cont_pormais=cont_pormais+1
C		endif
C	enddo
C		media_pormenos=sum_pormenos/cont_pormenos
C		media_pormenos=-media_pormenos
C		media_pormais=sum_pormais/cont_pormais
C	
C	cont_permhmenos=0
C	cont_permhmais=0
C	sum_permhmenos=0.0
C	sum_permhmais=0.0
C
C	do i=1, 488
C		if(dif_permh(i).LT.0.0)then
C			sum_permhmenos=sum_permhmenos+abs(dif_permh(i))
C			cont_permhmenos=cont_permhmenos+1
C		endif
C		if(dif_permh(i).GT.0.0)then
C			sum_permhmais=sum_permhmais+(dif_permh(i))
C			cont_permhmais=cont_permhmais+1
C		endif
C	enddo
C		media_permhmenos=sum_permhmenos/cont_permhmenos
C		media_permhmenos=-media_permhmenos
C		media_permhmais=sum_permhmais/cont_permhmais
C
C	cont_permzmenos=0
C	cont_permzmais=0
C	sum_permzmenos=0.0
C	sum_permzmais=0.0
CC
C	do i=1, 488
C		if(dif_permz(i).LT.0.0)then
C			sum_permzmenos=sum_permzmenos+abs(dif_permz(i))
C			cont_permzmenos=cont_permzmenos+1
C		endif
C		if(dif_permz(i).GT.0.0)then
C			sum_permzmais=sum_permzmais+(dif_permz(i))
C			cont_permzmais=cont_permzmais+1
C		endif
C	enddo
C		media_permzmenos=sum_permzmenos/cont_permzmenos
C		media_permzmenos=-media_permzmenos
C		media_permzmais=sum_permzmais/cont_permzmais
C
	open(10,file='dif_poro.dat',status='unknown')
	open(11,file='dif_permh.dat',status='unknown')
	open(12,file='dif_permz.dat',status='unknown')

	do i=1, 700
		write(10,*) dif_por(i)
		write(11,*) dif_permh(i)
		write(12,*) dif_permz(i)
	enddo
	
	close(10)
	close(11)
	close(12)

	stop
	end
